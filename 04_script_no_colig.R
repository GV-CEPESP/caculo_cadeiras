rm(list = ls())

library(magrittr)
library(tidyverse)

# 1. template vagas -------------------------------------------------------

vagas <- readr::read_rds("data/vagas.rds")

vagas <- vagas %>% 
  dplyr::filter(CD_CARGO == 6)

# 2. template resultado ---------------------------------------------------

#Vamos usar a base munzona porque teremos apenas os votos que foram considerados
#Primeiro os candidatos
resultado <- readr::read_rds("data/munzona.rds")

resultado <- resultado %>% 
  dplyr::filter(CD_CARGO == 6) 

resultado <- resultado %>% 
  dplyr::mutate(TIPO_VOTO = "NOMINAL") %>% 
  select(ANO_ELEICAO, SG_UF, CD_CARGO, DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO, 
         NM_CANDIDATO, DS_SITUACAO_CANDIDATURA, TP_AGREMIACAO, NR_PARTIDO, 
         SG_PARTIDO,NM_PARTIDO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO, 
         QT_VOTOS_NOMINAIS, TIPO_VOTO) 

resultado <- resultado %>% group_by(ANO_ELEICAO, SG_UF, CD_CARGO, DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO, 
                                    NM_CANDIDATO, DS_SITUACAO_CANDIDATURA, TP_AGREMIACAO, NR_PARTIDO, 
                                    SG_PARTIDO,NM_PARTIDO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO, TIPO_VOTO) %>% 
  summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS, na.rm = T)) %>% ungroup()%>% 
  rename(QT_VOTOS = QT_VOTOS_NOMINAIS)

#Depois os votos por partido
resultado_partido <- readr::read_rds("data/munzona_partido.rds")

resultado_partido <- resultado_partido %>% 
  dplyr::filter(CD_CARGO == 6) 

resultado_partido <- resultado_partido %>% 
  mutate( TIPO_VOTO =  "LEGENDA") %>% 
  select(ANO_ELEICAO, SG_UF, CD_CARGO, DS_CARGO, 
         TP_AGREMIACAO, NR_PARTIDO, 
         SG_PARTIDO,NM_PARTIDO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, 
         QT_VOTOS_LEGENDA, TIPO_VOTO) %>% 
  group_by(ANO_ELEICAO, SG_UF, CD_CARGO, DS_CARGO, TP_AGREMIACAO, NR_PARTIDO, 
           SG_PARTIDO,NM_PARTIDO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, TIPO_VOTO) %>% 
  summarise(QT_VOTOS_LEGENDA = sum(QT_VOTOS_LEGENDA, na.rm = T)) %>% 
  ungroup() %>% 
  rename(QT_VOTOS = QT_VOTOS_LEGENDA)

Missing <- setdiff(colnames(resultado), colnames(resultado_partido))  # Find names of missing columns
resultado_partido[Missing] <- NA                    # Add them, filled with '0's

#Juntas as bases e ajusta
resultado<-rbind(resultado, resultado_partido)
rm(resultado_partido)

resultado<- resultado %>% rename(NR_VOTAVEL = NR_CANDIDATO) %>% 
  mutate(NR_VOTAVEL = ifelse(is.na(NR_VOTAVEL), NR_PARTIDO, NR_VOTAVEL)) %>% 
  select(-DS_COMPOSICAO_COLIGACAO)

resultado$eleito<-ifelse(str_detect(resultado$DS_SIT_TOT_TURNO, "ELEITO POR"),1,0)

cand_df <- readr::read_rds("data/candidatos.rds") %>% filter(CD_CARGO==6)

#Candidatos votáveis por partido 
lista_partido_valida_df<-cand_df %>% filter(CD_CARGO==6) %>% 
  select(SG_UF, NR_PARTIDO, SQ_COLIGACAO) %>% distinct()

resultado <- resultado %>% 
  filter(SQ_COLIGACAO %in% unique(lista_partido_valida_df$SQ_COLIGACAO))

## 2.1. votos validos por lista (legenda + nominal)
resultado <- resultado %>% 
  dplyr::mutate(NR_PARTIDO = stringr::str_sub(NR_VOTAVEL,1,2),
                NR_PARTIDO = as.numeric(NR_PARTIDO)) %>% 
  dplyr::filter(!(NR_PARTIDO %in% c(95,96,97)))

## Idade - critério de desempate 
cand_perfil<- cand_df %>% select(SG_UF, NR_CANDIDATO, SQ_CANDIDATO, NR_IDADE_DATA_POSSE)

template_total <- resultado %>% 
  mutate(QT_VOTOS=as.integer(QT_VOTOS)) %>% 
  left_join(cand_perfil) %>% 
  arrange(desc(QT_VOTOS)) %>% 
  dplyr::group_by(ANO_ELEICAO, SG_UF, CD_CARGO, NR_PARTIDO) %>% 
  dplyr::summarise(TIPO_VOTO  = list(TIPO_VOTO),
                   NR_VOTAVEL = list(NR_VOTAVEL),
                   QT_VOTOS   = list(QT_VOTOS),
                   CANDIDATOS = list(tibble::tibble(TIPO_VOTO = unlist(TIPO_VOTO),
                                                    NR_VOTAVEL = unlist(NR_VOTAVEL),
                                                    QT_VOTOS = unlist(QT_VOTOS), 
                                                    IDADE = unlist(NR_IDADE_DATA_POSSE)))) %>% 
  dplyr::select(-NR_VOTAVEL,-QT_VOTOS,-TIPO_VOTO) %>% 
  dplyr::left_join(vagas) %>% 
  dplyr::mutate(TOTAL_VOTOS        = purrr::map_int(CANDIDATOS, ~sum(.$QT_VOTOS)),
                QUO_ELEITORAL      = round(sum(TOTAL_VOTOS) / QT_VAGAS,0),
                QUO_PARTIDARIO     = TOTAL_VOTOS/QUO_ELEITORAL,
                CADEIRAS           = floor(TOTAL_VOTOS/QUO_ELEITORAL))

## 2.2. Número de Elegíveis (regra dos 10%)
template_total$N_ELEGIVEIS <- NA

for(i in seq_along(template_total$CANDIDATOS)){
  barreira <- as.integer(template_total$QUO_ELEITORAL[[i]] * 0.10)
  n_elegiveis <- sum(template_total$CANDIDATOS[[i]]$QT_VOTOS > barreira &
                       template_total$CANDIDATOS[[i]]$TIPO_VOTO == 'NOMINAL' )
  template_total$N_ELEGIVEIS[[i]] <- n_elegiveis
}

#Se o número de elegíveis for maior que o de cadeiras, o partido já vai perder as cadeiras pela regra nova
template_total <- template_total %>%
  mutate(CADEIRAS_rBarreira= ifelse(N_ELEGIVEIS<CADEIRAS, N_ELEGIVEIS, CADEIRAS))

# 3. Cálculo de quantidade de eleitos por coligação em cada regra------------------------

# 3.2 Regras 2014 incluindo nanicos na partilha-----------------------------------------------------------------------
# Sem barreira dos 10% e com divisão com partidos que não atingiram o QE

for(i in seq_along(vagas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==vagas$SG_UF[i])
  j=1
  while(sum(x$CADEIRAS)<vagas$QT_VAGAS[i]){
    print(j)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse(media==max(.$media),1,0))
    x$idade<- 0
    
    #em caso de empate, vamos dar a cadeira pra o candidato mais velho
    if (sum(x$vencedor)>1){
        for(n in seq_along(x$CANDIDATOS)){
          idade <- as.integer(x$CANDIDATOS[[n]]$IDADE[(x$CADEIRAS[[n]])+1])
          
          if(is.na(idade)){
            idade <- as.integer(x$CANDIDATOS[[n]]$IDADE[(x$CADEIRAS[[n]])+2])}
          
          else{x$idade[[n]]<-idade}}
        
        x<- x %>% mutate(vencedor = ifelse(idade==max((.$idade)*(.$vencedor)),1,0))
        x<- x %>% mutate(CADEIRAS = CADEIRAS + vencedor)} 
    else{x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)}
    j=j+1}
  if(i==1){banco_r2<-x}else{banco_r2<-rbind(banco_r2, x)}
  rm(x)
}
sum(banco_r2$CADEIRAS)

banco_r2<-banco_r2 %>% ungroup() %>% 
  select(SG_UF, NR_PARTIDO, CADEIRAS) %>%
  rename(CADEIRAS_r2_excolig=CADEIRAS)

# 3.4. Contrafactual ------------------------------------------------------

#Com barreira dos 10% e com divisão com partidos que não atingiram o QE
for(i in seq_along(vagas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==vagas$SG_UF[i])
  k=1
  while(sum(x$CADEIRAS_rBarreira)<vagas$QT_VAGAS[i]){
    print(k)
    x<-x %>% 
      mutate(media=TOTAL_VOTOS/(CADEIRAS_rBarreira+1))
    x<-x %>% 
      mutate(vencedor=0)
    x<-x %>% 
      mutate(vencedor=ifelse((dense_rank(desc(.$media))==1)&((N_ELEGIVEIS-CADEIRAS_rBarreira)>0),1,0))
    #em caso de não haver vencedores, vamos distribuir a cadeira até ter um vencedor
    j=2
    while(sum(x$vencedor)==0){
      x<-x %>%
        mutate(vencedor=ifelse((dense_rank(desc(.$media))==j)&((N_ELEGIVEIS-CADEIRAS_rBarreira)>0),1,0))
      j=j+1
    }
    #em caso de empate, vamos dar a cadeira pra o candidato mais velho
    if (sum(x$vencedor)>1){
      for(n in seq_along(x$CANDIDATOS)){
        idade <- as.integer(x$CANDIDATOS[[n]]$IDADE[(x$CADEIRAS_rBarreira[[n]])+1])
        
        if(is.na(idade)){
          idade <- as.integer(x$CANDIDATOS[[n]]$IDADE[(x$CADEIRAS_rBarreira[[n]])+2])}
        
        else{x$idade[[n]]<-idade}}
      
      x<- x %>% mutate(vencedor = ifelse(idade==max((.$idade)*(.$vencedor)),1,0))
      x<- x %>% mutate(CADEIRAS_rBarreira = CADEIRAS_rBarreira + vencedor)} 
    else{x<-x %>% mutate(CADEIRAS_rBarreira = CADEIRAS_rBarreira + vencedor)}
    k=k+1
  }
  if(i==1){banco_r4<-x}else{banco_r4<-rbind(banco_r4, x)}
  rm(x)
}
sum(banco_r4$CADEIRAS_rBarreira)

banco_r4 <- banco_r4 %>% ungroup() %>% 
  select(SG_UF, NR_PARTIDO, CADEIRAS_rBarreira) %>%
  rename(CADEIRAS_r4_excolig=CADEIRAS_rBarreira)

# 4. Retornar eleitos para banco de coligações --------------------------------------------

template_total <- template_total %>% 
  left_join(banco_r2) %>% 
  left_join(banco_r4)

sum(template_total$CADEIRAS_r2_excolig, na.rm = TRUE)
sum(template_total$CADEIRAS_r4_excolig, na.rm = TRUE)

analise <- template_total %>% 
  group_by(NR_PARTIDO, SG_UF) %>% 
  select(CADEIRAS_r2_excolig, CADEIRAS_r4_excolig)

#5. Banco de Eleitos ----------------------------------------------------
votos_cand <- resultado %>% 
  select(SG_UF, NR_VOTAVEL, DS_SIT_TOT_TURNO, eleito, QT_VOTOS) %>% 
  rename(NR_CANDIDATO = NR_VOTAVEL) %>%
  ungroup()

template_candidato <- cand_df %>% 
  filter(CD_CARGO==6) %>% 
  select(SG_UF, ANO_ELEICAO, CD_CARGO, DS_CARGO, NR_CANDIDATO,
         NM_CANDIDATO, DS_SITUACAO_CANDIDATURA, NR_IDADE_DATA_POSSE,
         NM_COLIGACAO, SQ_COLIGACAO, NR_PARTIDO, NM_PARTIDO) %>% 
  left_join(votos_cand) %>%
  group_by(SG_UF, NR_PARTIDO) %>% 
  arrange(desc(QT_VOTOS), desc(NR_IDADE_DATA_POSSE)) %>% 
  #importante, só vamos considerar quem está com a candidatura apta
  filter(DS_SITUACAO_CANDIDATURA=="APTO") %>% 
  mutate(ranking_colig = 1,
         ranking_colig = cumsum(ranking_colig)) %>% 
  distinct()

#juntar as cadeiras obtidas por cada coligação
template_candidato <- template_candidato %>% 
  left_join(analise) %>% 
  group_by(SG_UF) %>% 
  mutate(eleito_r2_excolig = ifelse(ranking_colig <= CADEIRAS_r2_excolig, 1, 0)) %>% 
  mutate(eleito_r4_excolig = ifelse(ranking_colig <= CADEIRAS_r4_excolig, 1, 0))

template_candidato %>% 
  count(SQ_COLIGACAO, ranking_colig) %>% 
  filter(n > 1)

sum(template_candidato$eleito_r2_excolig, na.rm = T)
sum(template_candidato$eleito_r4_excolig, na.rm = T)

write_csv(template_candidato, 'candidados_eleitos_sem_colig.csv')
