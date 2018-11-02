rm(list = ls())

library(magrittr)
library(tidyverse)

# 1. template vagas -------------------------------------------------------

vagas <- readr::read_rds("data/vagas.rds")

vagas <- vagas %>% 
  dplyr::filter(CD_CARGO == 6)

# 2. template resultado ---------------------------------------------------

resultado <- readr::read_rds("data/resultado_2018.rds")

resultado <- resultado %>% 
  dplyr::filter(CD_CARGO == 6) %>% 
  dplyr::mutate(TIPO_VOTO = ifelse(NR_VOTAVEL %/% 100 > 1, "NOMINAL", "LEGENDA"))

coligacao_df <- readr::read_rds("data/coligacao.rds")

cand_df <- readr::read_rds("data/candidatos.rds")

coligacao_df <- coligacao_df %>% 
  filter(SQ_COLIGACAO %in% unique(cand_df$SQ_COLIGACAO))

purrr::map(list(coligacao_df, cand_df, resultado,vagas), ungroup) %>% 
  purrr::map(count, SG_UF)

coligacao_df <- coligacao_df %>% 
  dplyr::filter(CD_CARGO == 6) %>% 
  dplyr::select(ANO_ELEICAO, NR_TURNO, CD_CARGO, SG_UF, TP_AGREMIACAO, SG_PARTIDO, NR_PARTIDO, NM_COLIGACAO, DS_COMPOSICAO_COLIGACAO, SQ_COLIGACAO) %>% 
  dplyr::mutate(NM_COLIGACAO = ifelse(NM_COLIGACAO == "#NULO#", SG_PARTIDO, NM_COLIGACAO),
                DS_COMPOSICAO_COLIGACAO = stringr::str_split(DS_COMPOSICAO_COLIGACAO, "/"),
                DS_COMPOSICAO_COLIGACAO = purrr::map(DS_COMPOSICAO_COLIGACAO, stringr::str_trim))

coligacao_df <- unique(coligacao_df)

#Candidatos votáveis por coligação 
lista_colig_valida_df<-cand_df %>% filter(CD_CARGO==6) %>% 
  select(SG_UF, NR_PARTIDO, SQ_COLIGACAO) %>% distinct()

coligacao_df <- coligacao_df %>% 
  filter(SQ_COLIGACAO %in% unique(lista_colig_valida_df$SQ_COLIGACAO))

## 2.1. votos validos por lista (legenda + nominal)
resultado <- resultado %>% 
  dplyr::mutate(NR_PARTIDO = stringr::str_sub(NR_VOTAVEL,1,2),
                NR_PARTIDO = as.numeric(NR_PARTIDO)) %>% 
  dplyr::filter(!(NR_PARTIDO %in% c(95,96,97)))


template_total <- resultado %>% 
  dplyr::left_join(coligacao_df) %>% 
  dplyr::group_by(ANO_ELEICAO, NR_TURNO, SG_UF, CD_CARGO, SQ_COLIGACAO) %>% 
  dplyr::summarise(TIPO_VOTO  = list(TIPO_VOTO),
                   NR_VOTAVEL = list(NR_VOTAVEL),
                   QT_VOTOS   = list(QT_VOTOS),
                   CANDIDATOS = list(tibble::tibble(TIPO_VOTO = unlist(TIPO_VOTO),
                                                    NR_VOTAVEL = unlist(NR_VOTAVEL),
                                                    QT_VOTOS = unlist(QT_VOTOS)))) %>% 
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
# 3.1 Regras 2014 -----------------------------------------------------------------------
# Sem barreira dos 10% e sem divisão com partidos que não atingiram o QE

for(i in seq_along(vagas$SG_UF)){
  print(i)
  x <- template_total %>% 
    filter(SG_UF == vagas$SG_UF[i],
           QUO_PARTIDARIO >= 1)
  j <- 1
  
  while(sum(x$CADEIRAS) < vagas$QT_VAGAS[i]){
    print(j)
    x <- x %>% 
      mutate(media    = TOTAL_VOTOS/(CADEIRAS + 1),
             vencedor = 0,
             vencedor = ifelse(media == max(.$media),1,0),
             CADEIRAS = CADEIRAS + vencedor)

    j <- j + 1
  }
  if(i == 1){
    banco_r1 <- x
  } else {
      banco_r1 <- rbind(banco_r1, x)
  }
  rm(x)
}

sum(banco_r1$CADEIRAS)

check <- banco_r1 %>% 
  group_by(SG_UF, QT_VAGAS) %>%
  summarise(eleitos_UFC=sum(CADEIRAS)) %>% 
  mutate(dif=QT_VAGAS-eleitos_UFC) %>%
  arrange(SG_UF)

banco_r1 <- banco_r1 %>%
  select(SG_UF, SQ_COLIGACAO, CADEIRAS) %>% 
  rename(CADEIRAS_r1 = CADEIRAS)

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
    x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)
    j=j+1
  }
  if(i==1){banco_r2<-x}else{banco_r2<-rbind(banco_r2, x)}
  rm(x)
}
sum(banco_r2$CADEIRAS)

banco_r2<-banco_r2 %>%
  select(SG_UF, SQ_COLIGACAO, CADEIRAS) %>%
  rename(CADEIRAS_r2=CADEIRAS)

#3.3 Regras 2014 com barreira dos 10% -----------------------------------------------------------------------
#Com barreira dos 10% e sem divisão com partidos que não atingiram o QE

for(i in seq_along(vagas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==vagas$SG_UF[i])
  x<- x  %>% filter(QUO_PARTIDARIO>=1)
  k=1
  while(sum(x$CADEIRAS_rBarreira)<vagas$QT_VAGAS[i]){
    print(k)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS_rBarreira+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse((dense_rank(desc(.$media))==1)&((N_ELEGIVEIS-CADEIRAS_rBarreira)>0),1,0))
    #em caso de não haver vencedores, vamos distribuir a cadeira até ter um vencedor
    j=2
    while(sum(x$vencedor)==0){
      x<-x %>% mutate(vencedor=ifelse((dense_rank(desc(.$media))==j)&((N_ELEGIVEIS-CADEIRAS_rBarreira)>0),1,0))
      j=j+1
      }
    x<-x %>% mutate(CADEIRAS_rBarreira = CADEIRAS_rBarreira + vencedor)
    k=k+1
  }
  if(i==1){banco_r3<-x}else{banco_r3<-rbind(banco_r3, x)}
  rm(x)
}
sum(banco_r3$CADEIRAS_rBarreira)

banco_r3 <- banco_r3 %>%
  select(SG_UF, SQ_COLIGACAO, CADEIRAS_rBarreira) %>%
  rename(CADEIRAS_r3=CADEIRAS_rBarreira)

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
    x<-x %>% 
      mutate(CADEIRAS_rBarreira = CADEIRAS_rBarreira + vencedor)
    k=k+1
  }
  if(i==1){banco_r4<-x}else{banco_r4<-rbind(banco_r4, x)}
  rm(x)
}
sum(banco_r4$CADEIRAS_rBarreira)

banco_r4 <- banco_r4 %>%
  select(SG_UF, SQ_COLIGACAO, CADEIRAS_rBarreira) %>%
  rename(CADEIRAS_r4=CADEIRAS_rBarreira)

# 4. Retornar eleitos para banco de coligações --------------------------------------------

template_total <- template_total %>% 
  left_join(banco_r1, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","SQ_COLIGACAO")) %>% 
  left_join(banco_r2, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","SQ_COLIGACAO")) %>% 
  left_join(banco_r3, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","SQ_COLIGACAO")) %>% 
  left_join(banco_r4, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","SQ_COLIGACAO"))

sum(template_total$CADEIRAS_r1, na.rm = TRUE)
sum(template_total$CADEIRAS_r2, na.rm = TRUE)
sum(template_total$CADEIRAS_r3, na.rm = TRUE)
sum(template_total$CADEIRAS_r4, na.rm = TRUE)

analise <- template_total %>% 
  group_by(SQ_COLIGACAO, SG_UF) %>% 
  summarise_at(c("CADEIRAS_r1", "CADEIRAS_r2", "CADEIRAS_r3", "CADEIRAS_r4"), funs(sum)) %>% 
  modify_at(c("CADEIRAS_r1", "CADEIRAS_r2", "CADEIRAS_r3", "CADEIRAS_r4"), ~ifelse(is.na(.), 0, .))

# 5. Banco de Eleitos - v.Gabi

votos_cand <- resultado %>% 
  select(SG_UF, NR_VOTAVEL, QT_VOTOS) %>% 
  rename(NR_CANDIDATO = NR_VOTAVEL) %>%
  ungroup() %>%
  select(-NR_TURNO)

chave_join <- colnames(votos_cand[1:5])

template_candidato <- cand_df %>% 
  filter(CD_CARGO==6) %>% 
  select(SG_UF, ANO_ELEICAO, CD_CARGO, DS_CARGO, NR_CANDIDATO,
         NM_CANDIDATO, DS_SITUACAO_CANDIDATURA, NR_IDADE_DATA_POSSE,
         NM_COLIGACAO, SQ_COLIGACAO, NR_PARTIDO, NM_PARTIDO) %>% 
  left_join(votos_cand, by = chave_join) %>%
  group_by(SG_UF, SQ_COLIGACAO) %>% 
  arrange(desc(QT_VOTOS), desc(NR_IDADE_DATA_POSSE)) %>% 
  #importante, só vamos considerar quem está com a candidatura apta
  #filter(DS_SITUACAO_CANDIDATURA=="APTO") %>% 
  mutate(ranking_colig = 1,
         ranking_colig = cumsum(ranking_colig))

#juntar as cadeiras obtidas por cada coligação
template_candidato <- template_candidato %>% 
  left_join(analise) %>% 
  mutate(eleito_r1 = ifelse(ranking_colig <= CADEIRAS_r1, 1, 0)) %>% 
  mutate(eleito_r2 = ifelse(ranking_colig <= CADEIRAS_r2, 1, 0)) %>% 
  mutate(eleito_r3 = ifelse(ranking_colig <= CADEIRAS_r3, 1, 0)) %>% 
  mutate(eleito_r4 = ifelse(ranking_colig <= CADEIRAS_r4, 1, 0))

template_candidato %>% 
  count(SQ_COLIGACAO, ranking_colig) %>% 
  filter(n > 1)

sum(template_candidato$eleito_r1, na.rm = T)
sum(template_candidato$eleito_r2, na.rm = T)
sum(template_candidato$eleito_r3, na.rm = T)
sum(template_candidato$eleito_r4, na.rm = T)

# 6. Banco de bancada 

template_partidos <- template_candidato %>%
  group_by(NR_PARTIDO, NM_PARTIDO) %>% 
  summarise_at(c("eleito_r1", "eleito_r2", "eleito_r3", "eleito_r4"), funs(sum(.,na.rm = T)))

template_partidos_UF <- template_candidato %>%
  group_by(NR_PARTIDO, NM_PARTIDO, SG_UF) %>% 
  summarise_at(c("eleito_r1", "eleito_r2", "eleito_r3", "eleito_r4"), funs(sum(.,na.rm = T)))

<<<<<<< HEAD:script_calc.R
temp_RN<-template_total %>% filter(SG_UF=="RN")
=======
PSL <- template_partidos_UF %>% 
  filter(NR_PARTIDO == 17)

write_csv(template_candidato, 'candidados_eleitos.csv')
>>>>>>> fb27971313a424e8fdd73205631ec9b621da0074:02_script_calc.R
