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

coligacao_df <- coligacao_df %>% 
  dplyr::filter(CD_CARGO == 6) %>% 
  dplyr::select(ANO_ELEICAO, NR_TURNO, CD_CARGO, SG_UF, TP_AGREMIACAO, SG_PARTIDO, NR_PARTIDO, NM_COLIGACAO, DS_COMPOSICAO_COLIGACAO) %>% 
  dplyr::mutate(NM_COLIGACAO = ifelse(NM_COLIGACAO == "#NULO#", SG_PARTIDO, NM_COLIGACAO),
                DS_COMPOSICAO_COLIGACAO = stringr::str_split(DS_COMPOSICAO_COLIGACAO, "/"),
                DS_COMPOSICAO_COLIGACAO = purrr::map(DS_COMPOSICAO_COLIGACAO, stringr::str_trim))

coligacao_df <- unique(coligacao_df)

## 2.1. votos validos por lista (legenda + nominal)
resultado <- resultado %>% 
  dplyr::mutate(NR_PARTIDO = stringr::str_sub(NR_VOTAVEL,1,2),
                NR_PARTIDO = as.numeric(NR_PARTIDO)) %>% 
  dplyr::filter(!(NR_PARTIDO %in% c(95,96,97)))

template_total <- resultado %>% 
  dplyr::left_join(coligacao_df) %>% 
  dplyr::group_by(ANO_ELEICAO, NR_TURNO, SG_UF, CD_CARGO, NM_COLIGACAO) %>% 
  dplyr::summarise(TIPO_VOTO  = list(TIPO_VOTO),
                   NR_VOTAVEL = list(NR_VOTAVEL),
                   QT_VOTOS   = list(QT_VOTOS),
                   CANDIDATOS = list(tibble::tibble(TIPO_VOTO = unlist(TIPO_VOTO),
                                                    NR_VOTAVEL = unlist(NR_VOTAVEL),
                                                    QT_VOTOS = unlist(QT_VOTOS)))) %>% 
  dplyr::select(-NR_VOTAVEL,-QT_VOTOS,-TIPO_VOTO) %>% 
  dplyr::left_join(vagas) %>% 
  dplyr::mutate(TOTAL_VOTOS        = purrr::map_int(CANDIDATOS, ~sum(.$QT_VOTOS)),
                QUO_ELEITORAL      = sum(TOTAL_VOTOS) / QT_VAGAS,
                QUO_PARTIDARIO     = TOTAL_VOTOS/QUO_ELEITORAL,
                CADEIRAS = floor(TOTAL_VOTOS/QUO_ELEITORAL))

template_total$N_ELEGIVEIS <- NA

for(i in seq_along(template_total$CANDIDATOS)){
  barreira <- as.integer(template_total$QUO_ELEITORAL[[i]] * 0.10)
  n_elegiveis <- sum(template_total$CANDIDATOS[[i]]$QT_VOTOS > barreira)
  template_total$N_ELEGIVEIS[[i]] <- n_elegiveis
}

sobras<-template_total %>% group_by(SG_UF, QT_VAGAS) %>% summarise(eleitos_UFC=sum(CADEIRAS)) %>% 
  mutate(dif=QT_VAGAS-eleitos_UFC) %>% arrange(SG_UF)

rodadas<-sobras[c("SG_UF","dif")]

#Vamos calcular o resultado pelas regras eleitorais 2014
#Sem barreira dos 10% e sem divisão com partidos que não atingiram o QE

for(i in seq_along(rodadas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==rodadas$SG_UF[i])
  x<- x  %>% filter(QUO_ELEITORAL>=1)
  for (rodada in 1:rodadas$dif[i]){
    print(rodada)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse(media==max(.$media),1,0))
    x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)
  }
  if(i==1){banco_r1<-x}else{banco_r1<-rbind(banco_r1, x)}
  rm(x)
}
sum(banco_r1$CADEIRAS)

check<-banco_r1 %>% group_by(SG_UF, QT_VAGAS) %>% summarise(eleitos_UFC=sum(CADEIRAS)) %>% 
  mutate(dif=QT_VAGAS-eleitos_UFC) %>% arrange(SG_UF)

#3. Cálculo de quantidade de eleitos por coligação em cada regra------------------------
#3.1 Regras 2014 -----------------------------------------------------------------------
#Sem barreira dos 10% e sem divisão com partidos que não atingiram o QE

for(i in seq_along(rodadas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==rodadas$SG_UF[i])
  x<- x  %>% filter(QUO_ELEITORAL>=1)
  for (rodada in 1:rodadas$dif[i]){
    print(rodada)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse(media==max(.$media),1,0))
    x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)
  }
  if(i==1){banco_r1<-x}else{banco_r1<-rbind(banco_r1, x)}
  rm(x)
}
sum(banco_r1$CADEIRAS)

check<-banco_r1 %>% group_by(SG_UF, QT_VAGAS) %>% summarise(eleitos_UFC=sum(CADEIRAS)) %>% 
  mutate(dif=QT_VAGAS-eleitos_UFC) %>% arrange(SG_UF)
banco_r1<-banco_r1 %>% select(SG_UF, NM_COLIGACAO, CADEIRAS) %>% rename(CADEIRAS_r1=CADEIRAS)

#3.2 Regras 2014 incluindo nanicos na partilha-----------------------------------------------------------------------
#Sem barreira dos 10% e com divisão com partidos que não atingiram o QE

for(i in seq_along(rodadas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==rodadas$SG_UF[i])
  for (rodada in 1:rodadas$dif[i]){
    print(rodada)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse(media==max(.$media),1,0))
    x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)
  }
  if(i==1){banco_r2<-x}else{banco_r2<-rbind(banco_r2, x)}
  rm(x)
}
sum(banco_r2$CADEIRAS)
banco_r2<-banco_r2 %>% select(SG_UF, NM_COLIGACAO, CADEIRAS) %>% rename(CADEIRAS_r2=CADEIRAS)

#3.3 Regras 2018 -----------------------------------------------------------------------
#Com barreira dos 10% e com divisão com partidos que não atingiram o QE

for(i in seq_along(rodadas$SG_UF)){
  print(i)
  x<-filter(template_total, SG_UF==rodadas$SG_UF[i])
  for (rodada in 1:rodadas$dif[i]){
    print(rodada)
    x<-x %>% mutate(media=TOTAL_VOTOS/(CADEIRAS+1))
    x<-x %>% mutate(vencedor=0)
    x<-x %>% mutate(vencedor=ifelse((dense_rank(desc(.$media))==1)&((N_ELEGIVEIS-CADEIRAS)>0),1,0))
    #em caso de não haver vencedores, vamos distribuir a cadeira até ter um vencedor
    j=2
    while(sum(x$vencedor)==0){
      x<-x %>% mutate(vencedor=ifelse((dense_rank(desc(.$media))==j)&((N_ELEGIVEIS-CADEIRAS)>0),1,0))
      j=j+1
      }
    x<-x %>% mutate(CADEIRAS = CADEIRAS + vencedor)
  }
  if(i==1){banco_r3<-x}else{banco_r3<-rbind(banco_r3, x)}
  rm(x)
}
sum(banco_r3$CADEIRAS)
banco_r3<-banco_r3 %>% select(SG_UF, NM_COLIGACAO, CADEIRAS) %>% rename(CADEIRAS_r3=CADEIRAS)

#4. Retornar eleitos para banco de coligações --------------------------------------------
template_total<-left_join(template_total, banco_r1, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","NM_COLIGACAO"))
template_total<-left_join(template_total, banco_r2, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","NM_COLIGACAO"))
template_total<-left_join(template_total, banco_r3, by = c("ANO_ELEICAO","NR_TURNO","CD_CARGO","SG_UF","NM_COLIGACAO"))

analise<-template_total %>% group_by(NM_COLIGACAO) %>% summarise_at(c("CADEIRAS_r1", "CADEIRAS_r2", "CADEIRAS_r3"), funs(sum))