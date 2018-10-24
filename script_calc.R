rm(list = ls())

library(magrittr)

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
                QUO_PARTIDARIO     = TOTAL_VOTOS/QUO_ELEITORAL)

template_total$N_ELEGIVEIS <- NA

for(i in seq_along(template_total$CANDIDATOS)){
  barreira <- as.integer(template_total$QUO_ELEITORAL[[i]] * 0.10)
  n_elegiveis <- sum(template_total$CANDIDATOS[[i]]$QT_VOTOS > barreira)
  template_total$N_ELEGIVEIS[[i]] <- n_elegiveis
}
