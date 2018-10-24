rm(list = ls())

suppressWarnings(dir.create("data"))

# 1. Pacotes --------------------------------------------------------------

library(magrittr) # %>% 

# 2. Banco - Vagas --------------------------------------------------------

temp_dir <- tempdir()
temp_file <- tempfile()

download.file("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_2018.zip",
              destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

brasil_path <- list.files(temp_dir, pattern = "\\BRASIL.csv", full.names = TRUE)

vagas <- readr::read_csv2(brasil_path)

vagas <- vagas %>% 
  dplyr::select(SG_UF, CD_CARGO, DS_CARGO, QT_VAGAS) %>% 
  dplyr::mutate(DS_CARGO = stringr::str_to_upper(DS_CARGO))

readr::write_rds(vagas, "data/vagas.rds")

# 3. Banco - Votos --------------------------------------------------------

temp_dir <- tempdir()
temp_file <- tempfile()

u0 <- "http://www.tse.jus.br/hotsites/pesquisas-eleitorais/resultados_anos/votacao/votacao_secao_eleitoral_2018.html"

links_votacao <- xml2::read_html(u0) %>% 
  rvest::xml_nodes(xpath = "//div[@class='prepend-3 span-17']/p/a|//div[@class='prepend-3 span-19']/p/a") %>% 
  rvest::xml_nodes(xpath = "//a") %>% 
  rvest::html_attr("href") %>% 
  .[38:64]

for(link in links_votacao){
  download.file(link, temp_file)
  unzip(temp_file, exdir = temp_dir)
}

# Agregando resultados por uf

secao_path <- list.files("~/Downloads/backup/", pattern = "votacao_secao_2018_[A-Z]{2}\\.csv", full.names = TRUE)
banco_ls <- vector("list", length = length(secao_path))

temp_dir <- tempdir(check = T)

dir.create(paste0(temp_dir,"/data_parsed"))

for(i in seq_along(banco_ls)){
  readr::read_csv2(secao_path[[i]], locale = readr::locale(encoding = "ISO-8859-1")) %>% 
    dplyr::group_by(ANO_ELEICAO, NR_TURNO, SG_UF, CD_CARGO, DS_CARGO, NR_VOTAVEL) %>% 
    dplyr::summarise(QT_VOTOS = sum(QT_VOTOS)) %>% 
    readr::write_rds(paste0(temp_dir,"/data_parsed/",i,".rds"))
}

files_parsed_path <- list.files(paste0(temp_dir, "/data_parsed"), full.names = TRUE)

banco <- purrr::map(files_parsed_path, readr::read_rds)

banco <- dplyr::bind_rows(banco)

readr::write_rds(banco, "data/resultado_2018.rds")


# 3. Coligação ------------------------------------------------------------

temp_file <- tempfile()
temp_dir <- tempdir()

u0 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_coligacao/consulta_coligacao_2018.zip"

download.file(u0, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

files_path <- list.files(temp_dir, 
                         pattern = "consulta_coligacao_2018_[A-Z]{2}\\.csv",
                         full.names = TRUE)

coligacao_ls <- purrr::map(files_path, readr::read_csv2, locale = readr::locale(encoding = "ISO-8859-1"))

coligacao_df <- dplyr::bind_rows(coligacao_ls)

readr::write_rds(coligacao_df, "data/coligacao.rds")


# 4. Candidatos -----------------------------------------------------------

temp_file <- tempfile()
temp_dir <- tempdir()

u0 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2018.zip"

download.file(u0, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

files_path <- list.files(temp_dir, 
                         pattern = "consulta_cand_2018_[A-Z]{2}\\.csv",
                         full.names = TRUE)

cand_ls <- purrr::map(files_path, readr::read_csv2, locale = readr::locale(encoding = "ISO-8859-1"))

cand_df <- dplyr::bind_rows(cand_ls)

readr::write_rds(cand_df, "data/candidatos.rds")
