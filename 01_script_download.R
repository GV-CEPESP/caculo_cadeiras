rm(list = ls())

suppressWarnings(dir.create("data"))
suppressWarnings(dir.create("raw_data"))
suppressWarnings(dir.create("raw_data/vagas"))
suppressWarnings(dir.create("raw_data/coligacao"))
suppressWarnings(dir.create("raw_data/votos_secao"))
suppressWarnings(dir.create("raw_data/candidatos"))

# 1. Pacotes --------------------------------------------------------------

library(magrittr) # %>% 

# 2. Banco - Vagas --------------------------------------------------------

temp_dir <- "raw_data/vagas/"
temp_file <- "raw_data/vagas/vagas.zip"

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

<<<<<<< HEAD
temp_dir <- "raw_data/votos_secao/"
=======
temp_dir <- tempdir()
temp_file <- tempfile()
print(temp_dir)
>>>>>>> b956dc2a465ba307f5f520dd5c47a73ce7feb8eb

u0 <- "http://www.tse.jus.br/hotsites/pesquisas-eleitorais/resultados_anos/votacao/votacao_secao_eleitoral_2018.html"

links_votacao <- xml2::read_html(u0) %>% 
  rvest::xml_nodes(xpath = "//div[@class='prepend-3 span-17']/p/a|//div[@class='prepend-3 span-19']/p/a") %>% 
  rvest::xml_nodes(xpath = "//a") %>% 
  rvest::html_attr("href") %>% 
  .[38:64]

for(link in links_votacao){
  nome <- stringr::str_extract(link, "votacao_secao_2018_[A-Z]{2}.zip")
  download.file(link, paste0(temp_dir,nome))
}

# Agregando resultados por uf

<<<<<<< HEAD
zip_path <- list.files(temp_dir, pattern = "votacao_secao_2018_[A-Z]{2}\\.zip", full.names = TRUE)
=======
secao_path <- list.files(temp_dir, pattern = "votacao_secao_2018_[A-Z]{2}\\.csv", full.names = TRUE)
banco_ls <- vector("list", length = length(secao_path))
>>>>>>> b956dc2a465ba307f5f520dd5c47a73ce7feb8eb

purrr::walk(zip_path, unzip, exdir = temp_dir)

secao_path <- list.files(temp_dir, pattern = "votacao_secao_2018_[A-Z]{2}\\.csv", full.names = TRUE)

dir.create(paste0(temp_dir,"/data_parsed"))

for(i in seq_along(secao_path)){
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

temp_file <- "raw_data/coligacao/coligacao.zip"
temp_dir <- "raw_data/coligacao/"

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

temp_file <- "raw_data/candidatos/candidatos.zip"
temp_dir <- "raw_data/candidatos/"

u0 <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2018.zip"

download.file(u0, destfile = temp_file)

unzip(temp_file, exdir = temp_dir)

files_path <- list.files(temp_dir, 
                         pattern = "consulta_cand_2018_[A-Z]{2}\\.csv",
                         full.names = TRUE)

cand_ls <- purrr::map(files_path, readr::read_csv2, locale = readr::locale(encoding = "ISO-8859-1"))

cand_df <- dplyr::bind_rows(cand_ls)

readr::write_rds(cand_df, "data/candidatos.rds")
