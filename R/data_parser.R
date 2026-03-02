#' Parser de Dados com Cabeçalho Customizado
#' 
#' Lê um arquivo Excel/CSV onde as 3 primeiras linhas são:
#' 1. ID / Código da Questão
#' 2. Enunciado da Questão
#' 3. Tipo de Variável com a tag [TYPE]
#' A partir da 4ª linha, são os dados reais.
#'
#' @param file_path Caminho do arquivo a ser lido
#' @param ext Extensão do arquivo ("xlsx", "xls" ou "csv")
#' @return Uma lista estruturada contendo o dicionário (metadata) e os dados limpos (data)
parse_custom_data <- function(file_path, ext = "xlsx") {
  library(readxl)
  library(readr)
  library(dplyr)
  library(shiny)
  
  # Leitura isolada do cabeçalho de 3 linhas e dos dados corporais
  if (ext %in% c("xls", "xlsx")) {
    raw_head <- read_excel(file_path, col_names = FALSE, n_max = 3)
    raw_data <- read_excel(file_path, col_names = FALSE, skip = 3)
  } else if (ext == "csv") {
    raw_head <- read_csv(file_path, col_names = FALSE, n_max = 3, show_col_types = FALSE)
    raw_data <- read_csv(file_path, col_names = FALSE, skip = 3, show_col_types = FALSE)
  } else {
    stop("Extensão de arquivo não suportada para este parser.")
  }
  
  # Tratamento do cabeçalho
  ids <- as.character(raw_head[1, ])
  enunciados <- as.character(raw_head[2, ])
  tipos <- as.character(raw_head[3, ])
  
  # VALIDAÇÃO CRUCIAL NO SHINY
  # Impede a execução silenciosa ou erros catastróficos, emitindo um erro amigável ao usuário.
  shiny::validate(
    shiny::need(
      any(grepl("\\[TYPE\\]", tipos, ignore.case = TRUE)),
      "ERRO DE LEITURA: A tag [TYPE] não foi encontrada na linha 3 do arquivo. Verifique se o documento segue o padrão de 3 linhas de metadados antes da tabela de dados."
    )
  )
  
  # Estruturação e retorno
  metadata <- data.frame(
    id_questao = ids,
    enunciado = enunciados,
    tipo = tipos,
    stringsAsFactors = FALSE
  )
  
  dados_reais <- raw_data
  colnames(dados_reais) <- ids
  
  return(list(
    metadata = metadata,
    data = readr::type_convert(dados_reais)
  ))
}
