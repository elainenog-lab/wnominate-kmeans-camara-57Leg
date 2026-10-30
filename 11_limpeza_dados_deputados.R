library(dplyr)
library(stringr)
library(stringi)
library(lubridate)

# Função de limpeza e padronização de nomes dos dados da camara

limpar_texto <- function(texto) {
  texto %>%
    stri_trans_general("Latin-ASCII") %>% # 1. Remove acentos e Ç (ex: "BRAGANÇA" -> "BRAGANCA")
    str_to_upper() %>%                    # 2. Converte para maiúsculas
    str_squish() # 3. Remove espaços extras
}

# Ler o arquivo especificando o encoding Latin-1 e separador ";"
deputados <- read.csv("data/deputados_todos.csv", stringsAsFactors = FALSE, sep = ";")

# Correção de nomes e filtro de legislatura
deputados <- deputados %>%
  mutate(
    nome = case_when(
      nome %in% c("Missionária Michele Collins") ~ "Michele Collins",
      TRUE ~ nome
    )
  ) %>%
#Mantém a da ultima legislatura
  filter(
    idLegislaturaFinal == 57
  )

deputados <- deputados %>%
  mutate(
    nomeCivil_limpo = limpar_texto(nomeCivil),
    nome_limpo = limpar_texto(nome)
  )

