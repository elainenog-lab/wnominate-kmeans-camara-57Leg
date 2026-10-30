#Criar a tabela dados_agrupados com os dados do TSE e deputados da Camara

# Criar a tabela 'tse_deputados' com cruzamento seguro e sem perda de parlamentares
tse_deputados <- deputados %>%
  inner_join(
    tse_cand_deputados_2022,
    by = c("nomeCivil_limpo" = "NM_CANDIDATO_LIMPO")
  )


###############################
library(dplyr)
library(tibble)
library(stringr)

coordenadas_limpo <- coordenadas %>%
  rownames_to_column(var = "nome_original") %>%
  mutate(
    # Extrai a sigla do partido de dentro dos parênteses
    partido = str_extract(nome_original, "(?<=\\()[^)]+(?=\\))"),
    
    # Remove o partido entre parênteses para manter apenas o nome
    nome_limpo = str_remove(nome_original, "\\s*\\([^)]*\\)"),
    
    # Padroniza o nome (maiúsculas, sem acentos, etc.)
    nome_limpo = limpar_texto(nome_limpo)
  ) %>%
  # Opcional: remove a coluna original
  dplyr::select(-nome_original)

# Cruzar com a sua tabela 'dados_agrupados' ou 'tse_deputados'
# Realizar o cruzamento dos dataframes
dados_agrupados <- coordenadas_limpo %>%
  inner_join(
    tse_deputados,
    #by = c("nome_limpo" = "nome_limpo", "partido" = "SG_PARTIDO")
    by = "nome_limpo"
  )

df <- as.data.frame(dados_agrupados)
# Exportando para CSV
write.csv(df, "dados_agregados_v9.csv", row.names = FALSE, fileEncoding = "UTF-8")

