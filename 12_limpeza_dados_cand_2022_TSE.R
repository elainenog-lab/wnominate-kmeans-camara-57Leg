library(dplyr)
library(stringr)
library(stringi)
library(lubridate)

# Função de limpeza e padronização de nomes dos candidados 2022 do TSE
# Base de dados consulta_cand_2022_BRASIL.csv obtida de https://dadosabertos.tse.jus.br
tse_cand_deputados_2022 <- read.csv(
  "data/consulta_cand_2022_BRASIL.csv",
  stringsAsFactors = FALSE,
  sep = ";",
  fileEncoding = "latin1"
)

# Filtrar apenas para Deputados Federais 
tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
  filter(DS_CARGO == "DEPUTADO FEDERAL")


# Lista de nomes a serem removidos (em maiúsculas para comparação segura)
#nomes_para_remover <- c(
#  "JULIO CESAR COSTA LIMA" #versão entregue
#  ,"IGOR ALVES BACELAR"
#  ,"PEDRO PAULO DE SOUZA"
#  ) # remover porque tem o mesmo nome de urna

#tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
##  # Remove se o NM_CANDIDATO estiverem na lista de exclusão
#  filter(
#    !(NM_CANDIDATO) %in% nomes_para_remover)

#havia dois candidatos homônimos
nomes_urna_para_remover <- c("CARLINHOS RODRIGUES")
tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
# Remove se o NM_CANDIDATO estiverem na lista de exclusão
  filter(
    !(NM_URNA_CANDIDATO) %in% nomes_urna_para_remover)


# receitas de deputados
tse_cand_deputados_2022_receitas <- read.csv("data/receitas_candidatos_2022_BRASIL.csv",
                                             stringsAsFactors = FALSE,
                                             sep = ";",
                                             fileEncoding = "latin1"
)

# Tratamento e soma das doações por candidato (SQ_CANDIDATO)
tse_cand_deputados_2022_receitas_sumarizadas <- tse_cand_deputados_2022_receitas %>%
  # Trata o valor numérico (troca vírgula por ponto)
  mutate(
    VR_RECEITA = ifelse(str_detect(VR_RECEITA, "#|^$"), "0", VR_RECEITA),
    VR_RECEITA = str_replace(VR_RECEITA, ",", "."),
    VR_RECEITA = as.numeric(VR_RECEITA),
    VR_RECEITA = coalesce(VR_RECEITA, 0),
    SQ_CANDIDATO = as.character(SQ_CANDIDATO)
  ) %>%
  # Agrupa por candidato e calcula o total de doações/receitas recebidas
  group_by(SQ_CANDIDATO) %>%
  summarise(
    SOMA_DOACOES_CAMPANHA = sum(VR_RECEITA, na.rm = TRUE),
    .groups = "drop"
  )

tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
  mutate(SQ_CANDIDATO = as.character(SQ_CANDIDATO)) %>%
  left_join(
    tse_cand_deputados_2022_receitas_sumarizadas%>%
      mutate(SQ_CANDIDATO = as.character(SQ_CANDIDATO)) %>%
      # Mantém a chave SQ_CANDIDATO necessária para o join
      dplyr::select(SQ_CANDIDATO, SOMA_DOACOES_CAMPANHA),
    by = "SQ_CANDIDATO"
  )

#deputados_bens
tse_cand_deputados_2022_bens <- read.csv(
  "data/bem_candidato_2022_BRASIL.csv",
  stringsAsFactors = FALSE,
  sep = ";",
  fileEncoding = "latin1"
)

# Ajusta o campo trocando vírgula por ponto e converte para numeric
tse_cand_deputados_2022_bens <- tse_cand_deputados_2022_bens %>%
  mutate(
    # Trata valores textuais do TSE ou em branco trocando por "0"
    VR_BEM_CANDIDATO = ifelse(str_detect(VR_BEM_CANDIDATO, "#|^$"), "0", VR_BEM_CANDIDATO),
    # Substitui a vírgula pelo ponto
    VR_BEM_CANDIDATO = str_replace(VR_BEM_CANDIDATO, ",", "."),
    # Converte para numérico
    VR_BEM_CANDIDATO = as.numeric(VR_BEM_CANDIDATO),
    # Garante 0 para NAs residuais
    VR_BEM_CANDIDATO = coalesce(VR_BEM_CANDIDATO, 0)
  )

# Faz o agrupamento e a soma dos bens por candidato
tse_cand_deputados_2022_bens_sumarizados <- tse_cand_deputados_2022_bens %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(
    SOMA_VR_BEM_CANDIDATO = sum(VR_BEM_CANDIDATO, na.rm = TRUE),
    .groups = "drop"
  )

# Realiza o join trazendo a soma dos bens para a tabela principal de deputados/candidatos
tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
  # Garantir tipo character para evitar falha no cruzamento
  mutate(SQ_CANDIDATO = as.character(SQ_CANDIDATO)) %>%
  left_join(
    tse_cand_deputados_2022_bens_sumarizados %>%
      mutate(SQ_CANDIDATO = as.character(SQ_CANDIDATO)) %>%
      # Mantém a chave SQ_CANDIDATO necessária para o join
      dplyr::select(SQ_CANDIDATO, SOMA_VR_BEM_CANDIDATO),
    by = "SQ_CANDIDATO"
  )

#Correçao de nomes para ficar igual a deputados da Camara de deputados
tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
  mutate(
    NM_CANDIDATO = case_when(
      NM_CANDIDATO %in% c("JOSÉ AUGUSTO PUPIO REIS JÚNIOR") ~ "JOSÉ AUGUSTO PUPPIO REIS JÚNIOR",
      NM_CANDIDATO %in% c("GUILHERME ARISTOTELES UCHOA CAVALCANTI PESSOA DE MELO JUNIOR") ~ "GUILHERME A. UCHOA CAVALCANTI PESSOA DE MELO JUNIOR",
      NM_CANDIDATO %in% c("LUIZ PHILIPPE DE ORLEANS E BRAGANÇA") ~ "LUIZ PHILIPPE DE ORLEANS BRAGANÇA",
      NM_CANDIDATO %in% c("EMANUEL PINHEIRO DA SILVA PRIMO TEIXEIRA") ~ "EMANUEL PINHEIRO DA SILVA PRIMO",
      NM_CANDIDATO %in% c("MÁRIO SÍLVIO MENDES NEGROMONTE JÚNIOR") ~ "MARIO S. M. NEGROMONTE JR.",
      NM_CANDIDATO %in% c("LUIZ ANTONIO DA COSTA CARVALHO CORREA DA SILVA") ~ "LUIZ ANTONIO C. C. CORRÊA DA SILVA",
      NM_CANDIDATO %in% c("PAULO ROBERTO FOLETTO") ~ "PAULO ROBERTO FOLLETTO",
      NM_CANDIDATO %in% c("CARLA SIMARA LUCIANA DA SILVA SALASÁRIO AYRES") ~ "CARLA SIMARA LUCIANA DA SILVA AYRES",
      NM_CANDIDATO %in% c("CRISTIANE LOPES DA LUZ BENARROSH") ~ "CRISTIANE LOPES DA LUZ",
      NM_CANDIDATO %in% c("ELIANE DE SOUSA ALVES MACHADO") ~ "ELIANE DE SOUSA ALVES",
      NM_CANDIDATO %in% c("FABIANO LOPES FERREIRA") ~ "FABIANO LOPES FERREIRA CAZECA",
      NM_CANDIDATO %in% c("JOSÉ LOURENÇO BONFIM JÚNIOR") ~ "JOSE LOURENÇO BOMFIM JUNIOR",
      NM_CANDIDATO %in% c("PATRUS ANANIAS DE SOUSA") ~ "PATRUS ANANIAS DE SOUZA",
      NM_CANDIDATO %in% c("PEDRO HENRIQUE DE ANDRADE LIMA CARNEIRO") ~ "CAMPOS	PEDRO HENRIQUE DE A. L. C. CAMPOS",
      NM_CANDIDATO %in% c("RENATA HELLMEISTER DE ABREU MELO") ~ "RENATA HELLMEISTER DE ABREU",
      NM_CANDIDATO %in% c("SIMONE APARECIDA CURRALADAS DOS SANTOS") ~ "SIMONE APARECIDA MARQUETO CURRALADAS",
      NM_CANDIDATO %in% c("JOSÉ ESTEPHAN BARBARY FILHO") ~ "JOSÉ ESTEPHAN AMORIM BARBARY FILHO",
      NM_CANDIDATO %in% c("DAIZE MICHELE DE AGUIAR GONÇALVES") ~ "DAÍZE MICHELE DE AGUIAR GONÇALVES COLLINS",
      NM_CANDIDATO %in% c("CARLOS ROBERTO COELHO DE MATTOS JUNIOR") ~ "Carlos Roberto Jordy Coelho de Mattos",
      NM_CANDIDATO %in% c("PEDRO HENRIQUE DE ANDRADE LIMA CARNEIRO CAMPOS") ~ "PEDRO HENRIQUE DE A. L. C. CAMPOS",
      NM_CANDIDATO %in% c("MAURO IVAN FARIAS DE SANTIAGO JUNIOR") ~ "MAURO IVAN F. SANTIAGO JR",
      NM_CANDIDATO %in% c("SÔNIA BONE DE SOUSA SILVA SANTOS") ~ "SONIA BONE DE SOUSA SILVA",
      
      
      
      TRUE ~ NM_CANDIDATO
    )
  )

tse_cand_deputados_2022 <- tse_cand_deputados_2022 %>%
  mutate(
    NM_CANDIDATO_LIMPO = limpar_texto(NM_CANDIDATO),
    NM_URNA_CANDIDATO_LIMPO = limpar_texto(NM_URNA_CANDIDATO)
  )

