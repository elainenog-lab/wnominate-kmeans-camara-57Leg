# Manter dados Sim e Não
dados_limpos <- dados %>% 
  filter(voto %in% c("Sim", "Não"))

tabela_agrupada <- dados_limpos %>%
  group_by(idVotacao, voto) %>%
  summarise(quantidade = n(), .groups = "drop")

tabela_proporcoes <- dados_limpos %>%
  group_by(idVotacao) %>%
  summarise(
    total_votos = n(),
    prop_sim = sum(voto == "Sim") / total_votos,
    prop_nao = sum(voto == "Não") / total_votos
  ) %>%
  ungroup()

dados_filtrados <- dados_limpos
unique(dados_filtrados$deputado_siglaPartido)

dados_filtrados <- dados_filtrados %>%
  mutate(deputado_siglaPartido = case_when(
    deputado_siglaPartido %in% c("PTB", "PATRIOTA", "PRD") ~ "PTB/PATRIOTA/PRD",
    deputado_siglaPartido %in% c("PROS", "SOLIDARIEDADE")  ~ "PROS/SOLIDARIEDADE",
    deputado_siglaPartido %in% c("PSC", "PODE")           ~ "PSC/PODE",
    TRUE ~ deputado_siglaPartido # Mantém os outros partidos como estão
  ))

#correção de nomes
dados_filtrados <- dados_filtrados %>%
  mutate(deputado_nome = case_when(
    deputado_nome %in% c("Josivaldo JP", "Josivaldo Jp") ~ "Josivaldo JP",
    #deputado_nome  %in% c("Ivan Junior", "Ivan Júnior")  ~ "Ivan Junior",
    TRUE ~ deputado_nome # Mantém os outros nomes como estão
  ))

partido_mais_frequente <- dados_filtrados %>%
  group_by(deputado_nome) %>%
  # Calcula qual partido aparece mais vezes na base para aquele deputado
  summarise(partido_predominante = names(which.max(table(deputado_siglaPartido))), .groups = "drop")

# Adiciona a coluna partido_predominante à tabela dados_filtrados
dados_filtrados <- dados_filtrados %>%
  left_join(partido_mais_frequente, by = "deputado_nome")

