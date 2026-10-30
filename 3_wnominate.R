library(pscl)
library(wnominate)

dados_com_codigos <- dados_filtrados %>%
  mutate(voto_num = case_when(
    voto == "Sim" ~ 1,
    voto == "Não" ~ 0,
    TRUE ~ 9
  ))

matriz_votos_num <- dados_com_codigos %>%
  # Cria a coluna combinada diretamente com o partido_predominante que já existe
  mutate(deputado_partido = paste0(deputado_nome, " (", partido_predominante, ")")) %>%
  group_by(deputado_partido, idVotacao) %>%
  summarise(voto_num = min(voto_num), .groups = "drop") %>%
  pivot_wider(
    names_from = idVotacao,
    values_from = voto_num,
    values_fill = NA  # Código para ausência/não votou
  ) %>%
  column_to_rownames("deputado_partido") %>%
  as.matrix()

#legis_data <- legis_data[rownames(matriz_votos_num), , drop = FALSE]

rc <- rollcall(
  data = matriz_votos_num,
  yea = 1,
  nay = 0,
  missing = NA,
  notInLeg = 9,
  legis.names = rownames(matriz_votos_num),
  #legis.data = legis_data,
  desc = "Votações da Câmara dos Deputados"
)

deputado_esq <- "Afonso Florence (PT)"
deputado_dir <- "Abilio Brunini (PL)"
# 2 dimensoes
resultado_2D <- wnominate(
  rc, 
  dims = 2, 
  polarity = c(deputado_dir, deputado_esq),
  lop = 0.10,
  minvotes = 25 # Ignora deputados com menos de 25 votos registrados
)
# 3. Visualizar o mapa espacial da Câmara
plot(resultado_2D)
summary(resultado_2D)

# 3 dimensoes
deputado_esq <- "Afonso Florence (PT)"
deputado_dir <- "Abilio Brunini (PL)"
deputado_inf <- "Adail Filho (REPUBLICANOS)"

resultado_3D <- wnominate(
  rc, 
  dims = 3, 
  polarity = c(deputado_dir, deputado_esq, deputado_inf),
  lop = 0.10,
  minvotes = 25 # Ignora deputados com menos de 25 votos registrados
)

plot(resultado_3D)
summary(resultado_3D)

