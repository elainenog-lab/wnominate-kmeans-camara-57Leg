library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

# Extração e preparação das coordenadas
coordenadas_deputados <- resultado_2D$legislators %>%
  rownames_to_column(var = "deputado_partido") %>%
  dplyr::select(deputado_partido, coord1D, coord2D) %>%
  drop_na() %>%
  mutate(partido = str_extract(deputado_partido, "(?<=\\().+?(?=\\))"))

# Seleção dos partidos desejados
partidos_selecionados <- c(
  "PT", "PSOL", "PV", "PDT", "REDE", "PSB", "PCdoB", 
  "REPUBLICANOS", "PSDB", "AVANTE", "MDB", "UNIÃO", "MISSÃO",
  "PROS/SOLIDARIEDADE","PSD", "PSC/PODE", "PTB/PATRIOTA/PRD",
  "CIDADANIA", "PP", "PL", "NOVO"
)

# Filtrar os partidos selecionados
df_facet <- coordenadas_deputados %>%
  filter(partido %in% partidos_selecionados)

# Mapeamento manual de cores (usando a coluna original 'partido' como chave de cor)
cores_partidos <- c(
  "PT"                 = "red",
  "PSOL"               = "darkred",
  "PV"                 = "lightgreen",
  "PDT"                = "yellow",
  "REDE"               = "darkgreen",
  "PSD"                = "red",
  "NOVO"               = "orange",
  "REPUBLICANOS"       = "green",
  "PSDB"               = "pink",
  "AVANTE"             = "darkgrey",
  "MDB"                = "black",
  "UNIÃO"              = "purple",
  "CIDADANIA"          = "brown",
  "PP"                 = "blue",
  "PL"                 = "blue",
  "PROS/SOLIDARIEDADE" = "khaki4",
  "PSB"                = "firebrick",
  "PSC/PODE"           = "cyan4",
  "PCdoB"              = "magenta4",
  "PTB/PATRIOTA/PRD"   = "sienna",
  "MISSÃO"             = "coral"
)

# Construção do gráfico facetado
ggplot(df_facet, aes(x = coord1D, y = coord2D, color = partido)) +
  # Pontos coloridos de cada partido
  geom_point(alpha = 0.8, size = 2) +
  
  # Eixos de referência (linhas pontilhadas em zero)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
  
  # Aplicação da escala de cores customizada
  scale_color_manual(values = cores_partidos) +
  
  # Limites dos eixos
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  
  # Facetas usando a coluna 'partido' (sem a contagem)
  facet_wrap(~ partido, ncol = 4) +
  
  # Rótulos dos eixos e títulos
  labs(
    x = "1ª Dimensão",
    y = "2ª Dimensão"
  ) +
  
  # Estilo visual
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold")
  )
