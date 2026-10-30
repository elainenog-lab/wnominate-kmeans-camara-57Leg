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

# Mapeamento manual das cores desejadas por partido
cores_partidos <- c(
  "PT"                 = "red",
  "PSOL"               = "darkred",
  "PV"                 = "lightgreen",
  "PDT"                = "gold",
  "REDE"               = "darkgreen",
  "PSD"                = "salmon",        # Ajustado para diferenciar do PT
  "NOVO"               = "orange",
  "REPUBLICANOS"       = "green",
  "PSDB"               = "pink",
  "AVANTE"             = "darkgrey",
  "MDB"                = "black",
  "UNIÃO"              = "purple",
  "CIDADANIA"          = "brown",
  "PP"                 = "blue",
  "PL"                 = "navy",          # Ajustado para diferenciar do PP
  "PROS/SOLIDARIEDADE" = "khaki4",
  "PSB"                = "firebrick",
  "PSC/PODE"           = "cyan4",
  "PCdoB"              = "magenta4",
  "PTB/PATRIOTA/PRD"   = "sienna",
  "MISSÃO"             = "coral"          # Adicionado para incluir todos os selecionados
)

# Filtrar o dataframe para os partidos mapeados
df_unico <- coordenadas_deputados %>%
  filter(partido %in% names(cores_partidos))

# Construção do gráfico único
ggplot(df_unico, aes(x = coord1D, y = coord2D, color = partido)) +
  # Linhas pontilhadas centrais de referência (origem 0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  
  # Pontos coloridos de cada partido
  geom_point(alpha = 0.7, size = 2) +
  
  # Aplicação da escala de cores customizada
  scale_color_manual(values = cores_partidos, name = "Partido") +
  
  # Limites dos eixos W-NOMINATE (-1 a 1)
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  
  # Rótulos dos eixos e títulos
  labs(
    x = "1ª Dimensão",
    y = "2ª Dimensão",
    #title = "Distribuição Espacial W-NOMINATE por Partido - Câmara dos Deputados",
    #subtitle = paste0("Total de deputados analisados: ", nrow(df_unico))
  ) +
  
  # Estilo visual acadêmico e posição da legenda
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

