library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# Extrair as coordenadas de todos os deputados do resultado W-NOMINATE
df_coordenadas <- resultado_2D$legislators %>%
  rownames_to_column(var = "deputado_partido") %>%
  dplyr::select(deputado_partido, coord1D, coord2D) %>%
  drop_na(coord1D, coord2D) # Remove parlamentares sem coordenadas estimadas

# Gerar o gráfico com todos os pontos em azul
ggplot(df_coordenadas, aes(x = coord1D, y = coord2D)) +
  # Linhas pontilhadas centrais de referência (origem 0,0)
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  
  # Pontos de todos os deputados em azul
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  
  # Limites padronizados do espaço W-NOMINATE (-1 a 1)
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  
  # Nomeação exata dos eixos e título
  labs(
    x = "1ª Dimensão",
    y = "2ª Dimensão",
    #title = "Distribuição Espacial W-NOMINATE - Câmara dos Deputados",
    #subtitle = paste0("Total de deputados analisados: ", nrow(df_coordenadas))
  ) +
  

  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 13)
  )

