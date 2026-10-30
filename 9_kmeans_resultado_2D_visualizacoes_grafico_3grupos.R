library(ggplot2)
library(dplyr)

# Agrupa em 3 clusters
set.seed(123)
coordenadas <- resultado_2D$legislators %>%
  dplyr::select(coord1D, coord2D) %>%
  drop_na()

km_3g <- kmeans(coordenadas, centers = 3, nstart = 25)
# Garantir que a coluna de Cluster está no dataframe coordenadas
coordenadas$Cluster_2D_3g <- as.factor(km_3g$cluster)

ggplot(coordenadas, aes(x = coord1D, y = coord2D, color = Cluster_2D_3g)) +
  # Linhas de referência no zero (eixos cruzados e tracejados)
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 0.6) +
  
  geom_point(alpha = 0.8, size = 2.5) +
  stat_ellipse(type = "t", level = 0.95) + # Desenha as elipses em volta dos grupos
  # MAPEAMENTO MANUAL PARA BATER COM A FIGURA DA DIREITA
  scale_color_manual(
    values = c(
      "3" = "#e41a1c", # Vermelho (para o grupo da Esquerda)
      "2" = "#377eb8", # Azul     (para o grupo do Centro/Abaixo)
      "1" = "#4daf4a"  # Verde    (para o grupo da Direita)
    )
  ) + 
  labs(
    #title = "Clusters K-Means",
    x = "1ª Dimensão",
    y = "2ª Dimensão"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                                # Remove as linhas de grade internas
    axis.line = element_line(color = "black", linewidth = 0.5), # Desenha as linhas dos eixos X e Y
    axis.ticks = element_line(color = "black")                  # Adiciona os tracinhos (ticks) nos eixos
  )

