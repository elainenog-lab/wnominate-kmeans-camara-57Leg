library(ggplot2)
library(dplyr)

# Extrair coordenadas
coordenadas <- resultado_2D$legislators %>%
  dplyr::select(coord1D, coord2D) %>%
  drop_na()

# Calcular o WSS para K de 1 a 10 usando R nativo
set.seed(123)
wss <- numeric(10)

for (k in 1:10) {
  # Fit the model
  model <- kmeans(coordenadas, centers = k, nstart = 25)
  # Save the within cluster sum of squares
  wss[k] <- model$tot.withinss
}

# Criar o gráfico do cotovelo
cotovelo_df <- data.frame(K = 1:10, WSS = wss)

ggplot(cotovelo_df, aes(x = K, y = WSS)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    #title = "Método do cotovelo (Elbow method)",
    x = "Número de clusters (K)",
    y = "Soma dos quadrados dentro dos clusters (WSS)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                                # Remove as linhas de grade internas
    axis.line = element_line(color = "black", linewidth = 0.5), # Desenha as linhas dos eixos X e Y
    axis.ticks = element_line(color = "black")                  # Adiciona os tracinhos (ticks) nos eixos
  )

