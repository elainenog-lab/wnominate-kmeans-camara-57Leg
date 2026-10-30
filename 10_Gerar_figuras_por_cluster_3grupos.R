library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(RColorBrewer)
# Agrupa em 3 clusters
set.seed(123)
coordenadas <- resultado_2D$legislators %>%
  dplyr::select(coord1D, coord2D) %>%
  drop_na()

km_3g <- kmeans(coordenadas, centers = 3, nstart = 25)
# Garantir que a coluna de Cluster está no dataframe coordenadas
coordenadas$Cluster_2D_3g <- as.factor(km_3g$cluster)

# Preparar os dados dividindo os deputados em 4 colunas por cluster
num_cols <- 4

df_clusters_separados <- coordenadas %>%
  rownames_to_column(var = "deputado_partido") %>%
  mutate(Cluster = paste("Cluster", Cluster_2D_3g)) %>%
  group_by(Cluster) %>%
  arrange(deputado_partido, .by_group = TRUE) %>%
  mutate(
    id_linha = row_number(),
    # Define a quantidade máxima de linhas necessárias por coluna
    linhas_por_col = ceiling(n() / num_cols),
    # Atribui a coluna (1 a 4) e a posição Y vertical de cada nome
    coluna_painel = ceiling(id_linha / linhas_por_col),
    posicao_y = id_linha - (coluna_painel - 1) * linhas_por_col
  ) %>%
  ungroup()

# Mapeamento das cores da paleta Set1 para os clusters
clusters_unicos <- sort(unique(df_clusters_separados$Cluster))
cores_set1 <- brewer.pal(n = max(3, length(clusters_unicos)), name = "Set1")
paleta_cores <- setNames(cores_set1[1:length(clusters_unicos)], clusters_unicos)

# Gerar e salvar uma imagem para cada Cluster com 4 colunas e cor temática
walk(clusters_unicos, function(nome_cluster) {
  
  # Cor específica deste cluster
  cor_cluster <- paleta_cores[nome_cluster]
  
  # Filtrar parlamentares apenas do cluster atual
  dados_sub <- df_clusters_separados %>%
    filter(Cluster == nome_cluster)
  
  total_deputados <- nrow(dados_sub)
  max_linhas <- max(dados_sub$posicao_y)
  
  # Construir o gráfico/quadro do cluster em 4 colunas
  p <- ggplot(dados_sub, aes(x = coluna_painel, y = -posicao_y, label = deputado_partido)) +
    geom_text(size = 2.8, hjust = 0, color = cor_cluster, fontface = "bold") +
    scale_x_continuous(limits = c(0.8, 4.8), breaks = 1:4) +
    labs(
      #title = paste0("Lista de Deputados - ", nome_cluster),
      #subtitle = paste0("Total de parlamentares: ", total_deputados),
      x = "", y = ""
    ) +
    theme_void() +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = cor_cluster),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30", margin = margin(b = 15))
    )
  
  # Salvar o arquivo PNG individual para o cluster
  nome_arquivo <- paste0("deputados__", gsub(" ", "_", tolower(nome_cluster)), "_4colunas.png")
  
  ggsave(
    filename = nome_arquivo,
    plot = p,
    width = 14,                                # Largura expandida para suportar 4 colunas
    height = max(5, max_linhas * 0.25 + 2),     # Altura ajustada dinamicamente com base nas linhas
    dpi = 600
  )
})

