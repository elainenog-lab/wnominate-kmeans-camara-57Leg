#bens

library(ggplot2)
library(dplyr)
library(scales)

# ==============================================================================
# 1. TRATAMENTO DOS DADOS E TABELA RESUMO POR GRUPO
# ==============================================================================

dados_bens <- dados_agrupados %>%
  filter(!is.na(SOMA_VR_BEM_CANDIDATO)) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    Bens_Mi = SOMA_VR_BEM_CANDIDATO 
  )

# Tabela com o resumo estatístico completo por Grupo / Cluster
tabela_resumo_bens <- dados_bens %>%
  group_by(Cluster_2D_3g) %>%
  summarise(
    Deputados = n(),
    Média_Mi = round(mean(Bens_Mi, na.rm = TRUE), 2),
    Mediana_Mi = round(median(Bens_Mi, na.rm = TRUE), 2),
    Desvio_Padrão = round(sd(Bens_Mi, na.rm = TRUE), 2),
    IQR = round(IQR(Bens_Mi, na.rm = TRUE), 2),
    Mínimo_Mi = round(min(Bens_Mi, na.rm = TRUE), 3),
    Máximo_Mi = round(max(Bens_Mi, na.rm = TRUE), 2)
  )

cat("=== TABELA DESCRITIVA DO PATRIMÔNIO DOS DEPUTADOS POR GRUPO (R$ MI) ===\n")
print(as.data.frame(tabela_resumo_bens))

# ==============================================================================
# 2. GRÁFICO BOXPLOT POR GRUPO
# ==============================================================================
dados_bens <- dados_agrupados %>%
  filter(!is.na(SOMA_VR_BEM_CANDIDATO)) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    Bens_Mi = SOMA_VR_BEM_CANDIDATO / 1e6 # Transforma os valores para Milhões de R$
  )
# Cálculo das medianas e do 3º quartil (Q3) para posicionamento dos rótulos
medianas_bens <- dados_bens %>%
  group_by(Cluster_2D_3g) %>%
  summarise(
    mediana = median(Bens_Mi, na.rm = TRUE),
    q3 = quantile(Bens_Mi, 0.75, na.rm = TRUE)
  )

ggplot(dados_bens, aes(x = Cluster_2D_3g, y = Bens_Mi, fill = Cluster_2D_3g)) +
  
  # Boxplot focado no intervalo interquartil (outliers ocultos para não achatar o gráfico)
  geom_boxplot(outlier.shape = NA, alpha = 0.8, color = "#2c3e50") +
  
  # Adiciona texto com o valor exato da mediana acima do topo da caixa (Q3)
  geom_text(
    data = medianas_bens,
    aes(x = Cluster_2D_3g, y = q3 + 0.35, label = paste0("Mediana:\nR$ ", round(mediana, 2), " Mi")),
    fontface = "bold",
    size = 3.5,
    color = "black",
    inherit.aes = FALSE
  ) +
  
  # Formatação do Eixo Y em moeda (R$ ... Mi)
  scale_y_continuous(
    labels = label_currency(prefix = "R$ ", suffix = " Mi", big.mark = ".", decimal.mark = ",")
  ) +
  
  # Delimita a janela visual no eixo Y sem excluir dados do cálculo
  coord_cartesian(ylim = c(0, max(medianas_bens$q3, na.rm = TRUE) + 1.2)) +
  
  # Mapeamento de cores para os 3 grupos
  scale_fill_manual(
    values = c(
      "1" = "#ff4d4d", # Vermelho
      "2" = "#3b52ff", # Azul 
      "3" = "#37d043"  # Verde
    )
  ) +
  
  # Títulos e rótulos
  labs(
#    title = "Comparativo do Patrimônio Declarado dos Deputados por Grupo",
 #   subtitle = "Foco na amplitude interquartil e mediana dos bens declarados (Sem Outliers)",
    x = "Grupo / Cluster",
    y = "Patrimônio Declarado (em Milhões de R$)"
  ) +
  
  # Tema visual limpo
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 13, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 15)),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e5e5e5", linewidth = 0.5)
  )

# ==============================================================================
# 3. TESTE ANOVA ONE-WAY E VERIFICAÇÃO DE PRESSUPOSTOS
# ==============================================================================

# 1. Modelo ANOVA Paramétrico
modelo_anova_bens <- aov(Bens_Mi ~ Cluster_2D_3g, data = dados_bens)

cat("\n=== RESULTADO DA ANOVA ONE-WAY ===\n")
print(summary(modelo_anova_bens))

