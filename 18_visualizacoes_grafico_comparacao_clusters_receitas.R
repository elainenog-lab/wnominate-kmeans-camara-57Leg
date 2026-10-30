library(ggplot2)
library(dplyr)
library(scales)

# ==============================================================================
# 1. TRATAMENTO DOS DADOS (RECURSOS EM MILHÕES DE R$) E TABELA DESCRITIVA
# ==============================================================================

dados_campanha <- dados_agrupados %>%
  filter(!is.na(SOMA_DOACOES_CAMPANHA)) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    Recursos_Mi = SOMA_DOACOES_CAMPANHA
  )

# Tabela resumo descritiva por Grupo
tabela_resumo_campanha <- dados_campanha %>%
  group_by(Cluster_2D_3g) %>%
  summarise(
    Deputados = n(),
    `Média` = round(mean(Recursos_Mi, na.rm = TRUE), 2),
    `Mediana` = round(median(Recursos_Mi, na.rm = TRUE), 2),
    `Desvio Padrão` = round(sd(Recursos_Mi, na.rm = TRUE), 2),
    `IQR` = round(IQR(Recursos_Mi, na.rm = TRUE), 2),
    `Mínimo` = round(min(Recursos_Mi, na.rm = TRUE), 3),
    `Máximo` = round(max(Recursos_Mi, na.rm = TRUE), 2)
  )

cat("=== TABELA DESCRITIVA DOS RECURSOS DE CAMPANHA POR GRUPO ===\n")
print(as.data.frame(tabela_resumo_campanha))

# ==============================================================================
# 2. GRÁFICO BOXPLOT DOS RECURSOS OBTIDOS
# ==============================================================================
dados_campanha <- dados_agrupados %>%
  filter(!is.na(SOMA_DOACOES_CAMPANHA)) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    Recursos_Mi = SOMA_DOACOES_CAMPANHA / 1e6 # Converte para Milhões de R$
  )

# Tabela resumo descritiva por Grupo
tabela_resumo_campanha <- dados_campanha %>%
  group_by(Cluster_2D_3g) %>%
  summarise(
    Deputados = n(),
    `Média (R$ Mi)` = round(mean(Recursos_Mi, na.rm = TRUE), 2),
    `Mediana (R$ Mi)` = round(median(Recursos_Mi, na.rm = TRUE), 2),
    `Desvio Padrão` = round(sd(Recursos_Mi, na.rm = TRUE), 2),
    `IQR (R$ Mi)` = round(IQR(Recursos_Mi, na.rm = TRUE), 2),
    `Mínimo (R$ Mi)` = round(min(Recursos_Mi, na.rm = TRUE), 3),
    `Máximo (R$ Mi)` = round(max(Recursos_Mi, na.rm = TRUE), 2)
  )


# Posição dos rótulos de mediana acima da caixa (3º Quartil + margem)
medianas_campanha <- dados_campanha %>%
  group_by(Cluster_2D_3g) %>%
  summarise(
    mediana = median(Recursos_Mi, na.rm = TRUE),
    q3 = quantile(Recursos_Mi, 0.75, na.rm = TRUE)
  )

ggplot(dados_campanha, aes(x = Cluster_2D_3g, y = Recursos_Mi, fill = Cluster_2D_3g)) +
  
  # Boxplot com foco no intervalo interquartil (outliers ocultos)
  geom_boxplot(outlier.shape = NA, alpha = 0.8, color = "#2c3e50") +
  
  # Adiciona rótulo da mediana exata acima do topo da caixa
  geom_text(
    data = medianas_campanha,
    aes(x = Cluster_2D_3g, y = q3 + 0.25, label = paste0("Mediana:\nR$ ", round(mediana, 2), " Mi")),
    fontface = "bold",
    size = 3.5,
    color = "black",
    inherit.aes = FALSE
  ) +
  
  # Formatação do eixo Y em moeda (R$ ... Mi)
  scale_y_continuous(
    labels = label_currency(prefix = "R$ ", suffix = " Mi", big.mark = ".", decimal.mark = ",")
  ) +
  
  # Ajusta a janela visual do eixo Y sem excluir dados do cálculo
  coord_cartesian(ylim = c(0, max(medianas_campanha$q3, na.rm = TRUE) + 0.9)) +
  
  # Paleta de cores padronizada
  scale_fill_manual(
    values = c(
      "1" = "#ff4d4d", # Vermelho
      "2" = "#3b52ff", # Azul 
      "3" = "#37d043"  # Verde
    )
  ) +
  
  # Títulos e rótulos
  labs(
#    title = "Comparativo dos Recursos de Campanha dos Deputados por Grupo",
 #   subtitle = "Valores expressos em Milhões de R$ (sem exibição de outliers)",
    x = "Grupo / Cluster",
    y = "Recursos Arrecadados (em Milhões de R$)"
    ) +
  
  # Tema visual
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
# 3. TESTE ANOVA ONE-WAY E DIAGNÓSTICOS
# ==============================================================================

# 1. Modelo ANOVA Paramétrico
modelo_anova_camp <- aov(Recursos_Mi ~ Cluster_2D_3g, data = dados_campanha)

cat("\n=== RESULTADO DA ANOVA ONE-WAY (RECURSOS DE CAMPANHA) ===\n")
print(summary(modelo_anova_camp))
