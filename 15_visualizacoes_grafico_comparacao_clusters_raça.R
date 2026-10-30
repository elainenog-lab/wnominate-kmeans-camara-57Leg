#raça
library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 1. TRATAMENTO DOS DADOS E TABELA DE FREQUÊNCIAS
# ==============================================================================

# Ajustar os nomes e a ordem dos fatores de Raça/Cor conforme padrão do TSE
dados_raca <- dados_agrupados %>%
  filter(!is.na(DS_COR_RACA), !DS_COR_RACA %in% c("NÃO INFORMADO", "NÃO DECLARADO")) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    Cor_Raca = factor(tolower(DS_COR_RACA), 
                      levels = c("amarela", "branca", "indígena", "parda", "preta"))
  )

# Criar a tabela de contingência (matriz de frequências cruzadas)
tabela_contingencia <- table(dados_raca$Cluster_2D_3g, dados_raca$Cor_Raca)

# Calcular proporções por cluster para rotulagem gráfica
dados_grafico <- dados_raca %>%
  count(Cluster_2D_3g, Cor_Raca) %>%
  group_by(Cluster_2D_3g) %>%
  mutate(
    total = sum(n),
    prop = n / total,
    pct = prop * 100,
    # Oculta rótulos de categorias com menos de 1.5% para não sobrepor o texto
    pct_label = ifelse(pct >= 1, paste0(format(round(pct, 1), nsmall = 1, dec = ","), "%"), "")
  )

# ==============================================================================
# 2. GERAÇÃO DO GRÁFICO (100% STACKED BAR CHART)
# ==============================================================================

ggplot(dados_grafico, aes(x = Cluster_2D_3g, y = prop, fill = Cor_Raca)) +
  
  # Barra empilhada proporcional (fill)
  geom_bar(stat = "identity", position = "fill", width = 0.50, color = NA) +
  
  # Rótulos percentuais centralizados dentro de cada segmento
  geom_text(
    aes(label = pct_label),
    position = position_fill(vjust = 0.5),
    size = 3.5,
    color = "black"
  ) +
  
  # Escala do Eixo Y de 0% a 100% com marcações a cada 10%
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.10),
    expand = c(0, 0)
  ) +
  
  # Paleta de cores exata da imagem de referência
  scale_fill_manual(
    values = c(
      "amarela"  = "#1b2a4a", # Azul Marinho Escuro
      "branca"   = "#f2f7ef", # Branco Suave / Esverdeado
      "indígena" = "#a1d9ce", # Azul Claro / Menta
      "parda"    = "#e63946", # Vermelho / Coral
      "preta"    = "#457b9d"  # Azul Ardosia
    )
  ) +
  
  # Títulos e Rótulos
  labs(
    #title = "Proporção das raças dos parlamentares da 57ª Legislatura por grupo",
    x = "",
    y = "",
    fill = ""
  ) +
  
  # Tema limpo e formatação idêntica à referência
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0, margin = margin(b = 15)),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e5e5e5", linewidth = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10, color = "black"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# ==============================================================================
# 3. TESTE DO QUI-QUADRADO DE INDEPENDÊNCIA (PEARSON)
# ==============================================================================

# Executar o Teste do Qui-Quadrado
teste_qui2 <- chisq.test(tabela_contingencia, simulate.p.value = TRUE)

cat("\n=== TESTE DO QUI-QUADRADO DE INDEPENDÊNCIA ===\n")
print(teste_qui2)

