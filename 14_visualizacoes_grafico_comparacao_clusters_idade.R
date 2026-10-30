library(dplyr)
library(lubridate)

# Preparação das variáveis
dados_agrupados <- dados_agrupados %>%
  mutate(
    # Converter a coluna de cluster para fator (fator categórico)
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    
    # Calcular a Idade no ano da eleição
    # Assume que DT_NASCIMENTO está no formato "DD/MM/YYYY" padrão do TSE
    DT_NASC_formatada = dmy(DT_NASCIMENTO),
    Idade = ANO_ELEICAO - year(DT_NASC_formatada)
  )

###################
#idade
library(ggplot2)

# Gerar o boxplot no estilo visual de referência (sem rótulos de mediana)
ggplot(dados_agrupados, aes(x = Cluster_2D_3g, y = Idade, fill = Cluster_2D_3g)) +
  
  # 1. Caixas preenchidas a cor sólida com contorno preto fino
  geom_boxplot(color = "black", linewidth = 0.6, outlier.shape = 1) +
  
  # 2. Escala do eixo Y de 0 a 100 anos, com intervalos de 10 em 10
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  
  # 3. Cores de preenchimento sólidas (Vermelho, Azul e Verde)
  scale_fill_manual(
    values = c("#ff4d4d", "#3b52ff", "#37d043")
  ) +
  
  # 4. Título, Subtítulo, Eixos e Fonte
  labs(
    #title = "Distribuição das idades dos parlamentares da 57ª Legislatura por grupo",
    x = "Cluster",
    y = "Idade (em anos)"
  ) +
  
  # 5. Aplicação do tema limpo
  theme_minimal() +
  
  # 6. Ajustes de tipografia, grelha e remoção da legenda
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 15), color = "#222222"),
    axis.title.x = element_text(size = 11, color = "#000000", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, color = "#000000", margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.5),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 10, color = "#555555", margin = margin(t = 15))
  )
########################
#idade

library(dplyr)

# Tabela sintética de idades por grupo (Cluster)
tabela_idades_cluster <- dados_agrupados %>%
  group_by(Cluster = Cluster_2D_3g) %>%
  summarise(
    qtd_deputados = n(),
    media_idade   = round(mean(Idade, na.rm = TRUE), 1),
    desvio_padrao = round(sd(Idade, na.rm = TRUE), 1),
    mediana_idade = median(Idade, na.rm = TRUE),
    min_idade     = min(Idade, na.rm = TRUE),
    max_idade     = max(Idade, na.rm = TRUE)
  )

# Exibir o resultado no console
print(tabela_idades_cluster)
####################
library(dplyr)

# 1. Garantir que o Cluster está formatado como Fator
dados_agrupados <- dados_agrupados %>%
  mutate(Cluster_2D_3g = as.factor(Cluster_2D_3g))

# 2. Executar o modelo de ANOVA One-Way
modelo_anova <- aov(Idade ~ Cluster_2D_3g, data = dados_agrupados)

# Exibir a Tabela da ANOVA
cat("=== RESULTADO DA ANOVA ONE-WAY ===\n")
summary(modelo_anova)

#################################

