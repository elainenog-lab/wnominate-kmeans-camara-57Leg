## genero

library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 1. TRATAMENTO DOS DADOS E TABELA DE PROPORÇÃO POR GÊNERO
# ==============================================================================

dados_genero <- dados_agrupados %>%
  filter(!is.na(DS_GENERO), 
         !DS_GENERO %in% c("NÃO INFORMADO", "NÃO DECLARADO")) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    
    # Padronização da variável de gênero
    Genero = case_when(
      grepl("FEMININO", DS_GENERO, ignore.case = TRUE)  ~ "Feminino",
      grepl("MASCULINO", DS_GENERO, ignore.case = TRUE) ~ "Masculino",
      TRUE ~ stringr::str_to_title(DS_GENERO)
    ),
    
    # Ordem dos fatores para empilhamento nas barras
    Genero = factor(Genero, levels = c("Feminino", "Masculino"))
  )

# Tabela de frequências absolutas e percentuais por grupo
tabela_proporcao_genero <- dados_genero %>%
  group_by(Cluster_2D_3g, Genero) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    Total_Cluster = sum(n),
    Proporcao = n / Total_Cluster,
    Pct = Proporcao * 100,
    Pct_Formatted = paste0(format(round(Pct, 1), nsmall = 1, dec = ","), "%")
  )

cat("=== TABELA DE PROPORÇÕES POR GÊNERO E GRUPO ===\n")
print(tabela_proporcao_genero)

# Preparação de rótulos para o gráfico
dados_grafico_gen <- tabela_proporcao_genero %>%
  mutate(
    # Exibe porcentagem apenas para faixas >= 1,5% para evitar sobreposição visual
    pct_label = ifelse(Pct >= 1.5, Pct_Formatted, "")
  )

# ==============================================================================
# 2. GERAÇÃO DO GRÁFICO EMPILHADO (ESTILO DO MODELO)
# ==============================================================================

ggplot(dados_grafico_gen, aes(x = Cluster_2D_3g, y = Proporcao, fill = Genero)) +
  
  # Barra empilhada 100% proporcional
  geom_bar(
    stat = "identity", 
    position = position_fill(reverse = TRUE), 
    width = 0.50
  ) +
  
  # Rótulos percentuais centralizados em cada segmento
  geom_text(
    aes(label = pct_label),
    position = position_fill(vjust = 0.5, reverse = TRUE),
    size = 3.8,
    color = "black"
  ) +
  
  # Eixo Y formatado de 0% a 100%
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.10),
    expand = c(0, 0)
  ) +
  
  # Paleta de cores para gênero
  scale_fill_manual(
    values = c(
      "Feminino"  = "#e63a45", # Vermelho / Coral
      "Masculino" = "#3b6b8c"  # Azul Petróleo
    )
  ) +
  
  # Títulos e fontes conforme o padrão do gráfico de referência
  labs(
    #title = "Proporção do gênero dos parlamentares por grupo na 57° Legislatura",
    x = "",
    y = "",
    fill = ""
  ) +
  
  # Tema visual
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

# Tabela de contingência cruzada (Grupo x Gênero)
tabela_contingencia_gen <- table(dados_genero$Cluster_2D_3g, dados_genero$Genero)

cat("\n=== TABELA DE CONTINGÊNCIA (FREQUÊNCIAS ABSOLUTAS) ===\n")
print(tabela_contingencia_gen)

# Teste do Qui-Quadrado de Pearson com simulação de Monte Carlo
teste_qui2_gen <- chisq.test(tabela_contingencia_gen, simulate.p.value = TRUE, B = 2000)

cat("\n=== RESULTADO DO TESTE DO QUI-QUADRADO ===\n")
print(teste_qui2_gen)

