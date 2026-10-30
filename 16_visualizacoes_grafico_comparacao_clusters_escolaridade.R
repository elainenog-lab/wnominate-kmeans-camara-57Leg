#escolaridade
library(dplyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 1. TRATAMENTO DOS DADOS E ORDENAÇÃO DE BAIXO PARA CIMA
# ==============================================================================

dados_escolaridade <- dados_agrupados %>%
  filter(!is.na(DS_GRAU_INSTRUCAO), 
         !DS_GRAU_INSTRUCAO %in% c("NÃO INFORMADO", "NÃO DECLARADO")) %>%
  mutate(
    Cluster_2D_3g = as.factor(Cluster_2D_3g),
    
    # Padronização dos nomes das categorias
    Escolaridade = case_when(
      grepl("FUNDAMENTAL", DS_GRAU_INSTRUCAO) ~ "Fundamental",
      grepl("MÉDIO", DS_GRAU_INSTRUCAO)       ~ "Médio",
      grepl("INCOMPLETO", DS_GRAU_INSTRUCAO) & grepl("SUPERIOR", DS_GRAU_INSTRUCAO) ~ "Superior Incompleto",
      grepl("COMPLETO", DS_GRAU_INSTRUCAO) & grepl("SUPERIOR", DS_GRAU_INSTRUCAO)   ~ "Superior Completo",
      TRUE ~ stringr::str_to_title(DS_GRAU_INSTRUCAO)
    ),
    
    # Ordem dos fatores (do nível mais baixo para o mais alto):
    # Base (Y = 0%): Fundamental
    # 2º nível:      Médio
    # 3º nível:      Superior Incompleto
    # Topo (Y = 100%): Superior Completo
    Escolaridade = factor(
      Escolaridade, 
      levels = c("Fundamental", "Médio", "Superior Incompleto", "Superior Completo")
    )
  )

# Processamento de proporções e rótulos
dados_grafico_esc <- dados_escolaridade %>%
  count(Cluster_2D_3g, Escolaridade) %>%
  group_by(Cluster_2D_3g) %>%
  mutate(
    total = sum(n),
    prop = n / total,
    pct = prop * 100,
    # Exibe porcentagem apenas para faixas >= 1,5% para evitar sobreposição visual
    pct_label = ifelse(pct >= 1.5, paste0(format(round(pct, 1), nsmall = 1, dec = ","), "%"), "")
  )

# ==============================================================================
# 2. GERAÇÃO DO GRÁFICO (EMPILHAMENTO GARANTIDO DE BAIXO PARA CIMA)
# ==============================================================================

ggplot(dados_grafico_esc, aes(x = Cluster_2D_3g, y = prop, fill = Escolaridade)) +
  
  # Barra empilhada proporcional mantendo a ordem dos fatores (reverse = FALSE)
  geom_bar(
    stat = "identity", 
    position = position_fill(reverse = TRUE), 
    width = 0.50
  ) +
  
  # Rótulos percentuais centralizados perfeitamente na mesma ordem das barras
  geom_text(
    aes(label = pct_label),
    position = position_fill(vjust = 0.5, reverse = TRUE),
    size = 3.5,
    color = "black"
  ) +
  
  # Escala do Eixo Y de 0% a 100% (intervalos de 10%)
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.10),
    expand = c(0, 0)
  ) +
  
  # Mapeamento de cores da base ao topo
  scale_fill_manual(
    values = c(
      "Fundamental"         = "#e63a45", # Vermelho (Base)
      "Médio"               = "#3b6b8c", # Azul Petróleo
      "Superior Incompleto" = "#f2b705", # Amarelo / Dourado
      "Superior Completo"   = "#9eccd3"  # Azul Claro / Menta (Topo)
    )
  ) +
  
  # Títulos e rótulos
  labs(
   # title = "Proporção dos níveis de escolaridade dos parlamentares por grupo",
    x = "",
    y = "",
    fill = ""
  ) +
  
  # Tema visual limpo
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

# 1. Tabela de contingência cruzada (Agrupamentos x Escolaridade)
tabela_escolaridade <- table(dados_escolaridade$Cluster_2D_3g, dados_escolaridade$Escolaridade)

cat("\n=== TABELA DE FREQUÊNCIAS OBSERVADAS ===\n")
print(tabela_escolaridade)

# 2. Execução do Teste do Qui-Quadrado de Pearson
# 'simulate.p.value = TRUE' realiza simulação de Monte Carlo, indicada caso existam células com poucas observações (< 5)
teste_qui2_esc <- chisq.test(tabela_escolaridade, simulate.p.value = TRUE, B = 2000)

cat("\n=== RESULTADO DO TESTE DO QUI-QUADRADO ===\n")
print(teste_qui2_esc)
