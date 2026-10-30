library(tidyverse)
library(klaR)
library(dplyr)
library(scatterplot3d)
library(tidyr)

# Importar o CSV
dados2023 <- read.csv("data/votacoesVotos-2023.csv", stringsAsFactors = FALSE, sep = ";")
dados2024 <- read.csv("data/votacoesVotos-2024.csv", stringsAsFactors = FALSE, sep = ";")
dados2025 <- read.csv("data/votacoesVotos-2025.csv", stringsAsFactors = FALSE, sep = ";")
dados2026 <- read.csv("data/votacoesVotos-2026.csv", stringsAsFactors = FALSE, sep = ";")

# Combinar os DataFrames empilhando as linhas
dados <- bind_rows(dados2023, dados2024, dados2025, dados2026)

