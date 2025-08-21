# ==== Extraindo csv com dados de detentores de LFTs e plotando em um gráfico ====

library(writexl)
library(GetBCBData)
library(tidyverse)
library(dplyr)
library(beepr)
library(ggplot2)
library(tidyr)
library(scales)


choose.files() # Apenas para triggar o funcionamento correto do choose.dir()
diretorio <- choose.dir()
setwd(diretorio)
getwd()

# Importando dados do csv
lft <- read.csv("DetentoresLFT.csv", sep = ";", dec = ",", header = TRUE)

colnames(lft) <- c("Ano", "IFs", "FIs", "Não-residentes", "Previdência", "Governo", "Seguradoras", "Outros")

# Verificando a estrutura dos dados
View(lft)

# Definindo a coluna de anos como Date
lft$Ano <- as.Date(as.character(lft$Ano), format = "%d/%m/%Y")

str(lft)

# Transformando os dados para formato longo (necessário para ggplot)
lft_long <- lft %>%
  pivot_longer(cols = -Ano, 
               names_to = "Categoria", 
               values_to = "Percentual") %>%
  mutate(Percentual = Percentual * 100)  # Convertendo para percentual (0-100)

# Verificando os dados transformados
head(lft_long)

# Criando o gráfico
grafico_lft <- ggplot(lft_long, aes(x = Ano, y = Percentual, color = Categoria, linetype = Categoria)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Composição dos Detentores de LFT ao Longo do Tempo",
    x = "Data",
    y = "Percentual (%)",
    color = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 11),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.margin = margin(2, 2, 2, 2),
    legend.box.spacing = unit(0.1, "cm"),
    legend.spacing.y = unit(0.1, "cm")
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_manual(values = c(
    "IFs" = "#FF0000",           # Vermelho forte - Instituições Financeiras
    "FIs" = "#0000FF",           # Azul forte - Fundos de Investimento
    "Não-residentes" = "#00AA00", # Verde forte
    "Previdência" = "#FF8800",    # Laranja
    "Governo" = "#8800FF",        # Roxo
    "Seguradoras" = "#00FFFF",    # Ciano
    "Outros" = "#FF00FF"          # Magenta
  )) +
  scale_linetype_manual(values = c(
    "IFs" = "solid",
    "FIs" = "solid", 
    "Não-residentes" = "dashed",
    "Previdência" = "solid",
    "Governo" = "solid",
    "Seguradoras" = "solid",
    "Outros" = "dotted"
  )) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

print(grafico_lft)

# Salvando o gráfico
ggsave("Detentores_LFT.png", 
       plot = grafico_lft, 
       width = 12, 
       height = 6, 
       dpi = 300, 
       bg = "transparent")

cat("Gráfico salvo como 'Detentores_LFT.png' no diretório de trabalho.\n")

# Reproduzindo o som de notificação
beep(sound = 1)