# ==== Extraindo composição da DPMFi e salvando em um arquivo Excel ====

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

# Importando dados do BCB
DPMFi <- GetBCBData::gbcbd_get_series(
        id = c(
                "Câmbio" = 4173, "TR" = 4174, "IGP-M" = 4175, "IGP-DI" = 4176,
                "Over/Selic" = 4177, "Préfixado" = 4178, "TJLP" = 4179,
                "OUTROS" = 4180, "IPCA" = 12001, "INPC" = 12002
        ),
        first.date = "2000-01-01",
        last.date = "2024-12-31",
        format.data = "wide"
)
tail(DPMFi)



# Transformando os dados ANTES de converter para formato long
DPMFi_processed <- DPMFi %>%
    mutate(OUTROS = OUTROS + TR + TJLP) %>%
    mutate(IGP = `IGP-M` + `IGP-DI`) %>%
    select(-c(`IGP-M`, `IGP-DI`, TR, TJLP))

# Agora convertendo para formato long
DPMFi_long <- DPMFi_processed %>%
  pivot_longer(cols = -ref.date, 
               names_to = "Variavel", 
               values_to = "Valor")

# Verificando as variáveis finais
print("Variáveis após processamento:")
print(unique(DPMFi_long$Variavel))

# Criando o gráfico
grafico_dpmfi <- ggplot(DPMFi_long, aes(x = ref.date, y = Valor, color = Variavel, linetype = Variavel)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = "Data",
    y = "Percentual (%)",
    color = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_line(color = "gray95", size = 0.3),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = "top"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_manual(values = c(
    "Over/Selic" = "#FF0000",     # Vermelho forte
    "IPCA" = "#0000FF",           # Azul forte
    "Préfixado" = "#00AA00",      # Verde forte
    "Câmbio" = "#FF8800",         # Laranja
    "OUTROS" = "#8800FF",         # Roxo
    "IGP" = "#00FFFF",            # Ciano
    "INPC" = "#FF00FF"            # Magenta
  )) +
  scale_linetype_manual(values = c(
    "Over/Selic" = "dashed",      # Linha pontilhada para Over/Selic
    "IPCA" = "solid",
    "Préfixado" = "solid",
    "Câmbio" = "solid",
    "OUTROS" = "solid",
    "IGP" = "solid",
    "INPC" = "solid"
  )) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

print(grafico_dpmfi)

# Salvando o gráfico
ggsave("DPMFi_composicao.png", 
       plot = grafico_dpmfi, 
       width = 12, 
       height = 6, 
       dpi = 300, 
       bg = "transparent")

cat("Gráfico salvo como 'DPMFi_composicao.png' no diretório de trabalho.\n")


# Reproduzindo o som de notificação
beep(sound = 1)
