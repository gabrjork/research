# ==== Extraindo composição da DPMFi e salvando em um arquivo Excel ====

library(writexl)
library(GetBCBData)
library(tidyverse)
library(dplyr)
library(beepr)

choose.files() # Apenas para triggar o funcionamento correto do choose.dir()
diretorio <- choose.dir()
setwd(diretorio)
getwd()

# Importando dados do BCB
DPMFi <- GetBCBData::gbcbd_get_series(
        id = c(
                "Câmbio" = 4173, "TR" = 4174, "IGP-M" = 4175, "IGP-DI" = 4176,
                "Over/Selic" = 4177, "Préfixado" = 4178, "TJLP" = 4179,
                "OUTROS" = 4180, "IPCA" = 120001, "INPC" = 12002
        ),
        first.date = "2000-01-01",
        last.date = "2024-12-31",
        format.data = "wide"
)
tail(DPMFi)

DPMFi <- DPMFi %>%
    mutate(OUTROS = OUTROS + TR + TJLP) %>%
    mutate(IGP = IGP_M + IGP_DI) %>%
    select(-c(IGP_M, IGP_DI, TR, TJLP))

# Salvando o arquivo com um timestamp
timestamp <- format(Sys.time(), "%Y%m%d")
nome_arquivo <- paste0("dpmfi_", timestamp, ".xlsx")

write_xlsx(DPMFi, nome_arquivo)

# Reproduzindo o som de notificação
beep(sound = 1)