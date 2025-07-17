install.packages("GetTDData")
library(GetTDData)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

choose.files()
diretorio <- choose.dir()
setwd(diretorio)

# Verificar se pivot_longer está disponível
if (!exists("pivot_longer")) {
  print("Erro: pivot_longer não encontrada. Tentando carregar tidyr...")
  library(tidyr)
}

# Definir período de análise
data_inicio <- as.Date("2011-01-01")
data_fim <- as.Date("2024-12-31")

# Buscar dados de todos os títulos
td_data <- td_get(
    asset_codes = c("LFT", "LTN", "NTN-B"),
    first_year = 2011,
    last_year = 2024,
    dl_folder = get_cache_folder()
)

# Verificar estrutura dos dados
print("Estrutura dos dados carregados:")
str(td_data)

print("Primeiras linhas:")
head(td_data)

print("Colunas disponíveis:")
names(td_data)

print("Tipos de ativos disponíveis:")
unique(td_data$asset_code)

# Filtrar dados por tipo de ativo
ltn_data <- td_data %>% 
  filter(grepl("LTN", asset_code))

ntnb_data <- td_data %>% 
  filter(grepl("NTN-B", asset_code))

lft_data <- td_data %>% 
  filter(grepl("LFT", asset_code))

print(paste("Dados LTN:", nrow(ltn_data), "observações"))
print(paste("Dados NTNB:", nrow(ntnb_data), "observações"))
print(paste("Dados LFT:", nrow(lft_data), "observações"))

# Função para criar série temporal contínua de títulos com vencimento ~5 anos
criar_serie_5_anos <- function(dados, nome_titulo) {
  if(nrow(dados) == 0) {
    print(paste("Sem dados para filtrar:", nome_titulo))
    return(data.frame())
  }
  
  print(paste("Criando série contínua de 5 anos para:", nome_titulo))
  
  # Para cada data, encontrar o título mais próximo de 5 anos
  serie_continua <- dados %>%
    mutate(
      anos_ate_venc = as.numeric(difftime(matur_date, ref_date, units = "days")) / 365.25,
      diff_5_anos = abs(anos_ate_venc - 5)  # Distância de 5 anos
    ) %>%
    # Para cada data, pegar o título mais próximo de 5 anos
    group_by(ref_date) %>%
    filter(diff_5_anos == min(diff_5_anos)) %>%
    # Se há empate, pegar o primeiro
    slice(1) %>%
    ungroup() %>%
    # Filtrar apenas títulos que estão entre 3 e 7 anos (para manter qualidade)
    filter(anos_ate_venc >= 3, anos_ate_venc <= 7) %>%
    arrange(ref_date) %>%
    select(ref_date, price_bid, yield_bid, asset_code, anos_ate_venc)
  
  if(nrow(serie_continua) > 0) {
    print(paste("✓", nome_titulo, "série contínua:", nrow(serie_continua), "observações"))
    print(paste("Faixa de vencimentos:", round(min(serie_continua$anos_ate_venc), 1), 
                "a", round(max(serie_continua$anos_ate_venc), 1), "anos"))
    
    # Mostrar quais títulos foram mais utilizados
    uso_titulos <- serie_continua %>%
      count(asset_code, sort = TRUE) %>%
      head(5)
    print("Títulos mais utilizados:")
    print(uso_titulos)
  } else {
    print(paste("✗ Nenhum dado", nome_titulo, "adequado para série contínua"))
  }
  
  return(serie_continua)
}

# Criar séries contínuas de 5 anos
print("=== CRIANDO SÉRIES CONTÍNUAS ===")
ltn_serie_5anos <- criar_serie_5_anos(ltn_data, "LTN")
ntnb_serie_5anos <- criar_serie_5_anos(ntnb_data, "NTNB")
lft_serie_5anos <- criar_serie_5_anos(lft_data, "LFT")

print("=== DIAGNÓSTICO ===")
print(paste("LTN série criada:", nrow(ltn_serie_5anos), "linhas"))
print(paste("NTNB série criada:", nrow(ntnb_serie_5anos), "linhas"))
print(paste("LFT série criada:", nrow(lft_serie_5anos), "linhas"))

# Verificar se temos dados
if(exists("ltn_serie_5anos")) {
  print("✓ ltn_serie_5anos existe")
  if(nrow(ltn_serie_5anos) > 0) {
    print("✓ ltn_serie_5anos tem dados")
    print(head(ltn_serie_5anos))
  } else {
    print("✗ ltn_serie_5anos está vazio")
  }
} else {
  print("✗ ltn_serie_5anos não existe")
}

if(exists("ntnb_serie_5anos")) {
  print("✓ ntnb_serie_5anos existe")
  if(nrow(ntnb_serie_5anos) > 0) {
    print("✓ ntnb_serie_5anos tem dados")
    print(head(ntnb_serie_5anos))
  } else {
    print("✗ ntnb_serie_5anos está vazio")
  }
} else {
  print("✗ ntnb_serie_5anos não existe")
}

if(exists("lft_serie_5anos")) {
  print("✓ lft_serie_5anos existe")
  if(nrow(lft_serie_5anos) > 0) {
    print("✓ lft_serie_5anos tem dados")
    print(head(lft_serie_5anos))
  } else {
    print("✗ lft_serie_5anos está vazio")
  }
} else {
  print("✗ lft_serie_5anos não existe")
}

# Criar gráfico apenas com yields
if (nrow(ltn_serie_5anos) > 0 || nrow(ntnb_serie_5anos) > 0 || nrow(lft_serie_5anos) > 0) {
  
  print("✓ Criando gráfico de yields contínuos...")
  
  # Preparar dados apenas com yields
  dados_yields <- data.frame()
  
  if(nrow(ltn_serie_5anos) > 0) {
    ltn_yields <- ltn_serie_5anos %>%
      select(ref_date, yield_bid, anos_ate_venc) %>%
      mutate(
        tipo = "LTN",
        data = as.Date(ref_date),
        yield = yield_bid
      ) %>%
      filter(!is.na(yield))
    
    dados_yields <- bind_rows(dados_yields, ltn_yields)
  }
  
  if(nrow(ntnb_serie_5anos) > 0) {
    ntnb_yields <- ntnb_serie_5anos %>%
      select(ref_date, yield_bid, anos_ate_venc) %>%
      mutate(
        tipo = "NTNB",
        data = as.Date(ref_date),
        yield = yield_bid
      ) %>%
      filter(!is.na(yield))
    
    dados_yields <- bind_rows(dados_yields, ntnb_yields)
  }
  
  if(nrow(lft_serie_5anos) > 0) {
    lft_yields <- lft_serie_5anos %>%
      select(ref_date, yield_bid, anos_ate_venc) %>%
      mutate(
        tipo = "LFT",
        data = as.Date(ref_date),
        yield = yield_bid
      ) %>%
      filter(!is.na(yield))
    
    dados_yields <- bind_rows(dados_yields, lft_yields)
  }
  
  if(nrow(dados_yields) > 0) {
    print("✓ Preparando dados para gráfico de correlação preço vs yield...")
    
    # Preparar dados combinando preços e yields
    dados_completos <- data.frame()
    
    if(nrow(ltn_serie_5anos) > 0) {
      ltn_completo <- ltn_serie_5anos %>%
        select(ref_date, price_bid, yield_bid, anos_ate_venc) %>%
        mutate(
          tipo = "LTN",
          data = as.Date(ref_date),
          preco = price_bid,
          yield = yield_bid
        ) %>%
        filter(!is.na(preco), !is.na(yield))
      
      dados_completos <- bind_rows(dados_completos, ltn_completo)
    }
    
    if(nrow(ntnb_serie_5anos) > 0) {
      ntnb_completo <- ntnb_serie_5anos %>%
        select(ref_date, price_bid, yield_bid, anos_ate_venc) %>%
        mutate(
          tipo = "NTNB",
          data = as.Date(ref_date),
          preco = price_bid,
          yield = yield_bid
        ) %>%
        filter(!is.na(preco), !is.na(yield))
      
      dados_completos <- bind_rows(dados_completos, ntnb_completo)
    }
    
    if(nrow(lft_serie_5anos) > 0) {
      lft_completo <- lft_serie_5anos %>%
        select(ref_date, price_bid, yield_bid, anos_ate_venc) %>%
        mutate(
          tipo = "LFT",
          data = as.Date(ref_date),
          preco = price_bid,
          yield = yield_bid
        ) %>%
        filter(!is.na(preco), !is.na(yield))
      
      dados_completos <- bind_rows(dados_completos, lft_completo)
    }
    
    if(nrow(dados_completos) > 0) {
      # Normalizar preços para melhor visualização (escala 0-100)
      dados_completos <- dados_completos %>%
        group_by(tipo) %>%
        mutate(
          preco_norm = (preco - min(preco, na.rm = TRUE)) / 
                       (max(preco, na.rm = TRUE) - min(preco, na.rm = TRUE)) * 100
        ) %>%
        ungroup()
      
      # Calcular fatores de escala para eixo Y secundário
      range_preco <- range(dados_completos$preco_norm, na.rm = TRUE)
      range_yield <- range(dados_completos$yield, na.rm = TRUE)
      
      # Fator para transformar yield para a escala do preço
      yield_scale <- diff(range_preco) / diff(range_yield)
      yield_offset <- range_preco[1] - range_yield[1] * yield_scale
      
      # Função para criar gráfico individual por título
      criar_grafico_individual <- function(dados_titulo, nome_titulo, cor_preco, cor_yield) {
        if(nrow(dados_titulo) == 0) {
          return(NULL)
        }
        
        # Normalizar preços para o título específico
        dados_titulo <- dados_titulo %>%
          mutate(
            preco_norm = (preco - min(preco, na.rm = TRUE)) / 
                         (max(preco, na.rm = TRUE) - min(preco, na.rm = TRUE)) * 100
          )
        
        # Calcular fatores de escala para eixo Y secundário
        range_preco <- range(dados_titulo$preco_norm, na.rm = TRUE)
        range_yield <- range(dados_titulo$yield, na.rm = TRUE)
        
        # Fator para transformar yield para a escala do preço
        yield_scale <- diff(range_preco) / diff(range_yield)
        yield_offset <- range_preco[1] - range_yield[1] * yield_scale
        
        # Criar gráfico
        p <- ggplot(dados_titulo, aes(x = data)) +
          # Linha de preços (eixo Y primário)
          geom_line(aes(y = preco_norm, color = "Preço"), 
                    size = 1.2, alpha = 0.8) +
          # Linha de yields (eixo Y secundário, transformado)
          geom_line(aes(y = yield * yield_scale + yield_offset, color = "Yield"), 
                    size = 1.2, alpha = 0.8, linetype = "dashed") +
          
          # Definir cores manualmente
          scale_color_manual(
            name = "",
            values = c("Preço" = cor_preco, "Yield" = cor_yield)
          ) +
          
          # Eixo Y secundário
          scale_y_continuous(
            name = "Preço (Normalizado 0-100)",
            sec.axis = sec_axis(
              trans = ~ (. - yield_offset) / yield_scale,
              name = "Yield (%)",
              labels = scales::percent_format(scale = 1)
            )
          ) +
          
          scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
          
          labs(
            title = paste("Correlação Preço vs Yield -", nome_titulo),
            subtitle = "(Vencimento ~5 Anos | 2011-2024)",
            x = ""
          ) +
          
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "plain"),
            plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.title.y.left = element_text(color = "black", size = 11, face = "plain"),
            axis.title.y.right = element_text(color = "black", size = 11, face = "plain"),
            axis.text.y.left = element_text(color = "black", size = 9),
            axis.text.y.right = element_text(color = "black", size = 9),
            legend.position = "top",
            legend.text = element_text(size = 10),
            panel.grid.minor = element_blank(),
            plot.margin = margin(10, 15, 10, 15)
          )
        
        return(p)
      }
      
      # Criar gráficos individuais para cada título
      graficos <- list()
      
      if(nrow(ltn_serie_5anos) > 0) {
        dados_ltn <- dados_completos %>% filter(tipo == "LTN")
        graficos$LTN <- criar_grafico_individual(
          dados_ltn, "LTN (Prefixado)", 
          "#0059ffff", "#0035a7ff"
        )
      }
      
      if(nrow(ntnb_serie_5anos) > 0) {
        dados_ntnb <- dados_completos %>% filter(tipo == "NTNB")
        graficos$NTNB <- criar_grafico_individual(
          dados_ntnb, "NTN-B (IPCA+)", 
          "#ff0000ff", "#7A2B55"
        )
      }
      
      if(nrow(lft_serie_5anos) > 0) {
        dados_lft <- dados_completos %>% filter(tipo == "LFT")
        graficos$LFT <- criar_grafico_individual(
          dados_lft, "LFT (Selic)", 
          "#00ff37ff", "#01660aff"
        )
      }
      
      # Combinar gráficos verticalmente usando patchwork
      if(!require(patchwork)) {
        install.packages("patchwork")
        library(patchwork)
      }
      
      # Filtrar gráficos válidos
      graficos_validos <- graficos[!sapply(graficos, is.null)]
      
      if(length(graficos_validos) > 0) {
        # Combinar gráficos verticalmente
        if(length(graficos_validos) == 1) {
          p_combinado <- graficos_validos[[1]]
        } else if(length(graficos_validos) == 2) {
          p_combinado <- graficos_validos[[1]] / graficos_validos[[2]]
        } else {
          p_combinado <- graficos_validos[[1]] / graficos_validos[[2]] / graficos_validos[[3]]
        }
        
        # Adicionar título geral
        p_final <- p_combinado + 
          plot_annotation(
            title = "Análise de Correlação Preço vs Yield - Títulos Brasileiros",
            subtitle = "Séries contínuas com vencimento aproximado de 5 anos (2011-2024)",
            theme = theme(
              plot.title = element_text(size = 18, face = "plain", hjust = 0.5),
              plot.subtitle = element_text(size = 14, hjust = 0.5)
            )
          )
        
        print(p_final)
        
        # Criar timestamp para os arquivos
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        
        # Salvar o gráfico combinado com todos os títulos
        ggsave(
          filename = paste0("correlacao_tres_titulos_separados_", timestamp, ".png"),
          plot = p_final,
          width = 14,
          height = length(graficos_validos) * 4 + 2,  # Altura dinâmica
          dpi = 300,
          bg = "white"
        )
        
        print(paste0("✓ Gráfico combinado salvo como 'correlacao_tres_titulos_separados_", timestamp, ".png'"))
        
        # Criar versão sem NTNB (apenas LTN e LFT)
        if(length(graficos_validos) >= 2 && "NTNB" %in% names(graficos_validos)) {
          graficos_sem_ntnb <- graficos_validos[names(graficos_validos) != "NTNB"]
          
          if(length(graficos_sem_ntnb) == 1) {
            p_sem_ntnb <- graficos_sem_ntnb[[1]]
          } else {
            p_sem_ntnb <- graficos_sem_ntnb[[1]] / graficos_sem_ntnb[[2]]
          }
          
          # Adicionar título para versão sem NTNB
          p_final_sem_ntnb <- p_sem_ntnb + 
            plot_annotation(
              title = "Análise de Correlação Preço vs Yield - LTN e LFT",
              subtitle = "Séries contínuas com vencimento aproximado de 5 anos (2011-2024)",
              theme = theme(
                plot.title = element_text(size = 18, face = "plain", hjust = 0.5),
                plot.subtitle = element_text(size = 14, hjust = 0.5)
              )
            )
          
          # Salvar versão sem NTNB
          ggsave(
            filename = paste0("correlacao_sem_ntnb_", timestamp, ".png"),
            plot = p_final_sem_ntnb,
            width = 14,
            height = length(graficos_sem_ntnb) * 4 + 2,
            dpi = 300,
            bg = "white"
          )
          
          print(paste0("✓ Gráfico sem NTNB salvo como 'correlacao_sem_ntnb_", timestamp, ".png'"))
        }
        
      } else {
        print("✗ Nenhum gráfico válido foi criado")
      }
      
      # Análise de correlação entre preços e yields
      print("\n=== ANÁLISE DE CORRELAÇÃO PREÇO vs YIELD ===")
      
      correlacoes <- dados_completos %>%
        group_by(tipo) %>%
        summarise(
          correlacao_preco_yield = round(cor(preco, yield, use = "complete.obs"), 3),
          n_observacoes = n(),
          .groups = "drop"
        )
      
      print(correlacoes)
      
      # Estatísticas descritivas
      print("\n=== ESTATÍSTICAS DESCRITIVAS ===")
      estatisticas <- dados_completos %>%
        group_by(tipo) %>%
        summarise(
          # Preços
          preco_medio = round(mean(preco, na.rm = TRUE), 2),
          preco_min = round(min(preco, na.rm = TRUE), 2),
          preco_max = round(max(preco, na.rm = TRUE), 2),
          # Yields
          yield_medio = round(mean(yield, na.rm = TRUE), 2),
          yield_min = round(min(yield, na.rm = TRUE), 2),
          yield_max = round(max(yield, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      print(estatisticas)
      
    } else {
      print("✗ Não foi possível preparar dados completos para correlação")
    }
    
  } else {
    print("✗ Não foi possível preparar dados para o gráfico de yields")
  }
  
} else {
  print("✗ Dados insuficientes para criar visualização")
}

print("Script finalizado!")
