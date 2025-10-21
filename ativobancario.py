import pandas as pd
import os
import glob  # Para encontrar facilmente todos os arquivos CSV

# --- Configuração ---
# 1. Caminho para a pasta que contém seus arquivos CSV mensais
INPUT_FOLDER_PATH = 'ativo_2011'  # Caminho relativo a partir do script

# 2. Nome para o arquivo Excel consolidado de saída
OUTPUT_EXCEL_PATH = 'consolidated_bank_asset_summary.xlsx'

# 3. Códigos dos Bancos Alvo (garanta que sejam strings, pois códigos de CSV frequentemente são lidos como strings)
TARGET_BANK_CODES = [49906, 10069, 10045, 30379, 51152, 51626]

# 4. Nomes Exatos das Colunas nos seus CSVs (Sensível a MaiúsculasMinúsculas!)
BANK_CODE_COLUMN = 'Código'  # Coluna que contém os códigos únicos dos bancos
ASSET_COLUMN = 'Ativo Total'     # Coluna que contém os valores de 'Ativo Total'
DATE_COLUMN = 'Data'      # Coluna que contém a data do mêsrelatório

# 5. Número máximo de linhas a serem lidas do início de cada CSV
MAX_ROWS_TO_READ = 20
# --- Fim da Configuração ---


def aggregate_bank_assets(folder_path, output_path, target_codes, code_col, asset_col, date_col, max_rows):
    
    #Lê as primeiras 'max_rows' de CSVs mensais, soma os ativos para códigos de banco específicos
    #encontrados nessas linhas e consolida as somas mensais em um arquivo Excel.
    
    monthly_results = []  # Lista para armazenar tuplas de (data, soma_total_ativo)

    # Usa glob para encontrar todos os arquivos CSV na pasta
    csv_files = glob.glob(os.path.join(folder_path, '*.csv'))

    if not csv_files:
        print(
            f"ERRO: Nenhum arquivo CSV encontrado na pasta especificada '{os.path.abspath(folder_path)}'")
        return

    print(
        f"Encontrados {len(csv_files)} arquivos CSV. Processando (lendo no máx. {max_rows} linhas por arquivo)...")

    for file_path in csv_files:
        filename = os.path.basename(file_path)
        print(f"Processando arquivo {filename}...")

        try:
            # Lê apenas as primeiras MAX_ROWS_TO_READ linhas do arquivo CSV.
            # Isso inclui a linha de cabeçalho (se presente na linha 1).
            # Força a leitura da coluna de código do banco como string.
            df = pd.read_csv(
                file_path,
                sep=';',  # Ajuste o separador se necessário (ex ',' para CSV)
                dtype={code_col: str},
                nrows=max_rows,  # Lê apenas as primeiras 'max_rows' linhas
            )

            # --- Validação de Dados ---
            # 1. Verifica se as colunas necessárias existem (com base no cabeçalho lido nas primeiras linhas)
            required_cols = [code_col, asset_col, date_col]
            if not all(col in df.columns for col in required_cols):
                missing_cols = [
                    col for col in required_cols if col not in df.columns]
                print(
                    f"  AVISO: Pulando '{filename}'. Colunas obrigatórias ausentes nas primeiras {max_rows} linhas: {missing_cols}")
                continue  # Pula este arquivo

            # 2. Verifica se o arquivo está vazio após ler os cabeçalhos (se houver)
            if df.empty:
                print(
                    f"  AVISO: Pulando '{filename}'. Nenhum dado encontrado nas primeiras {max_rows} linhas (ou arquivo está vazio).")
                continue

            # --- Extração e Cálculo de Dados ---
            # 1. Extrai a data (assumindo que é a mesma para a parte relevante do arquivo, pega da primeira linha de dados)
            #    Trata o caso potencial onde o df só tem cabeçalho após o limite de nrows
            report_date = None
            if not df.empty:
                # Obtém a data da primeira linha lida
                report_date = df.iloc[0][date_col]

            # 2. Limpa e Converte a coluna de Ativo para numérico
            # Garante que a coluna seja do tipo string primeiro para usar o acessador .str com segurança
            df[asset_col] = df[asset_col].astype(str)
            # Remove separadores de milhar (pontos)
            df[asset_col] = df[asset_col].str.replace('.', '', regex=False)
            # Agora converte as strings limpas para numérico, tratando erros
            df[asset_col] = pd.to_numeric(df[asset_col], errors='coerce')

            # 3. Filtra linhas onde o Código do Banco é um dos alvos
            filtered_df = df[df[code_col].isin(target_codes)]

            # 4. Soma o 'Ativo Total' para os bancos filtrados dentro das linhas lidas
            total_asset_sum = filtered_df[asset_col].sum()

            # --- Armazena Resultado ---
            # Garante que realmente obtivemos uma data antes de anexar
            if report_date is not None:
                monthly_results.append(
                    {'Date': report_date, 'Aggregated_Total_Asset': total_asset_sum})
                print(
                    f"  - Data: {report_date}, Soma para bancos alvo (primeiras {max_rows} linhas): {total_asset_sum:,.2f}")
            else:
                print(
                    f"  AVISO: Não foi possível extrair a data das primeiras {max_rows} linhas para '{filename}'. Pulando resumo do arquivo.")

        except pd.errors.EmptyDataError:
            print(f"  AVISO: Pulando '{filename}'. Arquivo está vazio.")
        except FileNotFoundError:
            print(
                f"  ERRO: Arquivo não encontrado durante o loop de processamento: '{filename}'")
        except Exception as e:
            # Isso capturará outros erros que podem ocorrer mesmo dentro das primeiras 20 linhas
            print(f"  ERRO: Não foi possível processar o arquivo '{filename}'. Motivo: {e}")

    # --- Consolidação Final e Saída ---
    if not monthly_results:
        print("Nenhum dado válido processado das linhas iniciais dos arquivos. Não é possível criar o arquivo de saída.")
        return

    print("\nConsolidando resultados...")
    # Cria um DataFrame a partir dos resultados coletados
    summary_df = pd.DataFrame(monthly_results)

    # Opcional: Converte a coluna Data para objetos datetime e ordena
    try:
        summary_df['Date'] = pd.to_datetime(summary_df['Date'])
        summary_df = summary_df.sort_values(by='Date')
        print("Resultados ordenados por data.")
    except Exception as e:
        print(
            f"Aviso: Não foi possível analisar ou ordenar as datas consistentemente. Erro: {e}. A saída não será ordenada.")

    # Salva o resumo consolidado em um arquivo Excel
    try:
        print(f"Salvando resumo consolidado em {output_path}")
        summary_df.to_excel(output_path, index=False, engine='openpyxl')
        print("Consolidação completa! Arquivo de saída criado.")
    except Exception as e:
        print(
            f"ERRO: Não foi possível salvar o arquivo Excel de saída '{output_path}'. Motivo: {e}")
        print("Dados que teriam sido salvos:")
        print(summary_df)


# --- Bloco de execução principal ---
if __name__ == '__main__':
    if not os.path.isdir(INPUT_FOLDER_PATH):
        print(
            f"ERRO: Pasta de entrada não encontrada em '{os.path.abspath(INPUT_FOLDER_PATH)}'. Por favor, verifique a variável 'INPUT_FOLDER_PATH'.")
    else:
        aggregate_bank_assets(
            INPUT_FOLDER_PATH,
            OUTPUT_EXCEL_PATH,
            TARGET_BANK_CODES,
            BANK_CODE_COLUMN,
            ASSET_COLUMN,
            DATE_COLUMN,
            MAX_ROWS_TO_READ  # Passa o novo parâmetro aqui
        )
