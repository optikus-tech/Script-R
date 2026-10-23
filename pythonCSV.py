import os
from datetime import datetime
import psutil as p
import pandas as pd
#Dicionário de listas para armazenar os dados coletados
capturas = {
"data_coleta": [],
"cpu_uso_porcentagem": [],
"cpu_status": [],
"ram_uso_porcentagem": [],
"ram_status": [],
"disco_uso_porcentagem": [],
"disco_status": []
}
#Regra simples de discretização de status
def obter_status(percentual):
    if percentual < 80:
        return "NORMAL"
    elif percentual < 90:
        return "ALERTA"
    return "CRÍTICO"
def coletar_dados():
    agora = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    # Coleta CPU
    uso_cpu = p.cpu_percent(interval=10)
    status_cpu = obter_status(uso_cpu)
    # Coleta RAM
    uso_ram = p.virtual_memory().percent
    status_ram = obter_status(uso_ram)
    # Coleta Disco
    caminho_disco = os.environ.get("SystemDrive", "C:") + "\\" if os.name == "nt" else "/"
    uso_disco = p.disk_usage(caminho_disco).percent
    status_disco = obter_status(uso_disco)
    # Popula a estrutura de dicionário de listas
    capturas["data_coleta"].append(agora)
    capturas["cpu_uso_porcentagem"].append(uso_cpu)
    capturas["cpu_status"].append(status_cpu)
    capturas["ram_uso_porcentagem"].append(uso_ram)
    capturas["ram_status"].append(status_ram)
    capturas["disco_uso_porcentagem"].append(uso_disco)
    capturas["disco_status"].append(status_disco)
def main():
    print(f"Iniciando a coleta de 20 capturas...")
    for i in range(20):
        coletar_dados()
        print(f"Captura {i}/20 realizada com sucesso.")

    df = pd.DataFrame(capturas)
    nome_arquivo = "capturas_sistema2.csv"
    df.to_csv(nome_arquivo, index=False, encoding="utf-8")
    print(f"Coleta concluída! Dados salvos em '{nome_arquivo}'.")
if __name__ == "__main__":
    main()