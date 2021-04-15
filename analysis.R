### Passos para análise

## Importar ggplot2
## Ler base de dados para data.frame
## Pré-processar dados:
## -- Criar outro data.frame com formatos corretos
## -- Para cada character "sim" no data.frame original, deixar True no outro
## -- Para cada character "não" no data.frame original, deixar False no outro
## -- Para cada character lista de atividades no data.frame original, deixar como vector de characters (onde cada character é uma atividade) no outro
## ---- Usar strsplit
## -- Calcular frequências relativas para plotar histogramas
## -- Calcular frequências relativas para plotar gráficos de barras entre pares de variáveis
## -- Calcular correlação de Kendall entre pares de variáveis
