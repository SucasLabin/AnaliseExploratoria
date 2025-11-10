# Pacotes necessários
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

#importando base

base <- readxl::read_xlsx('Base_trabalho.xlsx')

# Transformar variáveis qualitativas em fatores
base <- base %>%
  mutate(
    escolaridade = as.factor(escolaridade),
    reincidente = as.factor(reincidente),
    sexo = as.factor(sexo),
    casado = as.factor(casado)
  )

# --- a) Visualizar estrutura ---
glimpse(base)

# --- b) Análise de dados faltantes ---
faltantes <- colSums(is.na(base))
print("Número de dados faltantes por variável:")
print(faltantes)

# Paleta de cores harmoniosa
paleta <- "Set2"

# --- d) p1: Histograma da variável idade ---
p1 <- ggplot(base, aes(x = idade)) +
  geom_histogram(bins = 20, fill = "#66c2a5", color = "white") +
  labs(title = "Distribuição de Idades", x = "Idade", y = "Frequência") +
  theme_minimal()

# --- d) Boxplot do tempo preso ---
p2 <- ggplot(base, aes(y = tempo_preso)) +
  geom_boxplot(fill = "#66c2a5") +
  labs(title = "Boxplot do Tempo Preso", y = "Tempo (meses)")

# --- e) Boxplot do score_periculosidade por escolaridade ---
p3 <- ggplot(base, aes(x = escolaridade, y = score_periculosidade, fill = escolaridade)) +
  geom_boxplot() +
  scale_fill_brewer(palette = paleta) +
  labs(title = "Score de Periculosidade por Escolaridade",
       x = "Escolaridade", y = "Score de Periculosidade")

# --- f) p4: Gráfico de barras empilhadas relativas da variável reincidente ---

reinc_df <- base %>%
  count(reincidente) %>%
  mutate(prop = n / sum(n))

# Criar o gráfico empilhado relativo (aqui será uma única barra empilhada)
p4 <- ggplot(reinc_df, aes(x = "", y = prop, fill = reincidente)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 5, color = "white") +
  scale_fill_brewer(palette = paleta, name = "Reincidente") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proporção de Reincidência",
       x = NULL, y = "Proporção (%)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# --- g) Salvar figuras em PNG e PDF ---
ggsave("grafico_idade_barras.png", p1, width = 6, height = 4)
ggsave("box_tempo_preso.png", p2, width = 6, height = 4)
ggsave("box_score_escolaridade.png", p3, width = 6, height = 4)
ggsave("barras_reincidente_empilhado.png", p4, width = 6, height = 4)

# Também salvar todos os gráficos em um único PDF
pdf("graficos_analise.pdf", width = 7, height = 5)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()
