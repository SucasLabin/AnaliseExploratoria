# bibliotecas
library(readxl)
library(dplyr)
library(ggplot2)


# a) Importar base e transformar variáveis qualitativas em fatores

base <- read_excel("Base_trabalho.xlsx")

base <- base %>%
  mutate(
    escolaridade = as.factor(escolaridade),
    reincidente = as.factor(reincidente),
    sexo = as.factor(sexo),
    casado = as.factor(casado),
    filhos = as.factor(filhos)
  )

# Visualizar estrutura
glimpse(base)


# b) Estatísticas descritivas: média, quartis e mediana

variaveis <- c("score_periculosidade", "idade", "tempo_preso")

estatisticas <- base %>%
  summarise(across(
    all_of(variaveis),
    list(
      media = mean,
      q1 = ~ quantile(.x, 0.25),
      mediana = median,
      q3 = ~ quantile(.x, 0.75)
    ),
    .names = "{.col}_{.fn}"
  ))


# c) Gráfico de dispersão entre tempo_preso e score_periculosidade

p_disp <- ggplot(base, aes(x = tempo_preso, y = score_periculosidade)) +
  geom_point(alpha = 0.6, color = "#2E86AB") +
  geom_smooth(method = "lm", color = "#E07A5F", se = TRUE) +
  labs(
    title = "Relação entre Tempo Preso e Score de Periculosidade",
    x = "Tempo Preso (meses)",
    y = "Score de Periculosidade"
  ) +
  theme_minimal()

# Salvar figura
ggsave("figura_dispersa_tempo_score.png", p_disp, width = 8, height = 5)


# d) Correlação entre tempo_preso e score_periculosidade

correlacao <- cor(base$tempo_preso, base$score_periculosidade, use = "complete.obs")


# e) Variância, desvio padrão e amplitude

estat_disp <- base %>%
  summarise(across(
    all_of(variaveis),
    list(
      variancia = var,
      desvio_padrao = sd,
      amplitude = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

