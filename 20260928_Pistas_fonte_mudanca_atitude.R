
###Pistas da fonte e mudança de atitude: a influência das elites políticas no Brasil


##Padrao grafico em preto e branco - 28/09/2026
#PT: quadrado; Lula: circulo; Bolsonaro: triangulo.
#Identificacao positiva: preenchido; rejeicao: vazado.
#A mesma chave e usada no texto principal e no apendice.
simbolos_grupos <- c(pt = 22, antipt = 22, lula = 21, antilula = 21,
                     bolsonaro = 24, antibolsonaro = 24)
fundos_grupos <- c(pt = "black", antipt = "white", lula = "black",
                   antilula = "white", bolsonaro = "black", antibolsonaro = "white")
#O losango identifica a condicao conjunta no grafico da amostra total.
simbolos_tratamentos <- c(conpt = 22, conlula = 21, conbolsonaro = 24,
                          conlulabolsonaro = 23)

#Exportacao com as proporcoes originais dos graficos inseridos nos documentos.
pasta_graficos <- getOption("pistas.pasta_graficos", "Graficos_20260928")
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
abrir_grafico <- function(numero, largura, altura) {
  ragg::agg_png(file.path(pasta_graficos, paste0("Grafico", numero, ".png")),
      width = largura * 3, height = altura * 3, res = 375,
      pointsize = 12, background = "white")
  par(col = "black", col.axis = "black", col.lab = "black", col.main = "black")
}

##Download dataframe

library(haven)

arquivo_dados <- if (file.exists("data_cues.SAV")) "data_cues.SAV" else "data.SAV"
data <- read_sav(arquivo_dados)
data$id_respondente <- seq_len(nrow(data))

##Composição da amostra e comparação com o Censo 2022

#Percentuais de referência obtidos no Censo Demográfico 2022 (IBGE/SIDRA).
#Escolaridade: Tabela 10061, pessoas de 18 anos ou mais por nível de instrução.
#Renda: Tabela 10300, pessoas de 14 anos ou mais por classes de rendimento
#domiciliar per capita em salários mínimos. As classes finas foram agregadas para ficarem comparáveis às três faixas do questionário.

pct <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)

tabela_composicao_censo <- data.frame(
  Caracteristica = c(
    "Sexo - Feminino",
    "Sexo - Masculino",
    "Região - Norte",
    "Região - Nordeste",
    "Região - Sudeste",
    "Região - Sul",
    "Região - Centro-Oeste",
    "Escolaridade - sem instrução ou fundamental incompleto",
    "Escolaridade - fundamental completo ou médio incompleto",
    "Escolaridade - médio completo ou superior incompleto",
    "Escolaridade - superior completo ou mais",
    "Renda - até 2 salários mínimos",
    "Renda - 2 a 5 salários mínimos",
    "Renda - acima de 5 salários mínimos"
  ),
  Amostra = c(
    pct(data$GENERO == 2),
    pct(data$GENERO == 1),
    pct(data$REGION_BR == 1),
    pct(data$REGION_BR == 2),
    pct(data$REGION_BR == 3),
    pct(data$REGION_BR == 4),
    pct(data$REGION_BR == 5),
    pct(data$EDU2 %in% c(1, 2)),
    pct(data$EDU2 == 3),
    pct(data$EDU2 == 4),
    pct(data$EDU2 %in% c(5, 6, 7, 8)),
    pct(data$INCOME == 1),
    pct(data$INCOME == 2),
    pct(data$INCOME == 3)
  ),
  Censo_2022 = c(
    51.5, 48.5,
    8.5, 26.9, 41.8, 14.7, 8.0,
    32.0, 15.4, 35.8, 16.8,
    82.2, 13.7, 4.1
  ),
  check.names = FALSE
)

print(tabela_composicao_censo, row.names = FALSE)


##Balanceamento

library(dplyr)
library(broom)

#Lista de variaveis para checar balanceamento
variaveis <- c("GENERO", "EDAD", "REGION_BR", "EDU2", "INCOME", "P1", "P2", "P3_1", "P3_2", "P3_3", "P3_4")

#Funcao para comparar cada grupo tratamento com o grupo controle
resultados <- lapply(variaveis, function(var) {
  formula <- as.formula(paste(var, "~ factor(GRUPO)"))
  modelo <- lm(formula, data = data)
  resumo <- tidy(modelo)
  resumo$variavel <- var
  return(resumo)
})

#Juntar todos os resultados
tabela_dif_medias <- bind_rows(resultados)

#Filtrar apenas as comparacoes com o grupo controle
tabela_dif_medias_filtrada <- tabela_dif_medias %>%
  filter(grepl("factor\\(GRUPO\\)", term)) %>%
  select(variavel, term, estimate, std.error, p.value)

##Overview grupo controle

#Criando um subset com os casos controle
data_controle <- subset(data, GRUPO == 1)

#Excluindo colunas indesejadas
data_controle <- data_controle[, !(names(data_controle) %in% c("key","numericalId",
                                                               "accessCount","startTime",
                                                               "duration","status",
                                                               "type","CodPanelista",
                                                               "DEVICE","ppi_tp",
                                                               "NSE_BR","ESCOLARIDAD",
                                                               "BLOCO_1","endTime",
                                                               "GRUPO"))]
data_controle <- data_controle[, -(27:76)]

#Analise descritiva

library(summarytools)

#Descritivo geral das variáveis
if (interactive()) view(dfSummary(data_controle, graph.col = TRUE))

##Resultados

#Recodificacoes

#Para olhar os dados agregados, transformando em formato longo

library(tidyverse)

df_long_bruto <- data %>%
  pivot_longer(
    cols = matches("^PBS\\d{1,2}[A-E]$"),
    names_to = c("questao", "condicao"),
    names_pattern = "PBS(\\d{1,2})([A-E])",
    values_to = "suporte"
  ) %>%
  mutate(
    questao = as.integer(questao),
    condicao = recode(condicao, "A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5)
  )


#Incidência e testes das respostas "Não sei"

nomes_questoes <- c("Privatização da Petrobras",
                    "Maconha para uso medicinal",
                    "Educação sexual nas escolas",
                    "Golpe militar em certas circunstâncias",
                    "Proibição da venda de armas de fogo",
                    "Cotas raciais",
                    "Democracia sem partidos",
                    "Aumentar impostos sobre os ricos",
                    "Política ambiental atrapalha o desenvolvimento",
                    "Gestão internacional da Amazônia",
                    "Agronegócio ocupar áreas de floresta",
                    "Impostos para preservar o meio ambiente",
                    "Médicos estrangeiros para o SUS")

df_nao_sei <- df_long_bruto %>%
  filter(!is.na(suporte)) %>%
  mutate(
    nao_sei = as.numeric(suporte == 3),
    recebeu_tratamento = as.numeric(GRUPO != 1),
    grupo_experimental = factor(
      GRUPO,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Controle", "Lula", "PT", "Bolsonaro", "Lula vs. Bolsonaro")
    )
  )

tabela_nao_sei <- df_nao_sei %>%
  group_by(questao) %>%
  summarise(
    respostas_validas = n(),
    nao_sei = sum(nao_sei),
    percentual_nao_sei = round(mean(nao_sei) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(questao_tematica = nomes_questoes[questao]) %>%
  select(questao, questao_tematica, respostas_validas, nao_sei, percentual_nao_sei)

print(tabela_nao_sei)

tabela_nao_sei_condicao <- df_nao_sei %>%
  group_by(grupo_experimental) %>%
  summarise(
    respostas_validas = n(),
    nao_sei = sum(nao_sei),
    percentual_nao_sei = round(mean(nao_sei) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(diferenca_bruta_controle_pp = percentual_nao_sei - percentual_nao_sei[grupo_experimental == "Controle"])

print(tabela_nao_sei_condicao)

vcov_cluster <- function(modelo, cluster) {
  X <- model.matrix(modelo)
  u <- residuals(modelo)
  cluster <- as.factor(cluster)
  ok <- complete.cases(X, u, cluster)
  X <- X[ok, , drop = FALSE]
  u <- u[ok]
  cluster <- droplevels(cluster[ok])
  bread <- solve(crossprod(X))
  meat <- matrix(0, ncol(X), ncol(X))
  for (cl in levels(cluster)) {
    idx <- cluster == cl
    score <- colSums(X[idx, , drop = FALSE] * u[idx])
    meat <- meat + tcrossprod(score)
  }
  G <- nlevels(cluster)
  N <- nrow(X)
  K <- ncol(X)
  ajuste <- (G / (G - 1)) * ((N - 1) / (N - K))
  ajuste * bread %*% meat %*% bread
}

tidy_cluster <- function(modelo, cluster) {
  vc <- vcov_cluster(modelo, cluster)
  se <- sqrt(diag(vc))
  estat <- coef(modelo) / se
  data.frame(
    term = names(coef(modelo)),
    estimate = coef(modelo),
    std.error = se,
    statistic = estat,
    p.value = 2 * pt(abs(estat), df = length(unique(cluster)) - 1, lower.tail = FALSE),
    row.names = NULL
  )
}

modelo_nao_sei_tratamento <- lm(nao_sei ~ recebeu_tratamento, data = df_nao_sei)
teste_nao_sei_tratamento <- tidy_cluster(modelo_nao_sei_tratamento, df_nao_sei$id_respondente)
print(teste_nao_sei_tratamento)

modelo_nao_sei_condicao <- lm(nao_sei ~ grupo_experimental, data = df_nao_sei)
teste_nao_sei_condicao <- tidy_cluster(modelo_nao_sei_condicao, df_nao_sei$id_respondente)
print(teste_nao_sei_condicao)

modelo_nao_sei_condicao_questao <- lm(nao_sei ~ grupo_experimental + factor(questao), data = df_nao_sei)
teste_nao_sei_condicao_questao <- tidy_cluster(modelo_nao_sei_condicao_questao, df_nao_sei$id_respondente)

tabela_teste_nao_sei_condicao <- teste_nao_sei_condicao_questao %>%
  filter(grepl("^grupo_experimental", term)) %>%
  mutate(
    grupo_experimental = sub("^grupo_experimental", "", term),
    diferenca_ajustada_pp = round(estimate * 100, 1),
    erro_padrao_pp = round(std.error * 100, 1),
    p.value = round(p.value, 3)
  ) %>%
  select(grupo_experimental, diferenca_ajustada_pp, erro_padrao_pp, p.value)

print(tabela_teste_nao_sei_condicao)


df_long <- df_long_bruto %>%
  filter(!is.na(suporte))

df_long <- df_long %>%
  mutate(suporte = na_if(suporte, 3))

df_long <- df_long %>%
  mutate(suporte = case_when(
    suporte == 2 ~ 1,
    suporte == 1 ~ 0,
    TRUE ~ suporte
  ))

#Colocando tudo para uma posicao mais liberal

df_long <- df_long %>%
  mutate(suporte = ifelse(questao %in% c(1, 4, 7, 9, 11),
                          1 - suporte,  
                          suporte))

#Criando variaveis de identidade

df_long$pt <- ifelse(df_long$P3_1 >= 7, 1, 0)
df_long$antipt <- ifelse(df_long$P3_1 <= 3, 1, 0)
df_long$bolsonaro <- ifelse(df_long$P3_4 >= 7, 1, 0)
df_long$antibolsonaro <- ifelse(df_long$P3_4 <= 3, 1, 0)
df_long$lula <- ifelse(df_long$P3_2 >= 7, 1, 0)
df_long$antilula <- ifelse(df_long$P3_2 <= 3, 1, 0)

#Criando variaveis de condicoes

df_long$controle <- ifelse(df_long$condicao == 1, 1, 0)
df_long$conlula <- ifelse(df_long$condicao == 2, 1, 0)
df_long$conpt <- ifelse(df_long$condicao == 3, 1, 0)
df_long$conbolsonaro <- ifelse(df_long$condicao == 4, 1, 0)
df_long$conlulabolsonaro <- ifelse(df_long$condicao == 5, 1, 0)

#PT (Grafico 1)

#Regressoes para o tratamento PT
model_pt <- lm(suporte ~ conpt, data = df_long[df_long$pt == 1 & (df_long$conpt == 1 | df_long$controle == 1),])
model_antipt <- lm(suporte ~ conpt, data = df_long[df_long$antipt == 1 & (df_long$conpt == 1 | df_long$controle == 1),])

#Coeficientes
pt_treat <- coef(model_pt)[2]
antipt_treat <- coef(model_antipt)[2]

#Erros padrao
pt_se <- coef(summary(model_pt))["conpt", "Std. Error"]
antipt_se <- coef(summary(model_antipt))["conpt", "Std. Error"]

#Intervalos de confianca 95%
pt_lower <- pt_treat - 1.96 * pt_se
pt_upper <- pt_treat + 1.96 * pt_se

antipt_lower <- antipt_treat - 1.96 * antipt_se
antipt_upper <- antipt_treat + 1.96 * antipt_se

#Posicao no eixo x
x_pos <- c(1, 2)

#Grafico
abrir_grafico(1, 557, 557)
plot(NULL, xlim = c(0.5, 2.5), ylim = range(c(pt_lower, pt_upper, antipt_lower, antipt_upper)), 
     xlab = "Grupo de Identidade", ylab = "Aumento na probabilidade de apoio à política liberal", xaxt = "n", 
     main = "")

axis(1, at = x_pos, labels = c("Petista", "Antipetista"))
abline(h = 0, lty = 2)

#Pontos e barras de confianca
segments(1, pt_lower, 1, pt_upper, col = "black")
points(1, pt_treat, pch = simbolos_grupos["pt"], bg = fundos_grupos["pt"], col = "black", cex = 1.5)

segments(2, antipt_lower, 2, antipt_upper, col = "black")
points(2, antipt_treat, pch = simbolos_grupos["antipt"], bg = fundos_grupos["antipt"], col = "black", cex = 1.5)

dev.off()

#Grafico 2
#Regressões para tratamento conlulabolsonaro 
model_lula_vs_bol_lula       <- lm(suporte ~ conlulabolsonaro, data = df_long[df_long$lula == 1 & (df_long$conlulabolsonaro == 1 | df_long$controle == 1),])
model_lula_vs_bol_antilula   <- lm(suporte ~ conlulabolsonaro, data = df_long[df_long$antilula == 1 & (df_long$conlulabolsonaro == 1 | df_long$controle == 1),])
model_lula_vs_bol_bolsonaro  <- lm(suporte ~ conlulabolsonaro, data = df_long[df_long$bolsonaro == 1 & (df_long$conlulabolsonaro == 1 | df_long$controle == 1),])
model_lula_vs_bol_antibolsonaro <- lm(suporte ~ conlulabolsonaro, data = df_long[df_long$antibolsonaro == 1 & (df_long$conlulabolsonaro == 1 | df_long$controle == 1),])

#Coeficientes e erros padrão
treat_lulavsbolsonaro <- c(
  coef(model_lula_vs_bol_lula)[2],
  coef(model_lula_vs_bol_antilula)[2],
  coef(model_lula_vs_bol_bolsonaro)[2],
  coef(model_lula_vs_bol_antibolsonaro)[2]
)

se_lulavsbolsonaro <- c(
  coef(summary(model_lula_vs_bol_lula))["conlulabolsonaro", "Std. Error"],
  coef(summary(model_lula_vs_bol_antilula))["conlulabolsonaro", "Std. Error"],
  coef(summary(model_lula_vs_bol_bolsonaro))["conlulabolsonaro", "Std. Error"],
  coef(summary(model_lula_vs_bol_antibolsonaro))["conlulabolsonaro", "Std. Error"]
)

lower_lulavsbolsonaro <- treat_lulavsbolsonaro - 1.96 * se_lulavsbolsonaro
upper_lulavsbolsonaro <- treat_lulavsbolsonaro + 1.96 * se_lulavsbolsonaro

#Regressoes para tratamento conlula
model_lula <- lm(suporte ~ conlula, data = df_long[df_long$lula == 1 & (df_long$conlula == 1 | df_long$controle == 1),])
model_antilula <- lm(suporte ~ conlula, data = df_long[df_long$antilula == 1 & (df_long$conlula == 1 | df_long$controle == 1),])

#Regressões para tratamento conbolsonaro
model_bolsonaro <- lm(suporte ~ conbolsonaro, data = df_long[df_long$bolsonaro == 1 & (df_long$conbolsonaro == 1 | df_long$controle == 1),])
model_antibolsonaro <- lm(suporte ~ conbolsonaro, data = df_long[df_long$antibolsonaro == 1 & (df_long$conbolsonaro == 1 | df_long$controle == 1),])

#Coeficientes
treat_lula <- c(coef(model_lula)[2], coef(model_antilula)[2])
treat_bolsonaro <- c(coef(model_bolsonaro)[2], coef(model_antibolsonaro)[2])

#Erros padrao
se_lula <- c(coef(summary(model_lula))["conlula", "Std. Error"], coef(summary(model_antilula))["conlula", "Std. Error"])
se_bolsonaro <- c(coef(summary(model_bolsonaro))["conbolsonaro", "Std. Error"], coef(summary(model_antibolsonaro))["conbolsonaro", "Std. Error"])

#Intervalos de confianca 95%
lower_lula <- treat_lula - 1.96 * se_lula
upper_lula <- treat_lula + 1.96 * se_lula

lower_bolsonaro <- treat_bolsonaro - 1.96 * se_bolsonaro
upper_bolsonaro <- treat_bolsonaro + 1.96 * se_bolsonaro

#Posicoes no eixo x
x_pos <- c(1, 2, 3)  # Lula, Bolsonaro, Lula vs Bolsonaro
offset <- 0.15

#Ajustar limite do grafico
ylim_max <- max(c(upper_lula, upper_bolsonaro, upper_lulavsbolsonaro)) + 0.05
ylim_min <- min(c(lower_lula, lower_bolsonaro, lower_lulavsbolsonaro))

#Grafico
abrir_grafico(2, 1115, 557)
plot(NULL, xlim = c(0.7, 3.3), 
     ylim = c(ylim_min, ylim_max), 
     xlab = "Condição de Tratamento", ylab = "Aumento na probabilidade de apoio à política liberal", 
     xaxt = "n", main = "Efeito Médio do Tratamento por Identidade")

axis(1, at = x_pos, labels = c("Lula", "Bolsonaro", "Lula vs Bolsonaro"))
abline(h = 0, lty = 2)

#Lula
segments(1 - offset, lower_lula[1], 1 - offset, upper_lula[1], col = "black")
points(1 - offset, treat_lula[1], pch = simbolos_grupos["lula"], bg = fundos_grupos["lula"], col = "black", cex = 1.5)
text(1 - offset, upper_lula[1] + 0.02, "Lulista", cex = 0.8, col = "black")

segments(1 + offset, lower_lula[2], 1 + offset, upper_lula[2], col = "black")
points(1 + offset, treat_lula[2], pch = simbolos_grupos["antilula"], bg = fundos_grupos["antilula"], col = "black", cex = 1.5)
text(1 + offset, upper_lula[2] + 0.02, "Antilulista", cex = 0.8, col = "black")

#Bolsonaro
segments(2 - offset, lower_bolsonaro[1], 2 - offset, upper_bolsonaro[1], col = "black")
points(2 - offset, treat_bolsonaro[1], pch = simbolos_grupos["bolsonaro"], bg = fundos_grupos["bolsonaro"], col = "black", cex = 1.5)
text(2 - offset, upper_bolsonaro[1] + 0.04, "Bolsonarista", cex = 0.8, col = "black")

segments(2 + offset, lower_bolsonaro[2], 2 + offset, upper_bolsonaro[2], col = "black")
points(2 + offset, treat_bolsonaro[2], pch = simbolos_grupos["antibolsonaro"], bg = fundos_grupos["antibolsonaro"], col = "black", cex = 1.5)
text(2 + offset, upper_bolsonaro[2] + 0.02, "Antibolsonarista", cex = 0.8, col = "black")

#Lula vs Bolsonaro
segments(3 - offset, lower_lulavsbolsonaro[1], 3 - offset, upper_lulavsbolsonaro[1], col = "black")
points(3 - offset, treat_lulavsbolsonaro[1], pch = simbolos_grupos["lula"], bg = fundos_grupos["lula"], col = "black", cex = 1.5)
text(3 - offset - 0.03, upper_lulavsbolsonaro[1] + 0.02, "Lulista", cex = 0.8, col = "black")

segments(3 + offset, lower_lulavsbolsonaro[2], 3 + offset, upper_lulavsbolsonaro[2], col = "black")
points(3 + offset, treat_lulavsbolsonaro[2], pch = simbolos_grupos["antilula"], bg = fundos_grupos["antilula"], col = "black", cex = 1.5)
text(3 + offset, upper_lulavsbolsonaro[2] + 0.02, "Antilulista", cex = 0.8, col = "black")

segments(3 - offset/2, lower_lulavsbolsonaro[3], 3 - offset/2, upper_lulavsbolsonaro[3], col = "black")
points(3 - offset/2, treat_lulavsbolsonaro[3], pch = simbolos_grupos["bolsonaro"], bg = fundos_grupos["bolsonaro"], col = "black", cex = 1.5)
text(3 - offset/2, upper_lulavsbolsonaro[3] + 0.02, "Bolsonarista", cex = 0.8, col = "black")

segments(3 + offset/2, lower_lulavsbolsonaro[4], 3 + offset/2, upper_lulavsbolsonaro[4], col = "black")
points(3 + offset/2, treat_lulavsbolsonaro[4], pch = simbolos_grupos["antibolsonaro"], bg = fundos_grupos["antibolsonaro"], col = "black", cex = 1.5)
text(3 + offset/2, upper_lulavsbolsonaro[4] + 0.02, "Antibolsonarista", cex = 0.8, col = "black")

#Linha base desenhada antes dos pontos
dev.off()

#Grafico 3
#Lista de tratamentos e grupos
tratamentos <- c("conlula", "conpt", "conbolsonaro", "conlulabolsonaro")
grupos_binarios <- c("lula", "antilula", "pt", "antipt", "bolsonaro", "antibolsonaro")

#Inicializar matrizes
coef_all <- matrix(NA, nrow = length(tratamentos), ncol = length(grupos_binarios))
se_all <- matrix(NA, nrow = length(tratamentos), ncol = length(grupos_binarios))

#Rodar todas as regressões e preencher coef_all e se_all
modelos_lista <- list()  

for (t in 1:length(tratamentos)) {
  trat <- tratamentos[t]
  for (g in 1:length(grupos_binarios)) {
    grupo <- grupos_binarios[g]
    formula <- as.formula(paste0("suporte ~ ", trat))
    dados <- df_long[df_long[[grupo]] == 1 & (df_long[[trat]] == 1 | df_long$controle == 1), ]
    
    if (nrow(dados) > 5) {
      modelo <- lm(formula, data = dados)
      coef_all[t, g] <- coef(modelo)[2]
      se_all[t, g] <- coef(summary(modelo))[2, "Std. Error"]
      
      print(paste0("Tratamento: ", trat, " | Grupo: ", grupo))
      print(summary(modelo))
      
      nome_modelo <- paste0(trat, "_", grupo)
      modelos_lista[[nome_modelo]] <- modelo
    } else {
      coef_all[t, g] <- NA
      se_all[t, g] <- NA
    }
  }
}


#Tabela A2: coeficientes dos modelos de probabilidade linear

decimal_comma <- function(x) sub("\\.", ",", sprintf("%.3f", x))

formatar_coeficiente <- function(modelo, termo) {
  resumo <- coef(summary(modelo))
  beta <- resumo[termo, "Estimate"]
  se <- resumo[termo, "Std. Error"]
  p_valor <- resumo[termo, "Pr(>|t|)"]
  estrelas <- ifelse(p_valor < 0.001, "***",
                     ifelse(p_valor < 0.01, "**",
                            ifelse(p_valor < 0.05, "*", "")))
  paste0(decimal_comma(beta), estrelas, "\n(", decimal_comma(se), ")")
}

rodar_modelo_apendice <- function(grupo, tratamento) {
  dados <- df_long[df_long[[grupo]] == 1 &
                     (df_long[[tratamento]] == 1 | df_long$controle == 1), ]
  lm(reformulate(tratamento, response = "suporte"), data = dados)
}

tratamentos_tabela_a2 <- c("PT" = "conpt",
                           "Lula" = "conlula",
                           "Bolsonaro" = "conbolsonaro",
                           "Lula vs. Bolsonaro" = "conlulabolsonaro")

grupos_tabela_a2 <- c("Lulistas" = "lula",
                      "Antilulistas" = "antilula",
                      "Petistas" = "pt",
                      "Antipetistas" = "antipt",
                      "Bolsonaristas" = "bolsonaro",
                      "Antibolsonaristas" = "antibolsonaro")

tabela_coeficientes_apendice <- data.frame(Condicao = names(tratamentos_tabela_a2),
                                           check.names = FALSE)

for (nome_grupo in names(grupos_tabela_a2)) {
  tabela_coeficientes_apendice[[nome_grupo]] <- sapply(
    tratamentos_tabela_a2,
    function(tratamento) {
      modelo <- rodar_modelo_apendice(grupos_tabela_a2[[nome_grupo]], tratamento)
      formatar_coeficiente(modelo, tratamento)
    }
  )
}

print(tabela_coeficientes_apendice, row.names = FALSE)


#Nomes dos grupos
grupos_nomes <- c("Lulista", "Antilulista", "Petista", "Antipetista", "Bolsonarista", "Antibolsonarista")
cores <- rep("black", length(grupos_binarios))
pchs <- unname(simbolos_grupos[grupos_binarios])
fundos <- unname(fundos_grupos[grupos_binarios])

#Intervalos de confianca
lower_all <- coef_all - 1.96 * se_all
upper_all <- coef_all + 1.96 * se_all

#Posicoes no eixo x e deslocamentos
x_pos <- 1:length(tratamentos)
offsets <- seq(-0.25, 0.25, length.out = 6)

#Limites do grafico
ylim_max <- max(upper_all, na.rm = TRUE) + 0.05
ylim_min <- min(lower_all, na.rm = TRUE) - 0.05

#Grafico
abrir_grafico(3, 1394, 557)
plot(NULL, xlim = c(0.5, length(tratamentos) + 0.5), ylim = c(ylim_min, ylim_max),
     xlab = "Condição de Tratamento", ylab = "Aumento na probabilidade de apoio à política liberal",
     xaxt = "n", main = "")

axis(1, at = x_pos, labels = c("Lula", "PT", "Bolsonaro", "Lula vs Bolsonaro"))
abline(h = 0, lty = 2)

#Adicionar pontos e barras de erro
for (j in x_pos) {
  for (i in 1:6) {
    x <- j + offsets[i]
    y <- coef_all[j, i]
    err_low <- lower_all[j, i]
    err_up  <- upper_all[j, i]
    segments(x, err_low, x, err_up, col = cores[i])
    points(x, y, pch = pchs[i], bg = fundos[i], col = cores[i], cex = 1.5)
  }
}

legend("bottomleft", legend = grupos_nomes, col = cores, pch = pchs,
       pt.bg = fundos, pt.cex = 1.5, cex = 0.8, bty = "n")
dev.off()

#Apêndice Metodológico

#Apoio inicial para cada uma das questoes politicas na condicao de controle

suporte.questao <- tapply(df_long$suporte[df_long$controle == 1], df_long$questao[df_long$controle == 1], function(x) mean(x, na.rm = T))

label <- c("Economia","Costumes1","Costumes2","Democracia","Lei e\nOrdem",
           "Políticas\nSociais","Institucional","Igualdade","Ambiente1",
           "Ambiente2","Ambiente3","Ambiente4","Relações\nInternacionais")

abrir_grafico(4, 1672, 557)
plot(sort(suporte.questao), xlab = "Questão política", main = "Proporção em favor de uma política liberal no grupo controle", ylim = c(0, 1), cex = 0, axes = F, ylab = "")
segments(x0 = seq(1, 13, 1), x1 = seq(1, 13, 1), y0 = 0, y1 = sort(suporte.questao), lwd = 10, lend = 2)
axis(1, at = seq(1, 13, 1), labels = label[order(suporte.questao)], cex.axis = .8)
axis(2, at = seq(0, 1, .1), las = 2)
box()

dev.off()

#Tratamento médio

#Tratamentos isolados
model_pt <- lm(suporte ~ conpt, data = df_long[df_long$conpt == 1 | df_long$controle == 1,])
model_lula <- lm(suporte ~ conlula, data = df_long[df_long$conlula == 1 | df_long$controle == 1,])
model_bolsonaro <- lm(suporte ~ conbolsonaro, data = df_long[df_long$conbolsonaro == 1 | df_long$controle == 1,])
model_lulabolsonaro <- lm(suporte ~ conlulabolsonaro, data = df_long[df_long$conlulabolsonaro == 1 | df_long$controle == 1,])

#Coeficientes
pt_treat <- coef(model_pt)[2]
lula_treat <- coef(model_lula)[2]
bolsonaro_treat <- coef(model_bolsonaro)[2]
lulabolsonaro_treat <- coef(model_lulabolsonaro)[2]

#Erros padrao
pt_se <- coef(summary(model_pt))["conpt", "Std. Error"]
lula_se <- coef(summary(model_lula))["conlula", "Std. Error"]
bolsonaro_se <- coef(summary(model_bolsonaro))["conbolsonaro", "Std. Error"]
lulabolsonaro_se <- coef(summary(model_lulabolsonaro))["conlulabolsonaro", "Std. Error"]

#Intervalos de confianca 95%
pt_lower <- pt_treat - 1.96 * pt_se
pt_upper <- pt_treat + 1.96 * pt_se

lula_lower <- lula_treat - 1.96 * lula_se
lula_upper <- lula_treat + 1.96 * lula_se

bolsonaro_lower <- bolsonaro_treat - 1.96 * bolsonaro_se
bolsonaro_upper <- bolsonaro_treat + 1.96 * bolsonaro_se

lulabolsonaro_lower <- lulabolsonaro_treat - 1.96 * lulabolsonaro_se
lulabolsonaro_upper <- lulabolsonaro_treat + 1.96 * lulabolsonaro_se

#Pontos para plot
points <- c(pt_treat, lula_treat, bolsonaro_treat, lulabolsonaro_treat)
a <- c(1, 2, 3, 4)

#Grafico
abrir_grafico(5, 837, 557)
plot(a, points,
     type = "n",
     axes = FALSE,
     xlab = "Condição de Tratamento",
     ylab = "Aumento na probabilidade de apoio à política liberal",
     ylim = c(min(c(pt_lower, lula_lower, bolsonaro_lower, lulabolsonaro_lower)) - 0.02,
              max(c(pt_upper, lula_upper, bolsonaro_upper, lulabolsonaro_upper)) + 0.02),
     col = "black",
     xlim = c(0.7, 4.3),
     cex = 1.5,
     main = "Efeitos médios dos tratamentos de pistas políticas")

#Barras de confianca
segments(x0 = 1, y0 = pt_lower, x1 = 1, y1 = pt_upper)
segments(x0 = 2, y0 = lula_lower, x1 = 2, y1 = lula_upper)
segments(x0 = 3, y0 = bolsonaro_lower, x1 = 3, y1 = bolsonaro_upper)
segments(x0 = 4, y0 = lulabolsonaro_lower, x1 = 4, y1 = lulabolsonaro_upper)

#Eixos
axis(1, at = a, labels = c("PT", "Lula", "Bolsonaro", "Lula vs. Bolsonaro"), cex.axis = 0.8)
axis(2, at = seq(-.2, .2, .05), las = 2, cex.axis = 0.8)

#Linhas de referencia
abline(h = 0, lty = 2)
abline(v = seq(1.5, 4.5, 1), lty = 2, col = "grey")

graphics::points(a, points,
                 pch = unname(simbolos_tratamentos[c("conpt", "conlula", "conbolsonaro", "conlulabolsonaro")]),
                 col = "black", bg = "black", cex = 1.5)

#Texto explicativo
text(3.6, max(points, na.rm = TRUE) + 0.03, "grupo de comparação: controle", cex = 0.75)

box()

dev.off()

#Efeito médio por questao

#Nome das questões
nomes_questoes <- c("Economia", "Costumes1", "Costumes2", "Democracia", "Lei e\nOrdem",
                    "Políticas\nSociais", "Institucional", "Igualdade", "Ambiente1",
                    "Ambiente2", "Ambiente3", "Ambiente4", "Relações\nInternacionais")

#Grupos e tratamentos
grupos <- c("pt", "antipt", "lula", "antilula", "bolsonaro", "antibolsonaro")
tratamentos <- c("conpt", "conlula", "conbolsonaro", "conlulabolsonaro")

#Função para rodar modelos por questão
rodar_modelos <- function(grupo, tratamento) {
  coefs <- numeric(13)
  ses <- numeric(13)
  for (i in 1:13) {
    df_sub <- df_long[df_long$questao == i &
                      df_long[[grupo]] == 1 &
                      (df_long[[tratamento]] == 1 | df_long$controle == 1), ]
    if (nrow(df_sub) > 5) {
      modelo <- lm(suporte ~ df_sub[[tratamento]], data = df_sub)
      coefs[i] <- coef(modelo)[2]
      ses[i] <- summary(modelo)$coefficients[2, 2]
    } else {
      coefs[i] <- NA
      ses[i] <- NA
    }
  }
  list(coef = coefs, se = ses)
}

#Exportar quatro paineis por grupo - vertical (1 coluna - 4 linhas)
count <- 0

for (grupo in grupos) {
  for (tratamento in tratamentos) {
    
    if (count %% 4 == 0) {
      abrir_grafico(6 + count / 4, 976, 1115)
      par(mfrow = c(4, 1), mar = c(5, 4, 3, 1))  # 4 gráficos empilhados
    }

    resultado <- rodar_modelos(grupo, tratamento)
    coefs <- resultado$coef
    ses <- resultado$se
    lower <- coefs - 1.96 * ses
    upper <- coefs + 1.96 * ses

    ordem <- order(coefs, na.last = NA)
    coefs_ord <- coefs[ordem]
    lower_ord <- lower[ordem]
    upper_ord <- upper[ordem]
    labels_ord <- nomes_questoes[ordem]

    plot(1:13, coefs_ord, type = "n", col = "black",
         ylim = range(c(lower_ord, upper_ord), na.rm = TRUE),
         axes = FALSE, xlab = "", ylab = "Efeito Estimado",
         main = paste0("Grupo: ", grupo, " | Tratamento: ", tratamento))
    abline(h = 0, lty = 2)
    segments(1:13, lower_ord, 1:13, upper_ord, col = "black")
    graphics::points(1:13, coefs_ord, pch = simbolos_grupos[grupo],
                     bg = fundos_grupos[grupo], col = "black", cex = 1.4)
    axis(1, at = 1:13, labels = labels_ord, las = 2, cex.axis = 0.7)
    axis(2)
    box()

    count <- count + 1
    if (count %% 4 == 0) dev.off()
  }
}



