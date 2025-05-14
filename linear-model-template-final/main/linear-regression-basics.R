library(lattice)
library(caret)
library(ggplot2)
library(dplyr)

source("helper_functions.R")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
adv <- read.csv("../data/2025_Advertising.csv", stringsAsFactors = FALSE)
adv <- na_mode_fill(adv)

if (!dir.exists("plots")) dir.create("plots")

# Gráfico 1: TV vs Sales
png("plots/tv_vs_sales.png", width = 900, height = 700)
scatter.smooth(adv$TV, adv$Sales, main = "TV vs Sales")
abline(lm(Sales ~ TV, data = adv), col = "red", lwd = 2)
dev.off()

# Gráfico 2: Radio vs Sales
png("plots/radio_vs_sales.png", width = 900, height = 700)
scatter.smooth(adv$Radio, adv$Sales, main = "Radio vs Sales")
abline(lm(Sales ~ Radio, data = adv), col = "red", lwd = 2)
dev.off()

# Gráfico 3: Newspaper vs Sales
png("plots/newspaper_vs_sales.png", width = 900, height = 700)
scatter.smooth(adv$Newspaper, adv$Sales, main = "Newspaper vs Sales")
abline(lm(Sales ~ Newspaper, data = adv), col = "red", lwd = 2)
dev.off()

# Correlaciones
cor_sales <- sapply(adv[, c("TV", "Radio", "Newspaper")], function(x) cor(x, adv$Sales))
print(round(cor_sales, 3))

# Validación cruzada
set.seed(123)
fold_MAE <- fold_MSE <- fold_MAPE <- numeric(10)
models <- vector("list", 10)

for (i in 1:10) {
  train_idx <- createDataPartition(adv$Sales, p = 0.75, list = FALSE)
  train_set <- adv[train_idx, ]
  test_set  <- adv[-train_idx, ]
  
  models[[i]] <- lm(Sales ~ TV + Radio + Newspaper, data = train_set)
  pred_i      <- predict(models[[i]], newdata = test_set)
  
  met <- regression_metrics(test_set$Sales, pred_i)
  fold_MAE[i] <- met["MAE"]; fold_MSE[i] <- met["MSE"]; fold_MAPE[i] <- met["MAPE"]
}

results_df <- data.frame(Fold = 1:10, MAE = fold_MAE, MSE = fold_MSE, MAPE = fold_MAPE)
print(results_df)

best_idx   <- which.min(results_df$MAE)
best_model <- models[[best_idx]]
cat("\n>>> Mejor modelo = Fold", best_idx, "\n")
print(summary(best_model))
print(anova(best_model))

# Predicciones y métricas
pred <- predict(best_model, newdata = adv)
adv <- adv %>%
  mutate(Pred = pred,
         Inversion = TV + Radio + Newspaper,
         Benefit = Sales / Inversion,
         Error = Sales - Pred)

worst_idx <- which.max(abs(adv$Error))
cat(sprintf("\nCampaña peor explicada: fila %d (Error abs = %.2f)\n",
            worst_idx, abs(adv$Error[worst_idx])))

cat("\nTop‑10 campañas por beneficio:\n")
print(adv %>% arrange(desc(Benefit)) %>% head(10) %>%
        select(TV, Radio, Newspaper, Sales, Inversion, Benefit))

cat("\nTop‑5 sobre‑estimadas:\n")
print(adv %>% arrange(desc(Error)) %>% head(5) %>%
        select(TV, Radio, Newspaper, Sales, Pred, Error))

cat("\nTop‑5 infra‑estimadas:\n")
print(adv %>% arrange(Error) %>% head(5) %>%
        select(TV, Radio, Newspaper, Sales, Pred, Error))

# Optimización de presupuesto
opt <- optimize_budget_linear(best_model,
                              channels = c("TV", "Radio", "Newspaper"),
                              budget_total = 50,
                              step = 5,
                              min_each = 5)

cat("\nAsignación óptima de 50k $:\n")
print(head(opt, 1))
