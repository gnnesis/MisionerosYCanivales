# 1. Rellenar NAs / valores vacíos con la moda
na_mode_fill <- function(df) {
  for (col in names(df)) {
    if (any(is.na(df[[col]]) | df[[col]] == "")) {
      moda <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      if (is.numeric(df[[col]])) moda <- as.numeric(moda)
      df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- moda
    }
  }
  return(df)
}

# 2. Métricas de regresión
regression_metrics <- function(y_true, y_pred) {
  mae  <- mean(abs(y_true - y_pred))
  mse  <- mean((y_true - y_pred)^2)
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  return(c(MAE = mae, MSE = mse, MAPE = mape))
}

# 3. Optimización lineal de presupuesto
optimize_budget_linear <- function(model,
                                   channels     = c("TV", "Radio", "Newspaper"),
                                   budget_total = 50,
                                   step         = 5,
                                   min_each     = 5) {
  
  grid_vals <- seq(min_each, budget_total, by = step)
  combos <- expand.grid(rep(list(grid_vals), length(channels)))
  names(combos) <- channels
  combos <- combos[rowSums(combos) == budget_total, , drop = FALSE]
  
  coef <- coef(model)
  combos$Sales_Pred <- as.numeric(coef[1] + as.matrix(combos) %*% coef[-1])
  combos[order(-combos$Sales_Pred), ]
}
