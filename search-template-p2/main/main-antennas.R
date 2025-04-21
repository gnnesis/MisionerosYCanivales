# Clear environment and console
rm(list = ls())
cat("\014")
graphics.off()

# Set working directory to the script’s location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import libraries for result display
library(kableExtra)
library(magrittr)
library(rlist)

# Source algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/stochastic-hill-climbing-search.R")
source("../algorithms/informed/hill-climbing-with-random-restart.R")
source("../algorithms/informed/stochastic-random-restart.R")
source("../algorithms/informed/local-beam-search.R")

# Source data‑analysis and plotting utilities
source("../algorithms/results-analysis/analyze-results.R")

# Source the problem definition
source("../problem/antennas-problem.R")

# Initialize problem (needed to print initial state, etc.)
filename <- "../data/antennas/antennas_1.txt"
problem  <- initialize.problem(filename)

# ——————————————————————————————
# Llamadas a los algoritmos (descomenta según necesites)
# ——————————————————————————————

# 1) Hill‑Climbing básico
# res <- hill.climbing.search(
#   problem        = problem,
#   max_iterations = 500,
#   count_print    = 50,
#   trace          = FALSE
# )

# 2) Stochastic Hill‑Climbing
# res <- stochastic.hill.climbing.search(
#   problem        = problem,
#   max_iterations = 500,
#   count_print    = 50,
#   trace          = FALSE
# )

# 3) Random Restart Hill‑Climbing
# res <- random.restart.hill.climbing(
#   file           = filename,
#   restarts       = 10,
#   max_iterations = 500,
#   count_print    = 50,
#   trace          = FALSE
# )

# 4) Stochastic Random Restart Hill‑Climbing
# res <- stochastic.random.restart(
#   file           = filename,
#   restarts       = 10,
#   max_iterations = 500,
#   count_print    = 50,
#   trace          = FALSE
# )

# 5) Beam Search
res <- local.beam.search(
  problem        = problem,
  beams          = 5,
  max_iterations = 500,
  count_print    = 50,
  trace          = FALSE
)

# ——————————————————————————————
# Imprimir y plotear resultados
# ——————————————————————————————

cat("Estado inicial:\n")
cat(to.string(problem$state.initial, problem), "\n\n")

cat("Resultado (", res$name, "):\n", sep = "")
cat("Estado final:\n")
cat(to.string(res$state_final$state, problem), "\n\n")

cat("Puntos sin cobertura: ", get.evaluation(res$state_final$state, problem), "\n\n", sep = "")
plot.state(res$state_final$state, problem)
