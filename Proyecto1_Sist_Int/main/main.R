# =========================================================================
# Main script para resolver el problema de los Misioneros y Caníbales
# =========================================================================

# Limpiar el entorno y la consola
rm(list=ls())
cat("\014")
graphics.off()

# Establecer el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importar librerías necesarias
library(kableExtra)
library(magrittr)

# Incluir los algoritmos de búsqueda
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Incluir funciones para análisis de resultados
source("../algorithms/results-analysis/analyze-results.R")

# Incluir el problema de los misioneros y caníbales
source("../problem/problem.R")

# Función para resolver el problema con diferentes configuraciones
solve.problem <- function(num_missionaries, num_cannibals, boat_capacity) {
  problem <- initialize.problem(num_missionaries, num_cannibals, boat_capacity)
  
  bfs_ts   <- breadth.first.search(problem, max_iterations = 2000, count_print = 500)   
  bfs_gs   <- breadth.first.search(problem, max_iterations = 2000, count_print = 500, graph_search = TRUE)
  dfs_ts   <- depth.first.search(problem, max_iterations = 2000, count_print = 500)
  dfs_gs   <- depth.first.search(problem, max_iterations = 2000, count_print = 500, graph_search = TRUE)
  dls_ts   <- depth.limited.search(problem, max_iterations = 2000, depth_limit = 10, count_print = 500)
  dls_gs   <- depth.limited.search(problem, max_iterations = 2000, depth_limit = 10, count_print = 500, graph_search = TRUE)
  ids_ts   <- iterative.deepening.search(problem, max_iterations = 2000, max_depth = 10, count_print = 500)
  ids_gs   <- iterative.deepening.search(problem, max_iterations = 2000, max_depth = 10, count_print = 500, graph_search = TRUE)
  ucs_ts   <- uniform.cost.search(problem, max_iterations = 2000, count_print = 500)
  ucs_gs   <- uniform.cost.search(problem, max_iterations = 2000, count_print = 500, graph_search = TRUE)
  gbfs_ts  <- greedy.best.first.search(problem, max_iterations = 2000, count_print = 500)
  gbfs_gs  <- greedy.best.first.search(problem, max_iterations = 2000, count_print = 500, graph_search = TRUE)
  a_star_ts <- a.star.search(problem, max_iterations = 2000, count_print = 500)
  a_star_gs <- a.star.search(problem, max_iterations = 2000, count_print = 500, graph_search = TRUE)
  
  results <- list(bfs_ts, bfs_gs, 
                  dfs_ts, dfs_gs, 
                  dls_ts, dls_gs, 
                  ids_ts, ids_gs,
                  ucs_ts, ucs_gs,
                  gbfs_ts, gbfs_gs,
                  a_star_ts, a_star_gs)
  
  # Imprimir resultados
  kable_material(kbl(results, caption = problem$name), c("striped", "hover"))
}

# Resolver el problema con diferentes configuraciones
solve.problem(3, 3, 2)  # 3 misioneros, 3 caníbales, barca de capacidad 2
solve.problem(5, 5, 3)  # 5 misioneros, 5 caníbales, barca de capacidad 3
solve.problem(10, 10, 4)  # 10 misioneros, 10 caníbales, barca de capacidad 4
