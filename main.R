# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import the required libraries
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Include functions for data analysis
source("../algorithms/results-analysis/analyze-results.R")

# Missionaries and Cannibals
source("../problem/missionaries-cannibals.R")

# Solve the problem using different algorithms
solve.problem <- function(missionaries, cannibals, boat_capacity) {
  problem <- initialize.problem(missionaries, cannibals, boat_capacity)
  
  bfs_res   <- breadth.first.search(problem, max_iterations = 2000)
  dfs_res   <- depth.first.search(problem, max_iterations = 2000)
  dls_res   <- depth.limited.search(problem, max_iterations = 2000, depth_limit = 20)
  ids_res   <- iterative.deepening.search(problem, max_iterations = 2000, max_depth = 20)
  ucs_res   <- uniform.cost.search(problem, max_iterations = 2000)
  gbfs_res  <- greedy.best.first.search(problem, max_iterations = 2000)
  a_star_res <- a.star.search(problem, max_iterations = 2000)
  
  results <- analyze.results(list(bfs_res, dfs_res, dls_res, ids_res, ucs_res, gbfs_res, a_star_res), problem)
  
  # Print results in an HTML Table
  kable_material(kbl(results, caption = problem$name), c("striped", "hover"))
}

# Solve different instances of the problem
solve.problem(3, 3, 2)
solve.problem(5, 5, 3)
solve.problem(10, 10, 4)
