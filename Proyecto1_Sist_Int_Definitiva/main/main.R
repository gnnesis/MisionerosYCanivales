# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import the necessary libraries
library(kableExtra)
library(magrittr)

# Import the problem functions
source("../problem/problem.R")

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Include functions for data analysis and result plotting
source("../algorithms/results-analysis/analyze-results.R")

# Function to solve the problem with dynamic missionary, cannibal counts and boat capacity
solve.problem <- function(missionaries, cannibals, boat_capacity) {
  # Initialize the problem with given parameters
  problem <- initialize.problem(missionaries, cannibals, boat_capacity)
  
  # Solve using various search algorithms
  bfs_ts <- breadth.first.search(problem, max_iterations = 2000, count_print = 100)
  bfs_gs <- breadth.first.search(problem, max_iterations = 2000, count_print = 100, graph_search = TRUE)
  
  dfs_ts <- depth.first.search(problem, max_iterations = 2000, count_print = 100)
  dfs_gs <- depth.first.search(problem, max_iterations = 2000, count_print = 100, graph_search = TRUE)
  
  dls_ts <- depth.limited.search(problem, max_iterations = 2000, count_print = 100, depth_limit = 10)
  dls_gs <- depth.limited.search(problem, max_iterations = 2000, count_print = 100, depth_limit = 10, graph_search = TRUE)
  
  ids_ts <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 100)
  ids_gs <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 100, graph_search = TRUE)
  
  ucs_ts   <- uniform.cost.search(problem, max_iterations = 2000, count_print = 100)
  ucs_gs   <- uniform.cost.search(problem, max_iterations = 2000, count_print = 100, graph_search = TRUE)
  
  gbfs_ts  <- greedy.best.first.search(problem, max_iterations = 2000, count_print = 100)
  gbfs_gs  <- greedy.best.first.search(problem, max_iterations = 2000, count_print = 100, graph_search = TRUE)
  
  a_star_ts <- a.star.search(problem, max_iterations = 2000, count_print = 100)
  a_star_gs <- a.star.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  
  # Analyze the results from all the searches
  results <- analyze.results(list(bfs_ts, bfs_gs, 
                                   dfs_ts, dfs_gs, 
                                   dls_ts, dls_gs, 
                                   ids_ts, ids_gs,
                                   ucs_ts, ucs_gs,
                                   gbfs_ts, gbfs_gs,
                                   a_star_ts, a_star_gs), problem)
  
  
  # Print the results in an HTML table
  kable_material(kbl(results, caption = "Missionaries and Cannibals: Search Results"), c("striped", "hover", "condensed", "responsive"))
}

# Call solve.problem with your desired parameters (e.g., 3 missionaries, 3 cannibals, boat capacity 2)
solve.problem(3, 3, 2)
solve.problem(5, 5, 3)
solve.problem(10, 10, 4)
