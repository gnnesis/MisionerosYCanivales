# Define the Missionaries and Cannibals problem
initialize.problem <- function(missionaries, cannibals, boat_capacity) {
  problem <- list()
  
  # Initial state: all missionaries and cannibals on the left bank
  problem$state_initial <- c(missionaries, cannibals, 1)
  
  # Final state: all missionaries and cannibals on the right bank
  problem$state_final <- c(0, 0, 0)
  
  # Boat capacity
  problem$boat_capacity <- boat_capacity
  
  # Possible actions: moving people across the river
  problem$actions_possible <- expand.grid(m = 0:boat_capacity, c = 0:boat_capacity)
  problem$actions_possible <- subset(problem$actions_possible, m + c > 0 & m + c <= boat_capacity)
  
  return(problem)
}

# Check if an action is applicable
is.applicable <- function(state, action, problem) {
  m_left <- state[1]
  c_left <- state[2]
  boat <- state[3]
  m_move <- action$m
  c_move <- action$c
  
  if (boat == 1) { # Moving from left to right
    new_m_left <- m_left - m_move
    new_c_left <- c_left - c_move
  } else { # Moving from right to left
    new_m_left <- m_left + m_move
    new_c_left <- c_left + c_move
  }
  
  new_m_right <- problem$state_initial[1] - new_m_left
  new_c_right <- problem$state_initial[2] - new_c_left
  
  valid_state <- new_m_left >= 0 && new_c_left >= 0 && new_m_right >= 0 && new_c_right >= 0
  safe_left <- new_m_left == 0 || new_m_left >= new_c_left
  safe_right <- new_m_right == 0 || new_m_right >= new_c_right
  
  return(valid_state && safe_left && safe_right)
}

# Apply an action and return the resulting state
effect <- function(state, action, problem) {
  m_left <- state[1]
  c_left <- state[2]
  boat <- state[3]
  m_move <- action$m
  c_move <- action$c
  
  if (boat == 1) {
    new_state <- c(m_left - m_move, c_left - c_move, 0)
  } else {
    new_state <- c(m_left + m_move, c_left + c_move, 1)
  }
  
  return(new_state)
}

# Check if the current state is the final state
is.final.state <- function(state, final_state, problem) {
  return(all(state == final_state))
}

# Transform a state into a string representation
to.string <- function(state, problem) {
  return(paste0("Missionaries: ", state[1], " | Cannibals: ", state[2], " | Boat: ", ifelse(state[3] == 1, "Left", "Right")))
}

# Define the cost function
get.cost <- function(action, state, problem) {
  base_time <- 15
  delay_missionaries <- 0.1 * action$m * base_time
  delay_cannibals <- 0.05 * action$c * base_time
  boarding_time <- (action$m + action$c) * 1
  
  return(base_time + delay_missionaries + delay_cannibals + boarding_time)
}

# Define the heuristic function
get.evaluation <- function(state, problem) {
  return(state[1] + state[2]) # Number of people left on the left bank
}
