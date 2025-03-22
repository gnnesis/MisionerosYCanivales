# Función para inicializar el problema con parámetros dinámicos
initialize.problem <- function(missionaries = 3, cannibals = 3, boat_capacity = 2) {
  problem <- list()
  
  # Nombre del problema
  problem$name <- "Missionaries and Cannibals"
  
  # Modificar las acciones posibles para permitir mover hasta 3 personas según la capacidad
  problem$actions_possible <- data.frame(action = c("move_missionary_left", "move_missionary_right", 
                                                    "move_cannibal_left", "move_cannibal_right", 
                                                    "move_both_left", "move_both_right", 
                                                    "move_two_missionaries_left", "move_two_missionaries_right",
                                                    "move_two_cannibals_left", "move_two_cannibals_right", 
                                                    "move_one_missionary_one_cannibal_left", "move_one_missionary_one_cannibal_right"),
                                         stringsAsFactors = FALSE)
  
  # Estado inicial
  problem$state_initial <- list(missionaries_left = missionaries, 
                                cannibals_left = cannibals, 
                                boat_side = "left")
  
  # Estado final
  problem$state_final <- list(missionaries_left = 0, 
                              cannibals_left = 0, 
                              boat_side = "right")
  
  return(problem)
}

# Función para verificar si una acción es aplicable
is.applicable <- function(state, action, problem) {
  result <- FALSE
  
  if (state$boat_side == "left") {
    if (action == "move_missionary_left" && state$missionaries_left > 0) {
      result <- TRUE
    }
    if (action == "move_cannibal_left" && state$cannibals_left > 0) {
      result <- TRUE
    }
    if (action == "move_both_left" && state$missionaries_left > 0 && state$cannibals_left > 0) {
      result <- TRUE
    }
    # Nuevas acciones con la capacidad 3
    if (action == "move_two_missionaries_left" && state$missionaries_left > 1) {
      result <- TRUE
    }
    if (action == "move_two_cannibals_left" && state$cannibals_left > 1) {
      result <- TRUE
    }
    if (action == "move_one_missionary_one_cannibal_left" && state$missionaries_left > 0 && state$cannibals_left > 0) {
      result <- TRUE
    }
  } else {
    if (action == "move_missionary_right" && state$missionaries_left < problem$state_initial$missionaries_left) {
      result <- TRUE
    }
    if (action == "move_cannibal_right" && state$cannibals_left < problem$state_initial$cannibals_left) {
      result <- TRUE
    }
    if (action == "move_both_right" && state$missionaries_left < problem$state_initial$missionaries_left && state$cannibals_left < problem$state_initial$cannibals_left) {
      result <- TRUE
    }
    # Nuevas acciones con la capacidad 3
    if (action == "move_two_missionaries_right" && state$missionaries_left < problem$state_initial$missionaries_left - 1) {
      result <- TRUE
    }
    if (action == "move_two_cannibals_right" && state$cannibals_left < problem$state_initial$cannibals_left - 1) {
      result <- TRUE
    }
    if (action == "move_one_missionary_one_cannibal_right" && state$missionaries_left < problem$state_initial$missionaries_left && state$cannibals_left < problem$state_initial$cannibals_left) {
      result <- TRUE
    }
  }
  
  return(result)
}

# Función para aplicar una acción y modificar el estado
effect <- function(state, action, problem) {
  result <- state
  
  if (state$boat_side == "left") {
    if (action == "move_missionary_left") {
      result$missionaries_left <- result$missionaries_left - 1
      result$boat_side <- "right"
    }
    if (action == "move_cannibal_left") {
      result$cannibals_left <- result$cannibals_left - 1
      result$boat_side <- "right"
    }
    if (action == "move_both_left") {
      result$missionaries_left <- result$missionaries_left - 1
      result$cannibals_left <- result$cannibals_left - 1
      result$boat_side <- "right"
    }
    # Nuevas acciones con capacidad 3
    if (action == "move_two_missionaries_left") {
      result$missionaries_left <- result$missionaries_left - 2
      result$boat_side <- "right"
    }
    if (action == "move_two_cannibals_left") {
      result$cannibals_left <- result$cannibals_left - 2
      result$boat_side <- "right"
    }
    if (action == "move_one_missionary_one_cannibal_left") {
      result$missionaries_left <- result$missionaries_left - 1
      result$cannibals_left <- result$cannibals_left - 1
      result$boat_side <- "right"
    }
  } else {
    if (action == "move_missionary_right") {
      result$missionaries_left <- result$missionaries_left + 1
      result$boat_side <- "left"
    }
    if (action == "move_cannibal_right") {
      result$cannibals_left <- result$cannibals_left + 1
      result$boat_side <- "left"
    }
    if (action == "move_both_right") {
      result$missionaries_left <- result$missionaries_left + 1
      result$cannibals_left <- result$cannibals_left + 1
      result$boat_side <- "left"
    }
    # Nuevas acciones con capacidad 3
    if (action == "move_two_missionaries_right") {
      result$missionaries_left <- result$missionaries_left + 2
      result$boat_side <- "left"
    }
    if (action == "move_two_cannibals_right") {
      result$cannibals_left <- result$cannibals_left + 2
      result$boat_side <- "left"
    }
    if (action == "move_one_missionary_one_cannibal_right") {
      result$missionaries_left <- result$missionaries_left + 1
      result$cannibals_left <- result$cannibals_left + 1
      result$boat_side <- "left"
    }
  }
  
  return(result)
}


# Función para verificar si el estado es final
is.final.state <- function (state, final_state, problem) {
  return(state$missionaries_left == final_state$missionaries_left &&
           state$cannibals_left == final_state$cannibals_left)
}

# Función para representar un estado como una cadena
to.string <- function (state, problem) {
  return(paste("Misioneros izquierda:", state$missionaries_left, 
               "Caníbales izquierda:", state$cannibals_left, 
               "Barca en:", state$boat_side))
}

# Función para calcular el coste de una acción
get.cost <- function(action, state, problem) {
  cost <- 1  # Coste base de la acción
  
  # Si la barca lleva 2 personas (un misionero y un caníbal), aumentamos el coste
  if (action == "move_both_left" || action == "move_both_right") {
    cost <- cost + 1  # Aumentamos el coste por mover dos personas
  }
  
  return(cost)
}

# Función heurística para las búsquedas informadas
get.evaluation <- function(state, problem) {
  # La evaluación podría basarse en el número de personas restantes en la orilla izquierda.
  remaining_people <- state$missionaries_left + state$cannibals_left
  
  # En este caso, podemos usar el número de personas restantes como una aproximación de la heurística
  return(remaining_people)
}
