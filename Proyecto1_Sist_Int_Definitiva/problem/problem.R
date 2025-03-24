############################################
# 1. INICIALIZAR EL PROBLEMA
############################################
initialize.problem <- function(missionaries, cannibals, boat_capacity) {
  problem <- list()
  
  problem$name          <- "Missionaries and Cannibals"
  problem$missionaries  <- missionaries
  problem$cannibals     <- cannibals
  problem$boat_capacity <- boat_capacity
  
  # Generamos dinámicamente todas las combinaciones de misioneros/caníbales
  # que quepan en la barca: 1 <= m + c <= boat_capacity, en ambos sentidos.
  actions <- character(0)
  for (m in 0:boat_capacity) {
    for (c in 0:boat_capacity) {
      if ((m + c) >= 1 && (m + c) <= boat_capacity) {
        actions <- c(actions, paste0("move_", m, "M_", c, "C_left"))
        actions <- c(actions, paste0("move_", m, "M_", c, "C_right"))
      }
    }
  }
  
  # Guardamos las acciones en un data.frame
  problem$actions_possible <- data.frame(action = actions, stringsAsFactors = FALSE)
  
  # Estado inicial
  problem$state_initial <- list(
    missionaries_left = missionaries,
    cannibals_left    = cannibals,
    boat_side         = "left"
  )
  
  # Estado final
  problem$state_final <- list(
    missionaries_left = 0,
    cannibals_left    = 0,
    boat_side         = "right"
  )
  
  return(problem)
}

############################################
# 2. COMPROBAR SI UNA ACCIÓN ES APLICABLE
############################################
is.applicable <- function(state, action, problem) {
  # 1) "Parseamos" la acción con expresiones regulares:
  #    "move_2M_1C_left" => m=2, c=1, side="left"
  matches <- regmatches(action, regexec("move_(\\d+)M_(\\d+)C_(left|right)", action))[[1]]
  m    <- as.numeric(matches[2])  # número de misioneros a mover
  c    <- as.numeric(matches[3])  # número de caníbales a mover
  side <- matches[4]             # "left" o "right"
  
  # 2) Comprobar que el barco esté en el lado correcto
  if (state$boat_side != side) {
    return(FALSE)
  }
  
  # 3) Comprobar que haya suficientes misioneros/caníbales en esa orilla:
  if (side == "left") {
    if (m > state$missionaries_left) return(FALSE)
    if (c > state$cannibals_left)    return(FALSE)
  } else {
    # Bote en la derecha: calculamos cuántos hay a la derecha
    M_right <- problem$missionaries - state$missionaries_left
    C_right <- problem$cannibals    - state$cannibals_left
    if (m > M_right) return(FALSE)
    if (c > C_right) return(FALSE)
  }
  
  # 4) Comprobar que no exceda la capacidad
  if ((m + c) > problem$boat_capacity) {
    return(FALSE)
  }
  
  # 5) Simular el estado resultante para validar la regla "no más caníbales que misioneros"
  #    Creamos un estado "ficticio" llamando a 'effect'
  next_state <- effect(state, action, problem)
  
  # 5a) Calculamos cuántos hay a la derecha
  M_right <- problem$missionaries - next_state$missionaries_left
  C_right <- problem$cannibals    - next_state$cannibals_left
  
  # 5b) Si hay misioneros en una orilla, no pueden ser menos que los caníbales
  #     Orilla Izquierda
  if (next_state$missionaries_left > 0 &&
      next_state$cannibals_left > next_state$missionaries_left) {
    return(FALSE)
  }
  #     Orilla Derecha
  if (M_right > 0 && C_right > M_right) {
    return(FALSE)
  }
  
  # Si todo va bien, se aprueba la acción
  return(TRUE)
}

############################################
# 3. EFECTO DE LA ACCIÓN
############################################
effect <- function(state, action, problem) {
  # Volvemos a parsear la acción
  matches <- regmatches(action, regexec("move_(\\d+)M_(\\d+)C_(left|right)", action))[[1]]
  m    <- as.numeric(matches[2])
  c    <- as.numeric(matches[3])
  side <- matches[4]
  
  # Creamos una copia del estado para modificarlo
  result <- state
  
  # Movemos (sumamos o restamos) misioneros/caníbales en función del lado
  if (side == "left") {
    # Barco va de izquierda a derecha
    result$missionaries_left <- result$missionaries_left - m
    result$cannibals_left    <- result$cannibals_left    - c
    result$boat_side         <- "right"
  } else {
    # Barco va de derecha a izquierda
    result$missionaries_left <- result$missionaries_left + m
    result$cannibals_left    <- result$cannibals_left    + c
    result$boat_side         <- "left"
  }
  
  return(result)
}

############################################
# 4. ESTADO FINAL
############################################
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

############################################
# 5. CALCULO DE COSTE
############################################
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
