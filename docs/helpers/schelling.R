library(dplyr)
source("./helpers/get_moore_neighborhood.R")
source("./helpers/board_to_df.R")

random_free_pos <- function(board) {
  if (!(0 %in% board)) {
    print("There are no free positions.")
    return(NULL)
  }
  axis_size <- dim(board)[1]
  pos <- sample(1:axis_size, replace = TRUE, 2)
  if (board[pos[1], pos[2]] == 0) {
    return(pos)
  } else {
    pos <- random_free_pos(board)
  }
  return(pos)
}

get_happiness_mat <- function(board, tolerance) {
  axis_size <- dim(board)[1]
  happiness <- matrix(1, nrow = axis_size, ncol = axis_size)
  
  for (i in 1:axis_size) {
    for (j in 1:axis_size) {
      agent_type <- board[i, j]
      if (agent_type == 0) {
        next
      }
      positions <- get_moore_neighborhood(i, j, axis_size, periodic = FALSE)
      sim_count <- 0
      for (pos in positions) {
        if (board[pos[1], pos[2]] == agent_type) {
          sim_count <- sim_count + 1
        }
      }
      if (sim_count < tolerance) {
        happiness[i, j] <- 0
      }
    }
  }
  
  return(happiness)
}

move_agents <- function(board, happiness) {
  board_upd <- board
  for (i in 1:20) {
    for (j in 1:20) {
      if ((happiness[i, j] == 0)) {
        new_pos <- random_free_pos(board_upd)
        board_upd[new_pos[1], new_pos[2]] <- board[i, j]
        board_upd[i, j] <- 0
      }
    }
  }
  return(board_upd)
}

schelling_game <- function(initial_state, tolerance, steps) {
  board <- initial_state
  d <- board_to_df(board) 
  d$step <- 0
  for (i in 1:steps) {
    happiness <- get_happiness_mat(board, tolerance)
    board <- move_agents(board, happiness)
    d_step <- board_to_df(board)
    d_step$step <- i
    d <- bind_rows(d, d_step)
  }
  d <- select(d, step, x, y, state)  # TODO: switch x and y
  d$x <- as.numeric(d$x)
  d$y <- as.numeric(d$y)
  d$state <- as.factor(d$state)
  return(d)
}