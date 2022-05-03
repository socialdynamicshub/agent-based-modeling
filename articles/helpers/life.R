library(dplyr)
source("./helpers/board_to_df.R")

life_step <- function(m) {
  axis_size <- dim(m)[1]
  m_upd <- matrix(nrow = axis_size, ncol = axis_size, rep(0, axis_size^2))
  
  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {
      
      if (equator == 1) {
        t <- axis_size
      } else {
        t <- equator - 1
      }
      if (equator == axis_size) {
        b <- 1
      } else {
        b <- equator + 1
      }
      if (meridian == 1) {
        l <- axis_size
      } else {
        l <- meridian - 1
      }
      if (meridian == axis_size) {
        r <- 1
      } else {
        r <- meridian + 1
      }

      positions <- list(
        c(t, l),
        c(t, meridian),
        c(t, r),
        c(equator, l),
        c(equator , r),
        c(b, l),
        c(b, meridian),
        c(b, r)
      )
      
      neigh_sum <- 0

      for (pos in positions) {
        neigh_sum <- neigh_sum + m[pos[1], pos[2]]  
      }
      
      if (m[equator, meridian] == 1) {
        if (neigh_sum %in% c(2, 3)) {
          m_upd[equator, meridian] <- 1
        }
      } else {
        if (neigh_sum == 3) {
          m_upd[equator, meridian] <- 1
        }
      }
      
    }
  }
 
  return(m_upd) 
}

life_game <- function(initial_state, steps) {
  m <- initial_state
  d <- board_to_df(m)
  d$step <- 0
  for (i in 1:steps) {
    m_upd <- life_step(m)
    d_step <- board_to_df(m_upd)
    d_step$step <- i
    d <- bind_rows(d, d_step)
    d_step$step <- i
    m <- m_upd
  }
  d <- select(d, step, x, y, state)
  d$x <- as.numeric(d$x)
  d$y <- as.numeric(d$y)
  d$state <- as.factor(d$state)
  return(d)
}