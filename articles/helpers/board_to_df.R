library(dplyr)
library(tidyr)

board_to_df <- function(m) {
  axis_size <- dim(m)[1]
  d <- data.frame(m)
  names(d) <- seq(1, axis_size)
  d <- pivot_longer(d, cols = names(d), names_to = "y", values_to = "state")
  d$x <- rep(seq(1, axis_size), each = axis_size)
  d <- select(d, x, y, state)
  d$x <- as.numeric(d$x)
  d$y <- as.numeric(d$y)
  d$state <- as.factor(d$state)
  return(d)
}
