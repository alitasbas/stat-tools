# functions

prob_to_z_score <- function(area, mu = 0, sd = 1, lower.tail = T) {
  z <- qnorm(area, mean = mu, sd = sd, lower.tail = lower.tail)
  return(z)
}

find_prob <- function(z = inf, mu = 0, sd = 1, lower.bound = F,  lower.tail = T) {
  if (lower.bound) {
    area <- pnorm(q = z, mean = mu, sd = sd) - pnorm(q = lower.bound, mean = mu, sd = sd)
    # ggplot2
  } else if (lower.tail == F) {
    area <- 1 - pnorm(q = z, mean = mu, sd = sd)
  } else {
    area <- pnorm(q = z, mean = mu, sd = sd)
  }
  return(area)
}

z_score <- function(x, mu = 0, sd = 1) {
  z <- (x - mu) / sd
  return(z)
}
