find_z_score <- function(area, mu = 0, sd = 1, lower.tail = T) {
  z <- qnorm(area, mu = mu, sd = sd, lower.tail = lower.tail)
}