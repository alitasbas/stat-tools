# libraries
library(ggplot2)

# functions

normal_table <- data.frame(x = seq(-4, 4, 0.05), y = dnorm(seq(-4, 4, 0.05)))

prob_to_z_score <- function(area, mu = 0, sd = 1, lower.tail = T) {
  z <- qnorm(area, mean = mu, sd = sd, lower.tail = lower.tail)
  # shaded_area = 
  # plot <- ggplot(normal_table, aes(x, y)) + geom_line() + geom_ribbon()
  return(z)
}

find_prob <- function(z = inf, mu = 0, sd = 1, lower.bound = F,  lower.tail = T) {
  if (lower.bound) {
    area <- round(pnorm(q = z, mean = mu, sd = sd) - pnorm(q = lower.bound, mean = mu, sd = sd), 5)
    # ggplot2
  } else if (lower.tail == F) {
    area <- round(1 - pnorm(q = z, mean = mu, sd = sd), 5)
  } else {
    lower.bound = -4
    area <- round(pnorm(q = z, mean = mu, sd = sd), 5)
  }
  shaded_area = normal_table[normal_table$x <= z & normal_table$x >= lower.bound,]
  
  plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
    geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
    labs(title = "Shaded Area Probability", x = "Z-score", y = "Density") +
    annotate("text", x = 3, y = 0.25, label = area, hjust = 0.5, vjust = -0.5, color = "skyblue", size = 7) +
    theme(text = element_text(size = 14))
  print(plot)
  return(area)
}

z_score <- function(x, mu = 0, sd = 1) {
  z <- (x - mu) / sd
  return(z)
}
