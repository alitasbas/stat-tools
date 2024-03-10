# libraries
library(ggplot2)
library(extrafont)
## font_import()
## loadfonts(device = "win")

# Standard Normal Table Functions

normal_table <- data.frame(x = seq(-4, 4, 0.05), y = dnorm(seq(-4, 4, 0.05)))


prob_to_z_score <- function(area, mu = 0, sd = 1, lower.tail = T, plot = T) {
  z <- round(qnorm(area, mean = mu, sd = sd, lower.tail = lower.tail), 5)
  
  if (plot) {
    shaded_area = normal_table[normal_table$x <= z, ]
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
      geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.75) +
      annotate("segment", x = z, xend = 2.4, y = 0, yend = 0.1, size = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "green") +
      annotate("text", x = 2.4, y = 0.1, label = paste("Z-score:", z), vjust = -1, color = "darkgreen", size = 5) + 
      annotate("text", x = -2.5, y = 0.2, label = paste("Area:", area), vjust = -1, color = "darkgreen", size = 5) +
      labs(y = "f(x)") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
    print(plot)
  }
  return(z)
}


find_prob <- function(z = inf, mu = 0, sd = 1, lower.bound = F,  lower.tail = T, plot = T) {
  if (lower.bound) {
    area <- round(pnorm(q = z, mean = mu, sd = sd) - pnorm(q = lower.bound, mean = mu, sd = sd), 5)
  } else if (lower.tail == F) {
    area <- round(1 - pnorm(q = z, mean = mu, sd = sd), 5)
  } else {
    lower.bound = -4
    area <- round(pnorm(q = z, mean = mu, sd = sd), 5)
  }
  
  if (plot) {
    shaded_area = normal_table[normal_table$x <= z & normal_table$x >= lower.bound, ]
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
      geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      
      labs(title = "Shaded Area Probability", x = "Z-score", y = "Density") +
      
      annotate("text", x = 3, y = 0.25, label = area, hjust = 0.5, vjust = -0.5, color = "skyblue", size = 7) +
      
      annotate("segment", x = lower.bound, xend = -3, y = 0, yend = 0.1, size = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "green") + 
      
      annotate("segment", x = z, xend = 3, y = 0, yend = 0.1, size = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "green") +
      
      annotate("text", x = -3, y = 0.1, label = lower.bound, vjust = -1, color = "darkgreen", size = 5) +
      
      annotate("text", x = 3, y = 0.1, label = z, vjust = -1, color = "darkgreen", size = 5) +
      
      theme(text = element_text(size = 14))
    
    print(plot)
  }
  return(area)
}

z_score <- function(x, mu = 0, sd = 1) {
  z <- (x - mu) / sd
  return(z)
}



# Hypothesis Testing Functions

z.test <- function(x, mu = 0, sd = 1, alternative = c("two-sided", "less", "greater"), alpha = 0.05, confint = F) {
  z <- z_score(x, mu, sd)
  lower_crit <- prob_to_z_score(alpha / 2, plot = F)
  upper_crit <- prob_to_z_score(1 - (alpha / 2), plot = F)
  if (alternative == "two-sided") {
    reject <- 1 - pnorm(abs(z)) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
  } else if (alternative == "less") {
    reject <- pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x < prob_to_z_score(alpha, plot = F), ]
  } else if (alternative == "greater") {
    reject <- 1 - pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x > prob_to_z_score(1 - alpha, plot = F), ]
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  
  if (reject) {
    print("We have sufficient evidence to reject Null Hyp.")
    
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), size = 1,
               arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = "REJECT NULL", x = 2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting")
    print(plot)
  } else if (reject == F) {
    print("We don't have sufficient evidence to reject Null Hyp.")
    
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), size = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting")
    print(plot)
  }
  
}

t.test <- function(x, mu = 0, sd = 1, alternative = c("two.sided", "less", "greater"), alpha = 0.05, confint = F) {
  
}


z.test(-1.65, alternative = "two-sided")


