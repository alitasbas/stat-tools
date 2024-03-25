# libraries
library(ggplot2)
library(extrafont)
## font_import()
## loadfonts(device = "win")

# Standard Normal Table Functions

normal_table <- data.frame(x = seq(-4, 4, 0.05), y = dnorm(seq(-4, 4, 0.05)))

test_stat <- function(x, mu = 0, sd = 1, n = 1) {
  stat <- (x - mu) / (sd / sqrt(n))
  return(stat)
}


prob_to_z_score <- function(area, mu = 0, sd = 1, lower.tail = T, plot = T) {
  x <- round(qnorm(area, mean = mu, sd = sd, lower.tail = lower.tail), 3)
  z <- round(test_stat(x, mu, sd), 3)
  
  if (lower.tail) {
    shaded_area = normal_table[normal_table$x <= z, ]
  } else if (lower.tail == F) {
    shaded_area = normal_table[normal_table$x >= z, ]
  }
  
  if (plot) {
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


find_prob <- function(x = inf, mu = 0, sd = 1, n = 1, lower.bound = F,  lower.tail = T, plot = T) {
  z <- round(test_stat(x, mu, sd, n), 3)
  
  if (lower.bound) {
    lower.bound <- round(test_stat(lower.bound, mu, sd, n), 3)
    area <- round(pnorm(q = z) - pnorm(q = lower.bound), 5)
  } else if (lower.tail == F) {
    lower.bound <- z
    z <- Inf
    area <- round(1 - pnorm(q = lower.bound), 5)
  } else {
    lower.bound = -Inf
    area <- round(pnorm(q = z), 5)
  }
  
  if (plot) {
    shaded_area = normal_table[normal_table$x <= z & normal_table$x >= lower.bound, ]
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
      geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      
      labs(title = "Shaded Area Probability", x = "Z-score", y = "Density") +
      
      annotate("text", x = 3, y = 0.25, label = area, hjust = 0.5, vjust = -0.5, color = "skyblue", size = 7) +
      
      annotate("segment", x = lower.bound, xend = -3, y = 0, yend = 0.1, linewidth = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "green") + 
      
      annotate("segment", x = z, xend = 3, y = 0, yend = 0.1, linewidth = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "green") +
      
      annotate("text", x = -3, y = 0.1, label = lower.bound, vjust = -1, color = "darkgreen", size = 5) +
      
      annotate("text", x = 3, y = 0.1, label = z, vjust = -1, color = "darkgreen", size = 5) +
      
      theme(text = element_text(size = 14))
    
    print(plot)
  }
  return(area)
}



# Hypothesis Testing Functions

z.test <- function(x, mu = 0, sd = 1, n = 1, alternative = c("two-sided", "less", "greater"),
                   alpha = 0.05, confint = F) {
  z <- round(test_stat(x, mu, sd, n), 3)
  
  
  if (alternative == "two-sided") {
    crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
    upper_crit <- crit; lower_crit <- -crit
    
    reject <- 1 - pnorm(abs(z)) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
    
  } else if (alternative == "less") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    
    reject <- pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
    
  } else if (alternative == "greater") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    
    reject <- 1 - pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
    
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  if (confint) {
    confint = round(x + c(lower_crit, upper_crit) * (sd / sqrt(n)), 3)
  }
  
  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
               arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
                "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 5),
              "\nWe have sufficient evidence to reject Null Hyp."))
  } 
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 5),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
  
}


t.test <- function(x, mu = 0, sd = 1, n = 2, alternative = c("two-sided", "less", "greater"),
                   alpha = 0.05, confint = F) {
  t <- round(test_stat(x, mu, sd, n), 3)
  sd <- sd / sqrt(n)
  
  t_table <- data.frame(x = seq(-4, 4, 0.05), y = dt(seq(-4, 4, 0.05), df = n-1))
  
  if (alternative == "two-sided") {
    crit <- round(qt(1 - (alpha / 2), df = n-1), 2)
    upper_crit <- crit; lower_crit <- -crit
    prob <- 2 * (1 - round(pt(abs(t), df = n-1), 5))
    
    reject <- 1 - pt(abs(t), df = n-1) < alpha / 2
    shaded_area <- t_table[t_table$x > upper_crit | normal_table$x < lower_crit, ]
  } else if (alternative == "less") {
    crit <- round(qt(1 - alpha, df = n-1), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    prob <- 1 - round(pt(abs(t), df = n -1), 5)
    
    reject <- pt(t, df = n-1) < alpha
    shaded_area <- t_table[t_table$x < upper_crit, ]
  } else if (alternative == "greater") {
    crit <- round(qt(1 - alpha, df = n-1), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    prob <- 1 - round(pt(abs(t), df = n-1), 5)
    
    reject <- 1 - pt(t, df = n-1) < alpha
    shaded_area <- t_table[t_table$x > lower_crit, ]
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  if (confint) {
    confint = round(c(x + lower_crit * sd, mu + upper_crit * sd), 3)
  }
  
  if (reject) {
    plot <- ggplot(t_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob,
              "\nWe have sufficient evidence to reject Null Hyp."))
  } 
  else if (reject == F) {
    plot <- ggplot(t_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob,
              "\nWe Don't have sufficient evidence to reject Null Hyp."))
  }
}


prop.test <- function(x, n, pi, alternative = c("two-sided", "greater", "less"),
                      confint = F, alpha = 0.05) {
  pi_hat <- x / n
  sd <- sqrt(pi * (1 - pi) / n)
  pi_hat_sd <- sqrt(pi_hat * (1 - pi_hat) / n)
  test_stat <- round((pi_hat - pi) / sd, 3)
  
  if (alternative == "two-sided") {
    reject <- 1 - pnorm(abs(test_stat)) < alpha / 2
    
    lower_crit <- round(prob_to_z_score(alpha / 2, plot = F), 2)
    upper_crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
  } else if (alternative == "less") {
    reject <- pnorm(test_stat) < alpha
    
    lower_crit <- round(prob_to_z_score(alpha, plot = F), 2)
    upper_crit <- Inf
  } else if (alternative == "greater") {
    reject <- 1 - pnorm(test_stat) < alpha
    
    lower_crit <- -Inf
    upper_crit <- round(prob_to_z_score(1 - (alpha), plot = F), 2)
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  
  if (confint) {
    confint = round(c(pi_hat + lower_crit * pi_hat_sd, pi_hat + upper_crit * pi_hat_sd), 3)
  }
  
  if (reject) {  
    cat(paste("Hyp Test:", alternative, "\nPop mean:", pi, "Samp mean:", pi_hat, "\nAlpha:", alpha, "\nCritical Value: ∓", upper_crit,
              "\nTest Statistic:", test_stat, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(test_stat)), 5),
              "\nWe have sufficient evidence to reject Null Hyp."))
  } else if (reject == F) {
    cat(paste("Hyp Test:", alternative, "\nPop mean:", pi, "Samp mean:", pi_hat, "\nAlpha:", alpha, "\nCritical Value: ∓", upper_crit,
              "\nTest Statistic:", test_stat, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(test_stat)), 5),
              "\nWe Don't have sufficient evidence to reject Null Hyp."))
  }
}


chi_square.test <- function(var, sigma2, n, alternative = c("two-sided", "greater", "less"),
                            confint = F, alpha = 0.05) {
  chi_test <- (n-1) * s / sigma
  if (alternative == "two-sided") { 
    chi_crit <- qchisq(c(alpha / 2, 1 - alpha / 2), n-1) 
    confint <- (n-1) * var / sort(chi_crit, decreasing = T)
    
    result <- chi_test < chi_crit[1] | chi_test > chi_crit[2]
  }
  else if (alternative == "greater") {
    chi_crit <- qchisq(1 - alpha, n-1)
    confint <- (n-1) * var / chi_crit
    
    result <- chi_test > chi_crit
  }
  else if (alternative == "less") {
    chi_crit <- qchisq(alpha, n-1)
    confint <- (n-1) * var / chi_crit
    
    result <- chi_test < chi_crit
  }
  
  
}


two_sample.z_test <- function(x1, x2, s1, s2, n1, n2, alternative = c("two-sided", "greater", "less"),
                            confint = F, alpha = 0.05, equal_var = F) {
  if (equal_var == F) {
    stand_error <- sqrt(s1^2 / n1 + s2^2 / n2)
    z <- round((x1 - x2) / stand_error, 3)
  } 
  else if (equal_var) {
    pooled_var <- ((n-1) * s1^2 + (n2-1) * s2^2) / (n1 + n2 - 2)
    stand_error <- sqrt(pooled_var / n1 + pooled_var / n2)
    z <- round((x1 - x2) / stand_error, 3)
  }
  
  # z.test(z, alternative = alternative, alpha = alpha, confint = confint) # The confidence int is wrong 
  
  if (alternative == "two-sided") {
    crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
    upper_crit <- crit; lower_crit <- -crit

    reject <- 1 - pnorm(abs(z)) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
  }
  else if (alternative == "less") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -Inf
    upper_crit <- crit

    reject <- pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
  }
  else if (alternative == "greater") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -crit
    upper_crit <- Inf

    reject <- 1 - pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
  }

  if (confint) {
    confint = round((x1 - x2) + c(lower_crit, upper_crit) * stand_error, 2)
  }

  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "orange", size = 4)

    print(plot)
    cat(paste("Hyp Test:", alternative, "Samp means:", x1, x2, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 3),
              "\nWe have sufficient evidence to reject Null Hyp."))
  }
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "orange", size = 4)

    print(plot)
    cat(paste("Hyp Test:", alternative,"Samp means:", x1, x2, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 3),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
}


two_sample.t_test <- function(x1, x2, s1, s2, n1, n2, alternative = c("two-sided", "greater", "less"),
                              confint = F, alpha = 0.05, equal_var = F) {
  if (equal_var == F) {
    stand_error <- sqrt(s1^2 / n1 + s2^2 / n2)
    t <- round((x1 - x2) / stand_error, 3)
    df <- round((s1^2 / n1 + s2^2 / n2) ^ 2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1)), 1)
  } 
  else if (equal_var) {
    pooled_var <- ((n1-1) * s1^2 + (n2-1) * s2^2) / (n1 + n2 - 2)
    stand_error <- sqrt(pooled_var / n1 + pooled_var / n2)
    t <- round((x1 - x2) / stand_error, 3)
    df <- n1 + n2 - 2
  }
  
  
  if (alternative == "two-sided") {
    crit <- round(qt(1 - (alpha / 2), df), 2)
    upper_crit <- crit; lower_crit <- -crit
    
    reject <- 1 - pt(abs(t), df) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
  } 
  else if (alternative == "less") {
    crit <- round(qt(1 - alpha, df), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    
    reject <- pt(t, df) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
  } 
  else if (alternative == "greater") {
    crit <- round(qt(1 - alpha, df), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    
    reject <- 1 - pt(t, df) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
  }
  
  if (confint) {
    confint = round((x1 - x2) + c(lower_crit, upper_crit) * stand_error, 2)
  }
  
  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nSamp means:", x1, x2, "\nDF:", df, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pt(abs(t), df), 3),
              "\nWe have sufficient evidence to reject Null Hyp."))
  }
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "green") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "orange", size = 4)
    
    print(plot)
    cat(paste("Hyp Test:", alternative,"\nSamp means:", x1, x2, "\nDF:", df, "\nAlpha:", alpha, "\nCritical Value: ∓", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pt(abs(t), df), 3),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
}


two_sample.prop_test <- function(x1, x2, n1, n2, alternative = c("two-sided", "greater", "less"),
                                 alpha = 0.05, confint = F) {
  p1_hat <- round(x1 / n1, 5)
  p2_hat <- round(x2 / n2, 5)
  
  p_hat <- round((x1 + x2) / (n1 + n2), 5)
  p_hat_sd <- sqrt(p_hat * (1-p_hat) * (1/n1 + 1/n2))
  
  test_stat <- round((p1_hat - p2_hat) / p_hat_sd, 3)
  
  cat(p1_hat, p2_hat, p_hat, test_stat)
  
  z.test(test_stat, alternative = alternative, alpha = alpha, confint = confint)
}


paired_diff.test <- function(diff_vec, mu = 0, alternative = c("two-sided", "greater", "less"),
                             confint = F, alpha = 0.05) {
  diff_mean <- mean(diff_vec)
  diff_sd <- sd(diff_vec)
  n <- length(diff_vec)
  
  if (n < 30) {
    t.test(diff_mean, mu, sd = diff_sd, n = n, alternative = alternative, alpha = alpha, confint = confint)
  } 
  else if (n >= 30) {
    z.test(diff_mean, mu, sd = diff_sd, n = n, alternative = alternative, alpha = alpha, confint = confint)
  }
}



