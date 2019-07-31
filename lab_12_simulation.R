#' generate_data
#'
#' @param n number of rows
#' @param p number of cols
#'
#' @return a list (covariates, a matrix, responses, a vector)
#' @export
#'
#' @examples generate_data(200, 10)
generate_data <- function(n, p) {
  return(list(covariates = matrix(rnorm(n*p), ncol = p),
              responses = rnorm(n)))
}


#' model_select
#'
#' @param covariates a n by p matrix
#' @param responses  vector of length n
#' @param cutoff if p-value < cutoff, then significant
#'
#' @return p-values of the new model
#' @export
#'
#' @examples
model_select <- function(covariates, responses, cutoff) {
  fit <- lm(responses ~ covariates)
  p_value <- summary(fit)$coefficients[-1,"Pr(>|t|)"]
  sig_var_index <- which(summary(fit)$coefficients[-1,"Pr(>|t|)"] < cutoff)
  
  if (all(sig_var_index == F)){
    return(NULL)
  }
  
  fit_new <- lm(responses ~ covariates[, sig_var_index])
  new_p_value <- summary(fit_new)$coefficients[-1,"Pr(>|t|)"]
  
  return(new_p_value)
}


library(ggplot2)
library(gridExtra)

#' run_simulation
#'
#' @param n_trials number of trials
#' @param n row of matrix
#' @param p col of matrix
#' @param cutoff p value < cutoff, then variable significant
#'
#' @return all the significant p-values
#' @export
#'
#' @examples
run_simulation <- function(n_trials, n, p, cutoff) {
  library(ggplot2)
  p_values <- c()
  for(i in (1:n_trials)){
    lst <- generate_data(n, p)
    current_p_value <- model_select(lst$covariates, lst$responses, cutoff)
    p_values <- c(p_values, current_p_value)
  }
  gg <- ggplot(data.frame(x = p_values), aes(x = x)) +
    geom_histogram(bins = 10) +
    labs(title = paste("n = ", n, ", p = ", p),
         x = "p values")
  return(list(p_values = p_values, graph = gg))
}



gg_p_value <- list()
i <- 1
for (n in c(100, 1000, 10000)){
  for (p in c(10, 20, 50)) {
    gg_p_value[[i]] <- run_simulation(1000, n, p, 0.05)$graph
    i <- i + 1
  }
}
grid.arrange(grobs = gg_p_value)