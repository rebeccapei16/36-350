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