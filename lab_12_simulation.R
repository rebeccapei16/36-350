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