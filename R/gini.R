#' Compute the Gini coefficient
#'
#' Compute the Gini coefficient
#'
#' @param pop population
#' @param rev revenue
#'
#' @export
#'
gini <- function(pop, rev) {
  pop <- cumsum(pop/sum(pop))
  rev <- cumsum(rev/sum(rev))
  pop <- c(0, pop)
  rev <- c(0, rev)
  1 - sum(purrr::map_dbl(seq_along(pop)[-1], function(i) {(pop[i] - pop[i - 1]) * (rev[i] + rev[i - 1])}))
}
