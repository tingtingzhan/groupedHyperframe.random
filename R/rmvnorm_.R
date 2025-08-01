
# @title Batch Process of Random Multivariate Normal Generation
# 
# @description
# Batch process of random multivariate normal generation [mvrnorm2].
# 
# @param n \link[base]{integer} scalar
# 
# @param mu \link[base]{list}, each element is passed to parameter `mu` of function [mvrnorm2]
# 
# @param Sigma \link[base]{list}, each element is passed to parameter `Sigma` of function [mvrnorm2]
# 
# @param ... additional parameters of function \link[MASS]{mvrnorm}
# 
# @returns
# Function [rmvnorm_()] returns a \link[base]{list} of \link[base]{matrix}es.
# 
# @examples
# rmvnorm_(n = 5L, mu = list(a = c(0, 3), b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
# rmvnorm_(n = 5L, mu = list(a = 0, b = c(3, 7)), Sigma = list(a = .5, b = 1.2))
# @keywords internal
# @export
#rmvnorm_ <- function(n, mu, sd, Sigma, ...) {
#  if (!setequal(names(mu), names(Sigma))) stop('`mu` and `Sigma` must have same names')
#  mapply(
#    FUN = mvrnorm2, 
#    mu = mu, 
#    Sigma = Sigma[names(mu)], 
#    MoreArgs = list(n = n, ...), 
#    SIMPLIFY = FALSE
#  )
#}
# WILL REMOVE!!!!!!!!






# in example by W. Joel Schneider from 
# https://stackoverflow.com/questions/59997925/how-to-generate-multivariate-normal-data-in-r
# ?MASS::mvrnorm is faster than ?mvtnorm::rmvnorm

#' @title Expand Types of `Sigma` in \link[MASS]{mvrnorm}
#' 
#' @description
#' To accommodate more types of `Sigma` in function \link[MASS]{mvrnorm}.
#' 
#' @param n \link[base]{integer} scalar, sample size
#' 
#' @param mu \link[base]{numeric} scalar or \link[base]{vector},
#' multivariate means \eqn{\mathbf{\mu}}'s
#' 
#' @param sd \link[base]{numeric} scalar or a \link[base]{vector}, standard deviation(s)
#' 
#' @param Sigma \link[base]{numeric} \link[base]{matrix}, 
#' \link[stats]{var}iance-\link[stats]{cov}ariance 
#' \link[base]{matrix} \eqn{\Sigma}
#' 
#' @param ... additional parameter of function \link[MASS]{mvrnorm}
#' 
#' @details
#' Argument of parameter `Sigma` could be
#' \describe{
#' \item{scalar}{First, `Sigma` is recycled to the \link[base]{length} of `mu`. 
#' Then a \link[base]{diag}onal \link[base]{matrix} with \link[base]{vector} `Sigma` on the diagonal elements
#' is used as the \link[stats]{var}iance-\link[stats]{cov}ariance 
#' \link[base]{matrix} \eqn{\Sigma}}
#' \item{\link[base]{vector}}{First, check that \link[base]{length} of `Sigma` and `mu` must be the same.
#' Then the \link[base]{diag}onal \link[base]{matrix} is used as \eqn{\Sigma}}
#' \item{\link[base]{matrix}}{`Sigma` is used as \eqn{\Sigma}}}
#' 
#' @returns 
#' Function [mvrnorm2()] returns a \link[base]{double} \link[base]{matrix}.
#' 
#' @note
#' Workhorse function \link[MASS]{mvrnorm} from package \CRANpkg{MASS} is faster than `?mvtnorm::rmvnorm`.
#' 
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @export
mvrnorm2 <- function(n, mu, sd, Sigma, ...) {
  d <- length(mu)
  if (missing(Sigma)) {
    if (missing(sd)) stop('must provide `sd` when `Sigma` is missing')
    nsd <- length(sd)
    if (nsd == 1L) sd <- rep(sd, times = d)
    if (length(sd) != d) stop('`length(sd)` not same with length(mu)')
    Sigma <- diag(x = sd^2, nrow = d, ncol = d)
  }
  mvrnorm(n = n, mu = mu, Sigma = Sigma) # matrix of `n`-by-`d`
}