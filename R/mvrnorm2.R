



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
#' @param Sigma \link[base]{numeric} \link[stats]{var}iance-\link[stats]{cov}ariace \link[base]{matrix}, see function \link[MASS]{mvrnorm}
#' 
#' @param ... additional parameter of function \link[MASS]{mvrnorm}
#' 
#' @details
#' 
#' Argument of parameter `sd` could be
#' 
#' \describe{
#' 
#' \item{scalar}{`sd` is recycled to the \link[base]{length} of `mu`}
#' 
#' \item{\link[base]{vector}}{check that \link[base]{length} of `sd` and `mu` must be the same}
#' 
#' }
#' 
#' Then a \link[base]{diag}onal \link[base]{matrix} with \link[base]{vector} `sd^2` on the diagonal elements
#' is used as the \link[stats]{var}iance-\link[stats]{cov}ariance 
#' \link[base]{matrix} \eqn{\Sigma}
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
mvrnorm2 <- function(
    n, 
    mu, 
    sd, 
    Sigma = diag(x = sd^2, nrow = d, ncol = d),
    ...
) {
  
  d <- length(mu)
  
  if (!missing(sd)) {
    nsd <- length(sd)
    if (nsd == 1L) sd <- rep(sd, times = d)
    if (length(sd) != d) stop('`length(sd)` not same with length(mu)')
  }
  
  force(Sigma)
  
  mvrnorm(n = n, mu = mu, Sigma = Sigma, ...) # matrix of `n`-by-`d`
  
}