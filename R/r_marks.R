

#' @title Create Random `marks` Generation Function for \link[spatstat.geom]{ppp.object}
#' 
#' @description
#' Create random `marks` generation \link[base]{function} for \link[spatstat.geom]{ppp.object}.
#' 
#' @param f \link[base]{function} of random number generation, 
#' e.g., \link[stats]{rlnorm}, \link[stats]{rnbinom}, etc.
#' Can also be the function name as a \link[base]{character} scalar.
#' 
#' @returns 
#' Function [r_marks()] returns a ***\link[base]{function}***,
#' which generates random marks of a \link[spatstat.geom]{ppp.object} 
#' following the probability distribution specified by argument `f`.
#' The returned ***\link[base]{function}*** 
#' \itemize{
#' \item {has first parameter `x` taking an argument of a \link[spatstat.geom]{ppp.object}.}
#' \item {returns a \link[spatstat.geom]{ppp.object}.}
#' }
#' 
#' @examples
#' r_marks(rlnorm)
#' r_marks('rnbinom')
#' 
#' (pp = spatstat.random::rpoispp(lambda = 100))
#' 
#' set.seed(12); pp |>
#'  r_marks(rlnorm)(sdlog = .5) |>
#'  r_marks(rnbinom)(size = 5L, prob = .3) |>
#'  r_marks(rfactor)(prob = c(2,1,3), levels = letters[1:3])
#'  
#' @keywords internal  
#' @importFrom groupedHyperframe append_marks<-
#' @export
r_marks <- function(f) {
  
  if (is.function(f)) {
    f_ <- substitute(f)
  } else if (is.character(f)) {
    if (length(f) != 1L || is.na(f) || !nzchar(f)) stop('`f` must be a character scalar')
    if (!exists(f)) stop('function ', sQuote(f), ' does not exist')
    f_ <- as.symbol(f)
  }
  if (!is.symbol(f_)) stop('`f` must be symbol')
  
  val <- list(f_, n = quote(x$n), quote(...)) |> as.call()
  
  .fn <- c(
    alist(x = , '...' =),
    call(
      name = '{', 
      #call(name = 'append_marks<-', quote(x), value = val), # correct, but ..
      call(name = '<-', call(name = 'append_marks', quote(x)), value = val), # pretty, but.. hahaha
      call(name = 'return', quote(x))
    )
  ) |> 
    as.function.default()
  # prefix dot (.) will not show up in ls(., all.names = FALSE)
  
  # clean the enclosure envir of `.fn` as much as possible
  rm(list = c(
    # '.fn', # no!! otherwise nothing to return ..
    'f', 'f_'
  ), envir = environment(.fn))
  
  return(.fn)
  
}














#' @title Generate Random \link[base]{factor}
#' 
#' @description
#' ..
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param prob \link[base]{numeric} \link[base]{vector}, see function \link[base]{sample.int}
#' 
#' @param levels \link[base]{character} \link[base]{vector}, see function \link[base]{factor}
#' 
# @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [rfactor()] is a wrapper of \link[base]{sample.int}.
#' 
#' 
#' 
#' @returns 
#' Function [rfactor()] returns a \link[base]{factor}.
#' 
#' @note
#' Function \link[stats]{rmultinom} is **not** what we need!
#' 
#' @examples
#' rfactor(n = 100L, prob = c(4,2,3))
#' rfactor(n = 100L, prob = c(4,2,3), levels = letters[1:3])
#' @keywords internal
#' @export
rfactor <- function(n, prob, levels = as.character(seq_len(nprob))) {
  nprob <- length(prob)
  if (length(levels) != nprob) stop('`levels` and `prob` must have same length')
  ret <- sample.int(n = nprob, size = n, prob = prob, replace = TRUE)
  attr(ret, which = 'levels') <- levels
  attr(ret, which = 'class') <- 'factor'
  return(ret)
}


