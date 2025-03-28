

#' @title Batch Process of Function [.rppp]
#' 
#' @param n \link[base]{integer} \link[base]{vector}, 
#' numbers of \link[spatstat.geom]{ppp.object}s to generate for each set of parameters
#' 
#' @param ... see examples, for now
#' 
#' @param win \link[spatstat.geom]{owin} window
#' 
#' @returns 
#' Function [grouped_rppp()] returns a \link[spatstat.geom]{hyperframe}.
#' 
#' @importFrom spatstat.geom owin hyperframe
#' @export
grouped_rppp <- function(n, ..., win = owin(xrange = c(-1,1), yrange = c(-1,1))) {
  
  dots <- list(...)
  dots <- dots[lengths(dots, use.names = FALSE) > 0L]
  
  tmp <- lapply(dots, FUN = function(.x) { # (.x = dots[[1L]])
    .mapply(FUN = list, dots = lapply(.x, FUN = function(.y) { # (.y = .x[[1L]])
      lapply(seq_len(nrow(.y)), FUN = function(i) .y[i, , drop = TRUE])
    }), MoreArgs = NULL)
  })
  pars <- .mapply(FUN = list, dots = tmp, MoreArgs = NULL)
  
  suppressMessages(ret0 <- mapply(FUN = function(n, p) { # (p = pars[[1L]])
    do.call(what = .rppp, args = c(p, list(n = n, win = win, element1 = FALSE)))
  }, p = pars, n = n))
  
  f1_ <- seq_along(n)
  f1 <- rep(f1_, times = n)
  attr(f1, which = 'levels') <- as.character(f1_)
  class(f1) <- 'factor'
  
  ret <- hyperframe(ppp = unlist(ret0, recursive = FALSE), f = f1)
  # attr(ret, which = 'group') <- ~ f # don't do this; I hate formula's environment!
  attr(ret, which = 'group') <- call(name = '~', quote(f))
  class(ret) <- c('groupedHyperframe', class(ret))
  return(ret)
  
}

