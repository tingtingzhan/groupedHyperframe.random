

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
  
  pars <- dots |>
    lapply(FUN = function(x) { # (x = dots[[1L]])
      x |>
        lapply(FUN = function(y) { # (y = x[[1L]])
          y |> 
            nrow() |> 
            seq_len() |> 
            lapply(FUN = function(i) y[i, , drop = TRUE])
        }) |> 
        .mapply(FUN = list, MoreArgs = NULL)
    }) |>
    .mapply(FUN = list, MoreArgs = NULL)
  
  ppp. <- mapply(FUN = function(n, p) { # (p = pars[[1L]])
    c(p, list(n = n, win = win, element1 = FALSE)) |>
      do.call(what = .rppp) |>
      suppressMessages()
  }, p = pars, n = n) |>
    unlist(recursive = FALSE)
  
  g1_ <- seq_along(n)

  ret <- hyperframe(
    ppp = ppp., 
    g1 = g1_ |> rep(times = n) |> as.factor(),
    g2 = n |> lapply(FUN = seq_len) |> unlist() |> as.factor()
  )
  attr(ret, which = 'group') <- '~g1/g2' |> str2lang()
  class(ret) <- c('groupedHyperframe', class(ret))
  return(ret)
  
}

