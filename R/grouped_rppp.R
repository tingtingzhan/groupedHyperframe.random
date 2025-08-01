

#' @title Simulate \link[groupedHyperframe:as.groupedHyperframe]{groupedHyperframe}
#' 
#' @param ... see examples, for now
#' 
#' @param n \link[base]{integer} \link[base]{vector}, 
#' numbers of \link[spatstat.geom]{ppp.object}s to generate for each set of parameters
#' 
#' @param win \link[spatstat.geom]{owin.object}
#' 
#' @param envir \link[base]{environment}
#' 
#' @returns 
#' Function [grouped_rppp()] returns a \link[groupedHyperframe:as.groupedHyperframe]{groupedHyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom square hyperframe
#' @export
grouped_rppp <- function(
    ..., 
    n, 
    win = square(), 
    envir = parent.frame()
) {
  
  cl <- match.call() |> as.list.default()
  ag <- cl[-1L]
  nm <- names(ag)
  dots <- if (!length(nm)) ag else ag[!nzchar(nm)]
  
  r <- dots |> vapply(FUN = \(i) (i[[1L]]) |> as.character(), FUN.VALUE = '')
  names(dots) <- r
  
  pars <- dots |>
    lapply(FUN = \(x) { # (x = dots[[1L]])
      x[[1L]] <- quote(list)
      x |>
        eval(envir = envir) |>
        lapply(FUN = \(y) { # (y = x[[1L]])
          y |> 
            nrow() |> 
            seq_len() |> 
            lapply(FUN = \(i) y[i, , drop = TRUE])
        }) |> 
        .mapply(FUN = list, MoreArgs = NULL)
    }) |>
    .mapply(FUN = list, MoreArgs = NULL)
  
  ppp. <- mapply(FUN = \(n, p) { # p = pars[[1L]]; n = n[1L]
    list(dots = p, n = n, win = win, element1 = FALSE) |>
      do.call(what = .rppp) |>
      suppressMessages()
  }, p = pars, n = n, SIMPLIFY = FALSE) |>
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