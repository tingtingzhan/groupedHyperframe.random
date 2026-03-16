


#' @keywords internal
#' @import cli
#' @import spatstat.random
#' @import groupedHyperframe
'_PACKAGE'




#' @importFrom utils citation
.onAttach <- function(libname, pkgname) {
  
  # .onAttach(libname = 'lib', pkgname = 'groupedHyperframe') # nah..
  
  'groupedHyperframe.random' |>
    citation() |>
    format(style = 'text') |> # utils:::format.citation
    col_green() |>
    style_bold() |>
    cli_inform(class = 'packageStartupMessage')
  
  '\n' |>
    cli_inform(class = 'packageStartupMessage')
  
}


