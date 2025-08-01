

#' @title Append to \link[spatstat.geom]{marks}
#' 
#' @param x a \link[spatstat.geom]{ppp.object}
#' 
#' @param value a \link[base]{factor} or \link[base]{numeric} \link[base]{vector}
#' 
#' @returns 
#' This syntax sugar returns a \link[spatstat.geom]{ppp.object}.
#' 
#' @examples
#' library(spatstat.geom)
#' 
#' # no marks
#' data(vesicles, package = 'spatstat.data')
#' plot(vesicles)
#' append_marks.ppp(vesicles) = rlnorm(n = npoints(vesicles))
#' plot(vesicles)
#' 
#' # vector marks, numeric
#' data(waka, package = 'spatstat.data')
#' plot(waka)
#' append_marks.ppp(waka) = rlnorm(n = npoints(waka))
#' plot(waka)
#' 
#' # vector marks, multitype
#' data(urkiola, package = 'spatstat.data')
#' plot(urkiola)
#' append_marks.ppp(urkiola) = rlnorm(n = npoints(urkiola))
#' plot(urkiola)
#' 
#' # dataframe marks
#' data(stonetools, package = 'spatstat.data')
#' plot(stonetools)
#' append_marks.ppp(stonetools) = rlnorm(n = npoints(stonetools))
#' plot(stonetools)
#' @keywords internal
#' @importFrom spatstat.geom markformat.ppp npoints.ppp
#' @export
`append_marks.ppp<-` <- function(x, value) {
  
  if (length(value) != npoints.ppp(x)) stop('length not match')
  
  switch(markformat.ppp(x), none = {
    x$markformat <- 'vector'
    x$marks <- value
    # tzh's \pkg{groupedHyperframe} requires ncol-1 'data.frame' marks
    # this fix is useless ...
    # x$markformat <- 'dataframe'
    # x$marks <- data.frame(m1 = value)
    # see tzh's note on ?spatstat.geom::superimpose.ppp hahaha
    
  }, vector = {
    x$markformat <- 'dataframe'
    x$marks <- data.frame(m1 = x$marks, m2 = value)
    
  }, dataframe = {
    value <- data.frame(value)
    names(value) <- paste0('m', length(x$marks)+1L)
    x$marks <- data.frame(x$marks, value)
      
  }, stop('unsupported markformat'))
  
  return(x)
  
} 




if (FALSE) {
  spatstat.geom::`marks<-`
  library(spatstat.geom)
  methods(`marks<-`)
  spatstat.geom::`marks<-.ppp` # not *exactly* what Tingting need
  spatstat.geom::append.psp # no
  base::append # not S3
}

