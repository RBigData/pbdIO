#' deltime
#'
#' @param ltime
#' Result of last call to deltime. 
#' @param text 
#' Text to display along with elapsed time.
#'
#' @return
#' "elapsed" component of current proc.time().
#' 
#' @export
deltime <- function(ltime=proc.time()["elapsed"], text=NULL) {
    time <- proc.time()["elapsed"]
    if(!is.null(text))
        comm.cat(comm.rank(), ":", text, time -ltime, "\n", quiet=TRUE)
    invisible(time)
}
