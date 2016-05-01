
deltime <- function(ltime=proc.time()["elapsed"], text=NULL) {
    time <- proc.time()["elapsed"]
    if(!is.null(text))
        comm.cat(comm.rank(), ":", text, time -ltime, "\n", quiet=TRUE)
    invisible(time)
}
