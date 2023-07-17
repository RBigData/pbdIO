#' `deltime` A convenient code timing function.
#'
#' @param ltime
#' Result of last call to deltime. 
#' @param text 
#' Text to display along with elapsed time.
#'
#' @return
#' "elapsed" component of current proc.time().
#' 
deltime <- function(ltime=proc.time()["elapsed"], text=NULL) {
    time <- proc.time()["elapsed"]
    if(!is.null(text))
        comm.cat(comm.rank(), ":", text, time -ltime, "\n", quiet=TRUE)
    invisible(time)
}

#' `copy_source` Copies files from a user-installed package. Intended for
#' non-standard directories that package developers include under the `inst` 
#' directory, which installation brings up one level to the top package 
#' directory and deletes `inst`.
#' 
#' @param dest_dir
#' Character string giving the directory where to copy demo shell script.
#' @param kind
#' Character string giving kind of job manager script, which is also the
#' directory name for its location in the package source.
#' 
#' @details
#' Details for \code{pbdIO}:
#' Running MPI codes on clusters requires batch submission to a job scheduler,
#' which is best done with a shell script. This function provides a means to 
#' extract such shell script examples from a package that includes them under 
#' extra directories that installation makes from `inst`.
#' 
copy_source <- function(dest_dir = ".", package = "pbdIO", kind = "*.sh$", 
                    source_dir = "slurm") {
  source_dir <- paste0(Sys.glob(Sys.getenv('R_LIBS_USER')), "/", package, "/", 
                       dir)
  if(!file.exists(source_dir)) stop(paste("copy_sh:", source_dir, 
                                          "does not exist"))
  files <- list.files(source_dir, pattern = kind)
  file.copy(files, dest_dir)
  obj = ifelse(length(files) == 1, "file", "files")
  paste(length(files), obj, "copied to", Sys.glob(dest_dir), "\n")
}