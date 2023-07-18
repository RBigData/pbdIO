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

#' Copy files from a user-installed package.
#' 
#' @param dest_dir
#' Character string giving the directory where to copy demo shell script.
#' @param package
#' Character string giving the package name.
#' @param kind
#' Character string giving kind of job manager script, which is also the
#' directory name for its location in the package source.
#' @param source_dir 
#' Character string of the package directory containing the file(s) to copy.
#' 
#' @details
#' Intended for code stored in
#' non-standard directories in packages included under the `inst` 
#' directory, which installation brings up one level to the top package 
#' directory and deletes `inst`. However, standard directories will work too.
#'  
#' Details for \code{pbdIO}:
#' Running MPI codes on clusters requires batch submission to a job scheduler,
#' which is best done with a shell script. This function provides a means to 
#' extract such shell script examples from a package that includes them under 
#' extra directories that installation makes from `inst`.
#' 
copy_source <- function(dest_dir = ".", package = "pbdIO", kind = "*.sh$", 
                    source_dir = "slurm") {
  source_dir <- paste0(Sys.glob(Sys.getenv('R_LIBS_USER')), "/", package, "/", 
                       source_dir)
  dest_dir = Sys.glob(dest_dir)
  if(!file.exists(source_dir)) stop(paste("copy_source:", source_dir, 
                                          "does not exist"))
  files <- list.files(source_dir, pattern = kind, full.names = TRUE)
  file.copy(files, dest_dir)
  obj = ifelse(length(files) == 1, "file", "files")
  invisible(cat(length(files), obj, "copied to", dest_dir, "\n"))
}