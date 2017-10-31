##
## Run this demo with
## mpirun -np 32 Rscript -e 'dir <- "your-airline-data-directory"; demo("quantiles", package="pbdIO", echo=FALSE)'
##

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(pbdMPI))
suppressPackageStartupMessages(library(pbdIO))

comm.quantile <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names=TRUE,
                          verbose=0) {
  N <- allreduce(length(x)) - allreduce(sum(is.na(x)))
  probs <- sort(probs)
  np <- length(probs)
  
  ## TODO is there a native R function for this?
  format_perc <- function(x, digits = max(2L, getOption("digits")),
                          probability = TRUE, use.fC = length(x) < 100, ...) {
    if (length(x)) {
      if (probability)
        x <- 100 * x
      paste0(if (use.fC)
        formatC(x, format = "fg", width = 1, digits = digits)
        else format(x, trim = TRUE, digits = digits, ...), "%")
    }
    else character(0)
  }
  q_names <- format_perc(probs)
  
  f.quant <- function(q, prob) {
    allreduce(sum(x <= q, na.rm=TRUE), op="sum")/N - prob
  }
  
  q_val <- NULL
  q_lo <- allreduce(min(x, na.rm=na.rm), op="min")
  q_hi <- allreduce(max(x, na.rm=na.rm), op="max")
  if(verbose) comm.cat(comm.rank(), "All quant lo hi:", q_lo, q_hi, "\n")
  for(i in seq(1, np)) {
    ## loop over all and use previous for bounds
    ## TODO are there better bounds?
    if(i > 1) q_lo <- q_val[i - 1]
    if(verbose > 1) comm.cat(comm.rank(), "lo hi:", q_lo, q_hi, "\n")
    if (probs[i] <= 1.0/N){
      q_val <- c(q_val, q_lo);
    }else{
      q <- uniroot(f.quant, c(q_lo, q_hi), prob=probs[i])
      q_val <- c(q_val, q$root)
    }
  }
  names(q_val) <- q_names
  q_val
}

a0 <- a <- deltime()

###
### This example assumes that external variable dir points to a directory
###   containing the airline unzipped csv data in 22 individual year files,
###   with a total size of about 12 GB. To download the files, see:
###   http://stat-computing.org/dataexpo/2009/the-data.html
###   Preferably, the directory resides on a parallel file system such as
###   lustre.
###

air <- comm.fread(dir, verbose=1, rebalance=TRUE, checksum=TRUE)
a <- deltime(a, "T Total comm.fread:")

## TODO set up a communicator on the fly that includes only non-empties.
##   communicator wrangling should be part of pbdMPI and pbdIO.
##   For now, do the rebalance!

ret <- tapply(air$DepDelay, air$DayOfWeek, comm.quantile,
              probs=c(0.50, 0.90, 0.99), na.rm=TRUE, verbose=0)

comm.print(round(do.call(rbind, ret), 3))
a <- deltime(a, "T Quantile time:")
a <- deltime(a0, "T Total time:")

finalize()

