suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(pbdIO))
source("comm.quantile.r")

a0 <- a <- deltime()

###
### This example assumes that dir is a directory containing the airline
###   unzipped csv data in 22 individual year files, with a total siize
###   of about 12 GB. To download the files, see:
###   http://stat-computing.org/dataexpo/2009/the-data.html
###   Preferably, the directory resides on a parallel file system such as
###   lustre.
###
dir <- "/lustre/atlas/scratch/ost/stf006/airline"

air <- comm.fread(dir, verbose=2, rebalance=TRUE, checksum=TRUE)
a <- deltime(a, "T Total comm.fread:")

## TODO set up a communicator on the fly that includes only non-empties.
##   communicator wrangling should be part of pbdMPI and pbdIO.
##   For now, do the rebalance!

ret <- tapply(air$DepDelay, air$DayOfWeek, comm.quantile,
              probs=c(0.50, 0.90, 0.99), na.rm=TRUE, verbose=0)
comm.print(round(do.call(rbind, ret), 3))
a <- deltime(a, "T Quantile time:")
a <- deltime(a0, "T Total time:")

## for the matrix example, do pca on all data, projecting airports into
##    a 2d picture
finalize()

