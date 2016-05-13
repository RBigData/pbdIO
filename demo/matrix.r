##
## Run this demo with
## mpirun -np 32 Rscript -e 'dir <- "your-airline-data-directory"; demo("matrix", package="pbdIO", echo=FALSE)'
##

## TODO Unfinished example code!
suppressPackageStartupMessages(library(pbdMPI))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(pbdDEMO))
suppressPackageStartupMessages(library(pbdML))
suppressPackageStartupMessages(library(pbdIO))
init.grid()
a0 <- a <- deltime()

dir <- "/lustre/atlas/scratch/ost/stf006/airline"
air <- comm.fread(dir, verbose=1, rebalance=TRUE, checksum=TRUE)
a <- deltime(a, "T Total comm.fread:")

## for the matrix example, do pca on all data, projecting airports
## into a 2d picture. Take all numerical variables, compute PCA, and
## plot airport labels in the first two pc space.

numeric <- unlist(allreduce(sapply(air, is.numeric), op="land"))
nafree <- unlist(allreduce(sapply(air, function(x) !any(is.na(x))), op="land"))
comm.cat("numeric and NA-free:\n", quiet=TRUE)
comm.cat("num", as.integer(numeric), "\n na", as.integer(nafree), "\n", quiet=TRUE)

air_num <- subset(air, select=numeric & nafree)
air_mat <- as.matrix(air_num)
comm.cat("dim(air_mat)", dim(air_mat), "class(air_mat)", class(air_mat), "\n", quiet=TRUE)
comm.cat("colnames(air_mat)", colnames(air_mat), "\n", quiet=TRUE)
a <- deltime(a, "matrix subset:")

air_dmat <- new("ddmatrix", DATA=air_mat, bldim=dim(air_mat), ICTXT=1)
comm.print(air_dmat)

library(pbdML)

a <- deltime(a0, "T Total time:")
finalize()
