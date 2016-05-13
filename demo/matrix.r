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
air <- comm.fread(dir, verbose=2, rebalance=TRUE, checksum=FALSE)
a <- deltime(a, "T Total comm.fread:")

## for the matrix example, do pca on all data, projecting airports
## into a 2d picture. Take all numerical variables, compute PCA, and
## plot airport labels in the first two pc space.
comm.print(sessionInfo())

numeric <- unlist(allreduce(sapply(air, is.numeric), op="land"))
nafree <- unlist(allreduce(sapply(air, function(x) !any(is.na(x))), op="land"))
comm.cat("num", as.integer(numeric), "\n na", as.integer(nafree), "\n")

air_num <- subset(air, select=numeric & nafree)
air_mat <- as.matrix(air_num)
comm.cat("dim(air_mat)", dim(air_mat), "class(air_mat)", class(air_mat), "\n")

bal.info <- balance.info(air_mat)
comm.print(names(bal.info))
comm.cat("bal.info send:", dim(bal.info$send), "recv:", dim(bal.info$recv), "\n")
comm.print(bal.info$send[1:5, ])
comm.print(bal.info$recv[1:5, ])
comm.print(bal.info$send[1000001:1000005, ])
comm.print(bal.info$recv[1000001:1000005, ])
comm.cat("dim(bal.info$send$belong):", dim(bal.info$send$belong), "\n")
comm.cat("dim(bal.info$recv$org):", dim(bal.info$recv$org), "\n")
comm.print(bal.info$N.allgbd)
comm.print(bal.info$new.N.allgbd)
comm.print(bal.info$gbd.major)

a <- deltime(a, "balance.info:")
comm.cat("gbd.major", .pbd_env$gbd.major, "divide.method", .pbd_env$divide.method, "\n")

## air_bal <- load.balance(air_mat)
## a <- deltime(a, "load.balance:")

## nrow_have <- unlist(allgather(nrow(air_bal)))
## comm.cat("nrow_have:", nrow_have, "\n")


a <- deltime(a0, "T Total time:")
finalize()
