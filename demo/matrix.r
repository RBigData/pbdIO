##
## Run this demo with
## mpirun -np 32 Rscript -e 'dir <- "your-airline-data-directory"; demo("matrix", package="pbdIO", echo=FALSE)'
##

## TODO Unfinished example code!
#suppressPackageStartupMessages(library(pbdMPI))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(pbdML))
suppressPackageStartupMessages(library(pbdIO))
suppressPackageStartupMessages(library(memuse))
init.grid()
a0 <- a <- deltime()

col_classes = c(rep("integer", 8), "character", "integer", "character",
    rep("integer", 5), "character", "character", rep("integer", 4),
    "character", rep("integer", 6))

## local subset of airline data - change to your data location!!
dir <- "../../R_Thai_Workshop/session-parallel2/data"
air <- comm.fread(dir, verbose=3, rebalance=TRUE, complete.cases=TRUE,
                  colClasses=col_classes)
a <- deltime(a, "T Total comm.fread:")

## for the matrix example, do pca on all data, projecting airports
## into a 2d picture. Take all numerical variables, compute PCA, and
## plot airport labels in the first two pc space.

## select the numeric columns
comm.cat(comm.rank(), "col.classes(air)", unlist(lapply(air, class)), "\n", quiet=TRUE, all.rank=TRUE)
airnames <- colnames(air)
numeric <- unlist(allreduce(sapply(air, is.numeric), op="land"))
comm.cat("numeric\n", quiet=TRUE)
comm.cat("num", as.integer(numeric), "\n", quiet=TRUE)
## variables from the R Journal iodata article. Select for complete cases
##   rebalancing
air_reg_df <- subset(air, select=c(ArrDelay, DayOfWeek, DepTime, DepDelay, Month))
comm.cat("colnames(air_reg_df)", colnames(air_reg_df), "\n", quiet=TRUE)

## subset complete cases
###!!!### replace with dplyr complete cases
comm.print(air_reg_df[1:5, ], all.rank=TRUE)
comm.cat(comm.rank(), "nrow:", nrow(air_reg_df), "\n", all.rank=TRUE, quiet=TRUE)
air_reg_df <- air_reg_df[complete.cases(air_reg_df), ]
comm.cat(comm.rank(), "nrow:", nrow(air_reg_df), "\n", all.rank=TRUE, quiet=TRUE)
a <- deltime(a, "complete cases subset:")

## now rebalance after subsettng!
air_reg_df <- pbdIO:::comm.rebalance.df(air_reg_df, lo.side="right", type="equal", verbose=3)
a <- deltime(a, "rebalance:")

## from the R Journal iodata article
form = ~ ArrDelay + DayOfWeek + DepTime + DepDelay + Month
## transform some variables
air_reg_df$DayOfWeek <- factor(air_reg_df$DayOfWeek, levels=1:7)
air_reg_df$Month <- factor(air_reg_df$Month, levels=1:12)
air_reg_df$DepTime <- sprintf("%04d", air_reg_df$DepTime)
air_reg_df$DepTime <- as.numeric(substr(air_reg_df$DepTime, 1, 2))*60 +
    as.numeric(substr(air_reg_df$DepTime, 3, 4))
comm.print(air_reg_df[1:5, 1:5], all.rank=TRUE)
a <- deltime(a, "factors and transformations:")

amm <- model.matrix(form, air_reg_df)
comm.cat(comm.rank(), "class(amm)", class(amm), "\n", all.rank=TRUE, quiet=TRUE)
comm.print(amm[1:5, 1:5], all.rank=TRUE)
a <- deltime(a, "model matrix:")

dimnames(amm) <- NULL
amm.d <- new("ddmatrix", Data=amm,
                 dim=c(allreduce(nrow(amm)), ncol(amm)),
                 ldim=dim(amm), bldim=dim(amm), ICTXT=2)
comm.print(submatrix(amm.d)[1:5, 1:5], all.rank=TRUE)
print(amm.d)
a <- deltime(a, "matrix new ddmatrix:")

amm.dbc <- as.blockcyclic(amm.d, bldim=c(2, 2))
print(dim(submatrix(amm.dbc)), all.rank=TRUE)
a <- deltime(a, "matrix blockcyclic ddmatrix:")

xx <- amm.dbc[, -2]
yy <- amm.dbc[, 2]
comm.print(dim(xx))
comm.print(dim(yy))
a <- deltime(a, "select columns:")

beta <- lm.fit(amm.dbc[, -2], amm.dbc[, 2])
coefs <- as.matrix(beta$coefficients)
comm.print(coefs)
comm.print(names(beta))
a <- deltime(a, "lm.fit:")

beta.coef <- solve(crossprod(xx), crossprod(xx, yy))
beta <- as.matrix(beta.coef)
comm.print(beta)
a <- deltime(a, "solve crossprod:")

xsvd <- svd(xx)
comm.print(xsvd$d)
a <- deltime(a, "svd xx:")

## redy for regression. use column indices to select response etc.

air_cross <- crossprod(amm.dbc)
print(air_cross)
a <- deltime(a, "matrix crossprod ddmatrix:")

library(pbdML)

a <- deltime(a0, "T Total time:")
finalize()
