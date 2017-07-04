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
dir <- "/lustre/atlas/scratch/ost/stf006/airline"
air <- comm.fread(dir, verbose=3, colClasses=col_classes)
a <- deltime(a, "T Total comm.fread:")

## variables from the R Journal iodata article. Select for complete cases
##   rebalancing
xy_df <- subset(air, select=c(ArrDelay, DayOfWeek, DepTime, DepDelay, Month))
comm.cat("colnames(xy_df)", colnames(xy_df), "\n", quiet=TRUE)

## subset complete cases
## comm.print(xy_df[1:5, ], all.rank=TRUE)
comm.cat("nrow: ")
comm.cat(nrow(xy_df), " ", all.rank=TRUE, quiet=TRUE)
comm.cat("\n")
xy_df <- xy_df[complete.cases(xy_df), ]
comm.cat("nrow: ")
comm.cat(nrow(xy_df), " ", all.rank=TRUE, quiet=TRUE)
comm.cat("\n")
a <- deltime(a, "complete cases subset:")

## now rebalance after subsettng!
xy_df <- pbdIO:::comm.rebalance.df(xy_df, lo.side="right", type="equal", verbose=1)
a <- deltime(a, "rebalance:")

## separate x and y
x_df <- subset(xy_df, select=c(DayOfWeek, DepTime, DepDelay, Month))
y_df <- subset(xy_df, select=c(ArrDelay))
a <- deltime(a, "separate x and y df:")

## transform some variables
x_df$DayOfWeek <- factor(x_df$DayOfWeek, levels=1:7)
x_df$Month <- factor(x_df$Month, levels=1:12)
x_df$DepTime <- sprintf("%04d", x_df$DepTime)
x_df$DepTime <- as.numeric(substr(x_df$DepTime, 1, 2))*60 +
    as.numeric(substr(x_df$DepTime, 3, 4))
a <- deltime(a, "factors and transformations:")

## create model matrix
form = ~ DayOfWeek + DepTime + DepDelay + Month
x_mm <- model.matrix(form, x_df)
a <- deltime(a, "model matrix:")

## glue x_mm distributed pieces into a ddmatrix
colnames_x_mm <- colnames(x_mm)
comm.cat("colnames_x_mm:", colnames_x_mm, "\n")
dimnames(x_mm) <- NULL
xd_mm <- new("ddmatrix", Data=x_mm, dim=c(allreduce(nrow(x_mm)), ncol(x_mm)),
             ldim=dim(x_mm), bldim=dim(x_mm), ICTXT=2)
## comm.print(submatrix(xd_mm)[1:5, 1:5], all.rank=TRUE)
print(xd_mm)
a <- deltime(a, "xd_mm new ddmatrix:")

## glue y distributed pieces into a ddmatrix
y <- as.matrix(y_df)
dimnames(y) <- NULL
yd <- new("ddmatrix", Data=y, dim=c(allreduce(nrow(y)), 1),
             ldim=dim(y), bldim=dim(y), ICTXT=2)
print(yd)
a <- deltime(a, "y new ddmatrix:")

xd_mm <- as.blockcyclic(xd_mm, bldim=c(2, 2))
yd <- as.blockcyclic(yd, bldim=c(2, 2))
a <- deltime(a, "xd_mm and yd blockcyclic ddmatrix:")

beta <- lm.fit(xd_mm, yd)
coefs <- as.matrix(beta$coefficients)
rownames(coefs) <- colnames_x_mm
comm.print(coefs)
comm.print(names(beta))
a <- deltime(a, "lm.fit:")

xtx <- crossprod(xd_mm)
print(xtx)
xty <- crossprod(xd_mm, yd)
print(xty)

beta.coef <- solve(xtx, xty)
beta <- as.matrix(beta.coef)
rownames(beta) <- colnames_x_mm
comm.print(beta)
a <- deltime(a, "solve crossprod:")

xsvd <- svd(xd_mm)
comm.print(xsvd$d)
a <- deltime(a, "svd xd_mm:")


a <- deltime(a0, "T Total time:")
finalize()
