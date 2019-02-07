#' comm.fread
#'
#' Given a directory, \code{comm.fread()} reads all csv files contained
#' in it in parallel with available resources.
#'
#' @param dir
#' A directory containing the files desired to be read.  The directory
#' should be accessible to all readers.
#' @param pattern
#' The pattern for files desired to be read.
#' @param readers
#' The number of readers.
#' @param verbose
#' Determines the verbosity level. Acceptable values are 0, 1, 2, and 3 for
#' least to most verbosity.
#' @param ...
#' Additional arguments to be passed to \code{data.table::fread()}.
#'
#' @return
#' TODO
#'
#' @examples
#' \dontrun{
#' ### Save code in a file "demo.r" and run with 2 processors by
#' ### SHELL> mpiexec -np 2 Rscript demo.r
#' library(pbdMPI)
#' library(pbdIO)
#'
#' path <- "/tmp/read"
#' comm.print(dir(path))
#' ## [1] "a.csv" "b.csv"
#'
#' X <- comm.fread(path)
#'
#' comm.print(X, all.rank=TRUE)
#' ## COMM.RANK = 0
#' ##    a b c
#' ## 1: 1 2 3
#' ## COMM.RANK = 1
#' ##    a b c
#' ## 1: 2 3 4
#'
#' finalize()
#' }
#'
#' @importFrom data.table fread rbindlist
#' @export
comm.fread <- function(dir, pattern="*.csv$", readers=comm.size(),
                       verbose=0, ...) {
    if (!is.character(dir) || length(dir) != 1 || is.na(dir))
        comm.stop("argument 'dir' must be a string")
    if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern))
        comm.stop("argument 'pattern' must be a string")
    if (!is.numeric(readers) || length(readers) != 1 || is.na(readers))
        comm.stop("argument 'readers' must be an integer")
    if (!(verbose %in% 0:3))
        comm.stop("argument 'verbose' must be 0, 1, 2, or 3")

    if(verbose) a <- deltime()
    files <- file.info(list.files(dir, pattern=pattern, full.names=TRUE))

    if (NROW(files) == 0)
        comm.stop(paste("Directory", dir,
                        "contains no files matching pattern", pattern))

    sizes <- files$size
    my_rank <- comm.rank()
    my_files <- comm.chunk(nrow(files), p=readers, lo.side="right",
                           form="vector")
    comm.print(my_files, all.rank=TRUE)
    if(verbose > 1) for(ifile in my_files)
                      cat(my_rank, rownames(files)[ifile], "\n")

    ## now fread all my_files and bind into one local data.frame
    l <- lapply(rownames(files)[my_files], function(file)
        suppressWarnings(fread(file, showProgress=FALSE, ...)))
    X <- rbindlist(l)

    # TODO if empty? Is length(X) is zero enough?
    ## rank 0 always reads, so it has all attributes. Propagate to NULLs.
    X0 <- bcast(X[0])
    if(length(X) == 0) X <- X0

    if(verbose) a <- deltime(a, "T    component fread time:")

    if(verbose) {
        nrow_have <- unlist(allgather(nrow(X)))
        comm.cat("nrow_have:", nrow_have, "\n")
    }

    X
}

check_sum <- function(X) {
    ## Report variable sums to check input
    my_numeric <- sapply(X, is.numeric)
    Xnumeric <- which(allreduce(my_numeric, op="land"))
    colSums(X[, Xnumeric, with=FALSE], na.rm=TRUE)
}

comm.rebalance.df <- function(X, verbose=0, ...) {
    ## Data frame X has unequal number of rows across ranks. This function
    ##   balances the rows by sending rows from ranks that have too many to
    ##   ranks that have too few.
    ##
    ## TODO makes copies with rbind(). Should this go into copyless C?
    ##
    my_rank <- comm.rank()
    nrow_have <- unlist(allgather(nrow(X)))
    N <- sum(nrow_have)

    ## TODO Three nrow_ vectors can be one with a bit more logic
    nrow_want <- comm.chunk(N, form="number", # type="equal",
                             all.rank=TRUE, ...)
    nrow_send <- pmax(nrow_have - nrow_want, 0)
    nrow_recv <- pmax(nrow_want - nrow_have, 0)
    if(verbose > 1) {
        comm.cat("nrow_have:", nrow_have, "\n")
        comm.cat("nrow_want:", nrow_want, "\n")
        comm.cat("nrow_send:", nrow_send, "\n")
        comm.cat("nrow_recv:", nrow_recv, "\n")
    }

    ## get global numeric column sums for error checking
    if(verbose > 2) before_sums <- check_sum(X)

    while(sum(nrow_send)) {
        recv_i <- 0
        senders <- (1:comm.size())[nrow_send > 0]
        for(proc_send in senders) {
            ## senders and receivers start from 1. Do -1 for rank!
            receivers <- (1:comm.size())[nrow_recv > 0]
            if(recv_i < length(receivers)) {
                recv_i <- recv_i + 1
                count_s <- nrow_send[proc_send]
                count_r <- nrow_recv[receivers[recv_i]]
                count <- min(count_s, count_r)
                if(my_rank + 1 == receivers[recv_i]) {
                    ## receivers and senders are disjoint sets
                    buffer <- matrix(NA, count, ncol(X))
                    buffer <- recv(buffer, rank.source=proc_send - 1)
                    ## can not use irecv because rbind follows!!
                    X <- rbind(X, buffer)
                }
                if(my_rank + 1 == proc_send) {
                    ## but two senders can be sending to same receiver
                    isend(X[1:count, ], rank.dest=receivers[recv_i] - 1)
                    X <- X[-(1:count), ]
                }
                nrow_recv[receivers[recv_i]] <- count_r - count
                nrow_send[proc_send] <- count_s - count
            }
        }
    }

    ## check if global column sums have not changed
    if(verbose > 2) {
        after_sums <- check_sum(X)
        equal <- all.equal(allreduce(before_sums), allreduce(after_sums))
        comm.cat("checksum equal:", equal, "on", ncol(X), "columns\n")
    } else if(verbose > 1) {
        nrow_have <- unlist(allgather(nrow(X)))
        comm.cat("nrow_have:", nrow_have, "\n")
    }

    X
}
