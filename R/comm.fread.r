
## given a directory, reads all csv files in parallel with available resources
#' @importFrom data.table fread
#' 
#' @export
comm.fread <- function(dir, pattern="*.csv", readers=comm.size(),
                       keepers=comm.size(), rebalance=TRUE, verbose=0,
                       checksum=TRUE) {
    if(verbose > 1) a <- deltime()
    files <- file.info(list.files(dir, pattern=pattern, full.names=TRUE))
    sizes <- files$size
    my_rank <- comm.rank()
    my.files <- get.jid(nrow(files), method="block0")
    my.files <- comm.chunk(nrow(file), lo.side="right", form="vector")
    if(verbose) for(ifile in my.files)
                    cat(my_rank, rownames(files)[ifile], "\n")
    X <- NULL
    for(file in rownames(files)[my.files]) {
        ##        if(my_rank < nrow(files)
        X <- rbind(X, suppressWarnings(fread(file, showProgress=FALSE)))
    }

    ## rank 0 always reads, so it has all attributes. Propagate to NULLs.
    X0 <- bcast(X[0])
    if(is.null(X)) X <- X0

    if(verbose > 1) a <- deltime(a, "T    component fread time:")

    check_sum <- function() {
        ## Report variable sums to check input
        my_numeric <- sapply(X, is.numeric)
        Xnumeric <- which(allreduce(my_numeric, op="land"))
        my_colsums <- colSums(X[, Xnumeric, with=FALSE], na.rm=TRUE)
        c_names <- names(my_colsums)
        colsums <- allreduce(my_colsums)
        names(colsums) <- c_names
        comm.print(colsums)
    }

    if(checksum) {
        check_sum()
        a <- deltime(a, "T    component check_sum time:")
    }

    if(rebalance) {
        ## rebalance to all ranks X csv
        nrow_have <- unlist(allgather(nrow(X)))
        N <- sum(nrow_have)
        comm.print(N)
        balanced_index <- get.jid(N, all=TRUE)
        new_index <- comm.chunk(N, form="number", type="equal",
                                lo.side="right", all.rank=TRUE)
        ## TODO Three nrow_ vectors can be one with a bit more logic
        nrow_want <- unlist(lapply(balanced_index, length))
        nrow_send <- pmax(nrow_have - nrow_want, 0)
        nrow_recv <- pmax(nrow_want - nrow_have, 0)
        if(verbose > 1) {
            comm.cat("nrow_have:", nrow_have, "\n")
            comm.cat("nrow_want:", nrow_want, "\n")
            comm.cat("new_index:", new_index, "\n")
            comm.cat("nrow_send:", nrow_send, "\n")
            comm.cat("nrow_recv:", nrow_recv, "\n")
        }

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
    }
    if(verbose) a <- deltime(a, "T    component rebalance time:")

    if(checksum) {
        check_sum()
        a <- deltime(a, "T    component check_sum time:")
    }

    if(verbose) {
        nrow_have <- unlist(allgather(nrow(X)))
        comm.cat("nrow_have:", nrow_have, "\n")
    }

    X
}
