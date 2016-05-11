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
        q <- uniroot(f.quant, c(q_lo, q_hi), prob=probs[i])
        q_val <- c(q_val, q$root)
    }
    names(q_val) <- q_names
    q_val
}

