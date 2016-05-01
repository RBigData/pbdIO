#' @export
comm.chunk <- function(N, form=c("number", "vector"),
                       type=c("balance", "equal"),
                       lo.side=c("left", "right"),
                       all.rank=FALSE, p=comm.size(), r=comm.rank()) {
### allocates N items into p equal groups with remainder and output options
###
    base <- N %/% p
    rem <- N - base*p
    items <- rep(base, p)

    ## options differ only if remainder!
    if(rem) {
        if(type[1] == "balance") {
            if(lo.side[1] == "right") {
                items[1:rem] <- base + 1
            } else if(lo.side[1] == "left") {
                items[(p - rem + 1):p] <- base + 1
            } else comm.cat("comm.chunk:", lo.side[1], "unknown\n")
        } else if(type[1] == "equal") {
            items <- items + 1
            rem <- p*(base + 1) - N
            if(lo.side[1] == "right") {
                i <- p
                increment <- -1
            } else if(lo.side[1] == "left") {
                i <- 1
                increment <- 1
            } else comm.cat("comm.chunk:", lo.side[1], "unknown\n")
            while(rem) {
                if(rem > base) {
                    items[i] <- 0
                    rem <- rem - base - 1
                } else {
                    items[i] <- items[i] - rem
                    rem <- 0
                }
                i <- i + increment
            }
        } else comm.cat("comm.chunk:", type[1], "unknown\n")
    }

    ## check if all allocated
    if(sum(items) != N) cat("warning: comm.chunk rank", comm.rank(),
              "split does not add up!\n")

    ## now output in correct form
    if(form[1] == "number") {
        if(all.rank) ret <- items
        else ret <- items[r + 1]
    } else {
        items_base <- c(0, cumsum(items)[-p])
        if(all.rank) ret <- lapply(1:length(items_base),
                     function(i) lapply(items, seq_len)[[i]] + items_base[i])
        else ret <- items_base[r + 1] + seq_len(items[r + 1])
    }
    ret
}
