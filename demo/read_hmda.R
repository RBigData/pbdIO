library(pbdIO)

## This example script (and its companion read_hmda.sh) assume that 2007-2010 
## files from https://www.consumerfinance.gov/data-research/hmda/historic-data/
## have been downloaded and expanded into CSV to the directory specified below:
## Lustre work directory where files live
dir = paste0(Sys.getenv("WORKDIR"), "/hmda")

rank = comm.rank()

## rank 0 gets and prints full file paths
if(rank == 0) {
  files = list.files(path = dir, pattern = "*.csv$", full.names = TRUE)
  print(files)
}

## Read data, pattern *.csv$ is default. Each rank reads different data.
## Troubleshooting may be needed here ... for some reason first read takes longer
my_x = comm.fread(dir, shcom = "cat")
comm.cat("rank", rank, "dim:", dim(my_x), "\n", all.rank = TRUE, quiet = TRUE)

## compute missing # and proportion
my_na_count = sum(is.na(my_x$rate_spread))
na_count = reduce(my_na_count)  # sum is default
comm.print(na_count)
n = reduce(nrow(my_x))
comm.print(na_count/n)

## three ways to compute min
my_min_spread = min(my_x$rate_spread, na.rm = TRUE)

min_spread = reduce(my_min_spread, op = "min") # only rank 0 gets result
print(min_spread, all.rank = TRUE)

min_spread = allreduce(my_min_spread, op = "min") # all ranks get result
print(min_spread, all.rank = TRUE)


min_spread = comm.min(my_x$rate_spread, na.rm = TRUE) # pbdMPI min function
print(min_spread, all.rank = TRUE)

finalize()
