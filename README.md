# pbdIO

* **Version:** 0.2-0
* **URL**: https://github.com/RBigData/pbdIO
* **License:** [MPL 2](https://www.mozilla.org/MPL/2.0/)
* **Author:** George Ostrouchov.

An interface to parallel input output packages aimed at cluster computers and intended for batch parallel MPI-based execution. (csv implemented, ADIOS2 (see "RBigData/hola") and hdf5 planned)

csv files: An MPI distributed parallel reading of multiple csv files in a directory. Each rank reads different files to produce a local data.table/data.frame via data.table's fread function. Use package pbdMPI's functions to do further global computations on the distributed data.

ADIOS2 files: coming soon (also see RBigData hola package)

HDF5 files: coming soon

## Installation

The package is maintained on GitHub, and can easily be installed by any of the packages that offer installations from GitHub:

```r
### Pick your preference
devtools::install_github("RBigData/pbdIO")
ghit::install_github("RBigData/pbdIO")
remotes::install_github("RBigData/pbdIO")
```
