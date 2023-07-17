#!/bin/bash
#SBATCH -J read
#SBATCH -A ccsd
#SBATCH -p burst
#SBATCH -N 2
#SBATCH --exclusive
#SBATCH --mem=0
#SBATCH -t 00:20:00
#SBATCH -e read.e
#SBATCH -o read.o

## -N requests use of a number of nodes
## --exclusive asks for all cores on the nodes
## --mem=0 requests all memory on the nodes

## --map-by  ppr:x:node in mpirun below runs x Rscript sessions per node
## there are 2 multicore CPUs per node and one (?) I/O channel per CPU, so 
##   reading is faster if limited to two ranks (readers) per node.

## There also appears to be a file cache as the second read is 10x 
##   faster than the first. Can this be improved with striping on lustre?

## modules are specific to or-slurm-login.ornl.gov (CADES SHPC condos)
source /software/cades-open/spack-envs/base/root/linux-centos7-x86_64/gcc-6.3.0/lmod-8.5.6-wdngv4jylfvg2j6jt7xrtugxggh5lpm5/lmod/lmod/init/bash
export MODULEPATH=/software/cades-open/spack-envs/base/modules/site/Core:/software/cades-open/modulefiles/core
module load gcc
module load openmpi
module load r/4.1.0-py3-X-flexiblas 
echo "loaded R with flexiblas"
module list

## Assumes read_hmda.R is accessible from where this script runs

## CADES nodes have 2 sockets per node and 2 (?) I/O channels
time mpirun --map-by ppr:1:socket Rscript read_hmda.r
time mpirun --map-by ppr:1:socket Rscript read_hmda.r
