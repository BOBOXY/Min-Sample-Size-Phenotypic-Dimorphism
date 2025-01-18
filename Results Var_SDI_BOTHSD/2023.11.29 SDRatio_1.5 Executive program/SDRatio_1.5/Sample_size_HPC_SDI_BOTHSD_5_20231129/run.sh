#!/bin/bash -x
#PBS -N Rjob
#PBS -l nodes=1:ppn=100
#PBS -j oe
#PBS -q batch

module load compiler/intel/intel-compiler-2017.5.239
module load mpi/intelmpi/2017.4.239

n_proc=$(cat $PBS_NODEFILE | wc -l)
outname="job"$PBS_JOBID"out.log"

cd $PBS_O_WORKDIR


/public/software/apps/R/4.2.3/bin/R CMD BATCH --no-save Sample_size_HPC_SDI_BOTHSD_5_20231129.R