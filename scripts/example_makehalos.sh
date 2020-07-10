#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=6
#SBATCH --time=48:00:00
#SBATCH --mem-per-cpu=16000

module load gfortran/6.3.0 hdf5/1.10.2

for sn in {199,156,131,114,100,88}
do
   srun -n 1 /home/dobreschkow/surfsuite/surfsuite makehalos -parameterset L210_N1024-Hydro-new -snapshot $sn -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/log$sn.txt &
done
wait