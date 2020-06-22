#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=3
#SBATCH --time=48:00:00
#SBATCH --mem-per-cpu=16000

module load gfortran/6.3.0 hdf5/1.10.2

for sn in {131,156,199}
do
   srun -n 1 /home/dobreschkow/surfsuite/surfsuite makehalos -parameterset L210_N1024-new-hyades -snapshot $sn -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/log.txt &
done
wait