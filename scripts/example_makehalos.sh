#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=8
#SBATCH --time=48:00:00
#SBATCH --mem-per-cpu=16000

module load gfortran/6.3.0 hdf5/1.10.2

for sn in {70,79,88,99,113,131,156}
do
   srun -n 1 /home/dobreschkow/surfsuite/surfsuite makehalos -parameterset L210_N1024-Hydro3D-hyades -snapshot $sn -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/log.txt &
done
wait