#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --n=4
#SBATCH --time=48:00:00
#SBATCH --cpu-per-task=1
#SBATCH --mem-per-cpu=16000

module load gfortran/6.3.0 hdf5/1.10.2

for sn in {0..198}
do
   srun -n 1 /home/dobreschkow/surfsuite/surfsuite sortparticles -simulation L210_N1024-Hydro3D-hyades -snapshot $sn -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/logfile3D.txt
done