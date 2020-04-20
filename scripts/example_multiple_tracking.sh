#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=5
#SBATCH --time=06:00:00
#SBATCH --mem-per-cpu=16000

for halo in {79085,455803,223918,672740,1118933}
do
srun -n 1 /home/dobreschkow/surfsuite/surfsuite trackhalo $halo -parameterset L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/surfsuite/track_halo_$halo.hdf -from 70 -to 199 -logfile /home/dobreschkow/surfsuite/log_track_$halo.txt &
done
wait