#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=8
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=16000

for halo in {79085,455803,223918,672740,1118933}
do
srun -n 1 /home/dobreschkow/surfsuite/surfsuite trackhalo $halo -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_79085.hdf -from 100 -to 199 -logfile /home/dobreschkow/surfsuite/log_track_$halo.txt &
done
wait