#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=5
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=16000

srun -n=1 /home/dobreschkow/surfsuite/surfsuite trackhalo 79085 -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_79085.hdf -from 100 -to 199 -logfile /home/dobreschkow/log_79085.txt
srun -n=1 /home/dobreschkow/surfsuite/surfsuite trackhalo 455803 -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_455803.hdf -from 100 -to 199 -logfile /home/dobreschkow/log_455803.txt
srun -n=1 /home/dobreschkow/surfsuite/surfsuite trackhalo 223918 -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_223918.hdf -from 100 -to 199 -logfile /home/dobreschkow/log_223918.txt
srun -n=1 /home/dobreschkow/surfsuite/surfsuite trackhalo 672740 -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_672740.hdf -from 100 -to 199 -logfile /home/dobreschkow/log_672740.txt
srun -n=1 /home/dobreschkow/surfsuite/surfsuite trackhalo 1118933 -simulation L210_N1024-Hydro3D-hyades -snapshot 199 -subhalos 1 -center 1 -parameterfile /home/dobreschkow/surfsuite/parameters.txt -outputfile /home/dobreschkow/track_halo_1118933.hdf -from 100 -to 199 -logfile /home/dobreschkow/log_1118933.txt