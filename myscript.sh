#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=2
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=32000

srun /home/dobreschkow/surfsuite/surfsuite makeall -simulation L210_N1024-Hydro3D-hyades -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/logfile3D.txt
srun /home/dobreschkow/surfsuite/surfsuite makeall -simulation L210_N1024-Hydro6D-hyades -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/logfile6D.txt