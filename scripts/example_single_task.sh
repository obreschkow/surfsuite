#!/bin/bash
#
#SBATCH --job-name=surfsuite
#
#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=32000

srun /home/dobreschkow/surfsuite/surfsuite makehalos -parameterset L210_N1024-Hydro6Dsubs-hyades -parameterfile /home/dobreschkow/surfsuite/parameters.txt -logfile /home/dobreschkow/surfsuite/log.txt