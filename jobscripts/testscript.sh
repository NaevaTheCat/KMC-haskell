#!/bin/bash

#TEMPLATE FOR JOB GENERATION
#ALLCAPS are variables to be replaced with values

##################
# Set PBS Commands 
# PBS commands must come at the top of this script, before any other commands.
# maximums: ncps=48, walltime=200:00:00, mem=256GB/48cores
#################

#PBS -l ncpus=1
#PBS -l mem=MEMORYLIMIT
#PBS -l walltime=HH:MM:00

#PBS -q workq

# Send email on abort, begin, and end
#PBS -m abe
#PBS -M rnoxley@gmail.com

###############
# Start the Job
###############

jobdir=$HOME/projects/KMC-haskell
stackdir=.stack-work/install/x86_64-linux-gmp4/lts-3.14/7.10.2/bin

cd $jobdir

$jobdir/$stackdir/KMC-haskell PRESSURE XDIM YDIM
