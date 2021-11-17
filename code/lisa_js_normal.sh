#!/bin/bash
#SBATCH -N 1
#SBATCH -p normal
#SBATCH -t 08:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=e.costantini@tilburguniversity.edu

## Description
# Project:   mipca_compare
# Objective: lisa job script (normal partition array type)
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

## USAGE on LISA:
##   sbatch -a 1-ARRAY_NUM exp5_js_mainSim_normal.sh
##
## ARGS:
##   ARRAY_NUM = Number of arrays to parallelize over
##
## NOTES:
##
##
##

## Load Modules
module load R

## Define Variables and Directories
projDir=$HOME/mipca_compare	  # Project directory
inDir=$projDir/code           # Source directory (for R)
ncores=`sara-get-num-cores` 	# Number of available cores
idJob=$SLURM_ARRAY_JOB_ID  	  # Master ID for the array of jobs
idTask=$SLURM_ARRAY_TASK_ID 	# Array index for the current job

## Define Output Directories
# Temporary
tmpOut="$TMPDIR"/$idJob\_$idTask
mkdir -p $tmpOut

# Final
outDir=$projDir/output/$idJob
	if [ ! -d "$outDir" ]; then 	# create if missing
	    mkdir -p $outDir
	fi

## Allow worker nodes to find my personal R packages:
export R_LIBS=$HOME/R/x86_64-pc-linux-gnu-library/4.0/
# for R_LIBS explain: https://statistics.berkeley.edu/computing/R-packages
# this is probably overkill but keep it in the loop for safety (and maybe ask Kyle about it)

## Store the stopos pool's name in the environment variable STOPOS_POOL:
export STOPOS_POOL=pool

## Loop Over Cores
for (( i=1; i<ncores ; i++ )) ; do
(
	## Get the next line or parameters from the stopos pool:
	stopos next

	## Did we get a line? If not, break the loop:
	if [ "$STOPOS_RC" != "OK" ]; then
	    break
	fi

	## If it's the first Stopos value, then Run the Rscript to store session info
	if [ $STOPOS_VALUE = 1 ]; then
	    Rscript ./lisa_step2_storeInfo.R $outDir/
	fi
	
	## Call the R script with the replication number from the stopos pool:
	Rscript $inDir/lisa_step3_run_doRep.R $STOPOS_VALUE $tmpOut/
	# script_name.R --options repetition_counter output_directory

 	## Remove the used parameter line from the stopos pool:
	stopos remove
) &
done
wait

## Compress the output directory:

 # Go to folder containing the stuff i want to zip
 cd $tmpOut/

 # Zip everything that is inside (./.)
 tar -czf "$TMPDIR"/$idJob\_$idTask.tar.gz ./.

## Copy output from scratch to output directory:
 cp -a "$TMPDIR"/$idJob\_$idTask.tar.gz $outDir/
