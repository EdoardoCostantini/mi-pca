# MI-PCR Comparison
Describe project briefly.

# Contents
This directory contains the following main subfolders:
- code: the main software to run the study
  - checks
  - experiments
  - functions
  - helper
  - plots
  - subroutines
- input
- output: the folder where the results of scripts located in code are stored
- lisa: used only to store content for lisa cluster runs

# How to replicate results

In the following guide, it is assumed that the machine on which the simulation is run 
already has the pacakges that are required by the simulation.
The script `init.R` contains the list of required packages.

## Running the simulation on Lisa

Lisa Cluster is a cluster computer system managed by SURFsara, a cooperative association of Dutch educational and
research institutions.
Researchers at most dutch universities can request access to this cluster computer.

- Decide the number of repetitions in the preparatory script `lisa_step1_write_repList.R`
  For example:
  ```
  goal_reps <- 500 
  ncores    <- 15 # Lisa nodes have 16 cores available (16-1 used)
  narray    <- ceiling(goal_reps/ncores) # number of arrays/nodes to use 
  ```
  Once you have specified these values, run the script on your computer. 
  This will create a `stopos_lines` text file that will define the repetition index.
  This file will be located in the `input` folder.
- Authenticate on Lisa
- From your terminal, upload the folder containing the project
  ```
  scp -r path/to/local/project user@lisa.surfsara.nl:project
  ```
  For example:
  ```
  scp -r lisa/20211116 ******@lisa.surfsara.nl:mipca_compare
  ```
- load the following modules
  ```
  module load 2020
  module load R/4.0.2-intel-2020a
  module load Stopos/0.93-GCC-9.3.0  
  ```
- go to the code folder on the lisa cloned project
  ``` 
  cd mipca_compare/code/ 
  ```
- run the prepping script by
  ```
  . lisa_prep.sh ../input/stopos_lines 
  ```
- submit the jobs by
  ```
  sbatch -a 1-34 lisa_js_normal.sh 
  ```
  Note that `1-34` defines the dimensionality of the array of jobs. 
  For a short partitioning, only 2 arrays are allowed.
  For other partitioning, more arrays are allowed.
  34 is the result of `ceiling(goal_reps/ncores)` for the chosen parameters in this example.
- When the array of jobs is done, you can pull the results to your machine by
  ```
  scp -r user@lisa.surfsara.nl:mipca_compare/output/folder path/to/local/project/output/folder
  ```
- Finally, the script `lisa_step4_results.R` goes through the Lisa result folder, unzips tar.gz
  packages and puts results together.

## Running the simulation on a PC / Mac

You can also replicate the simulation on a personal computer by following these steps: 

- Open the script `run_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `run_sim.R`
- results are then pooled by the `pooling.R` script.