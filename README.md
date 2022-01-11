# MI-PCR Comparison
*Readme last update: 2022-01-11* 

Here I describe the content of this repository and how to replicate the simulation study.

# Contents
This directory contains the following main sub-folders:
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
already has the packages that are required by the simulation.
The script `init.R` contains the list of required packages.

## Running the simulation on Lisa

Lisa Cluster is a cluster computer system managed by SURFsara, a cooperative association of Dutch educational and
research institutions.
Researchers at most dutch universities can request access to this cluster computer.

- Open the initialization script `init.R` and check that:
  - you have all the required packages installed;
  - the parameter `parms$run_type` is set to 1 (this will deploy the conditions for the final run of the simulation
    as opposed to the trial and convergence checks run). 
  - the fixed parameters and experimental factor levels are set to the desired values.
- Run on a personal computer the script `lisa_step0_time_est.R` to check how long it takes
  to perform a single run across all the conditions with the chosen simulation study set up.
  This will create an R object called `wall_time`.
- Open the script `lisa_js_normal.sh` and replace the wall time in the header with the value
  of `wall_time`.
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
- Go to the `lisa/` and create a folder with a meaningful name for the run (I usually use the 
  current date). Then, copy here the `code` and `input` folders and create an empty `output` 
  folder.
- Authenticate on Lisa
- Check all the packages called by the `init.R` script are available and install what is not.
- From your terminal, upload the folder containing the project
  ```
  scp -r path/to/local/project user@lisa.surfsara.nl:project
  ```
  For example:
  ```
  scp -r lisa/20211116 ******@lisa.surfsara.nl:mipca_compare
  ```
- Go back to the lisa terminal and load the following modules
  ```
  module load 2020
  module load R/4.0.2-intel-2020a
  module load Stopos/0.93-GCC-9.3.0  
  ```
  (or their most recent version at the time you are running this)
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
- The script `lisa_step4_results.R` goes through the Lisa result folder, unzips tar.gz
  packages and puts results together.
- Finally, the script `combine_results.R` computes bias, CIC, and all the outcome measures. 
  It also puts together the RDS objects that can be plotted with the functions stored in `./code/plots/`

## Running the simulation on a PC / Mac

You can also replicate the simulation on a personal computer by following these steps: 

- Open the initialization script `init.R` and check that:
  - you have all the required packages installed;
  - the parameter `parms$run_type` is set to 1 (this will deploy the conditions for the final run of the simulation
    as opposed to the trial and convergence checks run). 
  - the fixed parameters and experimental factor levels are set to the desired values.
- Open the script `pc_step1_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `pc_step1_sim.R`
- Run the script `pc_step2_unzip.R` to unzip the results and create a single .rds file
- Finally, the script `combine_results.R` computes bias, CIC, and all the outcome measures. 
  It also puts together the RDS objects that can be plotted with the functions stored in `./code/plots/`