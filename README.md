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

## Running the simulation on LISA

- Decide the number of repetitions in the preparatory script `lisa_step1_write.repList.R`
- Run `lisa_step1_write.repList.R`
- Log in to Lisa
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
  sbatch -a 1-2 lisa_js_short.sh 
  ```
  Note that `1-2` defines the dimensionality of the array of jobs. 
  For a short partitioning, only 2 arrays are allowed.
  For other partitioning, more arrays are allowed.
- When the array of jobs is done, you can pull the results to your machine by
```
scp -r user@lisa.surfsara.nl:mipca_compare/output/folder path/to/local/project/output/folder
```

## Running the simulation on a PC / Mac

- Open the script `run_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `run_sim.R`