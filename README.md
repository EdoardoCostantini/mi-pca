[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7529273.svg)](https://doi.org/10.5281/zenodo.7529273)

# MI-PCR Comparison

Here I describe the content of this repository and how to replicate the simulation study.

# How to replicate results

To replicate the study, you first need to make sure you have installed all the packages used.
You can use the `./input/prep_machine.R` script to install them.
You should pay special attention to the version of `mice` you are using. This study uses the special version of this package that is stored in the `input/` folder. The repository [mice.sim.pcr](https://github.com/EdoardoCostantini/mice.sim.pcr/tree/master) stores the code for this version.
In the following guide, it is assumed that the machine on which the simulation is run already has all packages installed.

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
  - you have all the required packages installed
  - the fixed parameters and experimental factor levels are set to the desired values
  - the parameter `parms$run_type` is set to 1 (this will deploy the conditions for the final run of the simulation as opposed to the trial and convergence checks run)
- Open the script `pc_step1_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `pc_step1_sim.R`
- Run the script `pc_step2_unzip.R` to unzip the results and create a single .rds file
- Finally, the script `combine_results.R` computes bias, CIC, and all the outcome measures. 
  It also puts together the RDS objects that can be plotted with the functions stored in `./code/plots/`
  
## Convergence checks

To perform convergence checks:

- Open the initialization script `init.R` and check that:
  - you have all the required packages installed
  - the fixed parameters and experimental factor levels are set to the desired values
  - the parameter `parms$run_type` is set to 3 (this will deploy the conditions for convergence check
- Open the script `pc_step1_sim.R`
- Define the number of desired repetitions by changing the parameter `reps`
- Define the number of clusters for parallelization by changing the parameter `clus`
- Run the script `pc_step1_sim.R`
- Open and run the script `checks/check_convergence.R` to obtain convergence plots for desired conditions and repetitions

# Output files

Notes on what the output files are:
- `20220607_175414.tar.gz` contains convergence checks
- `output/lisa/8469421/` contains results reported in 1st submission BRM paper;
- `output/lisa/9950505/` contains results version of BRM paper results with correct MI-OP method;
- `output/lisa/???????/` contains HD results requested by of BRM paper revision
- `20230303_154402.tar.gz` contains results for the non-graphical decision rules checks

# Repository content

Here is a brief description of the folders:
- `code`: the main software to run the study
  - `checks` folder contains some script to check a few details of the procedure are producing the expected results
  - `experiments` folder contains some initial trial scripts
  - `functions` folder with the main project specific functions
  - `helper` folder with functions to address file management and other small internal tasks 
  - `plots` folder containing some plotting scripts and pdfs
  - `subroutines` folder with the generic functions to run the simulation study
- `input` folder where all the input files (e.g., data) should be stored
- `output` folder where the results of the simulation study will be stored
- `test` folder containing unit testing files