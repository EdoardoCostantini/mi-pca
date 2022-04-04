# Title:    Initialization for Blade Computing run
# Author:   Edoardo Costantini

# 1. Install all packages you can
install.packages(pack_list)

# 2. Install some packages that you need but are not listed
install.packages("glmnet")
install.packages("pls")

# 2. Install Local mice.sim.pcr
install.packages("../prep/mice.pcr.sim", 
                 repos = NULL, 
                 type = "source")
