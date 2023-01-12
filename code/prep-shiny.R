# Project:   mipca_compare
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2022-12-17
# Modified:  2022-12-17
# Notes:     This script prepares the input data for the shiny app plot.mi.pca.

# Load data --------------------------------------------------------------------

# Results
ggshapeld <- readRDS("../output/9950505_main_gg_shape.rds") # 1st BRM submission w/ correct MI-OP
ggshapehd <- readRDS("../output/9987321_main_gg_shape.rds") # HD version with full pj

# Mids objects
mids_sim2 <- readRDS("../output/convergence-sim2-p242.rds")
mids_caseStudy_expert <- readRDS("../output/convergence-expert.rds")
mids_caseStudy_si4auxall <- readRDS("../output/convergence-si-4-aux-all.rds")
mids_caseStudy_pcraux <- readRDS("../output/convergence-pcraux.rds")
mids_caseStudy_vbv <- readRDS("../output/convergence-vbv.rds")
mids_caseStudy_default <- readRDS("../output/convergence-default.rds")

# Prepare result data ----------------------------------------------------------

# Combine data
ggshape <- rbind(
    cbind(j = 56, ggshapeld),
    cbind(j = 242, ggshapehd)
)

# Map names of methods
methods_names <- data.frame(
    original = c(
        "all", "all_oracle", "aux", "vbv",
        "MIOP", "MIOR", "MIMI", "CC", "OG"
    ),
    plot = c(
        "MI-PCR-ALL", "MI-PCR-ALL (oracle)", "MI-PCR-AUX", "MI-PCR-VBV",
        "MI-QP", "MI-OR", "MI-MI", "CC", "OG"
    )
)

# Change names of factors for plot
methods_names <- methods_names[methods_names$original %in% levels(ggshape$method), ]
levels(ggshape$method) <- methods_names$plot

# Round pj
ggshape$pj <- round(ggshape$pj, 2)

# Make a different factor for labelling npcs
ggshape$NPC <- ggshape[, "npc"]
levels(ggshape$NPC) <- list(
    "7+" = as.character(7:max(as.numeric(levels(ggshape[, "npc"])))),
    "1 to 6" = as.character(1:6),
    "0" = c("0")
)

# Make npc names better
ggshape$npc <- as.numeric(as.character(ggshape$npc))

# Make Parameter names better
current_levels <- levels(ggshape$par)

current_levels <- gsub("~1", " mean", current_levels)
current_levels <- gsub("r", " correlation ", current_levels)
current_levels <- gsub("~~", " covariance ", current_levels)
levels(ggshape$par) <- current_levels

# Give the desired name
dataResults <- ggshape

# mids objects -----------------------------------------------------------------

# Compress mids for simulation study

for (i in 1:length(mids_sim2)) {
    # Keep the only two objects you need for the trace plots
    mids_sim2[[i]] <- list(
        chainMean = mids_sim2[[i]]$chainMean,
        chainVar = mids_sim2[[i]]$chainVar,
        m = mids_sim2[[i]]$m,
        iteration = mids_sim2[[i]]$iteration
    )

    # Get rid of non-imputed values
    mids_sim2[[i]]$chainMean <- mids_sim2[[i]]$chainMean[1:4, , ]
    mids_sim2[[i]]$chainVar <- mids_sim2[[i]]$chainVar[1:4, , ]
}

# Collect mids for data application

mids_casestudy <- list(
    expert = mids_caseStudy_expert,
    si4auxall = mids_caseStudy_si4auxall,
    pcraux = mids_caseStudy_pcraux$mids,
    vbv = mids_caseStudy_vbv,
    default = mids_caseStudy_default
)

# Compress mids for data application

for (i in 1:length(mids_casestudy)) {
    # Keep the only two objects you need for the trace plots
    mids_casestudy[[i]] <- list(
        chainMean = mids_casestudy[[i]]$chainMean,
        chainVar = mids_casestudy[[i]]$chainVar,
        m = mids_casestudy[[i]]$m,
        iteration = mids_casestudy[[i]]$iteration
    )

    # Which variables are we interested in?
    rowindex <- c("yp1", "yp2", "yp3", "yc1", "yc2", "yc3")

    # Get rid of non-imputed values
    mids_casestudy[[i]]$chainMean <- mids_casestudy[[i]]$chainMean[rowindex, , ]
    mids_casestudy[[i]]$chainVar <- mids_casestudy[[i]]$chainVar[rowindex, , ]
}

# Give the desired name
dataMids <- list(
    sim = mids_sim2,
    fdd = mids_casestudy
)

# Store results ----------------------------------------------------------------

# Save the two objects as .rda ready for shiny app
save(dataMids, file = "../output/dataMids.rda")
save(dataResults, file = "../output/dataResults.rda")