# Project:   mipca_compare
# Objective: Unzip and prepare data for plotting
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-03

# Prep environment --------------------------------------------------------

rm(list = ls()) # to clean up
source("./init_extra_ngdr.R") # only for support functions

# Load Results ------------------------------------------------------------

inDir <- "../output/"
target_tar <- "20230302_153750.tar.gz"
output <- readTarGz(target_tar)

# Collect main results
rds_main <- do.call(rbind, output$out[-grep("ERROR", output$file_names)])

# Read error results
rds_error_names <- grep("ERROR", output$file_names)
if (length(rds_error_names) != 0) {
    rds_error <- do.call(rbind, output$out[rds_error_names])
} else {
    rds_error <- NULL
}

# Process results --------------------------------------------------------------

# Make value a number
rds_main$value <- as.numeric(rds_main$value)

# Make some variables factors
rds_main$K <- factor(rds_main$K)
rds_main$method <- factor(
    rds_main$method, 
    levels = c("noc", "naf", "nkaiser", "nparallel"),
    labels = c("noc", "naf", "nk", "np")
    )
rds_main$data <- factor(rds_main$data, levels = c("og", "na"), labels = c("Original data", "Complete cases"))
rds_main$K <- factor(rds_main$K, levels = rev(unique(rds_main$K)))

# Store the rds results
saveRDS(
    object = rds_main,
    file = paste0(
        inDir,
        "extra_ngdr_gg_shape.rds"
    )
)

# Subset the data as desired
rds_main_sub <- rds_main %>%
    filter(
        K %in% c(Inf, 5, 2),
        pj %in% unique(rds_main$pj)[c(1, 3, 4)],
        data == "Complete cases",
        P == 242,
        lv == TRUE
    )

# Take the average of value across repetitions for conditions of interest
res <- rds_main %>%
    filter(
        K %in% c(Inf, 5, 2),
        pj %in% unique(rds_main$pj)[c(1, 3, 4)],
        data == "Complete cases",
        P == 242,
        lv == TRUE
    ) %>%
    group_by(tag, P, K, D, interval, pj, lv, method, data) %>%
    summarise(
        mean = mean(value),
        median = floor(median(value)),
        min = min(value),
        max = max(value),
        IQR = IQR(value),
        lower = median(value) - 1.5 * IQR(value),
        upper = median(value) + 1.5 * IQR(value)
    )

# Make result a data.frame
res <- data.frame(res)

# IQR range plots --------------------------------------------------------------

ggplot(res, aes(factor(method), median)) +
    geom_point() +
    geom_errorbar(aes(ymin = min, ymax = max),
        width = .3,
        position = position_dodge(.9)
    ) +
    facet_grid(
        rows = vars(K),
        cols = vars(pj),
        scales = "free"
    ) +
    theme_bw() +
    geom_label(
        data = res,
        # label.padding = unit(.1, "lines"),
        # label.size = .1,
        # size = 1,
        aes(x = method, y = median, label = factor(median))
    ) +
    coord_cartesian(
        ylim = c(0, max(res$max))
    ) +
    theme(
        # text = element_text(size = 4),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )
