# Project:   mipca_compare
# Objective: subroutine runCellNgdr to run a single condition for non-graphical decision rule check
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-03
# Note:      A "cell" is a cycle through the set of conditions.
#            The function in this script generates 1 data set, performs
#            imputations for every condition in the set.

# Run cell subroutine ----------------------------------------------------------

runCell.extra.ngdr <- function(rp, cond, fs, parms) {
    # Example Internals --------------------------------------------------------

    # cond = conds[41, ]
    # cond <- conds %>%
    #     filter(
    #         lv == TRUE,
    #         K == Inf,
    #         pj == 0,
    #         P == 56
    #     )
    # rp   = 1
    tryCatch(
        {
            ### START TRYCATCH EXPRESSION

            # Process condition
            parms$largeP <- ifelse(cond$P == 242, TRUE, FALSE)

            # Data Generation --------------------------------------------------

            # Generate data with a latent structure
            if (cond$lv == TRUE) {
                dat <- genDataLatent(parms = parms, cond = cond)
            }
            if (cond$lv == FALSE) {
                dat <- genData(parms = parms, cond = cond)
            }

            # Discretize data
            dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms)

            # Impose Missingness
            preds <- dat$x[, parms$vmap$mp, drop = FALSE]
            targets <- dat_ordi[, parms$vmap$ta, drop = FALSE]
            target_miss <- amputePerVar(
                targets = targets,
                preds = preds,
                pm = parms$pm,
                type = "high"
            )
            dat_miss <- cbind(target_miss, dat_ordi[, -parms$vmap$ta])

            # Process the number of components ---------------------------------

            # Compute the correlation matrices
            dat_ordi_corr <- cor(dat_ordi)
            dat_miss_corr <- cor(na.omit(dat_miss))

            # Compute non-graphical solutions
            ngdr_miss <- nScree(dat_miss_corr)$Components
            ngdr_orig <- nScree(dat_ordi_corr)$Components

            # Append the data type
            ngdr_miss <- rbind(data = "na", value = ngdr_miss)
            ngdr_orig <- rbind(data = "og", value = ngdr_orig)

            # Combine the results
            res <- t(cbind(ngdr_orig, ngdr_miss))

            # Append the row names as a column
            res <- cbind(method = rownames(res), res)

            # Transform to data.frame
            res <- data.frame(res, row.names = NULL)

            # Append condition and repetition
            res <- cbind(cond, res)

            # Append repetition name
            res <- cbind(rep = rp, res)

            # Save the results
            saveRDS(res,
                file = paste0(
                    fs$outDir,
                    "rep_", rp, "_", cond$tag,
                    ".rds"
                )
            )

            ### END TRYCATCH EXPRESSION
        },
        error = function(e) {
            err <- paste0("Original Error: ", e)
            err_res <- cbind(rp = rp, cond, Error = err)
            saveRDS(err_res,
                file = paste0(
                    fs$outDir,
                    "rep_", rp, "_", cond$tag,
                    "_ERROR",
                    ".rds"
                )
            )
            return(NULL)
        }
    )
}

# Run repetition subroutine ----------------------------------------------------

runRep.extra.ngdr <- function(rp, conds, parms, fs) {
    # rp = 1

    # Set seed
    .lec.SetPackageSeed(rep(parms$seed, 6))
    if (!rp %in% .lec.GetStreams()) { # if the streams do not exist yet
        .lec.CreateStream(c(1:parms$nStreams))
    } # then
    .lec.CurrentStream(rp) # this is equivalent to setting the seed Rle

    # Cycle thorough conditions
    for (i in 1:nrow(conds)) {
        # i <- 1
        print(paste0(
            "Rep: ", rp,
            " / Cond: ", i,
            " / Time: ",
            Sys.time()
        ))

        runCell.extra.ngdr(
            rp = rp,
            cond = conds[i, ],
            fs = fs,
            parms = parms
        )
    }
}
