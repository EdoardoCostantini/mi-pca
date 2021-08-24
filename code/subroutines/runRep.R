# Project:   mipca_compare
# Objective: subroutine doRep to run all conditions for a single repetition
# Author:    Edoardo Costantini
# Created:   2021-08-24
# Modified:  2021-08-24

runRep <- function(rp, conds, parms, fs) {

# Example Internals -------------------------------------------------------

  # rp = 1

  ## Set seed
  .lec.SetPackageSeed(rep(parms$seed, 6))
  if(!rp %in% .lec.GetStreams()) # if the streams do not exist yet
    .lec.CreateStream(c(1 : parms$nStreams)) # then
  .lec.CurrentStream(rp) # this is equivalent to setting the seed Rle

  ## Progress report
  parms$rep_counter <- parms$rep_counter + 1 # increase progres report counter
  cat(paste0(Sys.time(), " - Starts Repetition: ", rp,
             "\n"),
      file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
      append = TRUE)

  # Cycle thorugh conditions
  for(i in 1 : nrow(conds)) {
    # i <- 1
    print(paste0("Rep: ", rp,
                 " / Cond: ", i,
                 " / Time: ",
                 Sys.time()))

    runCell(rp = rp,
            cond = conds[i, ],
            fs = fs,
            parms = parms)
  }

}
