# Project:   mipca_compare
# Objective: function computes the mi degrees of freedom
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-21

miDf <- function(m, b, t, dfCom) {
  fmi   <- fmi(m, b, t)
  df0   <- (m - 1) * (1 / fmi^2)
  dfObs <- (dfCom + 1) / (dfCom + 3) * dfCom * (1 - fmi)

  df0 / (1 + (df0 / dfObs))
}