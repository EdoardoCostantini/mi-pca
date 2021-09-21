# Project:   mipca_compare
# Objective: function computes relative increase in variance due to nonresponse
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-21

riv <- function(m, b, u){
  # relative increase in variance due to nonresponse
  # (van Buuren, 2018, p. 47)
  riv <- (1 + 1/m) * b/u
  return(riv)
}