# Project:   mipca_compare
# Objective: function computes proportion of variation due to missing data
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-21

fmi <- function(m, b, t){
  # proportion of variation attributable to the missing data
  # aka fmi
  # (van Buuren, 2018, p. 46)
  fmi <- (1 + 1/m) * b/t
  return(fmi)
}