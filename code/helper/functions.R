### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2020-06-21
### Note:     This script contains functions of level-1 which are functions
###           that cannot contain other functions I defined). This is the
###           lower functional level. I can use functions defined by other
###           pacakges, but not other functions I have written for this
###           project. If I wanted to include them I would then be working
###           with a subroutine.

# Discretize Variable -----------------------------------------------------

disVar <- function(j, var_type, parms){
  # Given a target discretizion (none = 1, ordinal = 2, binary = 3), 
  # this function discretize the numeric vector j
  if(var_type == 1){
    j_out <- j
  }
  if(var_type == 2){
    j_out <- as.numeric(cut(j, breaks = parms$K))
  } 
  if(var_type == 3){
    j_out <- as.numeric(cut(j, breaks = 2))
  }
  return(j_out)
}
