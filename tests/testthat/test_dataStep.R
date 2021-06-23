### Title:    Test File for the data generation context
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-06-23
### Modified: 2021-06-23

  context('- test subroutine dataStep()')

# Correct return of objects -----------------------------------------------

  expect_list <- rep(NA, nrow(conds))
  expect_data_frame <- rep(NA, nrow(conds))

  for (i in 1:nrow(conds)){
    dat_list <- dataStep(parms = parms, cond = conds[i, ])
    expect_list[i] <- is.list(dat_list) & !is.atomic(dat_list)
    expect_data_frame[i] <- all(sapply(dat_list$dat, is.data.frame))
  }

  # Tests
  test_that("Output is a list", {
    expect_equal(all(expect_list), TRUE)
  })
  test_that("Data sets generated are data.frame", {
    expect_equal(all(expect_data_frame), TRUE)
  })
