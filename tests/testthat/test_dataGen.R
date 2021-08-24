### Title:    Test File for the data generation context
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-25
### Modified: 2021-07-09

  context('- test function genData()')

# Correct proportion of discrete vars -------------------------------------
  set.seed(2134)

  # Expectation Storing Objects
  expect_not_NA <- rep(NA, nrow(conds))
  expect_discrete <- rep(NA, nrow(conds))
  expect_list <- rep(NA, nrow(conds))
  expect_df_observed <- rep(NA, nrow(conds))
  expect_matrix_rest <- rep(NA, nrow(conds))

  for (i in 1:nrow(conds)){
    dat_list <- genData(parms = parms, cond = conds[i, ])

    # Is not NA
    expect_not_NA[i] <- any(!is.na(dat_list$dat_ob))

    # Discreteness
    disc_pool_size <- length(parms$vmap_lv$disc_pool)
    disc_preds_num <- sum(sapply(dat_list$dat_ob, is.factor))
    ratio <- disc_preds_num/disc_pool_size
    expect_discrete[i] <- abs(conds[i, "D"] - ratio) < .1

    # Object Types
    expect_list[i] <- is.list(dat_list) & !is.atomic(dat_list)
    expect_df_observed[i] <- is.data.frame(dat_list$dat_ob)
    expect_matrix_rest[i] <- all(sapply(dat_list[-1], is.matrix))
  }

  # Tests
  test_that("Data is not empty", {
    expect_equal(all(expect_not_NA), TRUE)
  })
  test_that("Correct Proportion of discrete variables in all conditions", {
    expect_equal(all(expect_discrete), TRUE)
  })
  test_that("Output is a list", {
    expect_equal(all(expect_list), TRUE)
  })
  test_that("Data set observed items is data.frame", {
    expect_equal(all(expect_matrix_rest), TRUE)
  })
  test_that("Other objects are matrices", {
    expect_equal(all(expect_matrix_rest), TRUE)
  })