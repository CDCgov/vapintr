################# validate ###############

test_that("correct error thrown when symbol is missing", {

  errorMessage <- "Error: no symbol passed to JEMParamDist constructor"
  expect_error(JEMParamDist$new(symbol = NULL,
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Constant",
                                constant = 10),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = NA,
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Constant",
                                constant = 10),
               errorMessage, fixed = TRUE)

})

test_that("correct error thrown when units are missing", {

  errorMessage <- "Error: parameter units missing from the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = NULL,
                                dist_type = "Constant",
                                constant = 10),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = NA,
                                dist_type = "Constant",
                                constant = 10),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when distribution type is not one of the allowed parameter types", {

  errorMessage <- "Error: incorrect distribution type assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "NotAccepted",
                                constant = 10),
               errorMessage, fixed = TRUE)
})

# Check that all required parameters are assigned for each supported distribution type

#Constant

test_that("correct error thrown when constant parameter is not assigned for constant distribution", {

  errorMessage <- "Error: constant value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Constant",
                                constant = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Constant",
                                constant = NA),
               errorMessage, fixed = TRUE)
})

#Minimum

test_that("correct error thrown when minimum parameter is not assigned for uniform distribution", {

  errorMessage <- "Error: minimum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = NA,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not assigned for triangular distribution", {

  errorMessage <- "Error: minimum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = NULL,
                                mode = 1,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = NA,
                                mode = 1,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not assigned for PERT distribution", {

  errorMessage <- "Error: minimum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = NULL,
                                mode = 1,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = NA,
                                mode = 1,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not assigned for truncated normal distribution", {

  errorMessage <- "Error: minimum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = NULL,
                                arith_mean = 1,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = NA,
                                arith_mean = 1,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not assigned for truncated lognormal distribution", {

  errorMessage <- "Error: minimum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = NULL,
                                geom_mean = 1,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = NA,
                                geom_mean = 1,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Maximum
test_that("correct error thrown when maximum parameter is not assigned for uniform distribution", {

  errorMessage <- "Error: maximum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = 1,
                                maximum = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = 1,
                                maximum = NA),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not assigned for triangular distribution", {

  errorMessage <- "Error: maximum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = 3,
                                maximum = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = 3,
                                maximum = NA),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not assigned for PERT distribution", {

  errorMessage <- "Error: maximum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = 3,
                                maximum = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = 3,
                                maximum = NA),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not assigned for truncated normal distribution", {

  errorMessage <- "Error: maximum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = NA),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not assigned for truncated lognormal distribution", {

  errorMessage <- "Error: maximum value not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = NULL),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = NA),
               errorMessage, fixed = TRUE)
})

#Mode
test_that("correct error thrown when mode parameter is not assigned for triangular distribution", {

  errorMessage <- "Error: mode not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = NA,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when mode parameter is not assigned for PERT distribution", {

  errorMessage <- "Error: mode not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = NA,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Arithmetic mean
test_that("correct error thrown when arithmetic mean parameter is not assigned for truncated normal distribution", {

  errorMessage <- "Error: arithmetic mean not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = NULL,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = NA,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Arithmetic standard deviation
test_that("correct error thrown when arithmetic standard deviation parameter is not assigned for truncated normal distribution", {

  errorMessage <- "Error: arithmetic standard deviation not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Geometric mean
test_that("correct error thrown when geometric mean parameter is not assigned for truncated lognormal distribution", {

  errorMessage <- "Error: geometric mean not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = NULL,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = NA,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Geometric standard deviation
test_that("correct error thrown when geometric standard deviation parameter is not assigned for truncated lognormal distribution", {

  errorMessage <- "Error: geometric standard deviation not assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = NULL,
                                maximum = 5),
               errorMessage, fixed = TRUE)

  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = NA,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

# Check that all required parameters are numeric for each supported distribution type

#Constant

test_that("correct error thrown when constant parameter is not numeric for constant distribution", {

  errorMessage <- "Error: non-numeric constant value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Constant",
                                constant = is.character(5)),
               errorMessage, fixed = TRUE)

})

#Minimum
test_that("correct error thrown when minimum parameter is not numeric for uniform distribution", {

  errorMessage <- "Error: non-numeric minimum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = is.character(1),
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not numeric for triangular distribution", {

  errorMessage <- "Error: non-numeric minimum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = is.character(1),
                                mode = 3,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not numeric for PERT distribution", {

  errorMessage <- "Error: non-numeric minimum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = is.character(1),
                                mode = 3,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not numeric for truncated normal distribution", {

  errorMessage <- "Error: non-numeric minimum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = is.character(1),
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum parameter is not numeric for truncated lognormal distribution", {

  errorMessage <- "Error: non-numeric minimum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = is.character(1),
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Maximum
test_that("correct error thrown when maximum parameter is not numeric for uniform distribution", {

  errorMessage <- "Error: non-numeric maximum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = 1,
                                maximum = is.character(5)),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not numeric for triangular distribution", {

  errorMessage <- "Error: non-numeric maximum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = 3,
                                maximum = is.character(5)),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not numeric for PERT distribution", {

  errorMessage <- "Error: non-numeric maximum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = 3,
                                maximum = is.character(5)),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not numeric for truncated normal distribution", {

  errorMessage <- "Error: non-numeric maximum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = is.character(5)),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when maximum parameter is not numeric for truncated lognormal distribution", {

  errorMessage <- "Error: non-numeric maximum value assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = is.character(5)),
               errorMessage, fixed = TRUE)
})

#Mode
test_that("correct error thrown when mode parameter is not numeric for triangular distribution", {

  errorMessage <- "Error: non-numeric mode assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 1,
                                mode = is.character(3),
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when mode parameter is not numeric for PERT distribution", {

  errorMessage <- "Error: non-numeric mode assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 1,
                                mode = is.character(3),
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Arithmetic mean
test_that("correct error thrown when arithmetic mean parameter is not numeric for truncated normal distribution", {

  errorMessage <- "Error: non-numeric arithmetic mean assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = is.character(3),
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Arithmetic standard deviation
test_that("correct error thrown when arithmetic standard deviation parameter is not numeric for truncated normal distribution", {

  errorMessage <- "Error: non-numeric arithmetic standard deviation assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 3,
                                arith_stdev = is.character(0.5),
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Geometric mean
test_that("correct error thrown when geometric mean parameter is not numeric for truncated lognormal distribution", {

  errorMessage <- "Error: non-numeric geometric mean assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = is.character(3),
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Geometric standard deviation
test_that("correct error thrown when geometric standard deviation parameter is not numeric for truncated lognormal distribution", {

  errorMessage <- "Error: non-numeric geometric standard deviation assigned in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 3,
                                geom_stdev = is.character(0.5),
                                maximum = 5),
               errorMessage, fixed = TRUE)
})


# Check that the distribution parameters make logical sense

#Minimum is less than maximum

test_that("correct error thrown when minimum is not less than maximum for uniform distribution", {

  errorMessage <- "Error: the minimum is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Uniform",
                                minimum = 6,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum is not less than maximum for triangular distribution", {

  errorMessage <- "Error: the minimum is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Triangular",
                                minimum = 6,
                                mode = 3,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum is not less than maximum for PERT distribution", {

  errorMessage <- "Error: the minimum is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "PERT",
                                minimum = 6,
                                mode = 3,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum is not less than maximum for truncated normal distribution", {

  errorMessage <- "Error: the minimum is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 6,
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

test_that("correct error thrown when minimum is not less than maximum for truncated lognormal distribution", {

  errorMessage <- "Error: the minimum is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 6,
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Minimum is less than arithmetic mean
test_that("correct error thrown when minimum is not less than arithmetic mean for truncated normal distribution", {

  errorMessage <- "Error: the minimum is not less than the arithmetic mean in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 4,
                                arith_mean = 3,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})


#Arithmetic mean is less than maximum
test_that("correct error thrown when arithmetic mean is not less than maximum for truncated normal distribution", {

  errorMessage <- "Error: the arithmetic mean is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Normal",
                                minimum = 1,
                                arith_mean = 6,
                                arith_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

#Minimum is less than geometric mean
test_that("correct error thrown when minimum is not less than geometric mean for truncated lognormal distribution", {

  errorMessage <- "Error: the minimum is not less than the geometric mean in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 4,
                                geom_mean = 3,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})


#Geometric mean is less than maximum
test_that("correct error thrown when geometric mean is not less than maximum for truncated lognormal distribution", {

  errorMessage <- "Error: the geometric mean is not less than the maximum in the 'Ls' JEMParamDist object"
  expect_error(JEMParamDist$new(symbol = "Ls",
                                name = "Depth below grade to source",
                                units = "m",
                                dist_type = "Truncated Lognormal",
                                minimum = 1,
                                geom_mean = 6,
                                geom_stdev = 0.5,
                                maximum = 5),
               errorMessage, fixed = TRUE)
})

################# checkParamsAreNonNegative #####################

#Constant

test_that("correct error thrown when constant is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Constant",
                              constant = -1)

  errorMessage <- "Error: the assigned constant value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)


})

#Minimum

test_that("correct error thrown when uniform minimum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = -1,
                              maximum = 5)

  errorMessage <- "Error: the assigned minimum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when triangular minimum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = -1,
                              mode = 2,
                              maximum = 5)

  errorMessage <- "Error: the assigned minimum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when PERT minimum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = -1,
                              mode = 2,
                              maximum = 5)

  errorMessage <- "Error: the assigned minimum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when truncated normal minimum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = -1,
                              arith_mean = 2,
                              arith_stdev = 0.5,
                              maximum = 5)

  errorMessage <- "Error: the assigned minimum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when truncated lognormal minimum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = -1,
                              geom_mean = 2,
                              geom_stdev = 0.5,
                              maximum = 5)

  errorMessage <- "Error: the assigned minimum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

# Maximum

test_that("correct error thrown when uniform maximum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = -5,
                              maximum = -1)

  errorMessage <- "Error: the assigned maximum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when triangular maximum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = -5,
                              mode = -2,
                              maximum = -1)

  errorMessage <- "Error: the assigned maximum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when PERT maximum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = -5,
                              mode = -2,
                              maximum = -1)

  errorMessage <- "Error: the assigned maximum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})


test_that("correct error thrown when truncated normal maximum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = -5,
                              arith_mean = -2,
                              arith_stdev = 0.5,
                              maximum = -1)

  errorMessage <- "Error: the assigned maximum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when truncated lognormal maximum is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = -5,
                              geom_mean = -2,
                              geom_stdev = 0.5,
                              maximum = -1)

  errorMessage <- "Error: the assigned maximum value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

#Mode
test_that("correct error thrown when triangular mode is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = -5,
                              mode = -2,
                              maximum = 1)

  errorMessage <- "Error: the assigned mode in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when PERT mode is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = -5,
                              mode = -2,
                              maximum = 1)

  errorMessage <- "Error: the assigned mode in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)
})


#Arithmetic mean

test_that("correct error thrown when truncated normal arithmetic mean is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = -5,
                              arith_mean = -2,
                              arith_stdev = 0.5,
                              maximum = 1)

  errorMessage <- "Error: the assigned arithmetic mean in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})


#Arithmetic standard deviation

test_that("correct error thrown when truncated normal arithmetic standard deviation is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 1,
                              arith_mean = 2,
                              arith_stdev = -0.5,
                              maximum = 5)

  errorMessage <- "Error: the assigned arithmetic standard deviation in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

#Geometric mean


test_that("correct error thrown when truncated lognormal geometric mean is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = -5,
                              geom_mean = -2,
                              geom_stdev = 0.5,
                              maximum = 1)

  errorMessage <- "Error: the assigned geometric mean in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})

#Geometric standard deviation

test_that("correct error thrown when truncated lognormal geometric standard deviation is negative", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 1,
                              geom_mean = 2,
                              geom_stdev = -0.5,
                              maximum = 5)

  errorMessage <- "Error: the assigned geometric standard deviation in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(Ls_dist$checkParamsAreNonNegative(), errorMessage, fixed = TRUE)

})


################# paramsAreLessThanUpperBound #####################


test_that("test for value less than upper bound works as expected for constants", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Constant",
                              constant = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

#Minimum

test_that("test for value less than upper bound works as expected for minimums for a uniform distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = 2,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(2, TRUE)) # this will never occur because the maximum would have to equal the minimum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(2))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for minimums for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(2, TRUE)) # this will never occur because the maximum would have to equal the minimum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(2))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for minimums for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(2, TRUE)) # this will never occur because the maximum would have to equal the minimum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(2))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for minimums for a truncated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3.5,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(2, TRUE)) # this will never occur because the maximum would have to equal the minimum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(2))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for minimums for a truncated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3.5,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(2, TRUE)) # this will never occur because the maximum would have to equal the minimum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(2))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})
#Maximum
test_that("test for value less than upper bound works as expected for maximums for a uniform distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = 2,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for maximums for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for maximums for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for maximums for a truncated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3.5,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for maximums for a truncated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3.5,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

#Mode

test_that("test for value less than upper bound works as expected for modes for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(3.5, TRUE)) # this will never occur because the mode would have to equal the maximum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(3.5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

test_that("test for value less than upper bound works as expected for modes for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(3.5, TRUE))# this will never occur because the mode would have to equal the maximum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(3.5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

#Arithmetic mean

test_that("test for value less than upper bound works as expected for arithmetic mean for a truncated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3.5,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(3.5, TRUE))# this will never occur because the mean would have to equal the maximum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(3.5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

#Geometric mean

test_that("test for value less than upper bound works as expected for geometric mean for a truncated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3.5,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  #expect_true(Ls_dist$paramsAreLessThanUpperBound(3.5, TRUE))# this will never occur because the mean would have to equal the maximum
  expect_false(Ls_dist$paramsAreLessThanUpperBound(3.5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

#Custom
test_that("test for value less than upper bound works as expected for maximum for a custom distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Custom",
                              custom_distribution_data = 5)

  expect_true(Ls_dist$paramsAreLessThanUpperBound(10, TRUE))
  expect_true(Ls_dist$paramsAreLessThanUpperBound(5, TRUE))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(5))
  expect_false(Ls_dist$paramsAreLessThanUpperBound(1))

})

################# paramsAreGreaterThanLowerBound #####################


test_that("test for value greater than lower bound works as expected for constants", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Constant",
                              constant = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Minimum

test_that("test for value greater than lower bound works as expected for minimums for a uniform distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = 2,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for minimums for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for minimums for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for minimums for a truncated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for minimums for a truncated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Maximum

test_that("test for value greater than lower bound works as expected for maximums for a uniform distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Uniform",
                              minimum = 2,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE)) #this will never occur because maximum would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for maximums for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE)) #this will never occur because maximum would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for maximums for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE)) #this will never occur because maximum would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for maximums for a truncated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE)) #this will never occur because maximum would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for maximums for a truncated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(5, TRUE)) #this will never occur because maximum would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(5))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Mode
test_that("test for value greater than lower bound works as expected for modes for a triangular distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Triangular",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(3, TRUE)) #this will never occur because mode would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(3))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

test_that("test for value greater than lower bound works as expected for modes for a PERT distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 2,
                              mode = 3,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(3, TRUE)) #this will never occur because mode would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(3))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Arithmetic mean
test_that("test for value greater than lower bound works as expected for arithmetic mean for a trunctated normal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Normal",
                              minimum = 2,
                              arith_mean = 3,
                              arith_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(3, TRUE)) #this will never occur because mean would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(3))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Geometric mean

test_that("test for value greater than lower bound works as expected for geometric mean for a trunctated lognormal distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Truncated Lognormal",
                              minimum = 2,
                              geom_mean = 3,
                              geom_stdev = 0.5,
                              maximum = 5)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  #expect_true(Ls_dist$paramsAreGreaterThanLowerBound(3, TRUE)) #this will never occur because mean would have to equal minimum
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(3))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})

#Custom

test_that("test for value greater than lower bound works as expected for minimum for a custom distribution", {

  Ls_dist <- JEMParamDist$new(symbol = "Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "Custom",
                              custom_distribution_data = 2)

  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(1, TRUE))
  expect_true(Ls_dist$paramsAreGreaterThanLowerBound(2, TRUE))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(2))
  expect_false(Ls_dist$paramsAreGreaterThanLowerBound(10))

})
