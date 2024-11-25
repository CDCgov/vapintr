
# water tests

test_that("Water conversion from mg/L to ug/L", {

  output_concentration <- concUnitConvs(1, "mg/L", "ug/L", "Water")
  expect_equal(output_concentration, 1000)
  })

test_that("Water conversion from ppm to ug/L", {

  output_concentration <- concUnitConvs(1, "ppm", "ug/L", "Water")
  expect_equal(output_concentration, 1000)
})

test_that("Water conversion from mg/m3 to ug/L", {

  output_concentration <- concUnitConvs(1, "mg/m3", "ug/L", "Water")
  expect_equal(output_concentration, 1)
})


test_that("Water conversion from ppb to ug/L", {

  output_concentration <- concUnitConvs(1, "ppb", "ug/L", "Water")
  expect_equal(output_concentration, 1)
})

test_that("Water conversion from ug/L to ug/L", {

  output_concentration <- concUnitConvs(1, "ug/L", "ug/L", "Water")
  expect_equal(output_concentration, 1)
})

test_that("Water conversion from ug/m3 to ug/L", {

  output_concentration <- concUnitConvs(1, "ug/m3", "ug/L", "Water")
  expect_equal(output_concentration, 1/1000)
})

# air tests - output to ug/m3

test_that("Air conversion from mg/L to ug/m3", {

  output_concentration <- concUnitConvs(1, "mg/L", "ug/m3", "Air")
  expect_equal(output_concentration, 1000000)
})

test_that("Air conversion from mg/m3 to ug/m3", {

  output_concentration <- concUnitConvs(1, "mg/m3", "ug/m3", "Air")
  expect_equal(output_concentration, 1000)
})

test_that("Air conversion from ug/L to ug/m3", {

  output_concentration <- concUnitConvs(1, "ug/L", "ug/m3", "Air")
  expect_equal(output_concentration, 1000)
})

test_that("Air conversion from ug/m3 to ug/m3", {

  output_concentration <- concUnitConvs(1, "ug/m3", "ug/m3", "Air")
  expect_equal(output_concentration, 1)
})

test_that("Air conversion from ppm to ug/m3", {

  output_concentration <- concUnitConvs(1, "ppm", "ug/m3", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 6778.367298)
})

test_that("Air conversion from ppb to ug/m3", {

  output_concentration <- concUnitConvs(1, "ppb", "ug/m3", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 6.778367298)
})

# air tests - output to ppb

test_that("Air conversion from mg/L to ppb", {

  output_concentration <- concUnitConvs(1, "mg/L", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 147528.1518)
})

test_that("Air conversion from mg/m3 to ppb", {

  output_concentration <- concUnitConvs(1, "mg/m3", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 147.5281518)
})

test_that("Air conversion from ug/L to ppb", {

  output_concentration <- concUnitConvs(1, "ug/L", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 147.5281518)
})

test_that("Air conversion from ug/m3 to ppb", {

  output_concentration <- concUnitConvs(1, "ug/m3", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 0.147528152)
})

test_that("Air conversion from ppm to ppb", {

  output_concentration <- concUnitConvs(1, "ppm", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 1000)
})

test_that("Air conversion from ppb to ppb", {

  output_concentration <- concUnitConvs(1, "ppb", "ppb", "Air", Ts = 25, MW = 165.83)
  expect_equal(output_concentration, 1)
})

test_that("Catch-all for unsupported tests", {

  errorMessage <- "Error: Unsupported unit conversion applied in concUnitConvs"
  expect_error(concUnitConvs(1, "ng/L", "ppb", "Air", Ts = 25, MW = 165.83), errorMessage, fixed = TRUE)

})
