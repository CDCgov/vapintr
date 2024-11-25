test_that("one layer example - hcz determined by single layer", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1),
    SoilType = c("Silty Clay"),
    Thickness = c(10)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silty Clay"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("two layer example - hcz determined by lower layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2),
    SoilType = c("Sand", "Silty Clay"),
    Thickness = c(5.0, 5.0)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silty Clay"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("two layer example - hcz determined by lower layer thickness", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2),
    SoilType = c("Sand", "Silty Clay"),
    Thickness = c(9.40, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- boring_log_dfx$Thickness[boring_log_dfx$SoilType == "Silty Clay"]

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("two layer example - hcz determined by upper layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2),
    SoilType = c("Sand", "Silty Clay"),
    Thickness = c(9.90, 0.10)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Sand"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("three layer example - hcz determined by middle layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3),
    SoilType = c("Silt Loam", "Clay", "Silty Clay"),
    Thickness = c(9.00, 0.50, 0.50)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Clay"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("three layer example - hcz determined by lower layer thickness", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3),
    SoilType = c("Silt Loam", "Sandy Clay Loam", "Silty Clay"),
    Thickness = c(9.30, 0.10, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- boring_log_dfx$Thickness[boring_log_dfx$SoilType == "Silty Clay"]

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("three layer example - hcz determined by middle layer thickness", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Silt", "Silty Clay"),
    Thickness = c(9.30, 0.10, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- Ls - boring_log_dfx$upper_depth[boring_log_dfx$SoilType == "Silt"]

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("three layer example - hcz determined by upper layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3),
    SoilType = c("Silt", "Sandy Clay Loam", "Silty Clay"),
    Thickness = c(9.30, 0.10, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silt"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("four layer example - hcz determined by second layer from bottom thickness", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Sand", "Clay Loam", "Sandy Clay Loam",  "Silty Clay"),
    Thickness = c(9.0, 0.20, 0.20, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- Ls - boring_log_dfx$upper_depth[boring_log_dfx$SoilType == "Silty Clay"]

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("four layer example - hcz determined by second layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Sand", "Silt", "Silt Loam",  "Silty Clay"),
    Thickness = c(9.0, 0.20, 0.20, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silt Loam"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("four layer example - hcz determined by third layer from bottom thickness", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Sand", "Silt", "Sandy Clay Loam",  "Silty Clay"),
    Thickness = c(9.0, 0.20, 0.20, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- Ls - boring_log_dfx$upper_depth[boring_log_dfx$SoilType == "Silt"]

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("four layer example - hcz determined by third layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Sand", "Silt", "Sandy Clay Loam",  "Silty Clay"),
    Thickness = c(7.2, 2.0, 0.20, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silt"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})

test_that("four layer example - hcz determined by fourth layer hcz", {

  boring_log_dfx <- data.frame(
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Silty Clay", "Silt", "Silty Clay Loam",  "Silt Loam"),
    Thickness = c(9.0, 0.2, 0.20, 0.60)
  )

  Ls <- 10

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  SCS_dfx <- vapintr:::SCS_soil
  target_hcz <- SCS_dfx$hcz[SCS_dfx$SCS_Type == "Silty Clay"]/100

  expect_equal(hczCalc(boring_log_dfx, Ls), target_hcz)

})


