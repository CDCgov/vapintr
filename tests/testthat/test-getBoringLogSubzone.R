test_that("subzone containing three layers is correct", {

  boring_log_dfx <- data.frame(
    LayerOrder = c("1", "2", "3", "4", "5"),
    SoilType = c("Sand", "Silt", "Silty Clay", "Clay", "Clay Loam"),
    Thickness = c(2.0, 2.0, 2.0, 2.0, 2.0)
  )

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  subzone_upper_depth_bound <- 3
  subzone_lower_depth_bound <- 7

  subzone_dfx <- getBoringLogSubzone(boring_log_dfx, subzone_upper_depth_bound, subzone_lower_depth_bound)
  nSubzoneLayers <- 3

  expect_equal(nrow(subzone_dfx), nSubzoneLayers)
  expect_equal(subzone_dfx$upper_depth[1], subzone_upper_depth_bound)
  expect_equal(subzone_dfx$Thickness[1], subzone_dfx$lower_depth[1] - subzone_upper_depth_bound)
  expect_equal(subzone_dfx$lower_depth[nrow(subzone_dfx)], subzone_lower_depth_bound)
  expect_equal(subzone_dfx$Thickness[nrow(subzone_dfx)], subzone_lower_depth_bound - subzone_dfx$upper_depth[nrow(subzone_dfx)])

})

test_that("subzone containing parts of two layers is correct", {

  boring_log_dfx <- data.frame(
    LayerOrder = c("1", "2", "3", "4", "5"),
    SoilType = c("Sand", "Silt", "Silty Clay", "Clay", "Clay Loam"),
    Thickness = c(2.0, 2.0, 2.0, 2.0, 2.0)
  )

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  subzone_upper_depth_bound <- 3
  subzone_lower_depth_bound <- 5

  subzone_dfx <- getBoringLogSubzone(boring_log_dfx, subzone_upper_depth_bound, subzone_lower_depth_bound)
  nSubzoneLayers <- 2

  expect_equal(nrow(subzone_dfx), nSubzoneLayers)
  expect_equal(subzone_dfx$upper_depth[1], subzone_upper_depth_bound)
  expect_equal(subzone_dfx$Thickness[1], subzone_dfx$lower_depth[1] - subzone_upper_depth_bound)
  expect_equal(subzone_dfx$lower_depth[nrow(subzone_dfx)], subzone_lower_depth_bound)
  expect_equal(subzone_dfx$Thickness[nrow(subzone_dfx)], subzone_lower_depth_bound - subzone_dfx$upper_depth[nrow(subzone_dfx)])

})

test_that("subzone within one layer is correct", {

  boring_log_dfx <- data.frame(
    LayerOrder = c("1", "2", "3", "4", "5"),
    SoilType = c("Sand", "Silt", "Silty Clay", "Clay", "Clay Loam"),
    Thickness = c(2.0, 2.0, 2.0, 2.0, 2.0)
  )

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  subzone_upper_depth_bound <- 6.5
  subzone_lower_depth_bound <- 7.5

  subzone_dfx <- getBoringLogSubzone(boring_log_dfx, subzone_upper_depth_bound, subzone_lower_depth_bound)
  nSubzoneLayers <- 1

  expect_equal(nrow(subzone_dfx), nSubzoneLayers)
  expect_equal(subzone_dfx$upper_depth[1], subzone_upper_depth_bound)
  expect_equal(subzone_dfx$Thickness[1], subzone_dfx$lower_depth[1] - subzone_upper_depth_bound)
  expect_equal(subzone_dfx$lower_depth[nrow(subzone_dfx)], subzone_lower_depth_bound)
  expect_equal(subzone_dfx$Thickness[nrow(subzone_dfx)], subzone_lower_depth_bound - subzone_dfx$upper_depth[nrow(subzone_dfx)])

})

test_that("subzone on layer boundaries is correct", {

  boring_log_dfx <- data.frame(
    LayerOrder = c("1", "2", "3", "4", "5"),
    SoilType = c("Sand", "Silt", "Silty Clay", "Clay", "Clay Loam"),
    Thickness = c(2.0, 2.0, 2.0, 2.0, 2.0)
  )

  boring_log_dfx <- boringLogSetup(boring_log_dfx)

  subzone_upper_depth_bound <- 4
  subzone_lower_depth_bound <- 8

  subzone_dfx <- getBoringLogSubzone(boring_log_dfx, subzone_upper_depth_bound, subzone_lower_depth_bound)
  nSubzoneLayers <- 2

  expect_equal(nrow(subzone_dfx), nSubzoneLayers)
  expect_equal(subzone_dfx$upper_depth[1], subzone_upper_depth_bound)
  expect_equal(subzone_dfx$Thickness[1], subzone_dfx$lower_depth[1] - subzone_upper_depth_bound)
  expect_equal(subzone_dfx$lower_depth[nrow(subzone_dfx)], subzone_lower_depth_bound)
  expect_equal(subzone_dfx$Thickness[nrow(subzone_dfx)], subzone_lower_depth_bound - subzone_dfx$upper_depth[nrow(subzone_dfx)])

})

