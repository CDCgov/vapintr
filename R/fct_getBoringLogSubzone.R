#' @title Subset a data frame of soil strata layers
#'
#' @description Subsets a data frame of soil strata layers into just the layers
#'   contained within a subzone defined by an upper and lower depth.
#'
#' @param boring_log_dfx A data frame representing a soil strata log. The data
#'   frame must have the following fields: "Thickness", "upper_depth", and
#'   "lower_depth". Data in all three fields should be in meters. For
#'   soil strata data imported from vapintr's data import template, the "upper_depth" and
#'   "lower_depth" fields can be added using `boringLogSetup()`.
#' @param subzone_upper_depth_bound The upper depth of the subzone in meters. This
#'   is the subzone depth closest to the ground surface.
#' @param subzone_lower_depth_bound The lower depth of the subzone in meters. This
#'   is the subzone depth farthest from the ground surface.
#'
#' @returns A data frame of soil layers within the subzone. Any soil layers in
#'   boring_log_dfx that were not in the subzone are removed from the output
#'   data frame, and any layers that were partially in the subzone have had
#'   their thickness, upper_depth and/or lower_depth adjusted to reflect just
#'   the portion of the layer within the subzone.
#'
#' @examples
#' #Get example soil strata log data
#' soil_strata_log_data <- det_jem_sim_example_data[[4]]
#'
#' #Assign properties to each soil layer
#' soil_strata_log_data <- boringLogSetup(soil_strata_log_data)
#'
#' #Get the layers in a subzone of 3 to 5 meters beneath ground surface
#' getBoringLogSubzone(soil_strata_log_data, 3, 5)
#'
#' @export
#'

getBoringLogSubzone <- function(boring_log_dfx, subzone_upper_depth_bound, subzone_lower_depth_bound) {

  #Build a dataframe with just the boring log layers in the subzone specified by the upper and lower depth

  #Initialize the subzone as the entire boring log
  subzone_dfx <- boring_log_dfx
  subzone_dfx$part_of_subzone <- FALSE

  #Flag the layers in the subzone zone
  subzone_dfx$part_of_subzone <-
    #upper bound of layer is in the subzone
    (subzone_dfx$upper_depth >= subzone_upper_depth_bound & subzone_lower_depth_bound >= subzone_dfx$upper_depth) |
    #lower bound of layer in in the subzone
    (subzone_dfx$lower_depth >= subzone_upper_depth_bound & subzone_lower_depth_bound >= subzone_dfx$lower_depth) |
    #subzone is entirely within the layer
    (subzone_upper_depth_bound >= subzone_dfx$upper_depth & subzone_dfx$lower_depth >= subzone_lower_depth_bound) |
    #layer is entirely within the subzone
    (subzone_dfx$upper_depth >= subzone_upper_depth_bound & subzone_lower_depth_bound >= subzone_dfx$lower_depth)

  #Keep only the layers that are part of the subzone
  subzone_dfx <- subzone_dfx[subzone_dfx$part_of_subzone == TRUE, ]

  #Set the thicknesses and depth properties to reflect the modified subzone boundaries
  depthCondition <- subzone_dfx$upper_depth <= subzone_upper_depth_bound
  subzone_dfx$Thickness[depthCondition] <- subzone_dfx$lower_depth[depthCondition] - subzone_upper_depth_bound
  subzone_dfx$upper_depth[depthCondition] <- subzone_upper_depth_bound

  depthCondition <- subzone_dfx$lower_depth >= subzone_lower_depth_bound
  subzone_dfx$Thickness[depthCondition] <- subzone_lower_depth_bound - subzone_dfx$upper_depth[depthCondition]
  subzone_dfx$lower_depth[depthCondition] <- subzone_lower_depth_bound

  #Keep only the layers with thickness greater than zero
  subzone_dfx <- subzone_dfx[subzone_dfx$Thickness > 0, ]

  return(subzone_dfx)

}
