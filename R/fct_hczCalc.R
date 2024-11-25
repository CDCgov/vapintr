#' @title Calculate the height of the capillary zone
#'
#' @description Calculates the height of the capillary zone in meters (m).
#'   The function implements the following algorithm, which accounts for
#'   capillary rise across multiple soil layers.
#'
#'   1. Beginning at the water table, first identify the capillary rise
#'   associated with the soil type of each layer above the water table. The
#'   first layer is always included in the capillary zone. If the thickness of
#'   the first layer is greater than the capillary rise of the first layer, then
#'   stop.
#'
#'   2.	If the capillary rise of the second layer is greater than the distance
#'   from the base of the layer to the water table, the second layer also
#'   contributes to capillary rise and the capillary rise is that of the second
#'   layer. If the capillary rise of the second layer does not meet this
#'   condition, the layer may still be part of the capillary zone depending on
#'   the capillary rise in the third layer (if present). If the total thickness
#'   of the first two layers is greater than the overall capillary rise, then
#'   stop.
#'
#'   3.	If a third layer is present and its capillary rise is greater than the
#'   distance from the base of the layer to the water table, then the capillary
#'   rise is that of the third layer. If the total thickness of the first three
#'   layers is greater than the overall capillary rise, then stop.
#'
#'   4.	Continue testing each layer above the water table until a stop point is
#'   reached or until the maximum capillary rise (192.31 centimeters for silty
#'   clay) is achieved.
#'
#' @param boring_log_dfx A data frame of soil strata data beneath the building.
#'   The required fields for this data frame are added to data imported from the
#'   vapintr data import template in `boringLogSetup()`.
#'
#' @param Ls The depth to the water table (\ifelse{html}{\out{m}}{\eqn{m}}).
#'
#' @return The height of the capillary zone (\ifelse{html}{\out{m}}{\eqn{m}}).
#'
#' @examples
#' #Get example soil strata log data
#' soil_strata_log_data <- det_jem_sim_example_data[[4]]
#'
#' #Assign properties to each soil layer
#' soil_strata_log_data <- boringLogSetup(soil_strata_log_data)
#'
#' #Get the height of the capillary zone when the water table is 10 meters deep
#' #for the example soil strata log data
#' hczCalc(soil_strata_log_data, 10)
#' @export
#'

hczCalc <- function(boring_log_dfx, Ls) {

  hcz <- NA

  #Identify layer containing Ls based on sum of thicknesses
  for (layer_i in 1:nrow(boring_log_dfx))
  {

      #Second condition captures the case when the depth to water is right at the bottom of the log
      if ((Ls >= boring_log_dfx$upper_depth[layer_i] && Ls < boring_log_dfx$lower_depth[layer_i]) ||
      (layer_i == nrow(boring_log_dfx) && Ls == boring_log_dfx$lower_depth[layer_i]))
      {
        water_table_layer_index <- layer_i
        break
      }
  }

  #Depth to Water = Ls, which is comprised of an unsaturated zone and a capillary fringe
  #Let hcz represent the capillary zone height and huz represent the unsaturated zone height
  #The method below implements Jim Weaver's algorithm for calculating the capillary zone height
  #See the function description for a description of the algorithm

  available_thickness <- Ls - boring_log_dfx$upper_depth[water_table_layer_index]
  layer_hcz <- boring_log_dfx$hcz[water_table_layer_index]/100 #Divide by 100 to convert from cm to m
  current_layer_index <- water_table_layer_index
  current_hcz <- layer_hcz
  max_hcz <- current_hcz
  current_hcz_based_on_distance_to_layer_from_water_table <- FALSE

  while(available_thickness < max_hcz && current_layer_index > 1)
  {
    current_layer_index <- current_layer_index - 1
    distance_to_layer_from_water_table <- Ls - boring_log_dfx$lower_depth[current_layer_index]
    layer_hcz <- boring_log_dfx$hcz[current_layer_index]/100

    max_hcz <- max(max_hcz, layer_hcz)

    available_thickness <- available_thickness + boring_log_dfx$Thickness[current_layer_index]

    if (layer_hcz >= distance_to_layer_from_water_table){
      current_hcz <- layer_hcz
      current_hcz_based_on_distance_to_layer_from_water_table <- FALSE

      if (current_hcz <= available_thickness)
      {
        break
      }
    } else if (!current_hcz_based_on_distance_to_layer_from_water_table) {
      current_hcz <- distance_to_layer_from_water_table
      current_hcz_based_on_distance_to_layer_from_water_table <- TRUE
    }
  }

  hcz <- current_hcz

  return(hcz)
}
