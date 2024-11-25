#'@title Assign soil strata log properties
#'
#'@description Performs basic setup steps required by `runJE()` for
#'  soil strata log data. Assigns United States Department of Agriculture (USDA)
#'  Soil Conservation Service (SCS) soil type properties, sorts the soil
#'  strata layers by LayerOrder, and identifies the upper and lower depth of
#'  each layer.
#'
#'@param boring_log_dfx Dataframe of soil strata log information containing the
#'  following required fields:
#'
#'  * SoilType: One of 12 USDA SCS soil types. Supported values are "Clay",
#'  "Clay Loam", "Loam", "Loamy Sand", "Sand", "Sandy Clay", "Sandy Clay Loam",
#'  "Sandy Loam", "Silt", "Silt Loam", "Silty Clay", and "Silty Clay Loam".
#'  * LayerOrder: Layer order from ground surface. Layer 1 is the surface layer,
#'  layer 2 is beneath layer 1, and so on.
#'  * Thickness: Layer thickness. The
#'  package assumes Thickness values are entered in meters when this function is
#'  used.
#'
#'  Note that the LogID and Units fields associated with soil strata data in the
#'  import template are not used within this function
#'
#'@returns The same data frame that was entered as a function input, but with
#'  the following fields added:
#'
#'  * SCS: ID number associated with the layer's SCS soil type in the data table
#'  of standard soil type properties
#'  * nS: Layer total porosity (dimensionless)
#'  * nwS: Layer water-filled porosity (dimensionless)
#'  * rhoS: Layer bulk
#'  density (grams per cubic centimeter, or \ifelse{html}{\out{g/cm<sup>2</sup>}}{\eqn{g/cm^2}})
#'  * ncz: Capillary zone total
#'  porosity (dimensionless)
#'  * nwcz: Capillary zone water-filled porosity
#'  (dimensionless)
#'  * hcz: Capillary fringe height (meters, or m)
#'  * upper_depth: Layer upper depth, in the same units as the Thickness field values
#'  * lower_depth: Layer lower depth, in the same units as the Thickness field
#'  values
#'
#'@examples
#'# Get example soil strata log data
#'soil_strata_log_dfx <- det_jem_sim_example_data[[4]]
#'
#'# Run the boring log setup function
#'boringLogSetup(soil_strata_log_dfx)
#'
#'@export
#'

boringLogSetup <- function(boring_log_dfx){

  #Get boring log properties
  SCS_dfx <- SCS_soil

  #Remove any soil layers with a "Not Present" soil type
  boring_log_dfx <- boring_log_dfx[boring_log_dfx$SoilType != "Not Present", ]

  #Get soil type for boring log inputs
  boring_log_dfx <- boring_log_dfx %>% rowwise() %>% mutate(SCS = getSCSTypeNumber(.data$SoilType))

  #based on the SCS number, assign soil properties nSA, nwSA, rhoSA, ncz, nwcz, and hcz to the boring log data frame.
  boring_log_dfx <- mutate(boring_log_dfx,
                           nS = SCS_dfx$n[.data$SCS],
                           nwS = SCS_dfx$nw[.data$SCS],
                           rhoS =  SCS_dfx$rho[.data$SCS],
                           ncz = SCS_dfx$n[.data$SCS],
                           nwcz = SCS_dfx$ncz[.data$SCS],
                           hcz = SCS_dfx$hcz[.data$SCS]
  )

  #Order by the layer order
  boring_log_dfx <- boring_log_dfx[order(boring_log_dfx$LayerOrder), ]

  total_depth <- 0

  boring_log_dfx$upper_depth <- 0
  boring_log_dfx$lower_depth <- 0

  #Establish the upper and lower depth of the layer
  for (layer_i in 1:nrow(boring_log_dfx))
  {
    boring_log_dfx$upper_depth[layer_i] <- total_depth
    total_depth <- total_depth + boring_log_dfx$Thickness[layer_i]
    boring_log_dfx$lower_depth[layer_i] <- total_depth
  }

  #Round all depths to the nearest millimeter to deal with comparison issues in floating point arithmetic later on
  boring_log_dfx <- boring_log_dfx %>% mutate_at(vars(.data$Thickness, .data$upper_depth, .data$lower_depth), list(~ round(., 3)))

  #Recalculate thicknesses based on depths to address discrepancies caused by rounding
  boring_log_dfx$Thickness <- boring_log_dfx$lower_depth - boring_log_dfx$upper_depth

  return(boring_log_dfx)

}
