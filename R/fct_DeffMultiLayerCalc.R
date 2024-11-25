#' @title Calculate the effective diffusivity across multiple soil layers
#'
#' @description Calculates a weighted effective diffusivity for a contaminant
#'   across multiple soil layers. The weights are determined by the thickness of
#'   each layer.
#'
#' @param layer_dfx A data frame of soil layers. Required fields for this data
#'   frame are assigned in `boringLogSetup()`.
#'
#' @param Dair Contaminant diffusion coefficient in air (square centimeters per
#'   second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param Dwater Contaminant diffusion coefficient in water (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param Hs Henry's Law constant at system temperature (dimensionless)
#'
#' @param is_capillary_zone A true/false flag identifying whether the layers are
#'   in the capillary zone
#'
#' @return The effective diffusivity across all the entered layers in
#'   \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}}.
#'
#' @examples
#' #Get example soil strata log data
#' soil_strata_log_data <- det_jem_sim_example_data[[4]]
#'
#' #Assign properties to each soil layer
#' soil_strata_log_data <- boringLogSetup(soil_strata_log_data)
#'
#' #Set example values for chemical properties (tetrachloroethylene)
#' Dair <- 0.0504664 #cm2/sec
#' Dwater <- 9.4551e-06 #cm2/sec
#' Hs <- 0.0177 #dimensionless
#'
#' #Calculate the effective diffusivity
#' DeffMultiLayerCalc(soil_strata_log_data, Dair, Dwater, Hs, FALSE)
#'
#' @export

DeffMultiLayerCalc = function(layer_dfx, Dair, Dwater, Hs, is_capillary_zone){

  #Calculate diffusivity using layer-specific equations
  #Can't do this as part of the boring log setup because properties here are chemical and temperature dependent
  deff_lx <- lapply(seq_along(layer_dfx$Thickness), function(i){
      DeffCalc(Dair = Dair, Dwater = Dwater,
                   nS = if(is_capillary_zone) {layer_dfx$ncz[i]} else {layer_dfx$nS[i]},
                   nwS = if(is_capillary_zone) {layer_dfx$nwcz[i]} else {layer_dfx$nwS[i]},
                   Hs = Hs)
  })

  #Convert thicknesses to a list
  thicknesses <- as.list(layer_dfx$Thickness)

  #Get value to sum in the denominator
  Li_deff <- lapply(seq_along(thicknesses), function(i){
    (thicknesses[[i]])/deff_lx[[i]]
  })

  DeffLayer <- Reduce("+", thicknesses)/Reduce("+", Li_deff)

  return(DeffLayer)
}

