#' @title Convert concentrations to new units
#'
#' @description Performs unit conversions for units commonly
#'   associated with contaminant concentrations in air and water. Supported units are:
#'
#'   * milligrams per liter (mg/L)
#'   * milligrams per cubic meter (mg/m3)
#'   * micrograms per liter (ug/L)
#'   * micrograms per cubic meter (ug/m3)
#'   * parts per million (ppm)
#'   * parts per billion (ppb)
#'
#' @param inputConc Concentration to be converted to a new set of units
#' @param inputUnits Units associated with the input concentration. Must be one
#'   of the following: "mg/L", "mg/m3", "ug/L", "ug/m3", "ppm", or "ppb".
#' @param outputUnits Units associated with the output concentration. Must be
#'   one of the following: "mg/L", "mg/m3", "ug/L", "ug/m3", "ppm", or "ppb".
#' @param medium Medium for the input concentration. Can be either "Water" or
#'   "Air".
#' @param Ts Temperature of the medium. Required for air unit conversions from a
#'   "parts per" system to a "mass/volume" system and vice-versa. Assumed to be
#'   input in degrees Celsius.
#' @param MW Chemical molecular weight. Required for air unit conversions from a
#'   "parts per" system to a "mass/volume" system and vice-versa. Assumed to be
#'   input in grams/mole.
#'
#' @returns Returns the input concentration converted to the output units
#'
#' @examples
#' #Convert a water concentration from ppm to ug/L
#' concUnitConvs(1, "ppm", "ug/L", "Water")
#'
#' #Convert an air concentration from ppb to ug/m3
#' concUnitConvs(1, "ppb", "ug/m3", "Air", Ts = 25, MW = 165.83)
#'
#' @export

concUnitConvs <- function(inputConc, inputUnits, outputUnits, medium, Ts = NA, MW = NA) {

  outputConc <- NULL

  #Supported input units are: mg/L, mg/m3, ug/L, ug/m3, ppm, ppb
  #Two unit systems: "mass/volume" and "parts per"
  #Complete conversions within the same system

  #Groundwater unit conversions (independent of temperature)
  #Program only supports water conversions to ug/L
  if (medium == "Water"){
    if(outputUnits == "ug/L")
    {
      if(inputUnits %in% c("mg/L", "ppm")){
        outputConc <- inputConc * 1000
      } else if (inputUnits %in% c("mg/m3", "ppb", "ug/L")){
        outputConc <- inputConc * 1
      } else if (inputUnits == "ug/m3"){
        outputConc <- inputConc * 1/1000
      }
    }
  }

  #Soil gas and indoor air unit conversions ()
  #Program only supports conversion to ug/m3 and ppb
  if (medium == "Air"){
    if (outputUnits == "ug/m3")
    {
      if (inputUnits == "mg/L") {
        outputConc <- inputConc * 1000000
      } else if (inputUnits %in% c("mg/m3", "ug/L")) {
        outputConc <- inputConc * 1000
      } else if (inputUnits %in% c("ug/m3")) {
        outputConc <- inputConc * 1
      } else if (inputUnits == "ppm") {
        outputConc <- (inputConc * 1000) * 12.187 * MW / (273.15+Ts)
      } else if (inputUnits == "ppb") {
        outputConc <- (inputConc) * 12.187 * MW / (273.15+Ts)
      }
    }
    else if (outputUnits == "ppb")
    {
      if (inputUnits == "mg/L") {
        outputConc <- (inputConc * 1000000) * (273.15+Ts) / (12.187 * MW)
      } else if (inputUnits %in% c("mg/m3", "ug/L")) {
        outputConc <- (inputConc * 1000) * (273.15+Ts) / (12.187 * MW)
      } else if (inputUnits == "ug/m3") {
        outputConc <- (inputConc) * (273.15+Ts) / (12.187 * MW)
      } else if (inputUnits == "ppm") {
        outputConc <- inputConc * 1000
      } else if (inputUnits == "ppb") {
        outputConc <- inputConc * 1
      }
    }
  }

  #catch-all for unsupported conversions
  if (is.null(outputConc))
  {
    stop("Error: Unsupported unit conversion applied in concUnitConvs")
  }

  return(outputConc)
}
