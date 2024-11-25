#' @title Perform basic unit conversions
#'
#' @description Performs basic parameter unit conversions related to distance,
#'   area, and temperature. Supported units are:
#'
#' * ft: feet
#' * m: meters
#' * ft2: square feet
#' * m2: square meters
#' * deg F: degrees Fahrenheit
#' * deg C: degrees Celsius
#'
#' @param inputNumber Number to be converted to a new set of units
#' @param inputUnits Units associated with the input number
#' @param outputUnits Units associated with the output number
#'
#' @returns The input number converted to the output units
#'
#' @examples
#' #Area conversion from square feet to square meters
#' paramUnitConvs(1, "ft2", "m2")
#'
#' @export

paramUnitConvs <- function(inputNumber, inputUnits, outputUnits) {

  outputNumber <- NULL

  #Length and area
  if(inputUnits == "ft" && outputUnits == "m"){
    outputNumber <- inputNumber / 3.28084
  } else if (inputUnits == "m" && outputUnits == "ft"){
    outputNumber <- inputNumber * 3.28084
  } else if (inputUnits == "ft2" && outputUnits == "m2"){
    outputNumber <- inputNumber / 3.28084 / 3.28084
  } else if (inputUnits == "m2" && outputUnits == "ft2"){
    outputNumber <- inputNumber * 3.28084 * 3.28084
  }

  #Temperature
  if (inputUnits == "deg C" && outputUnits == "deg F"){
    outputNumber <- inputNumber * 9/5 + 32
  } else if (inputUnits == "deg F" && outputUnits == "deg C"){
    outputNumber <- (inputNumber - 32) * 5/9
  }

  #catch-all for unsupported conversions
  if (is.null(outputNumber))
  {
    stop("Error: Unsupported unit conversion applied in paramUnitConvs")
  }

  return(outputNumber)
}
