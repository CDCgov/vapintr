#' @title Calculate Henry's Law constant at system temperature (Hs)
#'
#' @description Calculates Hs using default chemical properties at the
#'   subsurface system temperature
#'
#' @param Chem Chemical name. Enter a value for this parameter or for the CAS
#'   parameter to identify the chemical. If values are entered for both
#'   parameters, they must correspond to the same chemical. Only the chemical
#'   names in `chem_data$Chemical` are supported.
#'
#' @param CAS Chemical Abstract Service Registry Number. Enter a value for this
#'   parameter or for the Chem parameter to identify the chemical. If values are
#'   entered for both parameters, they must correspond to the same chemical.
#'   Only the CAS numbers in `chem_data$CAS` are supported.
#'
#' @param Ts Source temperature in degrees Celsius ( \ifelse{html}{\out{&degC}}{\eqn{^{\circ}C}}). Enter a single
#'   value for deterministic simulations or an mcnode object of sampled
#'   temperatures for a stochastic simulation.
#'
#' @param simulation_type Simulation type. Enter "DET" for a deterministic
#'   simulation and "MC" for a stochastic simulation.
#'
#' @returns Returns a list with two elements. The first element is the
#'   calculated value(s) of Hs and the second is the text of any warnings
#'   generated. The calculated Hs value is a single number in a deterministic
#'   simulation and an mcnode object in a stochastic simulation. Hs is
#'   dimensionless and represents the equilibrium gas concentration
#'   (Cgas) divided by the aqueous concentration (Caqueous) at
#'   the system temperature.
#'
#' @examples
#' #Calculate Hs at a fixed temperature
#' calcHs(Chem = "Tetrachloroethylene", Ts = 23, simulation_type = "DET")
#'
#' #Calculate Hs over a distribution of temperatures
#' library(mc2d)
#' calcHs(CAS = "79-01-6", Ts = mc2d::mcstoc(type = "U", min = 20, max = 25), simulation_type = "MC")
#'
#' @export


calcHs <- function(Chem = NA, CAS = NA, Ts, simulation_type) {

  Hs <- NA

  chemical_data <-
    getChemicalProperties(Chemical = Chem, CAS = CAS)

  if (any(is.na(chemical_data$Hc25)) == TRUE) {
    stop("Error: Henry's Law constant at standard temperature is not available for this compound.")
  }

  #Hr and chemical_data$H_prime_25 are functionally equivalent
  #but they end up being slightly different because of rounding in the conversion factors used.
  #But using Hr here more accurately reflects what is done in the JEM spreadsheet.
  Hr <- HrCalc(chemical_data$Hc25)

  warningMessageFlag <- FALSE

  if (is.na(chemical_data$DH_vb) || is.na(chemical_data$Tcrit) || is.na(chemical_data$Tboil)) {
    warningMessageFlag <- TRUE
  } else {
    Hs <-  try(exp(-(chemical_data$DH_vb * ((1 - (Ts + 273) / chemical_data$Tcrit) / (1 - chemical_data$Tboil / chemical_data$Tcrit)
    ) ^ (
      ifelse(chemical_data$Tboil / chemical_data$Tcrit < 0.57,  0.3,
             ifelse(chemical_data$Tboil / chemical_data$Tcrit > 0.71, 0.41,
                    0.74 * (chemical_data$Tboil / chemical_data$Tcrit) - 0.116
             )
      )
    ) / 1.9872) * (1 / (Ts + 273) - 1 / 298)) * chemical_data$Hc25 / (0.000082057 * (Ts + 273)),)

    if (is(Hs,"try-error")) {
      warningMessageFlag <- TRUE
      Hs <- if (simulation_type == "DET") {Hr} else {mc2d::mcdata(Hr, "U")}
    }
  }

  if (any(is.na(Hs)) == TRUE) {
    warningMessageFlag <- TRUE
    Hs <- if (simulation_type == "DET") {Hr} else {mc2d::mcdata(Hr, "U")}
  }

  warningMessage <- NA

  if(warningMessageFlag){
    warningMessage <- "HLC conversion to system temperature not available for this compound"
    warning(warningMessage)
  }

  return(list(Hs, warningMessage))
}
