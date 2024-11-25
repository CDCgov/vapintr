#' Soil Conservation Service soil property data
#'
#' @description  A data frame of properties for the 12 Soil Conservation Service
#'   (SCS) soil types. All properties were imported from version 6.0 of USEPA's
#'   Johnson and Ettinger model spreadsheet.
#'
#' @format A data frame with the following fields:
#'   \describe{
#'   \item{SCS_Type}{SCS soil type name}
#'   \item{n}{total porosity (dimensionless)}
#'   \item{nw}{water-filled porosity (dimensionless)}
#'   \item{rho}{bulk density (grams per cubic centimeter, or \ifelse{html}{\out{g/cm<sup>2</sup>}}{\eqn{g/cm^2}})}
#'   \item{ncz}{capillary zone water filled porosity (dimensionless)}
#'   \item{hcz}{capillary zone height (centimeters, or \ifelse{html}{\out{cm}}{\eqn{cm}})}
#'   }
"SCS_soil"

#' Chemical property data
#'
#' @description A data frame of chemical properties for the chemicals supported within
#'   vapintr. All properties were imported from version 6.0 of USEPA's Johnson
#'   and Ettinger model spreadsheet.
#'
#' @format A data frame with the following fields:
#'   \describe{
#'   \item{Chemical}{chemical name}
#'   \item{CAS}{Chemical Abstract Service Registry Number}
#'   \item{MW}{molecular weight (grams per mole, or \ifelse{html}{\out{&microg/mol}}{\eqn{g/mol}})}
#'   \item{Vc}{saturated vapor concentration (micrograms per cubic meter, or \ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})}
#'   \item{S}{pure component water solubility (milligrams per liter, or \ifelse{html}{\out{mg/L}}{\eqn{mg/L}})}
#'   \item{Hc25}{Henry's Law constant at 25 degrees Celsius (atmospheres-cubic
#'   meters per mole, or \ifelse{html}{\out{atm-m<sup>3</sup>/L}}{\eqn{atm-m^3/mol}})}
#'   \item{H_prime_25}{dimensionless Henry's Law constant at 25 degrees
#'   Celsius (dimensionless, gas concentration divided by aqueous concentration)}
#'   \item{Da}{diffusivity in air (square centimeters per second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})}
#'   \item{Dw}{diffusivity in water (square centimeters per second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})}
#'   \item{Tboil}{normal boiling point (Kelvin, or K)}
#'   \item{Tcrit}{critical temperature (Kelvin, or K)}
#'   \item{DH_vb}{enthalpy of vaporization at the normal boiling point
#'   (calories per mole, or \ifelse{html}{\out{cal/mol}}{\eqn{cal/mol}})}
#'   \item{Pet_HC_Flag}{flag indicating if the chemical is a petroleum hydrocarbon}
#'   }
"chem_data"

#' Deterministic simulation data for running function examples
#'
#' @description A list of 6 elements used for deterministic simulation function
#'   examples. The data are the same as those in the import file described
#'   in the `Deterministic Simulations` vignette. The first 5 output elements
#'   correspond with the following inputs to `runJE()`. The 6th output is an
#'   empty data frame of reference air concentration data.
#'
#'   1. contaminant_data
#'   2. building_data
#'   3. vadose_zone_data
#'   4. strata_logs_data
#'   5. settings_data
#'
"det_jem_sim_example_data"

#' Stochastic simulation data for running function examples
#'
#' @description A list of 6 elements used for stochastic simulation function
#'   examples. The first element contains a table of concentration data that
#'   must be converted to a `JEMParamDist` object using `getInputConc()` before
#'   entering it in `runJE()`. The following function call converts the
#'   concentration data into the form required by the contaminant_data parameter
#'   in `runJE()`:
#'
#'   `getInputConc(stoc_jem_sim_example_data[[1]],
#'   "Trichloroethylene", "Groundwater", "MC")`.
#'
#'   The next four list elements can be input directly into `runJE()` and
#'   correspond with the following `runJE()` inputs. The 6th element is a data
#'   frame of reference air concentration data which can be loaded into result
#'   figures but is not required for `runJE()`.
#'
#'   2. building_data
#'   3. vadose_zone_data
#'   4. strata_logs_data
#'   5. settings_data
#'
"stoc_jem_sim_example_data"
