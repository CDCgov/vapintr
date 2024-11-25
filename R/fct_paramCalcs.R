############################### Deff #################################

#' @title Calculate the effective diffusion coefficient (Deff)
#'
#' @description Calculates the chemical effective diffusion coefficient in one
#'   soil layer, in square centimeters per second (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param Dair Diffusion coefficient in air (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param Dwater Diffusion coefficient in water (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param nS Stratum total porosity (unitless)
#'
#' @param nwS Stratum water filled porosity (unitless)
#'
#' @param Hs Henry's Law constant at system temperature (unitless)
#'
#' @return The chemical effective diffusion coefficient (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @examples
#' DeffCalc(Dair = 0.0504664,
#'     Dwater = 9.4551E-06,
#'     nS = 0.385,
#'     nwS = 0.197,
#'     Hs = 0.72383798)
#'
#' @export

DeffCalc <- function(Dair, Dwater, nS, nwS, Hs) {
  Deff <- (Dair * (nS - nwS) ^ 3.33 + Dwater * (nwS ^ 3.33) / Hs) / (nS ^ 2)
  return(Deff)
}

############################### Cs #################################

#' @title Calculate the source vapor concentration (Cs)
#'
#' @description Calculates the source vapor concentration in micrograms per
#'   cubic meter ( \ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Cmedium Source concentration in units of micrograms per liter (\ifelse{html}{\out{&microg/L}}{\eqn{{\mu}g/L}})
#'   for groundwater or  \ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}} for subslab soil gas and exterior soil gas
#'
#' @param Hs Henry's Law constant at system temperature (dimensionless)
#'
#' @param source_medium The source medium. Choose from "Groundwater", "Subslab
#'   Soil Gas", or "Exterior Soil Gas"
#'
#' @return The source vapor concentration in \ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}}
#'
#' @examples
#' CsCalc(Cmedium = 10,
#'  Hs = 0.724,
#'  source_medium = "Groundwater")
#'
#' @export

CsCalc <- function(Cmedium, Hs, source_medium) {
  if(source_medium == "Groundwater"){
    Cs <- Cmedium * 1000 * Hs
  } else {
    Cs <- Cmedium
  }
  return(Cs)
}

############################### %Sat #################################

#' @title Calculate the percent of pure component saturated vapor concentration (%Sat)
#'
#' @description Calculates the percent of pure component saturated vapor concentration.
#'
#' @param Cs Source vapor concentration in micrograms per cubic meter (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Vc Saturated vapor concentration (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @return The percent saturation. The value is returned as a decimal and not as
#'   a percentage (i.e., a return value of 0.5 represents 50%)
#'
#' @examples
#' PercSatCalc(Cs = 7238,
#' Vc = 1.65E8)
#'
#' @export

PercSatCalc <- function(Cs, Vc){
  PercSat <- Cs/Vc
  return(PercSat)
}

############################### Hr #################################

#' @title Calculate the dimensionless Henry's Law constant (Hr)
#'
#' @description Calculates the dimensionless Henry's Law constant from a
#'   reference value of Henry's Law constant in units of atmospheres-cubic
#'   meters per mole (\ifelse{html}{\out{atm-m<sup>3</sup>/L}}{\eqn{atm-m^3/mol}}) at 25 degrees Celsius (\ifelse{html}{\out{&degC}}{\eqn{^{\circ}C}})
#'
#' @param Hc Henry's law constant at 25 \ifelse{html}{\out{&degC}}{\eqn{^{\circ}C}} (\ifelse{html}{\out{atm-m<sup>3</sup>/L}}{\eqn{atm-m^3/mol}})
#'
#' @return Dimensionless Henry's Law constant (dimensionless)
#'
#' @examples
#' HrCalc(Hc = 0.0177)
#'
#' @export

HrCalc <- function(Hc){
  Hr <- Hc/(0.000082057*(298))
  return(Hr)
}

############################### Qb #################################

#' @title Calculate the building ventilation rate (Qb)
#'
#' @description Calculates the building ventilation rate in cubic meters per
#'   hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Abf Enclosed space floor area in square meters (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}})
#'
#' @param Hb Enclosed space mixing height in meters (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @param ach Indoor air exchange rate in air changes per hour (\ifelse{html}{\out{hr<sup>-1</sup>}}{\eqn{hr^-1}})
#'
#' @return The building ventilation rate (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @examples
#' QbCalc(Abf = 150,
#' Hb = 2.44,
#' ach = 0.45)
#'
#' @export

QbCalc <- function(Abf, Hb, ach){
  Qb <- Abf*Hb*ach
    return(Qb)
}

############################### Qsoil #################################
#' @title Calculate the average vapor flow rate into the building (Qsoil)
#'
#' @description Calculates the average vapor flow rate into the building in
#'   cubic meters per hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Qsoil_Qb The ratio of the average vapor flow rate into the building
#'   divided by the building ventilation rate (dimensionless)
#'
#' @param Qb Building ventilation rate (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param foundation_type The foundation type. Choose from the following:
#'
#' * "Slab-grade" for a slab-on-grade foundation
#' * "Basement-slab" for a basement with slab foundation
#' * "Basement-dirt" for a basement with dirt floor foundation
#' * "Crawlspace-slab" for a crawlspace with slab foundation
#' * "Crawlspace-dirt" for a crawlspace with dirt floor foundation
#'
#' @return The average vapor flow rate into the building (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @examples
#' QsoilCalc(Qsoil_Qb = 0.0030,
#' Qb = 164.70,
#' foundation_type = "Slab-grade")
#'
#' @export

QsoilCalc <- function(Qsoil_Qb, Qb, foundation_type){
  if(foundation_type == "Basement-dirt" | foundation_type == "Crawlspace-dirt"){
    Qsoil <- NA
  } else {
    Qsoil <- Qsoil_Qb*Qb
  }

  return(Qsoil)
}

############################### Aparam #################################

#' @title Calculate the "A" parameter in the Johnson and Ettinger model equation
#'
#' @description Calculates the "A" parameter value in the Johnson and Ettinger
#'   model equation, which represents diffusive transport from the source to
#'   a building with a dirt floor foundation (dimensionless)
#'
#' @param DeffT Effective diffusion coefficient from the source to the building
#'   foundation in square centimeters per second (\ifelse{html}{\out{cm<sup>2</sup>}}{\eqn{cm^2/sec}})
#'
#' @param Abf Enclosed space floor area in square meters (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}})
#'
#' @param Lb Depth below grade to base of foundation in meters (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @param Qb Building ventilation rate in cubic meters per hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Ls Depth below grade to water table (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @return The Johnson and Ettinger model equation "A" parameter value (dimensionless)
#'
#' @examples
#' AparamCalc(DeffT = 0.000133929,
#' Abf = 150.00,
#' Lb = 0.10,
#' Qb = 164.70,
#' Ls = 9.54)
#'
#' @export

AparamCalc <- function(DeffT, Abf, Lb, Qb, Ls){
  Aparam <- DeffT * (Abf + 4 * Lb * sqrt(Abf)) * 0.36 / (Qb * (Ls - Lb))
  return(Aparam)
}

############################### Bparam #################################

#' @title Calculate the "B" parameter in the Johnson and Ettinger model equation
#'
#' @description Calculates the "B" parameter in the Johnson and Ettinger model
#'   equation, which represents the Peclet Number (Pe) for contaminant transport
#'   through the building foundation, or the ratio of advective to diffusive
#'   transport (dimensionless)
#'
#' @param Qsoil_Qb The ratio of the average vapor flow rate into the building
#'   divided by the building ventilation rate (dimensionless)
#'
#' @param Qb The building ventilation rate in cubic meters per hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Lf The foundation thickness in meters (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @param DeffBF The effective diffusion coefficient in the soil layer
#'   containing the building foundation in square centimeters per second
#'   (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param eta The fraction of the foundation area with cracks (dimensionless)
#'
#' @param Abf The enclosed space floor area in square meters (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}})
#'
#' @param Lb The depth below grade to the base of the foundation (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @return The Johnson and Ettinger model equation "B" parameter value (dimensionless)
#'
#' @examples
#' BparamCalc(Qsoil_Qb = 0.0030,
#' Qb = 164.70,
#' Lf = 0.10,
#' DeffBF = 0.00130364,
#' eta = 0.001,
#' Abf = 150.00,
#' Lb = 0.10)
#'
#' @export

BparamCalc <- function(Qsoil_Qb, Qb, Lf, DeffBF, eta, Abf, Lb){
  Bparam <- Qsoil_Qb * Qb * Lf / (DeffBF * eta * (Abf + 4 * Lb * sqrt(Abf)) * 0.36)
  return(Bparam)
}

############################### Cparam #################################
#' @title Calculate the "C" parameter in the Johnson and Ettinger model equation
#'
#' @description Calculates the "C" parameter value in the Johnson and Ettinger
#'   model equation, which represents convective transport from subslab
#'   to the building (dimensionless)
#'
#' @param Qsoil_Qb The ratio of the average vapor flow rate into the building
#'   divided by the building ventilation rate (dimensionless)
#'
#' @return The Johnson and Ettinger model equation "C" parameter value (dimensionless)
#'
#' @examples
#' CparamCalc(Qsoil_Qb = 0.0030)
#'
#' @export

CparamCalc <- function(Qsoil_Qb){
  Cparam <- Qsoil_Qb
  return(Cparam)
}

############################### alpha #################################

#' @title Calculate the source to indoor air attenuation factor (alpha)
#'
#' @description Calculates the source to indoor air attenuation factor
#'   (dimensionless). For groundwater, the source concentration is the soil gas
#'   concentration in equilibrium with the groundwater concentration.
#'
#' @param Aparam The Johnson and Ettinger model equation "A" parameter value
#'   (dimensionless)
#'
#' @param Bparam The Johnson and Ettinger model equation "B" parameter value
#'   (dimensionless)
#'
#' @param Cparam The Johnson and Ettinger model equation "C" parameter value
#'   (dimensionless)
#'
#' @param Qsoil_Qb The ratio of the average vapor flow rate into the building
#'   divided by the building ventilation rate (dimensionless)
#'
#' @return The source to indoor air attenuation factor
#'   (dimensionless)
#'
#' @param foundation_type The foundation type. Choose from the following:
#'
#' * "Slab-grade" for a slab-on-grade foundation
#' * "Basement-slab" for a basement with slab foundation
#' * "Basement-dirt" for a basement with dirt floor foundation
#' * "Crawlspace-slab" for a crawlspace with slab foundation
#' * "Crawlspace-dirt" for a crawlspace with dirt floor foundation
#'
#' @param source_medium The source medium. Choose from "Groundwater", "Subslab
#'   Soil Gas", or "Exterior Soil Gas"
#'
#' @examples
#' alphaCalc(Aparam = 4.8E-6,
#' Bparam = 6.8E2,
#' Cparam = 3.0E-3,
#' Qsoil_Qb = 0.0030,
#' foundation_type = "Slab-grade",
#' source_medium = "Groundwater")
#'
#' @export

alphaCalc <- function(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium){
  if(source_medium == "Subslab Soil Gas"){
    alpha <-  Qsoil_Qb
  } else if (foundation_type %in% c("Basement-dirt", "Crawlspace-dirt")) {
    alpha <- (Aparam / (1 + Aparam))
  } else {
    alpha <- (Aparam / (1 + Aparam * exp(-Bparam) + (Aparam / Cparam) * (1 - exp(-Bparam))))
  }
    return(alpha)
}

############################### Cia #################################

#' @title Calculate the modeled indoor air concentration
#'
#' @description Calculates the modeled indoor air concentration due to vapor
#'   intrusion in micrograms per cubic meter (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Cs The source vapor concentration (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param alpha The source to indoor air attenuation factor (dimensionless)
#'
#' @return The modeled indoor air concentration (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @examples
#' CiaCalc(Cs = 7238.379804,
#' alpha = 4.79586E-06)
#'
#' @export

CiaCalc <- function(Cs, alpha){
  Cia <- Cs*alpha
    return(Cia)
}

############################### Cia_ppb #################################
#' @title Calculate the modeled indoor air concentration in units of parts per
#'   billion (Cia_ppb)
#'
#' @description Calculates the modeled indoor air concentration in parts per
#'   billion (ppb) assuming standard temperature and pressure (25 degrees
#'   Celsius and 1 atmosphere)
#'
#' @param Cs The source vapor concentration in micrograms per cubic meter (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param alpha The source to indoor air attenuation factor (dimensionless)
#'
#' @param MW The chemical molecular weight in grams per mole (\ifelse{html}{\out{g/mol}}{\eqn{g/mol}})
#'
#' @return Cia_ppb
#'
#' @examples
#' Cia_ppbCalc(Cs = 7238.379804,
#' alpha = 4.79586E-06,
#' MW = 165.83)
#'
#' @export

Cia_ppbCalc <- function(Cs, alpha, MW){
  Cia_ppb <- Cs*alpha*24.46/MW
  return(Cia_ppb)
}

############################### Css #################################

#' @title Calculate the subslab vapor concentration (Css)
#'
#' @description Calculates the subslab vapor concentration in units of
#'   micrograms per cubic meter (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Cia The modeled indoor air concentration due to vapor intrusion (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Qb The building ventilation rate in cubic meters per hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Qsoil The average vapor flow rate into the building (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param foundation_type The foundation type. Choose from the following:
#'
#' * "Slab-grade" for a slab-on-grade foundation
#' * "Basement-slab" for a basement with slab foundation
#' * "Basement-dirt" for a basement with dirt floor foundation
#' * "Crawlspace-slab" for a crawlspace with slab foundation
#' * "Crawlspace-dirt" for a crawlspace with dirt floor foundation
#'
#' @return The subslab vapor concentration (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}}). The function returns `NA` for
#'   buildings with dirt floor foundations.
#'
#' @examples
#' CssCalc(Cia = 0.034714258,
#' Qb = 164.70,
#' Qsoil = 0.4941,
#' foundation_type = "Slab-grade")
#'
#' @export

CssCalc <- function(Cia, Qb, Qsoil, foundation_type){

  if(foundation_type == "Basement-dirt" | foundation_type == "Crawlspace-dirt"){
    Css <- NA
  } else {
    Css <- Cia*Qb/Qsoil
  }

  return(Css)
}

############################### Css_ppb #################################

#' @title Calculate the subslab vapor concentration in units of parts per
#'   billion (Css_ppb)
#'
#' @description Calculates the subslab vapor concentration in units of parts per billion
#'   (ppb)
#'
#' @param Cia The indoor air concentration due to vapor intrusion in micrograms
#'   per cubic meter (\ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})
#'
#' @param Qb The building ventilation rate in cubic meters per hour (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param Qsoil The average vapor flow rate into the building (\ifelse{html}{\out{m<sup>3</sup>/hr}}{\eqn{m^3/hr}})
#'
#' @param MW The chemical molecular weight in grams per mole (\ifelse{html}{\out{g/mol}}{\eqn{g/mol}})
#'
#' @return The subslab vapor concentration in parts per billion (\ifelse{html}{\out{ppb}}{\eqn{ppb}})
#'
#' @param foundation_type The foundation type. Choose from the following:
#'
#' * "Slab-grade" for a slab-on-grade foundation
#' * "Basement-slab" for a basement with slab foundation
#' * "Basement-dirt" for a basement with dirt floor foundation
#' * "Crawlspace-slab" for a crawlspace with slab foundation
#' * "Crawlspace-dirt" for a crawlspace with dirt floor foundation
#'
#' @examples
#' Css_ppbCalc(Cia = 0.034714258,
#' Qb = 164.70,
#' Qsoil = 0.4941,
#' MW = 165.83,
#' foundation_type = "Slab-grade")
#'
#' @export

Css_ppbCalc <- function(Cia, Qb, Qsoil, MW, foundation_type){

  if(foundation_type == "Basement-dirt" | foundation_type == "Crawlspace-dirt"){
    Css_ppb <- NA
  } else {
    Css_ppb <- Cia*Qb/Qsoil*24.46/MW
  }

  return(Css_ppb)
}

