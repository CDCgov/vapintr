% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{chem_data}
\alias{chem_data}
\title{Chemical property data}
\format{
A data frame with the following fields:
\describe{
\item{Chemical}{chemical name}
\item{CAS}{Chemical Abstract Service Registry Number}
\item{MW}{molecular weight (grams per mole, or \ifelse{html}{\out{&microg/mol}}{\eqn{g/mol}})}
\item{Vc}{saturated vapor concentration (micrograms per cubic meter, or \ifelse{html}{\out{&microg/m<sup>3</sup>}}{\eqn{{\mu}g/m^3}})}
\item{S}{pure component water solubility (milligrams per liter, or \ifelse{html}{\out{mg/L}}{\eqn{mg/L}})}
\item{Hc25}{Henry's Law constant at 25 degrees Celsius (atmospheres-cubic
meters per mole, or \ifelse{html}{\out{atm-m<sup>3</sup>/L}}{\eqn{atm-m^3/mol}})}
\item{H_prime_25}{dimensionless Henry's Law constant at 25 degrees
Celsius (dimensionless, gas concentration divided by aqueous concentration)}
\item{Da}{diffusivity in air (square centimeters per second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})}
\item{Dw}{diffusivity in water (square centimeters per second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})}
\item{Tboil}{normal boiling point (Kelvin, or K)}
\item{Tcrit}{critical temperature (Kelvin, or K)}
\item{DH_vb}{enthalpy of vaporization at the normal boiling point
(calories per mole, or \ifelse{html}{\out{cal/mol}}{\eqn{cal/mol}})}
\item{Pet_HC_Flag}{flag indicating if the chemical is a petroleum hydrocarbon}
}
}
\usage{
chem_data
}
\description{
A data frame of chemical properties for the chemicals supported within
vapintr. All properties were imported from version 6.0 of USEPA's Johnson
and Ettinger model spreadsheet.
}
\keyword{datasets}
