% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_calcHs.R
\name{calcHs}
\alias{calcHs}
\title{Calculate Henry's Law constant at system temperature (Hs)}
\usage{
calcHs(Chem = NA, CAS = NA, Ts, simulation_type)
}
\arguments{
\item{Chem}{Chemical name. Enter a value for this parameter or for the CAS
parameter to identify the chemical. If values are entered for both
parameters, they must correspond to the same chemical. Only the chemical
names in \code{chem_data$Chemical} are supported.}

\item{CAS}{Chemical Abstract Service Registry Number. Enter a value for this
parameter or for the Chem parameter to identify the chemical. If values are
entered for both parameters, they must correspond to the same chemical.
Only the CAS numbers in \code{chem_data$CAS} are supported.}

\item{Ts}{Source temperature in degrees Celsius ( \ifelse{html}{\out{&degC}}{\eqn{^{\circ}C}}). Enter a single
value for deterministic simulations or an mcnode object of sampled
temperatures for a stochastic simulation.}

\item{simulation_type}{Simulation type. Enter "DET" for a deterministic
simulation and "MC" for a stochastic simulation.}
}
\value{
Returns a list with two elements. The first element is the
calculated value(s) of Hs and the second is the text of any warnings
generated. The calculated Hs value is a single number in a deterministic
simulation and an mcnode object in a stochastic simulation. Hs is
dimensionless and represents the equilibrium gas concentration
(Cgas) divided by the aqueous concentration (Caqueous) at
the system temperature.
}
\description{
Calculates Hs using default chemical properties at the
subsurface system temperature
}
\examples{
#Calculate Hs at a fixed temperature
calcHs(Chem = "Tetrachloroethylene", Ts = 23, simulation_type = "DET")

#Calculate Hs over a distribution of temperatures
library(mc2d)
calcHs(CAS = "79-01-6", Ts = mc2d::mcstoc(type = "U", min = 20, max = 25), simulation_type = "MC")

}
