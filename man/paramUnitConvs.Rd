% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_paramUnitConvs.R
\name{paramUnitConvs}
\alias{paramUnitConvs}
\title{Perform basic unit conversions}
\usage{
paramUnitConvs(inputNumber, inputUnits, outputUnits)
}
\arguments{
\item{inputNumber}{Number to be converted to a new set of units}

\item{inputUnits}{Units associated with the input number}

\item{outputUnits}{Units associated with the output number}
}
\value{
The input number converted to the output units
}
\description{
Performs basic parameter unit conversions related to distance,
area, and temperature. Supported units are:
\itemize{
\item ft: feet
\item m: meters
\item ft2: square feet
\item m2: square meters
\item deg F: degrees Fahrenheit
\item deg C: degrees Celsius
}
}
\examples{
#Area conversion from square feet to square meters
paramUnitConvs(1, "ft2", "m2")

}
