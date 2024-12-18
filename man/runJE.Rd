% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_runJE.R
\name{runJE}
\alias{runJE}
\title{Run the Johnson and Ettinger model}
\usage{
runJE(
  contaminant_data,
  building_data,
  vadose_zone_data,
  strata_logs_data,
  settings_data
)
}
\arguments{
\item{contaminant_data}{A data frame or \code{JEMParamDist} object with input
contaminant data}

\item{building_data}{A data frame or list of \code{JEMParamDist} objects with input
building data}

\item{vadose_zone_data}{A data frame or list of \code{JEMParamDist} objects with
input vadose zone data}

\item{strata_logs_data}{A data frame with input data for the strata log or
logs that characterize the subsurface soils beneath the building}

\item{settings_data}{A data frame containing input simulation settings}
}
\value{
For deterministic simulations, \code{runJE()} returns a data frame
summarizing the model input parameters, intermediate calculations, and
output results. For stochastic simulations, \code{runJE()} returns an mc object
with the same information for each Monte Carlo iteration in the simulation.
}
\description{
Runs deterministic and stochastic simulations of the Johnson &
Ettinger model. See the following vignettes for more information.

\code{vignette("DeterministicSimulations", package ="vapintr")}

\code{vignette("StochasticSimulations", package = "vapintr")}
}
\examples{
#Run a deterministic simulation
contaminant_data <- det_jem_sim_example_data[[1]]
building_data <- det_jem_sim_example_data[[2]]
vadose_zone_data <- det_jem_sim_example_data[[3]]
strata_logs_data <- det_jem_sim_example_data[[4]]
settings_data <- det_jem_sim_example_data[[5]]

runJE(contaminant_data, building_data, vadose_zone_data, strata_logs_data,
settings_data)

#Run a stochastic simulation
contaminant_data_lx <- getInputConc(stoc_jem_sim_example_data[[1]], NA,
"Groundwater", "MC")

contaminant_data <- contaminant_data_lx[[1]]
building_data <- stoc_jem_sim_example_data[[2]]
vadose_zone_data <- stoc_jem_sim_example_data[[3]]
strata_logs_data <- stoc_jem_sim_example_data[[4]]
settings_data <- stoc_jem_sim_example_data[[5]]

runJE(contaminant_data, building_data, vadose_zone_data, strata_logs_data,
settings_data)

}
