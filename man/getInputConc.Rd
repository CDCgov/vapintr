% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_getInputConc.R
\name{getInputConc}
\alias{getInputConc}
\title{Get the contaminant concentration input for modeling}
\usage{
getInputConc(
  contaminant_data_dfx,
  contaminant,
  source_medium,
  simulation_type,
  use_aggregate_data_in_det_simulation = FALSE
)
}
\arguments{
\item{contaminant_data_dfx}{A data frame of measured contaminant
concentration data imported from the data import template. This data frame
should have the same fields as the concentration data frame exported by
\code{importTemplateData()}.}

\item{contaminant}{The name of the contaminant to be simulated. If inputs
are desired for more than one contaminant, enter \code{NA}.}

\item{source_medium}{The contaminant source medium. Choose from
"Groundwater", "Exterior Soil Gas", or "Subslab Soil Gas".}

\item{simulation_type}{The simulation type. Choose from "DET" for
deterministic simulations or "MC" for stochastic simulations.}

\item{use_aggregate_data_in_det_simulation}{True/False parameter for
deterministic simulations that indicates whether to aggregate the data by
contaminant and sample location ID (True) or to keep contaminant records
separate (False). See \code{vignette("DeterministicSimulations", package = "vapintr")} for more information.}
}
\value{
For a deterministic simulation, the function will return a data frame
of concentration data with the numeric value and units of the concentration
to simulate stored in the \code{Cmedium} and \code{Units} fields, respectively. If
contaminant data were aggregated, the data frame will contain one row per
contaminant with detections in the input data set. If the data were not
aggregated, the data frame will contain one row per detected contaminant
record.

For a stochastic simulation, the output is a list of \code{JEMParamDist} objects,
each representing a distribution of concentrations for one contaminant. The
contaminant associated with each object is identified in the object's \code{name}
property.
}
\description{
Creates the contaminant concentration input required to run the
Johnson and Ettinger model. This function returns a data frame of
concentrations to model for deterministic simulations and a list of
concentration distribution inputs to model for stochastic simulations. See
the following vignettes for more information.

\code{vignette("DeterministicSimulations", package ="vapintr")}

\code{vignette("StochasticSimulations", package = "vapintr")}
}
\examples{
#Assign example concentration data to a data frame
concentration_data <- stoc_jem_sim_example_data[[1]]

#Get deterministic simulation concentration inputs for individual records
getInputConc(concentration_data, NA, "Groundwater", "DET", FALSE)

#Get deterministic simulation concentration inputs aggregated by contaminant
getInputConc(concentration_data, NA, "Groundwater", "DET", TRUE)

#Get an output list of concentration inputs for a stochastic simulation
getInputConc(concentration_data, NA, "Groundwater", "MC")

}
