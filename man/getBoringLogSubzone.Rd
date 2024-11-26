% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_getBoringLogSubzone.R
\name{getBoringLogSubzone}
\alias{getBoringLogSubzone}
\title{Subset a data frame of soil strata layers}
\usage{
getBoringLogSubzone(
  boring_log_dfx,
  subzone_upper_depth_bound,
  subzone_lower_depth_bound
)
}
\arguments{
\item{boring_log_dfx}{A data frame representing a soil strata log. The data
frame must have the following fields: "Thickness", "upper_depth", and
"lower_depth". Data in all three fields should be in meters. For
soil strata data imported from vapintr's data import template, the "upper_depth" and
"lower_depth" fields can be added using \code{boringLogSetup()}.}

\item{subzone_upper_depth_bound}{The upper depth of the subzone in meters. This
is the subzone depth closest to the ground surface.}

\item{subzone_lower_depth_bound}{The lower depth of the subzone in meters. This
is the subzone depth farthest from the ground surface.}
}
\value{
A data frame of soil layers within the subzone. Any soil layers in
boring_log_dfx that were not in the subzone are removed from the output
data frame, and any layers that were partially in the subzone have had
their thickness, upper_depth and/or lower_depth adjusted to reflect just
the portion of the layer within the subzone.
}
\description{
Subsets a data frame of soil strata layers into just the layers
contained within a subzone defined by an upper and lower depth.
}
\examples{
#Get example soil strata log data
soil_strata_log_data <- det_jem_sim_example_data[[4]]

#Assign properties to each soil layer
soil_strata_log_data <- boringLogSetup(soil_strata_log_data)

#Get the layers in a subzone of 3 to 5 meters beneath ground surface
getBoringLogSubzone(soil_strata_log_data, 3, 5)

}
