#' @title Calculate the total effective diffusion coefficient from the source
#'   to the building
#'
#' @description Calculates the total effective diffusion coefficient from the
#'   source to the building. If the capillary zone is simulated, this function
#'   calculates a weighted effective diffusivity based on the height of the
#'   unsaturated zone and the capillary zone. Otherwise, it returns the
#'   diffusivity of the unsaturated zone above the source.
#'
#' @param DeffUZ Effective diffusion coefficient through the unsaturated zone
#'   (square centimeters per second, or \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param DeffCZ Effective diffusion coefficient through the capillary zone
#'   (\ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}})
#'
#' @param huz Height of the unsaturated zone (meters, or  \ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @param hcz Height of the capillary zone (\ifelse{html}{\out{m}}{\eqn{m}})
#'
#' @param simulate_capillary_zone A true/false flag indicating whether to
#'   simulate the capillary zone
#'
#' @returns The effective diffusivity from the source to the building, in
#'   \ifelse{html}{\out{cm<sup>2</sup>/sec}}{\eqn{cm^2/sec}}
#'
#' @examples
#'   DeffUZ <- 0.0045 #cm2/sec
#'   DeffCZ <- 0.00019 #cm2/sec
#'   huz <- 8.5 #m
#'   hcz <- 0.815 #m
#'   DeffTCalc(DeffUZ, DeffCZ, huz, hcz, TRUE)
#'
#' @export

DeffTCalc <-
  function(DeffUZ, DeffCZ, huz, hcz, simulate_capillary_zone) {

    if (simulate_capillary_zone)
    {
      DeffT <- (huz + hcz) / (huz / DeffUZ + hcz / DeffCZ)
    } else {
      DeffT <- DeffUZ
    }

    return(DeffT)
  }
