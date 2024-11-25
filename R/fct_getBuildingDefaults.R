#' @title Get default values for building parameters
#'
#' @description Gets default parameter values for a specified building and
#'   foundation type for use in deterministic and stochastic simulations. The
#'   default parameter values come from standard parameter values included in
#'   version 6.0 of USEPA's Johnson and Ettinger model spreadsheet. See the
#'   following vignettes for more information.
#'
#'   `vignette("DeterministicSimulations", package = "vapintr")`
#'
#'   `vignette("StochasticSimulations", package = "vapintr")`
#'
#'
#' @param building_setting The building type. Choose from "Residential" or
#'   "Commercial".
#'
#' @param foundation_type The foundation type. Choose from the following:
#'
#' * "Slab-grade" for a slab-on-grade foundation
#' * "Basement-slab" for a basement with slab foundation
#' * "Basement-dirt" for a basement with dirt floor foundation
#' * "Crawlspace-slab" for a crawlspace with slab foundation
#' * "Crawlspace-dirt" for a crawlspace with dirt floor foundation
#'
#' @param simulation_type The simulation type. Choose from "DET" for a
#'   deterministic simulation or "MC" for a stochastic simulation.
#'
#' @return A data frame of building parameter values. For deterministic
#'   simulations, the data frame will have one record per parameter value. For
#'   stochastic simulations, the data frame will have multiple records per
#'   parameter value, with each record identifying the distribution type and a
#'   distribution parameter value.
#'
#' @examples
#' getBuildingDefaults("Residential", "Basement-dirt", "DET")
#' getBuildingDefaults("Commercial", "Slab-grade", "MC")
#'
#' @export

getBuildingDefaults <- function(building_setting, foundation_type, simulation_type){

  if (simulation_type == "DET"){
    Lb <- case_when(foundation_type == "Basement-slab" ~ 2,
                    foundation_type == "Basement-dirt" ~ 2,
                    foundation_type == "Slab-grade" & building_setting == "Residential" ~ 0.1,
                    foundation_type == "Slab-grade" & building_setting == "Commercial" ~ 0.2,
                    TRUE ~ 1)

    Lf <- case_when(foundation_type == "Basement-slab" & building_setting == "Residential" ~ 0.1,
                    foundation_type == "Basement-slab" & building_setting == "Commercial" ~ 0.2,
                    foundation_type == "Basement-dirt" ~ 0,
                    foundation_type == "Slab-grade" & building_setting == "Residential" ~ 0.1,
                    foundation_type == "Slab-grade" & building_setting == "Commercial" ~ 0.2,
                    foundation_type == "Crawlspace-slab" ~ 0.1,
                    TRUE ~ 0)

    eta <- case_when(foundation_type == "Basement-dirt" ~ 1,
                     foundation_type == "Crawlspace-dirt" ~ 1,
                     TRUE ~ 0.001)

    Abf <- ifelse(building_setting == "Residential", 150, 1500)

    Hb <- case_when(foundation_type == "Basement-slab" & building_setting == "Residential" ~ 3.66,
                    foundation_type == "Basement-dirt" & building_setting == "Residential" ~ 3.66,
                    foundation_type == "Slab-grade" & building_setting == "Residential" ~ 2.44,
                    foundation_type == "Crawlspace-slab" & building_setting == "Residential" ~ 1.3,
                    foundation_type == "Crawlspace-dirt" & building_setting == "Residential" ~ 1.3,
                    TRUE ~ 3)

    ach <- ifelse(building_setting == "Residential", 0.45, 1.5)


    building_info_dfx <- data.frame (
      Parameter = c("Depth below grade to base of foundation",
                   "Foundation thickness",
                   "Fraction of foundation area with cracks",
                   "Enclosed space floor area",
                   "Enclosed space mixing height",
                   "Indoor air exchange rate",
                   "Qsoil/Qbuilding"),
      Units = c("m", "m", "-", "m2", "m", "1/hr", "-"),
      JEMSymbol = c("Lb", "Lf", "eta", "Abf", "Hb", "ach", "Qsoil_Qb"),
      Distribution = c("Constant", "Constant", "Constant", "Constant", "Constant", "Constant", "Constant"),
      DistributionParameterType = c("Constant", "Constant", "Constant", "Constant", "Constant", "Constant", "Constant"),
      DistributionParameterValue = c(Lb, Lf, eta, Abf, Hb, ach, 0.003))
  }
  else {

    ###################### Lb ######################

    #Set Lb distribution
    Lb_dist <- case_when(foundation_type == "Slab-grade" & building_setting == "Residential" ~ "Triangular",
                         TRUE ~ "PERT")

    #Set Lb distribution parameter types
    Lb_param1 <- "Maximum"
    Lb_param2 <- "Mode"
    Lb_param3 <- "Minimum"

    #Set Lb parameter values
    Lb_central <- case_when(foundation_type == "Basement-slab" ~ 2,
                            foundation_type == "Basement-dirt" ~ 2,
                            foundation_type == "Slab-grade" & building_setting == "Residential" ~ 0.1,
                            foundation_type == "Slab-grade" & building_setting == "Commercial" ~ 0.2,
                            TRUE ~ 1)
    Lb_min <- 0.1
    Lb_max <- 2.44

    Lb_value1 <- Lb_max
    Lb_value2 <- Lb_central
    Lb_value3 <- Lb_min

    ##################### Lf #######################

    #Set Lf distribution
    Lf_dist <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab") & building_setting == "Residential" ~ "Triangular",
                         foundation_type %in% c("Basement-slab","Slab-grade") & building_setting == "Commercial" ~ "PERT",
                         TRUE ~ "Constant")

    #Set Lf distribution parameter types
    Lf_param1 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ "Maximum",
                           TRUE ~ "Constant")
    Lf_param2 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ "Mode",
                           TRUE ~ "Not Applicable")
    Lf_param3 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ "Minimum",
                           TRUE ~"Not Applicable")

    #Set Lf parameter values
    Lf_central <- case_when(foundation_type == "Basement-slab" & building_setting == "Residential" ~ 0.1,
                            foundation_type == "Basement-slab" & building_setting == "Commercial" ~ 0.2,
                            foundation_type == "Basement-dirt" | foundation_type == "Crawlspace-dirt" ~ 0,
                            foundation_type == "Slab-grade" & building_setting == "Residential" ~ 0.1,
                            foundation_type == "Slab-grade" & building_setting == "Commercial" ~ 0.2,
                            foundation_type == "Crawlspace-slab" ~ 0.1,
                            TRUE ~ as.numeric(NA))
    Lf_min <- 0.1
    Lf_max <- 0.25

    Lf_value1 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ Lf_max,
                           TRUE ~ Lf_central)
    Lf_value2 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ Lf_central,
                           TRUE ~ as.numeric(NA))
    Lf_value3 <- case_when(foundation_type %in% c("Basement-slab","Slab-grade","Crawlspace-slab")  ~ Lf_min,
                           TRUE ~ as.numeric(NA))

    ##################### eta ######################

    #Set eta distribution
    eta_dist <- case_when(foundation_type == "Slab-grade" ~ "PERT",
                          foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ "Constant",
                          TRUE ~ "Triangular")

    #Set eta distribution parameter types
    eta_param1 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ "Constant",
                            TRUE ~ "Maximum")
    eta_param2 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ "Not Applicable",
                            TRUE ~ "Mode")
    eta_param3 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ "Not Applicable",
                            TRUE ~ "Minimum")

    #Set eta parameter values
    eta_central <- case_when(foundation_type == "Basement-dirt" ~ 1,
                             foundation_type == "Crawlspace-dirt" ~ 1,
                             TRUE ~ 0.001)

    eta_min <- case_when(foundation_type == "Slab-grade" ~ 0.00019,
                         foundation_type == "Basement-dirt" ~ as.numeric(NA),
                         foundation_type == "Crawlspace-dirt" ~ as.numeric(NA),
                         TRUE ~ 0.0001)

    eta_max <- case_when(foundation_type == "Slab-grade" ~ 0.0019,
                         foundation_type == "Basement-dirt" ~ as.numeric(NA),
                         foundation_type == "Crawlspace-dirt" ~ as.numeric(NA),
                         TRUE ~ 0.001)

    #Set eta parameter values
    eta_value1 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ eta_central,
                            TRUE ~ eta_max)
    eta_value2 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ as.numeric(NA),
                            TRUE ~ eta_central)
    eta_value3 <- case_when(foundation_type %in% c("Basement-dirt","Crawlspace-dirt")  ~ as.numeric(NA),
                            TRUE ~ eta_min)

    #################### Abf #######################

    #set Abf distribution
    Abf_dist <- "PERT"

    #Set Abf distribution parameter types
    Abf_param1 <- "Maximum"
    Abf_param2 <- "Mode"
    Abf_param3 <- "Minimum"

    #Set Abf parameter values

    Abf_max <- case_when(building_setting == "Residential"~ 200,
                         TRUE ~ 100000)
    #Abf_central <- ifelse(building_setting == "Residential", 150, 1500)
    Abf_central <- case_when(building_setting == "Residential"~ 150,
                             TRUE ~ 1500)
    Abf_min <- 80

    Abf_value1 <- Abf_max
    Abf_value2 <- Abf_central
    Abf_value3 <- Abf_min

    ##################### Hb ######################

    #Set Hb distribution
    Hb_dist <- case_when(foundation_type %in% c("Crawlspace-slab","Crawlspace-dirt")  ~ "Triangular",
                         TRUE ~ "PERT")

    #Set Hb parameter distribution types
    Hb_param1 <- "Maximum"
    Hb_param2 <- "Mode"
    Hb_param3 <- "Minimum"

    #Set Hb parameter values
    Hb_central <- case_when(foundation_type == "Basement-slab" & building_setting == "Residential" ~ 3.66,
                            foundation_type == "Basement-dirt" & building_setting == "Residential" ~ 3.6,
                            foundation_type == "Slab-grade" & building_setting == "Residential" ~ 2.44,
                            foundation_type == "Crawlspace-slab" & building_setting == "Residential" ~ 1.3,
                            foundation_type == "Crawlspace-dirt" & building_setting == "Residential" ~ 1.3,
                            TRUE ~ 3)
    Hb_max <- case_when(foundation_type == "Basement-slab" ~ 4.88,
                        foundation_type == "Basement-dirt" ~ 4.88,
                        foundation_type == "Slab-grade" ~ 3.05,
                        TRUE ~ 1.30)
    Hb_min <- case_when(foundation_type == "Basement-slab" ~ 2.44,
                        foundation_type == "Basement-dirt" ~ 2.44,
                        foundation_type == "Slab-grade" ~ 2.13,
                        TRUE ~ 0.5)

    Hb_value1 <- Hb_max
    Hb_value2 <- Hb_central
    Hb_value3 <- Hb_min

    ##################### ach ######################

    #set ach distribution
    ach_dist <- "PERT"

    #set ach parameter distribution types
    ach_param1 <- "Maximum"
    ach_param2 <- "Mode"
    ach_param3 <- "Minimum"


    #set ach parameter values

    #ach <- ifelse(building_setting == "Residential", 0.45, 1.5)
    ach_central <- case_when(building_setting == "Residential"~ 0.45,
                             TRUE ~ 1.5)

    #ach_min <- ifelse(building_setting == "Residential", 0.15, 0.3)
    ach_min <- case_when(building_setting == "Residential"~ 0.15,
                         TRUE ~ 0.3)

    #ach_max <- ifelse(building_setting == "Residential", 1.26, 4.1)
    ach_max <- case_when(building_setting == "Residential"~ 1.26,
                         TRUE ~ 4.1)

    ach_value1 <- ach_max
    ach_value2 <- ach_central
    ach_value3 <- ach_min

    ##################### Qsoil_Qb ######################

    #set Qsoil_Qb distribution
    Qsoil_Qb_dist <- "PERT"

    #set Qsoil_Qb parameter distribution types
    Qsoil_Qb_param1 <- "Maximum"
    Qsoil_Qb_param2 <- "Mode"
    Qsoil_Qb_param3 <- "Minimum"

    #set Qsoil_Qb parameter values
    Qsoil_Qb_max <- 0.05
    Qsoil_Qb_central <- 0.003
    Qsoil_Qb_min <- 0.0001

    Qsoil_Qb_value1 <- Qsoil_Qb_max
    Qsoil_Qb_value2 <- Qsoil_Qb_central
    Qsoil_Qb_value3 <- Qsoil_Qb_min

    ###########################################
    building_info_dfx <- data.frame (
      Parameter = c("Depth below grade to base of foundation",
                   "Depth below grade to base of foundation",
                   "Depth below grade to base of foundation",
                   "Foundation thickness",
                   "Foundation thickness",
                   "Foundation thickness",
                   "Fraction of foundation area with cracks",
                   "Fraction of foundation area with cracks",
                   "Fraction of foundation area with cracks",
                   "Enclosed space floor area",
                   "Enclosed space floor area",
                   "Enclosed space floor area",
                   "Enclosed space mixing height",
                   "Enclosed space mixing height",
                   "Enclosed space mixing height",
                   "Indoor air exchange rate",
                   "Indoor air exchange rate",
                   "Indoor air exchange rate",
                   "Qsoil/Qbuilding",
                   "Qsoil/Qbuilding",
                   "Qsoil/Qbuilding"),
      Units = c("m","m","m",
                "m","m","m",
                "-","-","-",
                "m2","m2","m2",
                "m","m","m",
                "1/hr","1/hr","1/hr",
                "-","-","-"),
      JEMSymbol = c("Lb","Lb","Lb",
                    "Lf","Lf","Lf",
                    "eta","eta","eta",
                    "Abf","Abf","Abf",
                    "Hb","Hb","Hb",
                    "ach","ach","ach",
                    "Qsoil_Qb","Qsoil_Qb","Qsoil_Qb"),
      Distribution = c(Lb_dist,Lb_dist,Lb_dist,
                       Lf_dist,Lf_dist,Lf_dist,
                       eta_dist, eta_dist, eta_dist,
                       Abf_dist,Abf_dist,Abf_dist,
                       Hb_dist,Hb_dist,Hb_dist,
                       ach_dist,ach_dist,ach_dist,
                       Qsoil_Qb_dist,Qsoil_Qb_dist,Qsoil_Qb_dist),
      DistributionParameterType = c(Lb_param1,Lb_param2,Lb_param3,
                        Lf_param1,Lf_param2,Lf_param3,
                        eta_param1, eta_param2, eta_param3,
                        Abf_param1,Abf_param2,Abf_param3,
                        Hb_param1,Hb_param2,Hb_param3,
                        ach_param1,ach_param2,ach_param3,
                        Qsoil_Qb_param1,Qsoil_Qb_param2,Qsoil_Qb_param3),
      DistributionParameterValue = c(Lb_value1,Lb_value2,Lb_value3,
                        Lf_value1,Lf_value2,Lf_value3,
                        eta_value1,eta_value2,eta_value3,
                        Abf_value1,Abf_value2,Abf_value3,
                        Hb_value1,Hb_value2,Hb_value3,
                        ach_value1,ach_value2,ach_value3,
                        Qsoil_Qb_value1,Qsoil_Qb_value2,Qsoil_Qb_value3))

  }

  return(building_info_dfx)

}
