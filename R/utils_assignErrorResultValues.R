#' @title Assign error results values
#'
#' @description Utility function that assigns `-Inf` values to JEM outputs when the
#'   simulation returned an error
#'
#' @param settings_data A data frame containing input simulation settings
#'
#' @returns either a data frame or mc object of `runJE()` output variables with
#'   `-Inf` assigned to each variable.
#'
#' @noRd

assignErrorResultsValues <- function(settings_data){

  #deterministic simulation
  if(settings_data$simulation_type == "DET"){
    JEMResults <- data.frame(
      alpha = -Inf,
      Cia = -Inf,
      Cia_ppb = -Inf,
      Css = -Inf,
      Css_ppb = -Inf,
      Aparam = -Inf,
      Bparam = -Inf,
      Cparam = -Inf,
      DeffT = -Inf,
      DeffBF = -Inf,
      DeffCZ = -Inf,
      DeffUZ = -Inf,
      hcz = -Inf,
      huz = -Inf,
      ach = -Inf,
      Hb = -Inf,
      eta = -Inf,
      Qb = -Inf,
      Abf = -Inf,
      Lb = -Inf,
      Lf = -Inf,
      Qsoil = -Inf,
      Cmedium = -Inf,
      Ts = -Inf,
      Cs = -Inf,
      percent_sat = -Inf,
      Ls = -Inf,
      is_groundwater_source = settings_data$source_medium == "Groundwater",
      sampled_log_index = -Inf
    )

  }
  #stochastic simulation
  else
  {
    JEMResults <- mc2d::mc(
      alpha = mc2d::mcdata(data = -Inf, type = "U"),
      Cia = mc2d::mcdata(data = -Inf, type = "U"),
      Cia_ppb = mc2d::mcdata(data = -Inf, type = "U"),
      Css = mc2d::mcdata(data = -Inf, type = "U"),
      Css_ppb = mc2d::mcdata(data = -Inf, type = "U"),
      Aparam = mc2d::mcdata(data = -Inf, type = "U"),
      Bparam = mc2d::mcdata(data = -Inf, type = "U"),
      Cparam = mc2d::mcdata(data = -Inf, type = "U"),
      DeffT = mc2d::mcdata(data = -Inf, type = "U"),
      DeffBF = mc2d::mcdata(data = -Inf, type = "U"),
      DeffCZ = mc2d::mcdata(data = -Inf, type = "U"),
      DeffUZ = mc2d::mcdata(data = -Inf, type = "U"),
      hcz = mc2d::mcdata(data = -Inf, type = "U"),
      huz = mc2d::mcdata(data = -Inf, type = "U"),
      ach = mc2d::mcdata(data = -Inf, type = "U"),
      Hb = mc2d::mcdata(data = -Inf, type = "U"),
      eta = mc2d::mcdata(data = -Inf, type = "U"),
      Qb = mc2d::mcdata(data = -Inf, type = "U"),
      Abf = mc2d::mcdata(data = -Inf, type = "U"),
      Lb = mc2d::mcdata(data = -Inf, type = "U"),
      Lf = mc2d::mcdata(data = -Inf, type = "U"),
      Qsoil = mc2d::mcdata(data = -Inf, type = "U"),
      Cmedium = mc2d::mcdata(data = -Inf, type = "U"),
      Ts = mc2d::mcdata(data = -Inf, type = "U"),
      Cs = mc2d::mcdata(data = -Inf, type = "U"),
      percent_sat = mc2d::mcdata(data = -Inf, type = "U"),
      Ls = mc2d::mcdata(data = -Inf, type = "U"),
      is_groundwater_source = mc2d::mcdata(data = settings_data$source_medium == "Groundwater", type = "U"),
      sampled_log_index = mc2d::mcdata(data = -Inf, type = "U")
    )
  }

  return(JEMResults)

}
