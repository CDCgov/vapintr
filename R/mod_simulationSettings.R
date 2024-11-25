#' UI function for displaying the table of simulation settings
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_simulationSettingsUI <- function(id){

  ns <- NS(id)
  tagList(

    tags$h3(textOutput(ns("table_title"))),
    tags$div(htmlOutput(ns("table_explanation"))),
    tags$div(tableOutput(ns("table")))

  )

}

#' Server function for the simulation settings table
#'
#' @noRd
mod_simulationSettingsServer <- function(id, data_upload){

   moduleServer(
    id,
    function(input, output, session){

      shinyjs::useShinyjs(html = TRUE)

      observe({
        shinyjs::toggleCssClass(id = "table_class", class = 'uploadSummary',
                    condition = !is.null(data_upload()))
      })

      output$table_title <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {
          "Simulation Settings"
        }
      })

      output$table_explanation <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {
          "The simulation settings table shows parameter values imported from the \"Settings\" sheet of the data import template. These parameters control the overall settings that apply to all simulated contaminants. For more information on these parameters, see the \"ReadMe\" sheet in the data import template.<br><br>"
        }
      })

      output$table <- renderTable({

        if (is.null(data_upload())) {
          # Just render nothing, because no file is uploaded.
          return(NULL)
        } else {

          # Apply the instance specific processing to the dataframe and render
          settings_dfx <- data_upload()[[5]]

          #Clean parameter values for display

          #Simulation type
          settings_dfx <- settings_dfx %>%
            mutate(simulation_type = if_else(.data$simulation_type == "DET",
                                             "Deterministic",
                                             "Stochastic"))

          #Source medium
          settings_dfx <- settings_dfx %>% mutate(source_medium = case_when(
            source_medium == "Groundwater" ~ "Groundwater",
            source_medium == "Exterior Soil Gas" ~ "Exterior soil gas",
            source_medium == "Subslab Soil Gas" ~ "Subslab soil gas",
            TRUE ~ source_medium))

          #Foundation type
          settings_dfx <- settings_dfx %>% mutate(foundation_type = case_when(
            foundation_type == "Crawlspace-slab" ~ "Closed crawl space w/ slab",
            foundation_type == "Basement-slab" ~ "Basement w/ slab",
            foundation_type == "Basement-dirt" ~ "Basement w/ dirt floor",
            foundation_type == "Crawlspace-dirt" ~ "Closed crawl space w/ dirt floor",
            foundation_type == "Slab-grade" ~ "Slab-on-grade",
            TRUE ~ foundation_type))

          #Apply default building parameters
          settings_dfx <- settings_dfx %>% mutate(apply_default_building_parameters = case_when(
            apply_default_building_parameters == TRUE ~ "Yes" ,
            apply_default_building_parameters == FALSE ~ "No",
            TRUE ~ ""))

          #Simulate capillary zone
          settings_dfx <- settings_dfx %>% mutate(simulate_capillary_zone = case_when(
            simulate_capillary_zone == TRUE ~ "Yes",
            simulate_capillary_zone == FALSE ~ "No",
            TRUE ~ ""))

          #Drop the simulate capillary zone field if it's a soil gas simulation
          if(settings_dfx$source_medium != "Groundwater"){
            settings_dfx <- settings_dfx %>% select(-.data$simulate_capillary_zone)
          }

          #Drop the number of Monte Carlo iterations field if it's a deterministic simulation
          if(settings_dfx$simulation_type != "Stochastic"){
            settings_dfx <- settings_dfx %>% select(-.data$number_of_monte_carlo_iterations)
          }

          # Transpose the data and rename the fields
          settings_dfx <- t(settings_dfx)

          #Make rownames into first column and name fields
          settings_dfx <- cbind(rownames(settings_dfx), data.frame(settings_dfx, row.names=NULL))
          colnames(settings_dfx) <- c("Parameter", "Value")

          #Clean parameter names for display
          settings_dfx <- settings_dfx %>%
            mutate(Parameter = str_replace(.data$Parameter, "simulation_type", "Simulation type")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "source_medium", "Source medium")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "apply_default_building_parameters", "Apply default building parameters")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "building_setting", "Building setting")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "foundation_type", "Foundation type")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "simulate_capillary_zone", "Simulate capillary zone")) %>%
            mutate(Parameter = str_replace(.data$Parameter, "number_of_monte_carlo_iterations", "Number of Monte Carlo iterations")) %>%

          return(settings_dfx)
        }
      })

    })
}


