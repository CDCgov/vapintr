#' UI function for the two export simulation data buttons
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_exportSimulationDataUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      id = ns("twoDataExportButtons"),
      fluidRow(
        column(6,
               class = "export-button-column",
               # Button to download data in PHAST template
               downloadButton(ns("exportSummaryStatistics"), "Export Summary Statistics",
                              class = "btn bg-blue-p b-blue-p btn-blue download")
        ),

        column(6,
               class = "export-button-column",
               # Button to download data in PHAST template
               downloadButton(ns("exportAllSimulationData_twoButtonLayout"), "Export All Simulation Data",
                              class = "btn bg-blue-p b-blue-p btn-blue download")
        )
      )
    ),
    tags$div(
      id = ns("oneDataExportButton"),
      fluidRow(
        column(12,
               class = "export-button-column",
               # Button to download data in PHAST template
               downloadButton(ns("exportAllSimulationData_oneButtonLayout"), "Export All Simulation Data",
                              class = "btn bg-blue-p b-blue-p btn-blue download")
        )
      )
    )
  )
}

#' Server function for the two export simulation data buttons
#'
#' @noRd
mod_exportSimulationDataServer <- function(id, data_upload, jem_data){

  moduleServer(
    id,
    function(input, output, session){

      observe({

        if (is.null(data_upload())) {
          shinyjs::hide("twoDataExportButtons")
          shinyjs::hide("oneDataExportButton")
        } else {
          #Show the export summary statistics button only if it's a stochastic scenario
          if(data_upload()[[5]]$simulation_type == "MC"){
            shinyjs::show("twoDataExportButtons")
            shinyjs::hide("oneDataExportButton")
          }
          else
          {
            shinyjs::hide("twoDataExportButtons")
            shinyjs::show("oneDataExportButton")
          }
        }

      })

      observe({

        if(!is.null(data_upload())){

          if (!is.null(jem_data())) {

            # Define download handler
            output$exportSummaryStatistics <- downloadHandler(

              filename = "SummaryStatistics.xlsx",

              content = function(file) {

                summary_statistics <- createExportSummaryStatistics(data_upload, jem_data)

                # Apply module specific function to dataframe and download
                writexl::write_xlsx(summary_statistics, path = file)

              }
            )
          }

        }

      })


      observe({

        if (!is.null(jem_data())) {

          #Define download handler
          output$exportAllSimulationData_twoButtonLayout <- downloadHandler(

            filename = "AllSimulationData.xlsx",

            content = function(file) {

              all_simulation_statistics <- createAllSimulationDataExport(data_upload, jem_data)

              #Apply module specific function to dataframe and download
              writexl::write_xlsx(all_simulation_statistics, path = file)

            }
          )

          #Define download handler
          output$exportAllSimulationData_oneButtonLayout <- downloadHandler(

            filename = "AllSimulationData.xlsx",

            content = function(file) {

              all_simulation_statistics <- createAllSimulationDataExport(data_upload, jem_data)

              #Apply module specific function to dataframe and download
              writexl::write_xlsx(all_simulation_statistics, path = file)

            }
          )

        }

      })

  })
}

