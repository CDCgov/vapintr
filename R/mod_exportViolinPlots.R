#' UI function for the two export violin plot buttons
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_exportViolinPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      id = ns("exportViolinPlotButtons"),
      br(),
      fluidRow(
        column(6,
               class = "export-button-column",
               # Button to download data in PHAST template
               downloadButton(ns("exportViolinPlot"), "Export Displayed Violin Plot",
                              class = "btn bg-blue-p b-blue-p btn-blue download")
        ),

        column(6,
               class = "export-button-column",
               # Button to download data in PHAST template
               downloadButton(ns("exportAllViolinPlots"), "Export All Violin Plots",
                              class = "btn bg-blue-p b-blue-p btn-blue download")
        )
      )
    )
  )
}

#' Server function for the two export violin plot buttons
#'
#' @noRd
mod_exportViolinPlotServer <- function(id, selection, data_upload, jem_data){

  moduleServer(
    id,
    function(input, output, session){

      shinyjs::useShinyjs(html = TRUE)

      observe({

        if (is.null(data_upload())) {
          shinyjs::hide("exportViolinPlotButtons")
        } else {
          #Show the export summary statistics button only if it's a stochastic scenario
          if(data_upload()[[5]]$simulation_type == "MC" && !is.null(jem_data())){
            shinyjs::show("exportViolinPlotButtons")

            #Disable the plot export function if all the contaminant results had errors
            contaminants_without_errors_count <- sum(sapply(1:length(jem_data()), function(i){is.null(jem_data()[[i]]$Error)}))

            if(contaminants_without_errors_count == 0){
              shinyjs::disable("exportAllViolinPlots")
            } else {
              shinyjs::enable("exportAllViolinPlots")
            }
          }
          else
          {
            shinyjs::hide("exportViolinPlotButtons")
          }
        }

      })

      observe({

        if (!(is.null(jem_data()) || selection$contaminant == "" || selection$units == "")) {

          shinyjs::enable("exportViolinPlot")

          output$exportViolinPlot <- downloadHandler(

            filename = paste0(selection$contaminant,"_", gsub("/","",selection$units),".png"),

            content = function(file) {

              id <- showNotification("Building violin plot figure...", duration = NULL, closeButton = FALSE)

              file_info <- createViolinPlotForExport(data_upload, jem_data, selection$contaminant, selection$units, session$clientData$pixelratio)

              on.exit(list(removeNotification(id), unlink(file_info$src)), add = TRUE)

              return(file.copy(file_info$src, file))

            },
            contentType = "image/png"
          )

        }
        else
        {
          shinyjs::disable("exportViolinPlot")
        }
      })

      observe({

        if (!is.null(jem_data())) {

          output$exportAllViolinPlots <- downloadHandler(

            filename = 'ViolinPlots.zip',

            content = function(file) {

              id <- showNotification("Building violin plot figures...", duration = NULL, closeButton = FALSE)

              contaminants_without_errors <- which(sapply(1:length(jem_data()), function(i){is.null(jem_data()[[i]]$Error)}))

              violin_plots_ugm3 <- lapply(seq_along(contaminants_without_errors), function(cont_index){

                jem_index <- contaminants_without_errors[cont_index]

                contaminant <- jem_data()[[jem_index]]$Contaminant

                return(createViolinPlotForExport(data_upload, jem_data, contaminant, "ug/m3", session$clientData$pixelratio))

              })

              violin_plots_ppb <- lapply(seq_along(contaminants_without_errors), function(cont_index){

                jem_index <- contaminants_without_errors[cont_index]

                contaminant <- jem_data()[[jem_index]]$Contaminant

                return(createViolinPlotForExport(data_upload, jem_data, contaminant, "ppb", session$clientData$pixelratio))

              })

              fs_ugm3 <- unlist(lapply(seq_along(contaminants_without_errors), function(cont_index){

                return(violin_plots_ugm3[[cont_index]]$src)

              }))

              fs_ppb <- unlist(lapply(seq_along(contaminants_without_errors), function(cont_index){

                return(violin_plots_ppb[[cont_index]]$src)

              }))

              fs <- c(fs_ugm3, fs_ppb)

              on.exit(list(removeNotification(id), unlink(fs)), add = TRUE)

              return(zip::zip(zipfile=file, files=fs, mode="cherry-pick"))

            },
            contentType = "application/zip"
          )

        }

      })

  })
}

