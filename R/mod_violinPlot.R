#' UI function for displaying the violin plots
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_violinPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = 'figure_class',
      h4(textOutput(ns("violin_plot_title"))),
      plotOutput(ns("violin_plot"), width = "100%", height = "100%")
    )
  )
}

# Generates the violin plot
#'
#' @noRd

mod_violinPlotServer <- function(id, selection, data_upload, jem_data){

  moduleServer(
    id,
    function(input, output, session){

      shinyjs::useShinyjs(html = TRUE)

      observe({

        if ((is.null(jem_data()) || selection$contaminant == "" || selection$units == "")) {
          shinyjs::hide("violin_plot_title")
          shinyjs::hide("violin_plot")
        } else {
          #Show the violin plot info only if it's a stochastic scenario
          if(data_upload()[[5]]$simulation_type == "MC"){
            shinyjs::show("violin_plot_title")
            shinyjs::show("violin_plot")

          }
          else
          {
            shinyjs::hide("violin_plot_title")
            shinyjs::hide("violin_plot")
          }
        }
      })

      output$violin_plot_title <- renderText({

        if (!(is.null(jem_data()) || selection$contaminant == "" || selection$units == "")){
          paste(selection$contaminant, "Violin Plot")
        }
      })

      output$violin_plot <- renderImage({

        if (!(is.null(jem_data()) || selection$contaminant == "" || selection$units == "")) {

          id <- showNotification("Building violin plot figure...", duration = NULL, closeButton = FALSE)
          on.exit(removeNotification(id), add = TRUE)

          return(createViolinPlotForExport(data_upload, jem_data, selection$contaminant, selection$units, session$clientData$pixelratio))

        }
        else
        {
          outfile <- tempfile(fileext = '.png')

          png(outfile)

          dev.off()

          return(list(src = outfile,
                      width = 1,
                      height = 1,
                      alt = "Data plot"))
        }
      }, deleteFile = TRUE)

    }
  )
}



