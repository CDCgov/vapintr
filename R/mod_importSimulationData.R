#' UI function for data import widget
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_importSimulationDataUI <- function(id){
  ns <- NS(id)
  tagList(

    # widget to upload Excel file
    fileInput(
      ns("file"), "",
      multiple = TRUE,
      accept = c(".xlsx", ".xls"),
      width = "100%"
      ),

    htmlOutput(ns("warning_message"))

  )
}

#' Server function for data import widget
#'
#' @noRd

mod_importSimulationDataServer <- function(id, jem_data) {
  moduleServer(

    # Set ID
    id,

    # Define core mechanism
    function(input, output, session){

      # reactive file path
      input_filepath_set <- reactive({

        jem_data(NULL) # reset user input

        req(input$file$datapath) # require a datapath has been uploaded to run

        # Disable tabs
        shinyjs::runjs("disableTabs()")

        output$warning_message <- renderUI({return(NULL)})

        tryCatch({
            uploaded_data <- importTemplateData(input$file$datapath)
            return(uploaded_data)
          }, error = function(e) {
            output$warning_message <- renderUI({
              HTML(paste0("<div class=warningmessage><h3>Upload Error:</h3>", e$message, "<br><br><i><b>Please fix the above error and re-import your data to continue.</b></i>", "</div>"))
            })
            return(NULL)
          }, silent = TRUE
        )
      })

      # Return the reactive value
      return(input_filepath_set)
    }
  )
}

