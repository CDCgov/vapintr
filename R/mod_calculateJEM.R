#' UI function for the Run JEM Simulation button
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_calculateJEMUI <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(htmlOutput(ns("calculate_button_explanation"))),
    # Action button to process csv
    shinyjs::hidden(actionButton(
      ns("calculate"), "Run JEM Simulation",
      class="btn bg-blue-p b-blue-p btn-blue text-white calculate"
    )
    )

  )
}

#' Server function supporting the Run JEM Simulation button
#'
#' @noRd

mod_calculateJEMServer <- function(id, data_upload, jem_data){

  moduleServer(
    id,
    function(input, output, session){

      # Create a reactive value for the input file path, defaults to null
      # This value can be written to using input_filepath(data) and read using input_filepath()
      # Any function that reads from this reactive value gets triggered when it is written to
      jem_data(NULL)

      # hide the calculate button if no data has been uploaded
      observe({

        if (is.null(data_upload())) {
          shinyjs::hide("calculate_button_explanation")
          shinyjs::hide("calculate")

        } else{
          shinyjs::show("calculate_button_explanation")
          shinyjs::show("calculate")

        }

      })


      output$calculate_button_explanation <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {

          return("<br>")
        }
      })

      # Now we set up an observer when the calculate button is pressed
      observeEvent(input$calculate, {

        # Make sure we are not reading datapath from null
        if (is.null(data_upload())) {

          jem_data(NULL)

        } else{

          id <- showNotification("Running JEM simulations...", duration = NULL, closeButton = FALSE)
          on.exit(removeNotification(id), add = TRUE)

          # withProgress(message = 'Running JEM Simulations', value = 0,{

            #Run simulation
            simulation_output <- runTemplateData(data_upload(), capture_warnings_and_errors = TRUE)

            #Clean warning messages
            for(i in 1:length(simulation_output)){
              if(!is.null(simulation_output[[i]]$Warning)){
                simulation_output[[i]]$Warning <- sub("The checkModelInputs function threw the following warnings: \n\n", "", simulation_output[[i]]$Warning)
              }
            }

            #Save output in jem_data()
            jem_data(simulation_output)

          # })

          # navigate to the next tab
          shinyjs::runjs("enableTabs()")
          shinyjs::runjs("activeTab('tabs-3-2')")

        }

      })



    }
  )
}


