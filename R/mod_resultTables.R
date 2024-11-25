#' UI function for generating the main JEM result table
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd

mod_resultTablesUI <- function(id){
  ns <- NS(id)
  tagList(

    # Basic data table
    DT::DTOutput(ns("table")),

    # Added so that the font-awesome icons will display
    tags$span(icon("tag"), style = "display: none;"),

    br()


  )
}

#' Server function for generating the main JEM result table
#'
#' @noRd

mod_resultTablesServer <- function(id, data_upload, jem_data){
  moduleServer(
    id,
    function(input, output, session){

      violin_plot_selections <- reactiveValues(contaminant="",unit ="")

      # Define table render handler
      output$table <- DT::renderDT({
        ####  Determine if need to parse data, or just read from cache

        #### Render table

        # Check if the path is null.
        if (is.null(jem_data())) {
          # Just render nothing, because no file was uploaded.
          return(NULL)

        } else {

          #Get the input data so we can know the simulation type
          input_data <- data_upload()
          settings_data <- input_data[[5]]

          # Apply the processing to the dataframe and render
          results_table <- createResultsSummary(settings_data, jem_data())

          # number of rows in the dataframe
          num_buttons <- length(results_table$Contaminant)

          # random id to avoid namespace collision of button listeners when refreshing table
          rand_id <- as.character(as.numeric(Sys.time())*1000)

          # need to do this to create a unique button id that gets generated every time the table is regenerated
          # might eventually make the app sluggish since it has to keep saving everything
          we_button_var <- paste0("warn_err_button_", as.character(as.numeric(Sys.time())))

          results_table <- results_table %>%
            mutate(ID = seq_len(nrow(results_table))) %>%
            dplyr::rename(warning_error = .data$`Warnings or Errors`) %>%
            dplyr::mutate(`Warnings or Errors` = mapply(makeWarnErrButton, .data$ID, .data$warning_error,
                                                MoreArgs=list(input_var=session$ns(paste0("warn_err_button", rand_id))))) %>%
            dplyr::select(-.data$ID)

          # render the data table
          if (settings_data$simulation_type == "DET"){
              standardWidth <- '100px'
              endCol <- 8
          }
          else
          {
            standardWidth <- '150px'
            endCol <- 6
          }

          # Warning and error buttons
          observeEvent(input[[paste0("warn_err_button", rand_id)]], {

            selectedRow_WE <- as.numeric(strsplit(input[[paste0("warn_err_button", rand_id)]], "_")[[1]][2])

            alert_message <- results_table[selectedRow_WE,]$warning_error

            if (stringr::str_detect(alert_message, "ERRORFLAG_")) {

              alert_message <- sub("ERRORFLAG_", "", alert_message)
              shinyalert::shinyalert("Errors", alert_message, type = "error", animation = FALSE, immediate = TRUE)

            } else{

              shinyalert::shinyalert("Warnings", alert_message, type = "warning", animation = FALSE, immediate = TRUE)

            }

          }, ignoreInit = TRUE)


          dt <- DT::datatable(results_table %>%
                                select(-.data$warning_error),
                              escape = FALSE,
                              options = list(
                                columnDefs = list(list(width = '20px', targets = 0),
                                                  list(width = standardWidth, targets = c(1:endCol)),
                                                  list(className = 'dt-left', targets = 1),
                                                  list(className = 'dt-center', targets = c(2:endCol))),
                                scrollX = TRUE
                              ))

          return(dt)
        }
      })

      return(violin_plot_selections)
      }
  )

}


