#' UI function the violin plot dropdowns
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_violinPlotDropDownUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      h2(textOutput(ns("violin_plot_menu_title")))
    ),
    tags$div(
      id = ns("violin_plot_drop_downs"),
      class = "alert alert-info",
      fluidRow(
        column(4,

               selectizeInput(ns("contaminant"), "Contaminant:",
                           choices=c())
        ),

        column(4,

               selectizeInput(ns("units"), "Units:",
                              choices=c())
        )
      )
    )
  )
}

#' Server function for the violin plot dropdowns
#'
#' @noRd
mod_violinPlotDropDownServer <- function(id, jem_data, data_upload, violin_plot_choices){

  moduleServer(
    id,
    function(input, output, session){

      selections <- reactiveValues(contaminant="")

      shinyjs::useShinyjs(html = TRUE)

      observe({

        if (is.null(data_upload())) {
          shinyjs::hide("violin_plot_drop_downs")
          shinyjs::hide("violin_plot_menu_title")
        } else {
          #Show the violin plot info only if it's a stochastic scenario
          if(data_upload()[[5]]$simulation_type == "MC"){
            shinyjs::show("violin_plot_drop_downs")
            shinyjs::show("violin_plot_menu_title")

          }
          else
          {
            shinyjs::hide("violin_plot_drop_downs")
            shinyjs::hide("violin_plot_menu_title")
          }
        }
      })

      observe({

        output$violin_plot_menu_title <- renderText({
          if (!is.null(jem_data())){
            "Modeled and Measured Data Plots"
          }
        })

      })

      observe({

        if (!is.null(jem_data())) {

          contaminant_choices <- sapply(1:length(jem_data()), function(i){
              contaminant_name <- ifelse(!is.null(jem_data()[[i]]$Error), "ERROR", jem_data()[[i]]$Contaminant)
              return(contaminant_name)
          })

          contaminant_choices <- contaminant_choices[contaminant_choices != "ERROR"]

          if (violin_plot_choices$contaminant == ""){ # set default choices to first choice if no details button is clicked

            updateSelectizeInput(session, "contaminant",
                              choices = contaminant_choices,
                              options = list(
                                placeholder = 'Select',
                                onInitialize = I('function() { this.setValue(""); }'))
            )

          }

        }

      })

      observe({

        if (!is.null(jem_data())) {

          units_choices <- c("ug/m3", "ppb")

          if (violin_plot_choices$unit == ""){ # set default choices to first choice if no details button is clicked

            updateSelectizeInput(session, "units",
                                 choices = units_choices,
                                 options = list(
                                   placeholder = 'Select',
                                   onInitialize = I('function() { this.setValue(""); }'))
            )

          }

        }

      })

      observe({

        if (!is.null(data_upload())) {

          if (!is.null(jem_data())){

            selections$contaminant <- input$contaminant
            selections$units <- input$units

          } else {

            selections$contaminant <- ""
            selections$units <- ""

          }

        }

      })

      return(selections)


    }
  )
}


