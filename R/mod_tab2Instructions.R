#' UI function for the tab 2 instructions
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd

mod_tab2InstructionsUI <- function(id){
  ns <- NS(id)
  tagList(

    tags$div(htmlOutput(ns("instructions_text")))

  )
}

#' tab2 Server Function
#'
#' @noRd
mod_tab2InstructionsServer <- function(id, data_upload, jem_data){

  moduleServer(
    id,
    function(input, output, session){

      observe({
        if (is.null(data_upload()) || is.null(jem_data())){
          shinyjs::hide("instructions_text")
        }
        else
        {
          shinyjs::show("instructions_text")
        }
      })

      output$instructions_text <- renderText({
        if (is.null(data_upload()) || is.null(jem_data())){
          return(NULL)
        } else {

          instructions_text <- "<strong>Instructions:</strong> Please explore the JEM simulation results below. "

          if(data_upload()[[5]]$simulation_type == "DET"){
            instructions_text <- paste0(instructions_text, "The Modeled Concentration Summary Table shows model-predicted indoor air concentrations for each detected contaminant in the imported data set. ")
            instructions_text <- paste0(instructions_text, "Use the search tool to find results for specific contaminants or input values, and use the triangles next to each column heading to sort the data. ")
            instructions_text <- paste0(instructions_text, "The model-predicted concentrations are identified in the Indoor Air Concentration fields. ")
            instructions_text <- paste0(instructions_text, "Click an icon in the Warnings or Errors field to load a pop-up menu that displays any warnings or errors generated during the simulation. ")
            instructions_text <- paste0(instructions_text, "Click the \"Export All Simulation Data\" button to create an Excel file with the data shown on screen and data for other evaluated variables. ")
          }
          else
          {
            instructions_text <- paste0(instructions_text, "The Modeled Concentration Summary Table shows 50<sup>th</sup> and 95<sup>th</sup> percentile statistics on the model-predicted indoor air concentrations for each contaminant. ")
            instructions_text <- paste0(instructions_text, "Use the search tool to find results for specific contaminants, and use the triangles next to each column heading to sort the data. ")
            instructions_text <- paste0(instructions_text, "Click an icon in the Warnings or Errors field to load a pop-up menu that displays any warnings or errors generated during the simulation. ")
            instructions_text <- paste0(instructions_text, "Click the \"Export Summary Statistics\" button to create an Excel file with the statistics shown on screen and statistics for other evaluated variables. ")
            instructions_text <- paste0(instructions_text, "The file will include minimum and maximum statistics in addition to 50<sup>th</sup> and 95<sup>th</sup> percentiles. ")
            instructions_text <- paste0(instructions_text, "Click the \"Export All Simulation Data\" button to create an Excel file with results from the individual Monte Carlo iterations for each contaminant. ")
            instructions_text <- paste0(instructions_text, "<br><br>")
            instructions_text <- paste0(instructions_text, "In the Modeled and Measured Data Plots section, use the dropdowns to visualize the data. ")
            instructions_text <- paste0(instructions_text, "Select a contaminant using the \"Contaminant\" dropdown, and select units for the displayed concentrations using the \"Units\" dropdown. ")
            instructions_text <- paste0(instructions_text, "Modeled indoor air concentrations will be displayed in a violin plot that also identifies the 50<sup>th</sup> and 95<sup>th</sup> percentile model-predicted concentrations. ")
            instructions_text <- paste0(instructions_text, "Measured indoor air or outdoor air data imported in the data template will be displayed in a scatter plot along with lines identifying imported reference air concentrations. ")
            instructions_text <- paste0(instructions_text, "Click the \"Export Displayed Violin Plot\" button to export the plot displayed onscreen, and click the \"Export All Violin Plots\" button to export violin plots for all contaminants. ")
          }

          return(instructions_text)
        }
      })


    }
  )

}


