#' UI function for displaying the soil strata profile
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_soilStrataImageUI <- function(id){

  ns <- NS(id)
  tagList(

    tags$h3(textOutput(ns("soil_strata_image_title"))),
    tags$div(htmlOutput(ns("soil_strata_explanation"))),
    tags$div(plotOutput(ns("soil_strata_image"), width = "100%", height = "100%"))

  )

}

#' Server function for displaying the soil strata profile
#'
#' @noRd
mod_soilStrataImageServer <- function(id, data_upload){

   moduleServer(
    id,
    function(input, output, session){

      shinyjs::useShinyjs(html = TRUE)

      observe({
        if(is.null(data_upload())){
          shinyjs::hide("soil_strata_image_title")
          shinyjs::hide("soil_strata_explanation")
          shinyjs::hide("soil_strata_image")
        }
        else
        {
          shinyjs::show("soil_strata_image_title")
          shinyjs::show("soil_strata_explanation")
          shinyjs::show("soil_strata_image")
        }
      })

      output$soil_strata_image_title <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {
          "Soil Strata Profiles"
        }
      })

      output$soil_strata_explanation <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {

          soil_strata_explanation_text <- ""
          soil_strata_explanation_text <- paste0(soil_strata_explanation_text, "The soil strata profiles image identifies the United States Department of Agriculture (USDA) Soil Conservation Service (SCS) soil types assigned in each soil strata profile imported in the data template. ")
          soil_strata_explanation_text <- paste0(soil_strata_explanation_text, "The figure legend identifies the pattern associated with each SCS soil type in vapintr and always includes all 12 SCS soil types, regardless of the imported soil types in the site data. ")
          soil_strata_explanation_text <- paste0(soil_strata_explanation_text, "Strata thicknesses are shown in meters regardless of the units entered for each profile, and the profiles are automatically scaled to reflect the maximum depth of any of the imported profiles. ")

          sampled_log_text <- ifelse(data_upload()[[5]]$simulation_type == "DET", "In a deterministic simulation, vapintr will always use the first profile shown to characterize the subsurface.", "In each Monte Carlo iteration of a stochastic simulation, vapintr will randomly select a soil strata profile to characterize the subsurface.")

          soil_strata_explanation_text <- paste0(soil_strata_explanation_text, sampled_log_text)
          soil_strata_explanation_text <- paste0(soil_strata_explanation_text, "<br><br>")

          return(soil_strata_explanation_text)
        }
      })

      output$soil_strata_image <- renderImage({

        if (!is.null(data_upload())) {

          id <- showNotification("Building soil strata figure...", duration = NULL, closeButton = FALSE)
          on.exit(removeNotification(id), add = TRUE)

          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile <- tempfile(fileext = '.png')

          # Apply the instance specific processing to the dataframe and render
          strata_dfx <- data_upload()[[4]]

          # Create a new site model figure object
          strata_figure <- StrataProfileFig$new(strata_dfx)

          #Generate the standard output plot showing the site model figure
          strata_figure$createStandardFigure()

          pixelratio <- session$clientData$pixelratio

          #standard width and height are in inches
          fig_width <- strata_figure$fig_width*96*pixelratio
          fig_height <- strata_figure$fig_height*96*pixelratio

          # Create the PNG
          png(outfile,
              width = fig_width,
              height = fig_height,
              res = 96)
          plot(strata_figure$fig_image)

          dev.off()

          return(list(src = outfile,
                      width = fig_width,
                      height = fig_height,
                      alt = "Soil strata profile figure"))
        }
        else
        {
          outfile <- tempfile(fileext = '.png')

          png(outfile)

          dev.off()

          return(list(src = outfile,
                      width = 1,
                      height = 1,
                      alt = "Soil strata profile figure"))
        }
      }, deleteFile = TRUE)

    })
}
