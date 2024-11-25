#' UI function for generating the conceptual site model image
#'
#' @description A shiny module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_CSMImageUI <- function(id){

  ns <- NS(id)
  tagList(

      tags$h3(textOutput(ns("csm_image_title"))),
      tags$div(htmlOutput(ns("csm_explanation"))),
      tags$div(plotOutput(ns("csm_image"), width = "100%", height = "100%"))
  )

}

#' Server function for generating the conceptual site model image
#'
#' @noRd
mod_CSMImageServer <- function(id, data_upload){

   moduleServer(
    id,
    function(input, output, session){

      shinyjs::useShinyjs(html = TRUE)

      observe({
        if(is.null(data_upload())){
          shinyjs::hide("csm_image_title")
          shinyjs::hide("csm_explanation")
          shinyjs::hide("csm_image")
        }
        else
        {
          shinyjs::show("csm_image_title")
          shinyjs::show("csm_explanation")
          shinyjs::show("csm_image")
        }
      })

      output$csm_image_title <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {
          "Conceptual Site Model"
        }
      })

      output$csm_explanation <- renderText({
        if (is.null(data_upload())){
          return(NULL)
        } else {

          csm_explanation_text <- ""
          csm_explanation_text <- paste0(csm_explanation_text, "The conceptual site model depicts the simulated site and building based on the imported simulation settings. It also shows the building and subsurface input parameters used to run the JEM. ")
          csm_explanation_text <- paste0(csm_explanation_text, "Of the parameters shown, <i>H<sub>b</sub></i> is the enclosed space mixing height, <i>ach</i> is the indoor air exchange rate, <i>A<sub>bf</sub></i> is the surface area of the subsurface floor and walls, and <i>eta</i> is the fraction of the foundation area with cracks. ")
          csm_explanation_text <- paste0(csm_explanation_text, "<i>Q<sub>soil</sub></i> is the subsurface flowrate of soil gas into the building, and <i>Q<sub>b</sub></i> is the ventilation rate of air to the outdoors. In vapintr's data import template, <i>Q<sub>soil</sub></i> and <i>Q<sub>b</sub></i> are represented by a single variable, <i>Q<sub>soil</sub>_Q<sub>b</sub></i>, which is defined as the ratio of <i>Q<sub>soil</sub></i> divided by <i>Q<sub>b</sub></i>. ")
          csm_explanation_text <- paste0(csm_explanation_text, "Towards the image's left side, <i>L<sub>b</sub></i> represents the depth from ground surface to the bottom of the foundation, and <i>L<sub>f</sub></i> is the foundation thickness. ")

          ls_ts_text <- ifelse(data_upload()[[5]]$source_medium == "Groundwater", "For groundwater simulations, <i>L<sub>s</sub></i> represents the depth from ground surface to the top of the water table, and <i>T<sub>s</sub></i> is the average groundwater temperature. ", "For soil gas simulations, <i>L<sub>s</sub></i> represents the depth from ground surface to the soil gas sample depth, and <i>T<sub>s</sub></i> is the average vadose zone temperature. ")

          csm_explanation_text <- paste0(csm_explanation_text, ls_ts_text)
          csm_explanation_text <- paste0(csm_explanation_text, "The image is not to scale.")


          return(csm_explanation_text)
        }
      })

      output$csm_image <- renderImage({

        if (!is.null(data_upload())) {

          id <- showNotification("Building conceptual site model figure...", duration = NULL, closeButton = FALSE)
          on.exit(removeNotification(id), add = TRUE)

          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile <- tempfile(fileext = '.png')

          # Apply the instance specific processing to the dataframe and render
          settings_dfx <- data_upload()[[5]]

          # Create a new site model figure object
          site_model_figure <- SiteModelFig$new(settings_dfx)

          #Generate the standard output plot showing the site model figure
          site_model_figure$createStandardFigure()

          pixelratio <- session$clientData$pixelratio

          #standard width and height are in inches
          fig_width <- site_model_figure$fig_width*96*pixelratio
          fig_height <- site_model_figure$fig_height*96*pixelratio

          # Create the PNG
          png(outfile,
              width = fig_width,
              height = fig_height,
              res = 96)
          plot(site_model_figure$fig_image)

          dev.off()

          return(list(src = outfile,
                      width = fig_width,
                      height = fig_height,
                      alt = "Conceptual site model figure"))
        }
        else
        {
          outfile <- tempfile(fileext = '.png')

          png(outfile)

          dev.off()

          return(list(src = outfile,
                      width = 1,
                      height = 1,
                      alt = "Conceptual site model figure"))
        }
      }, deleteFile = TRUE)

    })
}
