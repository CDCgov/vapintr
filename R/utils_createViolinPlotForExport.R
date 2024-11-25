#' @title Create a violin plot for export
#'
#' @description Creates a violin plot image for display in the shiny app and
#'   export to a png file
#'
#' @param data_upload Data uploaded to the shiny app
#'
#' @param jem_data Calculated JEM results within the shiny app
#'
#' @param contaminant The contaminant name for display in the violin plot
#'
#' @param units The violin plot's concentration units
#'
#' @param pixelratio The computer's pixelratio, imported from the session settings
#'
#' @returns A list containing a pointer to the saved image, the image width, the
#'   image height, and the image alt text
#'
#' @noRd

createViolinPlotForExport <- function(data_upload, jem_data, contaminant, units, pixelratio){

  # A temp file to save the output.
  # This file will be removed later by renderImage
  # set a temporary working directory

  outfile <- paste0(tempdir(), "\\", paste0(contaminant, "_", gsub("/", "", units), ".png"))

  #get medium
  medium <- tolower(data_upload()[[5]]$source_medium)

  #get measured data for the contaminant
  measured_data <- data_upload()[[1]]

  #get modeled data for the contaminant
  for(i in 1:length(jem_data()))
  {
    if(jem_data()[[i]]$Contaminant == contaminant){
      contaminant_index <- i
      break
    }
  }

  #Contaminant should have a standard name here since it's the name returned from vapintr
  #As a result, get the contaminant CASRN
  contaminant_casrn <- vapintr::chem_data[vapintr::chem_data$Chemical == contaminant, ]$CAS

  mcResults <- jem_data()[[contaminant_index]]$JEMResults

  #get reference values for the contaminant
  reference_data <- data_upload()[[6]]

  #Create plot
  result_figure <- ModelResultFig$new(mcResults, contaminant, contaminant_casrn, units, medium, measured_data, reference_data)
  result_figure$createStandardFigure()

  #standard width and height are in inches
  fig_width <- result_figure$fig_width*96*pixelratio
  fig_height <- result_figure$fig_height*96*pixelratio

  # Create the PNG
  png(outfile,
      width = fig_width,
      height = fig_height,
      res = 96)
  plot(result_figure$fig_image)

  dev.off()

  return(list(src = outfile,
              width = fig_width,
              height = fig_height,
              alt = paste(contaminant, "data plot")))

}


