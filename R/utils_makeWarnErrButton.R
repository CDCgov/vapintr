#' @title Make a warning and error button
#'
#' @description A helper function for creating the warning and error buttons in
#'   the shiny app
#'
#' @param id an id that corresponds to the row number
#'
#' @param warning_error the data column with the "R Warning and Error Flags" messages in it
#'
#' @param input_var argument corresponding to an id for the created button
#'
#' @return An object for the warning and error button in the results table
#'
#' @noRd

makeWarnErrButton <- function(id, warning_error, input_var) {

  if (warning_error != "") {

    which_icon <- ifelse(stringr::str_detect(warning_error, "ERRORFLAG_"), "exclamation-triangle", "exclamation-circle")

    return(as.character(actionButton(paste0("button_", id),
                                     label = "",
                                     icon = icon(which_icon, "font-awesome"),
                                     onclick = paste0('Shiny.onInputChange(\"', input_var, '\", this.id, {priority: "event"})'))))
  } else {
    return("")
  }
}
