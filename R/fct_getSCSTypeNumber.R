#' @title Get a soil type index for the soil type property reference table
#'
#' @description Converts a United States Department of Agriculture Soil
#'   Conservation Service (SCS) soil type into a numeric index for looking up
#'   properties in the SCS reference table (`SCS_soil`).
#'
#' @param scs_type The SCS soil type.
#'
#' @returns The numeric SCS soil type index
#'
#' @examples
#' getSCSTypeNumber("Silty Clay")
#'
#' @export


getSCSTypeNumber = function(scs_type) {
  stopifnot(class(scs_type) %in% c("numeric", "integer", "character", "mcnode"))

  if (is(scs_type, "character")) {
    if (!scs_type %in% SCS_soil$SCS_Type) {
      stop(paste0(
        scs_type,
        " not found. It must either be one of:\n",
        paste(SCS_soil$SCS_Type, collapse = "\n")
      ))
    }else{

      scs_type <- as.numeric(which(scs_type == SCS_soil$SCS_Type))
    }
  } else{
    if (is(scs_type,"numeric")) {
      if (scs_type > 13 | scs_type < 1) {
        stop(paste0(
          "Numeric index must correspond to:\n",
          paste0(
            paste(1:13, SCS_soil$SCS_Type, sep = ":"),
            collapse = "\n"
          )
        ))
      }
    }
  }

  return(scs_type)
}
