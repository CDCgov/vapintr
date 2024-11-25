#' @title Get standard properties for a specific chemical
#'
#' @description Gets a data frame of standard chemical properties for a specific
#'   chemical. The standard chemical properties included in vapintr were obtained
#'   from version 6.0 of USEPA's Johnson and Ettinger model spreadsheet.
#'
#' @param Chemical Chemical name. Enter a value for this parameter or for the CAS
#'   parameter to identify the chemical. If values are entered for both
#'   parameters, they must correspond to the same chemical. Only the chemical
#'   names in `chem_data$Chemical` are supported.
#'
#' @param CAS Chemical Abstract Service Registry Number. Enter a value for this
#'   parameter or for the Chemical parameter to identify the chemical. If values are
#'   entered for both parameters, they must correspond to the same chemical.
#'   Only the CAS numbers in `chem_data$CAS` are supported.
#'
#' @returns A data frame of standard chemical properties for the specified
#'   chemical.
#'
#' @examples
#' getChemicalProperties(Chemical = "Tetrachloroethylene")
#' getChemicalProperties(CAS = "127-18-4")
#'
#' @export

getChemicalProperties <- function(Chemical = NA, CAS = NA){

  stopifnot(length(Chemical) <= 1 & length(CAS) <= 1)

  if(all(is.na(c(Chemical, CAS)))) stop("Chemical and CAS missing.")
  if(!is.na(Chemical)) stopifnot(Chemical %in% chem_data$Chemical)

  #Remove leading zeros from CASNo before chem_data comparison
  CAS <- sub("^0+", "", CAS)

  if(!is.na(CAS)) stopifnot(CAS %in% chem_data$CAS)

  if(!is.na(Chemical) & !is.na(CAS)){

    chemical_data <- chem_data[chem_data$Chemical == Chemical &
                                 chem_data$CAS== CAS,]
    stopifnot(dim(chemical_data)[[1]] == 1)
  }

  if(!is.na(Chemical) & is.na(CAS)){

    chemical_data <- chem_data[chem_data$Chemical == Chemical,]
    stopifnot(dim(chemical_data)[[1]] == 1)

  }

  if(is.na(Chemical) & !is.na(CAS)){

    chemical_data <- chem_data[chem_data$CAS == CAS,]
    stopifnot(dim(chemical_data)[[1]] == 1)

  }

  return(chemical_data)

}
