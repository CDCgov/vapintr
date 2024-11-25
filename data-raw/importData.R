library(readxl)
library(dplyr)

#Get this file location
epaJEMv6SpreadsheetFilePath <- paste(getwd(), "/inst/extdata/EPA_JEM_v6_Spreadsheet.xlsm", sep="")

SCS_soil <- read_excel(epaJEMv6SpreadsheetFilePath,
                       sheet = "SOIL_DATA", skip = 4,
                       col_names = c("SCS_Type", "SCS_Symbol", "n", "nw", "rho", "Ks", "ncz", "hcz", "alpha1", "N", "M", "thetar", "mean_grain_diameter", "nw_range", "SCS_Soil_Name"),
                       n_max = 13)

#subset to only the used columns
SCS_soil <- SCS_soil[,c("SCS_Type", "n", "nw", "rho", "ncz", "hcz")]

#Note: The following four contaminants were not originally in the EPA file.
#ATSDR copied data for chemical synonyms over for these contaminants.
#The contaminant name added to the file is shown on the left of the dash, and the chemical synonym is on the right.
#Changes were made to both the CHEM_DATA and the ParametersSummary tables
#If the EPA file is ever updated to a new version, these chemicals will need to be re-added

# cis-1,3-dichloropropene (CASRN: 010061-01-5) - 1,3-Dichloropropene (CASRN: 542-75-6)
# trans-1,3-dichloropropene (CASRN: 010061-02-6) - 1,3-Dichloropropene (CASRN: 542-75-6)
# m,p-xylenes (CASRN: 179601-23-1) - Xylenes (CASRN: 1330-20-7)
# o,p-xylenes (CASRN: 136777-61-2) - Xylenes (CASRN: 1330-20-7)


chem_data <- read_excel(epaJEMv6SpreadsheetFilePath,
                        sheet = "CHEM_DATA", skip = 5,
                        col_names = c("Chemical", "CAS", "MW", "MW_source", "VP", "VP_source", "Vc", "Vc_source", "S", "S_source", "Hc25", "Hc25_source", "H_prime_25", "Da", "Da_source", "Dw", "Dw_source",
                        "Tboil", "Tboil_source", "Tcrit", "Tcrit_source", "DH_vb", "DH_vb_source", "KOC", "KOC_source",
                        "IUR", "IUR_source", "Rfc", "Rfc_source", "Mutagen", "Pet_HC_Flag", "Pet.HC.Flag_source", "Alternative_chemcal_name1", "Alternative_Chemical_Name_2"),
                        col_types = c("text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",
                                      "text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",
                                      "text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",
                                      "text",	"text",	"text",	"text")
)

#subset to only the used columns
chem_data <- chem_data[,c("Chemical", "CAS", "MW", "Vc", "S", "Hc25", "H_prime_25", "Da", "Dw", "Tboil", "Tcrit", "DH_vb", "Pet_HC_Flag")]

#coerce data to numbers (suppress warnings to avoid NA warning for text data)
chem_data[,c("MW", "Vc", "S", "Hc25", "H_prime_25", "Da", "Dw", "Tboil", "Tcrit", "DH_vb")]  <- suppressWarnings(sapply(chem_data[,c("MW", "Vc", "S", "Hc25", "H_prime_25", "Da", "Dw", "Tboil", "Tcrit", "DH_vb")], as.numeric))

usethis::use_data(SCS_soil,
                  chem_data,
                  internal = TRUE, overwrite = T)

#Get example deterministic data
jem_det_example_data_file_path <- paste(getwd(), "/inst/extdata/Deterministic_Simulation_Example_Data.xlsx", sep="")

#Import the example data
det_jem_sim_example_data <- importTemplateData(jem_det_example_data_file_path)

#Create one contaminant record for the example
det_jem_sim_example_data[[1]] <- data.frame(
  Contaminant = "Tetrachloroethylene",
  Cmedium = 20,
  Units = "ppb"
)

#Filter the data down to one soil strata log
det_jem_sim_example_data[[4]] <- det_jem_sim_example_data[[4]] %>%
  filter(LogID == "MW01")


#Get example stochastic data
jem_stoc_example_data_file_path <- paste(getwd(), "/inst/extdata/Stochastic_Simulation_Example_Data.xlsx", sep="")

#Import the example data
stoc_jem_sim_example_data <- importTemplateData(jem_stoc_example_data_file_path)

#Set the number of MC iterations to 100 for the examples so they run faster
stoc_jem_sim_example_data[[5]]$number_of_monte_carlo_iterations <- 100

usethis::use_data(SCS_soil,
                  chem_data,
                  det_jem_sim_example_data,
                  stoc_jem_sim_example_data,
                  internal = FALSE, overwrite = T)
