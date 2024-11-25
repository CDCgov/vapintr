#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  jem_data <- reactiveVal(NULL)

  # Get the reactive value from the import server
  modImportReactiveVals <- mod_importSimulationDataServer("import_ui_1", jem_data)

  # simulation settings table when user uploads data
  mod_simulationSettingsServer("simulation_settings_ui_1", modImportReactiveVals)

  # conceptual site model image when user uploads data
  mod_CSMImageServer("csm_image_ui_1", modImportReactiveVals)

  # soil strata image when user uploads data
  mod_soilStrataImageServer("soil_strata_image_ui_1", modImportReactiveVals)

  # calculate jem simulation button
  mod_calculateJEMServer("calculate_jem_ui_1", data_upload = modImportReactiveVals, jem_data)

  # Instructions on tab 2
  mod_tab2InstructionsServer("tab2_instructions_ui_1", data_upload = modImportReactiveVals, jem_data)

  # Summary table on Tab 2
  violin_plot_choices <- mod_resultTablesServer("results_summary_table", data_upload = modImportReactiveVals, jem_data)

  # Export simulation data buttons
  mod_exportSimulationDataServer("export_simulation_data_ui_1", data_upload = modImportReactiveVals, jem_data)

  #Violin plot dropdown on tab 2
  selection_choice <- mod_violinPlotDropDownServer("violin_plot_drop_down_ui_1", jem_data = jem_data, data_upload = modImportReactiveVals, violin_plot_choices = violin_plot_choices)

  #Violin plot
  mod_violinPlotServer("violin_plot_ui_1", selection = selection_choice, data_upload = modImportReactiveVals, jem_data = jem_data)

  # Export violin plot button
  mod_exportViolinPlotServer("export_violin_plot_ui_1", selection = selection_choice, data_upload = modImportReactiveVals, jem_data)

}
