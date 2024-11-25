#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    htmlTemplate(
      app_sys("app/www/index.html"),

      #Tab 1
      tab1_data_import = mod_importSimulationDataUI("import_ui_1"),
      tab1_simulation_settings = mod_simulationSettingsUI("simulation_settings_ui_1"),
      tab1_csm_image = mod_CSMImageUI("csm_image_ui_1"),
      tab1_soil_strata_image = mod_soilStrataImageUI("soil_strata_image_ui_1"),
      tab1_data_calc = mod_calculateJEMUI("calculate_jem_ui_1"),

      # Tab 2
      tab2_intructions = mod_tab2InstructionsUI("tab2_instructions_ui_1"),
      tab2_sumtable = mod_resultTablesUI("results_summary_table"),
      tab2_export_simulation_data = mod_exportSimulationDataUI("export_simulation_data_ui_1"),
      tab2_export_violin_plot = mod_exportViolinPlotUI("export_violin_plot_ui_1"),
      tab2_violin_plot_drop= mod_violinPlotDropDownUI("violin_plot_drop_down_ui_1"),
      tab2_violin_plot = mod_violinPlotUI("violin_plot_ui_1")

      #body = tagList()
      # add here other template arguments
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "vapintr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
