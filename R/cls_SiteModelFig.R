#' @title Create conceptual site model figures
#'
#' @description An R6 Class used to build standard conceptual site model figures
#'   based on the settings data inputs for `runJE()`. For more information on
#'   this class and how to use it, see the following vignette.
#'
#'   `vignette("ConceptualSiteModelAndSoilStrataFigures", package = "vapintr")`
#'
#' @examples
#' #Get settings data from the example data
#' settings_data <- det_jem_sim_example_data[[5]]
#'
#' #Initialize the site model figure object
#' site_model_figure <- SiteModelFig$new(settings_data)
#'
#' #Create the standard image using the SiteModelFig object
#' site_model_figure$createStandardFigure()
#'
#' #Plot the standard image
#' plot(site_model_figure$fig_image)
#'
#' @export

SiteModelFig <- R6::R6Class(
  classname = "SiteModelFig",
  inherit = StandardOutputFig,
  public = list(

    #' @field settings_data_dfx Data frame of simulation settings data. Should
    #'   have the same format as the settings_data parameter from `runJE()`.
    #' @field site_model_fig Site model image for plotting

    settings_data_dfx = NULL,
    site_model_fig = NULL,

    #' @description Function to create a new `SiteModelFig` object
    #'
    #' @param settings_data_dfx Data frame of simulation settings data. Should
    #'   have the same format as the settings_data parameter from `runJE()`.
    initialize = function(settings_data_dfx){

      self$settings_data_dfx <- settings_data_dfx
      private$buildSiteModelFig()

    },
    #' @description Assigns values to the standard properties inherited from the
    #'   `StandardOutputFig` class used to render the figure at a recommended size.
    createStandardFigure = function(){

      self$fig_image <- self$site_model_fig
      self$fig_width <- 6.5
      self$fig_height <- 6
    }
  ),
  private = list(
    buildSiteModelFig = function(){

      ################# General variables ##################

      box_width <- 6.5
      box_height <- 6

      fill_lx <- list(
        subsurface = "#DACEAE",
        building = "#F5EBE7",
        sky = "#E2E9F6",
        slab = "#737373",
        roof = "#C9C9C9",
        groundwater = "#B4C7E7",
        soil_gas = "black"
      )

      isResidential <- self$settings_data_dfx$building_setting == "Residential"
      isCommercial <- self$settings_data_dfx$building_setting == "Commercial"

      isGroundwater <- self$settings_data_dfx$source_medium == "Groundwater"
      isExteriorSoilGas <- self$settings_data_dfx$source_medium == "Exterior Soil Gas"
      isSubslabSoilGas <- self$settings_data_dfx$source_medium == "Subslab Soil Gas"

      hasBasement <- self$settings_data_dfx$foundation_type == "Basement-slab" || self$settings_data_dfx$foundation_type == "Basement-dirt"
      hasCrawlspace <- self$settings_data_dfx$foundation_type == "Crawlspace-slab" || self$settings_data_dfx$foundation_type == "Crawlspace-dirt"
      hasSlabOnGrade <- self$settings_data_dfx$foundation_type == "Slab-grade"

      ################# Image setup ##################

      #Initialize site model image with bounding box to establish grid (6.5 wide by 6 tall)
      bounding_box_df <- data.frame(x = c(0, 0, box_width, box_width),
                                    y = c(box_height, 0, 0, box_height))

      site_model_fig <- ggplot() +
        theme_void() +
        geom_polygon(data = bounding_box_df, mapping = aes(x = x, y = y), fill = "white")

      ground_surface_height <- 0.6*box_height

      #Add box for the subsurface and sky area
      subsurface_box_df <- data.frame(x = c(0, 0, box_width, box_width),
                                      y = c(ground_surface_height, 0, 0, ground_surface_height))

      site_model_fig <- site_model_fig +
        geom_polygon(data = subsurface_box_df, mapping = aes(x = x, y = y), fill = fill_lx$subsurface, color = "black")

      sky_box_df <- data.frame(x = c(0, 0, box_width, box_width),
                                      y = c(box_height, ground_surface_height, ground_surface_height, box_height))

      site_model_fig <- site_model_fig +
        geom_polygon(data = sky_box_df, mapping = aes(x = x, y = y), fill = fill_lx$sky, color = "black")

      ################# Building ##################

      #Add box for the building
      if(isResidential){

        bldg_left_wall <- 0.35*box_width
        bldg_right_wall <- 0.65*box_width
        inner_space_top_height <- 0.8*box_height

        #Build house base
        house_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                      y = c(inner_space_top_height, ground_surface_height, ground_surface_height, inner_space_top_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = house_df, mapping = aes(x = x, y = y), fill = fill_lx$building, color = "black")

        #Build house chimney
        chimney_df <- data.frame(x = c(bldg_right_wall - 0.05*box_width, bldg_right_wall - 0.05*box_width, bldg_right_wall, bldg_right_wall),
                                 y = c(0.95*box_height, inner_space_top_height, inner_space_top_height, 0.95*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = chimney_df, mapping = aes(x = x, y = y), fill = fill_lx$roof, color = "black")

        #Build house roof
        roof_df <- data.frame(x = c(bldg_left_wall - 0.05*box_width, bldg_right_wall + 0.05*box_width, 0.5*box_width),
                                   y = c(inner_space_top_height, inner_space_top_height, 0.95*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = roof_df, mapping = aes(x = x, y = y), fill = fill_lx$roof, color = "black")

      #Else commercial
      } else {

        bldg_left_wall <- 0.3*box_width
        bldg_right_wall <- 0.7*box_width
        inner_space_top_height <- 0.9*box_height

        #Build commercial building base
        commercial_building_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                               y = c(inner_space_top_height, ground_surface_height, ground_surface_height, inner_space_top_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = commercial_building_df, mapping = aes(x = x, y = y), fill = fill_lx$building, color = "black")

        #Build commercial building roof
        roof_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                              y = c(0.95*box_height, inner_space_top_height, inner_space_top_height, 0.95*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = roof_df, mapping = aes(x = x, y = y), fill = fill_lx$roof, color = "black")

      }

      ################# Foundation ##################

      foundation_top_height <- NA
      foundation_bottom_height <- NA

      #Slab-on-grade
      if(hasSlabOnGrade){

        foundation_top_height <- ground_surface_height
        foundation_bottom_height <- foundation_top_height - 0.01 * box_height

        slab_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                       y = c(foundation_top_height, foundation_bottom_height, foundation_bottom_height, foundation_top_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = slab_df, mapping = aes(x = x, y = y), fill = "#737373", color = "black")

      #Basement (slab or dirt)
      } else if (hasBasement) {

        basement_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                  y = c(ground_surface_height, 0.5 * box_height, 0.5 * box_height,ground_surface_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = basement_df, mapping = aes(x = x, y = y), fill = fill_lx$building, color = "black")

        basement_left_wall_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_left_wall + 0.01 * box_width, bldg_left_wall + 0.01 * box_width),
                                            y = c(ground_surface_height, 0.5 * box_height, 0.5 * box_height, ground_surface_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = basement_left_wall_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")

        basement_right_wall_df <- data.frame(x = c(bldg_right_wall, bldg_right_wall, bldg_right_wall - 0.01 * box_width, bldg_right_wall - 0.01 * box_width),
                                            y = c(ground_surface_height, 0.5 * box_height, 0.5 * box_height, ground_surface_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = basement_right_wall_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")

        #Draw stairs
        stairs_df <- data.frame(x = c(bldg_left_wall + 0.03*box_width,
                                      bldg_left_wall + 0.03*box_width,
                                      bldg_left_wall + 0.05*box_width,
                                      bldg_left_wall + 0.05*box_width,
                                      bldg_left_wall + 0.07*box_width,
                                      bldg_left_wall + 0.07*box_width,
                                      bldg_left_wall + 0.09*box_width,
                                      bldg_left_wall + 0.09*box_width,
                                      bldg_left_wall + 0.11*box_width,
                                      bldg_left_wall + 0.11*box_width),
                               y = c(ground_surface_height,
                                     ground_surface_height - 0.02*box_height,
                                     ground_surface_height - 0.02*box_height,
                                     ground_surface_height - 0.04*box_height,
                                     ground_surface_height - 0.04*box_height,
                                     ground_surface_height - 0.06*box_height,
                                     ground_surface_height - 0.06*box_height,
                                     ground_surface_height - 0.08*box_height,
                                     ground_surface_height - 0.08*box_height,
                                     ground_surface_height - 0.10*box_height))

        site_model_fig <- site_model_fig +
          geom_path(data = stairs_df, mapping = aes(x = x, y = y))


        #Add slab for basement w/ slab
        if (self$settings_data_dfx$foundation_type == "Basement-slab"){

          foundation_top_height <- 0.5*box_height
          foundation_bottom_height <- foundation_top_height - 0.01 * box_height

          slab_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                y = c(foundation_top_height, foundation_bottom_height, foundation_bottom_height, foundation_top_height))

          site_model_fig <- site_model_fig +
            geom_polygon(data = slab_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")
        } else {
          foundation_top_height <- 0.5 * box_height
          foundation_bottom_height <- 0.5 * box_height
        }


      #Crawlspace (slab or dirt)
      } else if (hasCrawlspace) {

        crawlspace_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                  y = c(0.62*box_height, 0.57*box_height, 0.57*box_height, 0.62*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = crawlspace_df, mapping = aes(x = x, y = y), fill = fill_lx$building, color = "black")

        crawlspace_left_wall_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_left_wall + 0.01 * box_width, bldg_left_wall + 0.01 * box_width),
                                            y = c(0.62*box_height, 0.57*box_height, 0.57*box_height, 0.62*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = crawlspace_left_wall_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")

        crawlspace_right_wall_df <- data.frame(x = c(bldg_right_wall, bldg_right_wall, bldg_right_wall - 0.01 * box_width, bldg_right_wall - 0.01 * box_width),
                                             y = c(0.62*box_height, 0.57*box_height, 0.57*box_height, 0.62*box_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = crawlspace_right_wall_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")

        #Add slab for crawlspace w/ slab
        if (self$settings_data_dfx$foundation_type == "Crawlspace-slab"){

          foundation_top_height <- 0.57 * box_height
          foundation_bottom_height <- 0.56 * box_height

          slab_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall, bldg_right_wall, bldg_right_wall),
                                y = c(0.57*box_height, 0.56*box_height, 0.56*box_height, 0.57*box_height))

          site_model_fig <- site_model_fig +
            geom_polygon(data = slab_df, mapping = aes(x = x, y = y), fill = fill_lx$slab, color = "black")
        } else {

          foundation_top_height <- 0.57 * box_height
          foundation_bottom_height <- 0.57 * box_height

        }
      }

      ################# Contaminant source ##################

      source_medium_options <- c("Groundwater", "Exterior Soil Gas", "Subslab Soil Gas")

      groundwater_box_top_height <- 0.15*box_height
      capillary_zone_box_top_height <- 0.2*box_height

      if(isGroundwater){

        #Groundwater source box
        groundwater_df <- data.frame(x = c(0, 0, box_width, box_width),
                                      y = c(groundwater_box_top_height, 0, 0, groundwater_box_top_height))

        site_model_fig <- site_model_fig +
          geom_polygon(data = groundwater_df, mapping = aes(x = x, y = y), fill = fill_lx$groundwater, color = "black")

        #Add capillary zone
        if(self$settings_data_dfx$simulate_capillary_zone == TRUE){
          capillary_zone_df <- data.frame(x = c(0, 0, box_width, box_width),
                                          y = c(capillary_zone_box_top_height, groundwater_box_top_height, groundwater_box_top_height, capillary_zone_box_top_height))

          site_model_fig <- site_model_fig +
            geom_polygon(data = capillary_zone_df, mapping = aes(x = x, y = y), fill = fill_lx$groundwater, color = "black", alpha = 0.4)
        }
      #Soil gas samples
      } else {

        soil_gas_sample_x <- ifelse(isExteriorSoilGas, 0.12*box_width, bldg_left_wall + 0.6*(0.5*box_width - bldg_left_wall))
        soil_gas_sample_y <- ifelse(isExteriorSoilGas, 0.1*box_height, foundation_bottom_height - 0.03*box_height)

        soil_gas_sample_top_height <- soil_gas_sample_y + 0.01*box_height
        soil_gas_sample_bottom_height <- soil_gas_sample_y - 0.01*box_height
        soil_gas_sample_left_edge <- soil_gas_sample_x - 0.01*box_width
        soil_gas_sample_right_edge <- soil_gas_sample_x + 0.01*box_width

        #Soil gas sample box
        soil_gas_sample_df <- data.frame(x = c(soil_gas_sample_left_edge, soil_gas_sample_left_edge, soil_gas_sample_right_edge, soil_gas_sample_right_edge),
                                     y = c(soil_gas_sample_top_height, soil_gas_sample_bottom_height, soil_gas_sample_bottom_height, soil_gas_sample_top_height))

        #Add soil gas sample
        site_model_fig <- site_model_fig +
          geom_polygon(data = soil_gas_sample_df, mapping = aes(x = x, y = y), fill = fill_lx$soil_gas, color = "black", alpha = 0.4)
      }


      #Source arrows data frame
      left_source_arrow_x <- 0.4 * box_width
      mid_source_arrow_x <- 0.5 * box_width
      right_source_arrow_x <- 0.6 * box_width

      if (isGroundwater){
        bottom_source_arrow_y <- 0.1 * box_height
        top_source_arrow_y <- 0.25 * box_height
      # } else if (isExteriorSoilGas) {
      #   bottom_source_arrow_y <- 0.02 * box_height
      #   top_source_arrow_y <- 0.17 * box_height
      } else {
        bottom_source_arrow_y <- 0.1 * box_height
        top_source_arrow_y <- 0.25 * box_height
      }

      source_arrows_df <- data.frame(
        x = c(left_source_arrow_x, mid_source_arrow_x, right_source_arrow_x),
        y = c(bottom_source_arrow_y, bottom_source_arrow_y, bottom_source_arrow_y),
        xend = c(left_source_arrow_x, mid_source_arrow_x, right_source_arrow_x),
        yend = c(top_source_arrow_y, top_source_arrow_y, top_source_arrow_y)
      )

      #Add source arrows
      site_model_fig <- site_model_fig +
        geom_segment(data = source_arrows_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                     linewidth = 1,
                     lineend = "round",
                     linejoin = "round")

      ################# Zone and source arrow labels ##################

      unsaturated_zone_height <- case_when(isGroundwater &
                                             self$settings_data_dfx$simulate_capillary_zone == TRUE ~ capillary_zone_box_top_height + 0.03*box_height,
                                           isGroundwater &
                                             self$settings_data_dfx$simulate_capillary_zone == FALSE ~ groundwater_box_top_height + 0.03*box_height,
                                           TRUE ~ 0.03*box_height)

      #Zone labels common to all images
      zone_label_df <- data.frame(
        x = c(box_width - 0.01 * box_width,
              box_width - 0.01 * box_width,
              bldg_right_wall - 0.01 * box_width,
              0.5*box_width),
        y = c(unsaturated_zone_height,
              box_height - 0.02*box_height,
              inner_space_top_height - 0.01*box_height,
              top_source_arrow_y + 0.03*box_height),
        label = c("Unsaturated Zone",
                  "Outdoor Air",
                  "Indoor Air",
                  "Contaminated Soil Vapor Diffusion")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = zone_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("right", "right", "right", "center"),
                  vjust = c("center", "center", "top", "center"),
                  size = 4)

      #Groundwater zone labels
      if(isGroundwater){
        zone_label_df <- data.frame(
          x = c(box_width - 0.01 * box_width),
          #y = c(groundwater_box_top_height - 0.025*box_height),
          y = c(0.03*box_height),
          label = c("Contaminated Groundwater")
        )

        site_model_fig <- site_model_fig +
          geom_text(data = zone_label_df,
                    mapping = aes(x = x, y = y, label = label),
                    hjust = c("right"),
                    size = 4)

        if(self$settings_data_dfx$simulate_capillary_zone == TRUE){
          zone_label_df <- data.frame(
            x = c(box_width - 0.01 * box_width),
            y = c(capillary_zone_box_top_height - 0.023*box_height),
            label = c("Capillary Zone")
          )

            site_model_fig <- site_model_fig +
              geom_text(data = zone_label_df,
                        mapping = aes(x = x, y = y, label = label),
                        hjust = c("right"),
                        size = 4)
        }
      #Soil gas sample label
      } else {
        soil_gas_sample_label_df <- data.frame(
          x = c(soil_gas_sample_x + ifelse(isExteriorSoilGas,0.025,0.01)*box_width),
          y = c(soil_gas_sample_y - ifelse(isExteriorSoilGas,0,0.02)*box_height),
          label = c(paste(ifelse(isExteriorSoilGas,"Exterior","Subslab"), "Soil\nGas Sample"))
        )

        site_model_fig <- site_model_fig +
          geom_text(data = soil_gas_sample_label_df,
                    mapping = aes(x = x, y = y, label = label),
                    hjust = c(ifelse(isExteriorSoilGas,"left","left")),
                    vjust = c(ifelse(isExteriorSoilGas,"center","top")),
                    size = 4)
      }


      ################# Qsoil and Qb information ###############
      Qsoil_right_arrow_df <- data.frame(x = bldg_right_wall + 0.01 * box_width,
                                        y = foundation_bottom_height - ifelse(self$settings_data_dfx$foundation_type %in% c("Crawlspace-dirt", "Basement-dirt"),0.06, 0.05)*box_height,
                                        xend = bldg_right_wall - 0.06 * box_width,
                                        yend = foundation_bottom_height + ifelse(self$settings_data_dfx$foundation_type %in% c("Crawlspace-dirt", "Basement-dirt"),0.04, 0.05)*box_height)

      text_above_foundation_height <- Qsoil_right_arrow_df$yend - 0.015*box_height

      site_model_fig <- site_model_fig +
        geom_curve(data = Qsoil_right_arrow_df,
                   mapping = aes(x = x, y = y, xend = xend, yend = yend),
                   arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                   linewidth = 1,
                   lineend = "round",
                   curvature = -0.5,
                   angle = 90)

      #Qb arrows data frame
      Qb_arrow_df <- data.frame(x = bldg_right_wall - 0.08 * box_width,
                                         y = ground_surface_height + 0.5*(inner_space_top_height - ground_surface_height),
                                         xend = bldg_right_wall + 0.08 * box_width,
                                         yend = ground_surface_height + 0.5*(inner_space_top_height - ground_surface_height))

      #Add Qb arrow
      site_model_fig <- site_model_fig +
        geom_segment(data = Qb_arrow_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
                     linewidth = 1,
                     lineend = "round",
                     linejoin = "round")

      #Advection and diffusion label
      advec_diff_label_df <- data.frame(
        x = c(Qsoil_right_arrow_df$x + 0.005 * box_width),
        y = c(Qsoil_right_arrow_df$y),
        label = c("Advection and Diffusion")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = advec_diff_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = "left",
                  size = 4)

      #Qsoil and Qb labels
      Qsoil_Qb_label_df <- data.frame(
        x = c(Qsoil_right_arrow_df$xend - ifelse(hasBasement, 0, 0.02) * box_width,
              Qb_arrow_df$xend + 0.01 * box_width),
        y = c(text_above_foundation_height + ifelse(hasBasement, 0.04, 0) * box_height,
              Qb_arrow_df$yend + 0.03*box_height),
        label = c("Q[soil]", "Q[b]")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Qsoil_Qb_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c(ifelse(hasBasement, "center", "right"), "center"),
                  size = 4,
                  parse = TRUE)

      ################# Abf, eta, Hb, and ach labels ###############

      #Abf and eta labels
      Abf_eta_label_df <- data.frame(
        x = c(bldg_left_wall + ifelse(hasBasement, 0.12, ifelse(hasSlabOnGrade,0.01,0.02)) * box_width),
        y = c(text_above_foundation_height),
        # label = c("A[bf]*`,`*~eta")
        label = c('A[bf]*","*~"eta"')
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Abf_eta_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("left"),
                  size = 4,
                  parse = TRUE)

      #Hb and ach labels
      Hb_ach_label_df <- data.frame(
        x = c(bldg_left_wall + 0.01 * box_width),
        y = c(inner_space_top_height - 0.01*box_height),
        label = c("H[b]*`,`*~ach")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Hb_ach_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("left"),
                  vjust = c("top"),
                  size = 4,
                  parse = TRUE)

      ################# Ls arrows and labels ################

      Ls_bottom_height <- ifelse(isGroundwater, groundwater_box_top_height, soil_gas_sample_y)

      #Ls arrows
      Ls_arrows_df <- data.frame(x = c(0.06*box_width, 0.06*box_width),
                                 y = c(ground_surface_height, Ls_bottom_height),
                                 xend = c(0.06*box_width, 0.06*box_width),
                                 yend = c(Ls_bottom_height, ground_surface_height))

      site_model_fig <- site_model_fig +
        geom_segment(data = Ls_arrows_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
                     linewidth = 0.5,
                     lineend = "round",
                     linejoin = "round")

      #Ls bottom line
      if(isExteriorSoilGas | isSubslabSoilGas){
        Ls_bottom_line_df <- data.frame(x = c(0.04*box_width),
                                   y = c(Ls_bottom_height),
                                   xend = c(soil_gas_sample_x - 0.02*box_width),
                                   yend = c(Ls_bottom_height))

        site_model_fig <- site_model_fig +
          geom_segment(data = Ls_bottom_line_df,
                       mapping = aes(x = x, y = y, xend = xend, yend = yend),
                       linewidth = 0.5)
      }


      #Ls label
      Ls_label_df <- data.frame(
        x = c(0.05*box_width),
        y = c(Ls_bottom_height + 0.5*(ground_surface_height-Ls_bottom_height)),
        label = c("L[s]")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Ls_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("right"),
                  size = 4,
                  parse = TRUE)

      ################# Lb and Lf ####################

      Lb_arrow_x <- 0.13*box_width
      Lf_arrow_x <- 0.20*box_width

      #Foundation lines
      foundation_line_df <- data.frame(x = c(bldg_left_wall, bldg_left_wall),
                                       y = c(foundation_bottom_height, foundation_top_height),
                                       xend = c(Lb_arrow_x - 0.01 * box_width, Lf_arrow_x - 0.01*box_width),
                                       yend = c(foundation_bottom_height, foundation_top_height))

      site_model_fig <- site_model_fig +
        geom_segment(data = foundation_line_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     linewidth = 0.5)

      #Lb arrows
      Lb_arrows_df <- data.frame(x = c(Lb_arrow_x,
                                       Lb_arrow_x),
                                 y = c(ifelse(hasBasement, ground_surface_height, foundation_bottom_height - ifelse(isSubslabSoilGas,0.06,0.04)*box_height),
                                       ifelse(hasBasement,foundation_bottom_height, ground_surface_height + 0.04*box_height)),
                                 xend = c(Lb_arrow_x,
                                          Lb_arrow_x),
                                 yend = c(foundation_bottom_height,
                                          ground_surface_height))

      site_model_fig <- site_model_fig +
        geom_segment(data = Lb_arrows_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
                     linewidth = 0.5,
                     lineend = "round",
                     linejoin = "round")

      #Establish height of the Lb label
      Lb_label_y <- ifelse(hasBasement, 0.5 * (foundation_bottom_height + ground_surface_height),
                           foundation_bottom_height - ifelse(isSubslabSoilGas,0.06,0.04)*box_height)

      #Lb label
      Lb_label_df <- data.frame(
        x = c(Lb_arrow_x - ifelse(hasBasement, 0.01, 0.02) * box_width),
        y = c(Lb_label_y),
        label = c("L[b]")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Lb_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("right"),
                  size = 4,
                  parse = TRUE)


      #Add additional Lb label line for slab on grade
      if(!hasBasement){
        Lb_line_df <- data.frame(x = c(Lb_arrow_x),
                                 y = c(Lb_label_y),
                                 xend = c(Lb_arrow_x - 0.01 * box_width),
                                 yend = c(Lb_label_y))

        site_model_fig <- site_model_fig +
          geom_segment(data = Lb_line_df,
                       mapping = aes(x = x, y = y, xend = xend, yend = yend),
                       linewidth = 0.5)

      }

      #Lf arrows
      Lf_arrows_df <- data.frame(x = c(Lf_arrow_x,
                                       Lf_arrow_x),
                                 y = c(foundation_bottom_height -ifelse(isSubslabSoilGas && !hasBasement, 0.06,0.04)*box_height,
                                       foundation_top_height + 0.04*box_height),
                                 xend = c(Lf_arrow_x,
                                          Lf_arrow_x),
                                 yend = c(foundation_bottom_height,
                                          foundation_top_height))

      site_model_fig <- site_model_fig +
        geom_segment(data = Lf_arrows_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
                     linewidth = 0.5,
                     lineend = "round",
                     linejoin = "round")

      #Add additional Lf line if it has a basement
      Lf_label_y <- ifelse(hasBasement, foundation_top_height + 0.04*box_height,
                           foundation_bottom_height - ifelse(isSubslabSoilGas,0.06,0.04)*box_height)

      Lf_line_df <- data.frame(x = c(Lf_arrow_x),
                               y = c(Lf_label_y),
                               xend = c(Lf_arrow_x - 0.01 * box_width),
                               yend = c(Lf_label_y))

      site_model_fig <- site_model_fig +
        geom_segment(data = Lf_line_df,
                     mapping = aes(x = x, y = y, xend = xend, yend = yend),
                     linewidth = 0.5)

      #Lf label
      Lf_label_df <- data.frame(
        x = c(Lf_arrow_x - 0.02 * box_width),
        y = c(Lf_label_y),
        label = c("L[f]")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Lf_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  hjust = c("right"),
                  size = 4,
                  parse = TRUE)

      ###################### Ts ######################

      Ts_label_x <- 0.03*box_width
      Ts_label_y <- 0.03*box_height

      Ts_label_df <- data.frame(
        x = c(Ts_label_x),
        y = c(Ts_label_y),
        label = c("T[s]")
      )

      site_model_fig <- site_model_fig +
        geom_text(data = Ts_label_df,
                  mapping = aes(x = x, y = y, label = label),
                  size = 4,
                  parse = TRUE)

      ################# Save figure ##################

      self$site_model_fig <- site_model_fig

    }
  )
)


