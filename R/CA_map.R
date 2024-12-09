#' Create a map of California with LMP prices and Transmission Lines
#' 
#' This function creates a map in a Shiny App of California with LMP prices and transmission lines. The function uses the `sf`, `leaflet`, `shiny`, and `rnaturalearth` packages, and allows the map to be interactive to view different aspects of the price and different types of transmission lines. This can be used to visualize where there are high electricity prices in the state at a certain time and how the transmission lines are distributed / how they are related to those prices. The user inputs a dataframe of the LMP prices of one time period (one 15 minute interval) and the function will plot the map with the LMP prices and transmission lines. The baseline instance (time period) is a typical day in California at 1pm, from the csv instance_normal.csv file. However, the user can make their own dataframe with the function `pulldata_instance` and then call it with this function to visualize it. 
#' 
#' @param instance A dataframe of the LMP prices of one time period (one 15 minute interval). The default dataframe is a normal day in California at 1pm. Users can change this dataframe to analyze a different instance that pulled from the `pulldata_instance` function. Or, to visualize the highest peak load in the last three years, use: "inst/extdata/instance_peak_load.csv"
#' 
#' @return A Shiny app page with a map of California and the prices.
#' 
#' @examples
#' CA_map()
#' 
#' @importFrom sf st_read
#' @importFrom leaflet colorBin addProviderTiles setView addPolygons addPolylines addCircleMarkers addLayersControl layersControlOptions renderLeaflet leafletOutput
#' @importFrom shiny shinyApp fluidPage tags 
#' @importFrom rnaturalearth ne_states
#' @importFrom utils read.csv
#' @importFrom tools toTitleCase
#' 
#' @export
CA_map <- function(instance = read.csv(system.file("extdata", "instance_normal.csv", package = "CAISOWrapR"))) {
  # Get the geometries for the United States
  
  if (length(instance) == 1 && instance == "peak_load") {
    instance = read.csv(system.file("extdata", "instance_peak_load.csv", package = "CAISOWrapR"))
  }
  
  if (!is.data.frame(instance)) {
    stop("Error: 'instance' must either be 'peak_load' or a data frame.")
  }

  us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  
  # Filter for California using the correct column name
  CA_states <- us_states[us_states$name %in% c("California"), ]
  
  # Load the transmission line shapefiles
  df_transmission <- sf::st_read("data/all_transmission_lines.shp")
  df_transmission_500 <- sf::st_read("data/500kv_transmission_lines.shp")
  
  # Merge LMP locations data with the instance dataframe
  instance <- merge_lmp_locations(instance)
  
  # Convert LMP, congestion, and losses to numeric
  instance$lmp <- as.numeric(instance$lmp)
  instance$congestion <- as.numeric(instance$congestion)
  instance$loss <- as.numeric(instance$loss)
  
  # Create color palettes for the LMP prices, congestion, and losses
  lmp_palette <- leaflet::colorBin(
    palette = c(
      "#333399", "#336699", "#66A3CC", "#88B497", "#A8D08D", 
      "#FFFFBF", "#FFD37F", "#FFA07A", "#FF6347", "#D7191C", "#990000"
    ),
    bins = c(-Inf, -40, -20, 0, 10, 20, 30, 50, 75, 100, 140, 180, Inf),
    na.color = "transparent"
  )
  
  congestion_palette <- leaflet::colorBin(
    palette = c(
      "#333399", "#336699", "#66A3CC", "#88B497", "#A8D08D", 
      "#FFFFBF", "#FFD37F", "#FFA07A", "#FF6347", "#D7191C", "#990000"
    ),
    bins = c(-Inf, -40, -20, -0.1, .1, 3, 10, 20, 30, 50, 100, 150, Inf),
    na.color = "transparent"
  )
  
  losses_palette <- leaflet::colorBin(
    palette = c(
      "#333399", "#336699", "#66A3CC", "#88B497", "#A8D08D", 
      "#FFFFBF", "#FFD37F", "#FFA07A", "#FF6347", "#D7191C", "#990000"
    ),
    bins = c(-Inf, -40, -20, -0.1, .1, 1, 3, 5, 7, 10, 15, 25, Inf),
    na.color = "transparent"
  )
  
  # Plot the map if there are valid geometries
  if (nrow(CA_states) > 0) {
    plot(sf::st_geometry(CA_states), col = "lightblue", border = "black")
  } else {
    cat("No geometries found for the specified states.")
  }
  
  # Create the Shiny app with an interactive map
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"), 
      shiny::selectInput("node_type", "Select Price Type", 
                         choices = c("LMP Prices" = "lmp", 
                                     "Congestion Prices" = "congestion", 
                                     "Loss Prices" = "loss"),
                         selected = "lmp"),
      leaflet::leafletOutput("map", width = "100%", height = "100%")
    ),
    server = function(input, output, session) {
      output$map <- leaflet::renderLeaflet({
        # Default color palette for LMP
        palette <- lmp_palette
        color_column <- "lmp"
        bins <- c(-Inf, -40, -20, 0, 10, 20, 30, 50, 75, 100, 140, 180, Inf)
        
        # Change palette and column based on user selection
        if (input$node_type == "congestion") {
          palette <- congestion_palette
          color_column <- "congestion"
          bins <- c(-Inf, -40, -20, -0.1, .1, 3, 10, 20, 30, 50, 100, 150, Inf)
        } else if (input$node_type == "loss") {
          palette <- losses_palette
          color_column <- "loss"
          bins <- c(-Inf, -40, -20, -0.1, .1, 1, 3, 5, 7, 10, 15, 25, Inf)
        }
        
        # Render the leaflet map with the selected node type
        leaflet::leaflet() %>%
          leaflet::addProviderTiles("CartoDB.Positron") %>%
          leaflet::setView(lng = -119.5, lat = 37.5, zoom = 6) %>%
          leaflet::addPolygons(data = CA_states, weight = 2, color = "black", 
                               fillColor = "lightblue", fillOpacity = 0.1) %>%
          leaflet::addPolylines(data = df_transmission, 
                                color = "blue", 
                                weight = ~kV_Sort / 200, 
                                opacity = 0.6, 
                                group = "All Transmission Lines") %>%
          leaflet::addPolylines(data = df_transmission_500, 
                                color = "blue", 
                                weight = 3, 
                                opacity = 0.6, 
                                group = "500kV Transmission Lines") %>%
          leaflet::addCircleMarkers(data = instance, 
                                    lng = ~longitude, 
                                    lat = ~latitude, 
                                    radius = 4, 
                                    color = ~palette(get(color_column)), 
                                    popup = paste("City: ", instance$city, "<br>",
                                                  paste(input$node_type, ": $", round(instance[[color_column]], 2), "<br>")),
                                    group = input$node_type) %>%
          leaflet::addLegend(position = "topleft", 
                             pal = palette, 
                             values = round(instance[[color_column]]),
                             title = paste(tools::toTitleCase(input$node_type), " Prices"), 
                             opacity = 1) %>%
          leaflet::addLayersControl(
            overlayGroups = c("All Transmission Lines", "500kV Transmission Lines", input$node_type),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      })
    }
  )
}

