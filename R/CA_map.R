#' create base map of NE
#' 
#' function creates a map of the Northeastern states of the United States
#' 
#' @return a map of the Northeastern states of the United States
#' @examples
#' CA_map()
#' 
#' @import sf
#' @import leaflet
#' @import shiny
#' @import rnaturalearth
#' 
#' @export
CA_map <- function(instance = (read.csv(system.file("extdata", "instance_normal.csv", package = "CAISOWrapR")))) {
  # Get the geometries for the United States
  us_states <- ne_states(country = "United States of America", returnclass = "sf")
  
  # Filter for the Calironia using the correct column name
  CA_states <- us_states[us_states$name %in% c("California"), ]
  df_transmission = st_read("data/all_transmission_lines.shp")
  df_transmission_500 = st_read("data/500kv_transmission_lines.shp")
  
  instance = merge_lmp_locations(instance)
  
  #convert lmp, congestion, and losses to numeric
  instance$lmp <- as.numeric(instance$lmp)
  instance$congestion <- as.numeric(instance$congestion)
  instance$loss <- as.numeric(instance$loss)
  
  # Create a color palette for the LMP prices
  lmp_palette <- colorBin(
    palette = c(
      "#333399", "#336699",  # Dark to medium blue
      "#66A3CC", "#88B497", "#A8D08D",          # Light blue to muted green
      "#FFFFBF", "#FFD37F", "#FFA07A", # Yellow to orange
      "#FF6347", "#D7191C", "#990000"  # Red shades
    ),
    bins = c(-Inf, -40, -20, 0, 10, 20, 30, 50, 75, 100, 140, 180, Inf),  # Breaks for each bin
    na.color = "transparent"  # Handle NA values
  )
  
  congestion_palette <- colorBin(
    palette = c(
      "#333399", "#336699",  # Dark to medium blue
      "#66A3CC", "#88B497", "#A8D08D",          # Light blue to muted green
      "#FFFFBF", "#FFD37F", "#FFA07A", # Yellow to orange
      "#FF6347", "#D7191C", "#990000"  # Red shades
    ),
    bins = c(-Inf, -40, -20, -0.1, .1, 3, 10, 20, 30, 50, 100, 150, Inf),  # Breaks for each bin
    na.color = "transparent"  # Handle NA values
  )
  
  losses_palette <- colorBin(
    palette = c(
      "#333399", "#336699",  # Dark to medium blue
      "#66A3CC", "#88B497", "#A8D08D",          # Light blue to muted green
      "#FFFFBF", "#FFD37F", "#FFA07A", # Yellow to orange
      "#FF6347", "#D7191C", "#990000"  # Red shades
    ),
    bins = c(-Inf, -40, -20, -0.1, .1, 1, 3, 5, 7, 10, 15, 25, Inf),  # Breaks for each bin
    na.color = "transparent"  # Handle NA values
  )
  
  
  
  # Plot the map if there are valid geometries
  if (nrow(CA_states) > 0) {
    plot(st_geometry(CA_states), col = "lightblue", border = "black")
  } else {
    cat("No geometries found for the specified states.")
  }
  
  #make the states moveable and dragable
  #add libraries
  shinyApp(
    ui = fluidPage(
      tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"), # Full screen height for the map
      leafletOutput("map", width = "100%", height = "100%")
    ),
    server = function(input, output, session) {
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -119.5, lat = 37.5, zoom = 6) %>%
          # Add California polygons
          addPolygons(data = CA_states, weight = 2, color = "black", 
                      fillColor = "lightblue", fillOpacity = 0.1#, 
                      #label = ~name
                      ) %>%
          # Add all transmission lines (as polylines)
          addPolylines(data = df_transmission, 
                       color = "blue", 
                       weight = ~kV_Sort / 200,  # Adjust weight based on kV_sort
                       opacity = 0.6, 
                       group = "All Transmission Lines") %>%          # Add 500kV transmission lines (as polylines)
          addPolylines(data = df_transmission_500, color = "blue", weight = 3, opacity = 0.6, group = "500kV Transmission Lines") %>%
          # Add LMP markers
          addCircleMarkers(data = instance, 
                           lng = ~longitude, 
                           lat = ~latitude, 
                           radius = 4, 
                           color = ~lmp_palette(lmp), 
                           popup = paste("City: ", instance$city, "<br>",
                                         "LMP: $", round(instance$lmp, 2), "<br>",
                                         "Energy: $", round(instance$energy, 2), "<br>",
                                         "Congestion: $", round(instance$congestion, 2), "<br>",
                                         "Loss: $", round(instance$loss, 2)),
                           group = "LMP Prices") %>%
          #add congestions
          addCircleMarkers(data = instance, 
                           lng = ~longitude, 
                           lat = ~latitude, 
                           radius = 4, 
                           color = ~congestion_palette(congestion), 
                           popup = paste("City: ", instance$city, "<br>",
                                         "Congestion: $", round(instance$congestion, 2)),
                           group = "Congestion Prices") %>%
          addCircleMarkers(data = instance, 
                           lng = ~longitude, 
                           lat = ~latitude, 
                           radius = 4, 
                           color = ~losses_palette(loss), 
                           popup = paste("City: ", instance$city, "<br>",
                                         "Loss: $", round(instance$loss, 2)),
                           group = "Loss Prices") %>%
          # Add layer control to toggle between the transmission lines layers
          addLayersControl(
            overlayGroups = c("All Transmission Lines", "500kV Transmission Lines", "LMP Prices",
                              "Congestion Prices", "Loss Prices"),
            options = layersControlOptions(collapsed = TRUE)
          )
      })
    }
  )
}




