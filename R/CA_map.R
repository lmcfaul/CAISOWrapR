#' create base map of NE
#' 
#' function creates a map of the Northeastern states of the United States
#' 
#' @return a map of the Northeastern states of the United States
#' @examples
#' NE_map()
#' 
#' @import sf
#' @import leaflet
#' @import shiny
#' @import rnaturalearth
#' 
#' @export
CA_map <- function() {
  # Get the geometries for the United States
  us_states <- ne_states(country = "United States of America", returnclass = "sf")
  
  # Filter for the Calironia using the correct column name
  CA_states <- us_states[us_states$name %in% c("California"), ]
  df_transmission = st_read("data/all_transmission_lines.shp")
  df_transmission_500 = st_read("data/500kv_transmission_lines.shp")
  
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
                      fillColor = "lightblue", fillOpacity = 0.6, 
                      label = ~name) %>%
          # Add all transmission lines (as polylines)
          addPolylines(data = df_transmission, 
                       color = "blue", 
                       weight = ~kV_Sort / 100,  # Adjust weight based on kV_sort
                       opacity = 0.8, 
                       group = "All Transmission Lines") %>%          # Add 500kV transmission lines (as polylines)
          addPolylines(data = df_transmission_500, color = "red", weight = 3, opacity = 0.8, group = "500kV Transmission Lines") %>%
          # Add layer control to toggle between the transmission lines layers
          addLayersControl(
            overlayGroups = c("All Transmission Lines", "500kV Transmission Lines"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })
    }
  )
}




