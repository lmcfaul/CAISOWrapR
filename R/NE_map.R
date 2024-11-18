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
NE_map <- function() {
  # Get the geometries for the United States
  us_states <- ne_states(country = "United States of America", returnclass = "sf")
  
  # Filter for the Northeast states using the correct column name
  northeast_states <- us_states[us_states$name %in% c("Maine", "New Hampshire", 
                                                      "Vermont", "Massachusetts", 
                                                      "Rhode Island", "Connecticut"), ]
  
  # Plot the map if there are valid geometries
  if (nrow(northeast_states) > 0) {
    plot(st_geometry(northeast_states), col = "lightblue", border = "black")
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
          setView(lng = -72.7, lat = 44.5, zoom = 6) %>%
          addPolygons(data = northeast_states, weight = 2, color = "black", 
                      fillColor = "lightblue", fillOpacity = 0.6, 
                      label = ~name)
      })
    }
  )
}




