# This script has all of the functions that analyze a single instance of data (one 15 minute interval) and combines the results into a pdf to export. The functions `summarize_lmps` and `price_violin` are called with an inputting dataframe of an instance of data. The user has to call the `pulldata_instance` function to get the data for the instance they want to analyze. The user can also just call the individual analysis functions if they want to see the results separately.


#' Analyze Single Instance Data
#' 
#' This function calls the functions `summarize_lmps` and `price_violin` with an inputting dataframe of an instance of data (one date and 15 minute interval) and combines the results into a pdf to export. First, the user has to call the `pulldata_instance` function to get the data for the instance they want to analyze.
#' 
#' @param instance_df A dataframe of locational marginal prices with LMP names
#' @param output_pdf_path The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.
#' 
#' @return A pdf file with the summary statistics and violin plot
#' 
#' @import grid
#' @import gridExtra
#' @import ggplot2
#' @import lubridate
#' @import gtable
#' 
#' @export
analyze_single_instance_data <- function(instance_df, output_pdf_path = "single_instance_data.pdf") {
  result_one <- summarize_lmps(instance_df)
  result_two <- price_violin(instance_df)
  
  interval_time <- unique(instance_df$interval_start_utc)[1]
  formatted_time <- format(ymd_hms(interval_time), "%Y-%m-%d at %H:%M")
  
  title <- textGrob(
    paste("California LMP Frequencies on", formatted_time),
    gp = gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  table_with_margins <- gtable_add_padding(
    tableGrob(result_one),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins <- gtable_add_padding(
    ggplotGrob(result_two),
    padding = unit(c(1, 1, 1, 1), "cm")
  )
  
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  grid.arrange(
    title,
    table_with_margins,
    plot_with_margins,
    nrow = 3,
    heights = c(0.1, 0.2, 0.7)
  )
  
  dev.off()
  
  message("Results saved to: ", output_pdf_path)
}


#' Summarize Electricity Statistics
#' 
#' This function will give summary statistics for the current state of electricity prices. It will take in a dataframe of locational marginal prices (that has been pulled from an API) and return the average price around NE-ISO, the standard deviation in prices, the lowest price, the highest price, and the locations for those prices. 
#'
#' @param instance_df A dataframe of locational marginal prices
#'
#' @return A dataframe of summary statistics
#'
#' @export
#'
#' @examples
#' # Example Data
#' example_df <- data.frame(
#'   lmp = c(20.5, 30.2, 25.1),
#'   location = c("Loc1", "Loc2", "Loc3")
#' )
#' summarize_lmps(example_df)
summarize_lmps = function(instance_df){
  avg_price = mean(instance_df$lmp)
  sd_price = sd(instance_df$lmp)
  
  cities_and_lmps = read.csv("data/cities_and_lmps.csv")
  
  min_price = min(instance_df$lmp)
  min_loc_lmp = instance_df$location[instance_df$lmp == min_price]
  min_loc_city = cities_and_lmps$name[cities_and_lmps$closest_lmp == min_loc_lmp]
  
  max_price = max(instance_df$lmp)
  max_loc_lmp = instance_df$location[instance_df$lmp == max_price]
  max_loc_city = cities_and_lmps$name[cities_and_lmps$closest_lmp == max_loc_lmp]
  
  summary_df = data.frame(
    Statistic = c("Average Price", "Standard Deviation", "Lowest Price", 
                  "Lowest Price Location", "Highest Price", "Highest Price Location"),
    Value = c(
      avg_price, 
      sd_price, 
      min_price, 
      paste(min_loc_city, collapse = ", "), 
      max_price, 
      paste(max_loc_city, collapse = ", ")
    ),
    stringsAsFactors = FALSE
  )
  
  rownames(summary_df) <- summary_df$Statistic
  summary_df$Statistic <- NULL
  
  return(summary_df)
}



#' Price Violin
#' 
#' This script generates a violin plot of the LMP prices in the dataset, 
#' with an overlaid boxplot showing the median, interquartile range, and whiskers.
#'
#' @param instance_df A dataframe of locational marginal prices with LMP names
#'
#' @return A violin plot of the prices with an overlaid boxplot
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot labs theme element_blank
#' @import ggthemes
#' @import lubridate
#'
#' @export
#' @examples
#' # Example Data
#' example_df <- data.frame(
#'   lmp = c(20.5, 30.2, 25.1),
#'   location = c("Loc1", "Loc2", "Loc3")
#' )
#' price_violin(example_df)
price_violin <- function(instance_df) {
  
  interval_time <- unique(instance_df$interval_start_utc)[1]
  formatted_time <- format(ymd_hms(interval_time), "%Y-%m-%d at %H:%M")
  
  p <- ggplot(instance_df, aes(x = "", y = lmp)) +
    geom_violin(fill = "gray", color = "black", alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "#E8E8E8", color = "black", outlier.shape = NA) + 
    labs(
      title = paste("California LMP Frequencies on", formatted_time),
      x = "Frequency",
      y = "Locational Marginal Price ($/MWh)"
    ) +
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank()
    ) +
    theme_solarized()
  
  return(p)
}

#' Add Padding to a gtable Object
#' 
#' This function adds padding to a `gtable` object by first inserting an invisible `rectGrob` around the table and then applying the specified padding.
#' It is a custom function to ensure that padding is applied correctly around the `grob` while preserving the layout of the original `gtable`.
#' 
#' @param grob A `gtable` object to which padding will be added.
#' @param padding A unit object specifying the padding to apply (e.g., `unit(c(1, 1, 1, 1), "cm")`).
#' 
#' @return A `gtable` object with the specified padding added.
#' 
#' @import grid
#' @import gtable
#' 
#' @examples
#' library(grid)
#' library(gtable)
#' 
#' # Create a simple table
#' example_table <- tableGrob(head(mtcars))
#' 
#' # Add padding to the table
#' padded_table <- gtable_add_padding(example_table, unit(c(1, 1, 1, 1), "cm"))
#' 
#' # Print the padded table
#' grid.newpage()
#' grid.draw(padded_table)
#' 
#' @export
gtable_add_padding <- function(grob, padding) {
  gtable::gtable_add_grob(
    grob,
    grid::rectGrob(
      gp = grid::gpar(fill = NA, col = NA)
    ),
    t = 1, l = 1, b = nrow(grob), r = ncol(grob),
    z = -Inf
  ) %>%
    gtable::gtable_add_padding(padding)
}
