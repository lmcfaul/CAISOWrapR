#' Analyze Single Instance Data
#' 
#' This function calls the functions `summarize_lmps` and `price_violin` with an inputting dataframe of an instance of data (one date and 15 minute interval) and combines the results into a PDF for export. First, the user must call the `pulldata_instance` function to retrieve the data for the instance they want to analyze.
#' 
#' @param instance_df A dataframe of locational marginal prices with LMP names. The default dataframe is a normal day in California at 1pm. Users can change this dataframe to analyze a different instance that pulled from the `pulldata_instance` function. Or, to visualize the highest peak load in the last three years, use: "inst/extdata/instance_peak_load.csv"
#' @param output_pdf_path The path to save the PDF file. The default path is "single_instance_data.pdf," but the user can modify this path to save the file in a different location.
#' 
#' @return A PDF file with the summary statistics and violin plot.
#' 
#' @export
analyze_single_instance_data <- function(instance_df = read.csv("inst/extdata/instance_normal.csv"), output_pdf_path = "single_instance_data.pdf") {
  result_one <- summarize_lmps(instance_df)
  result_two <- price_violin(instance_df)
  
  interval_time <- unique(instance_df$interval_start_utc)[1]
  formatted_time <- format(lubridate::ymd_hms(interval_time), "%Y-%m-%d at %H:%M")
  
  title <- grid::textGrob(
    paste("California LMP Frequencies on", formatted_time),
    gp = grid::gpar(fontsize = 16, fontface = "bold"),
    just = "center"
  )
  
  table_with_margins <- gtable::gtable_add_padding(
    gridExtra::tableGrob(result_one),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  plot_with_margins <- gtable::gtable_add_padding(
    ggplot2::ggplotGrob(result_two),
    padding = grid::unit(c(1, 1, 1, 1), "cm")
  )
  
  pdf(output_pdf_path, width = 8.5, height = 11)
  
  gridExtra::grid.arrange(
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
#' This function provides summary statistics for the current state of electricity prices. It takes in a dataframe of locational marginal prices (LMPs) and returns the average price, the standard deviation of prices, the lowest price, the highest price, and the locations for those prices.
#'
#' @param instance_df A dataframe of locational marginal prices.
#'
#' @return A dataframe of summary statistics. The default dataframe is a normal day in California at 1pm. Users can change this dataframe to analyze a different instance that pulled from the `pulldata_instance` function. Or, to visualize the highest peak load in the last three years, use: "inst/extdata/instance_peak_load.csv"
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
summarize_lmps <- function(instance_df = read.csv("inst/extdata/instance_normal.csv")){
  avg_price <- mean(instance_df$lmp)
  sd_price <- sd(instance_df$lmp)
  
  cities_and_lmps <- read.csv("inst/extdata/cities_and_lmps.csv")
  
  min_price <- min(instance_df$lmp)
  min_loc_lmp <- instance_df$location[instance_df$lmp == min_price]
  min_loc_city <- cities_and_lmps$name[cities_and_lmps$closest_lmp == min_loc_lmp]
  
  max_price <- max(instance_df$lmp)
  max_loc_lmp <- instance_df$location[instance_df$lmp == max_price]
  max_loc_city <- cities_and_lmps$name[cities_and_lmps$closest_lmp == max_loc_lmp]
  
  summary_df <- data.frame(
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
#' This function generates a violin plot of the LMP prices in the dataset, 
#' with an overlaid boxplot showing the median, interquartile range, and whiskers.
#'
#' @param instance_df A dataframe of locational marginal prices with LMP names. The default dataframe is a normal day in California at 1pm. Users can change this dataframe to analyze a different instance that pulled from the `pulldata_instance` function. Or, to visualize the highest peak load in the last three years, use: "inst/extdata/instance_peak_load.csv"
#'
#' @return A violin plot of the prices with an overlaid boxplot.
#'
#' @export
#' @examples
#' # Example Data
#' example_df <- data.frame(
#'   lmp = c(20.5, 30.2, 25.1),
#'   location = c("Loc1", "Loc2", "Loc3")
#' )
#' price_violin(example_df)
price_violin <- function(instance_df = read.csv("inst/extdata/instance_normal.csv")) {
  
  interval_time <- unique(instance_df$interval_start_utc)[1]
  formatted_time <- format(lubridate::ymd_hms(interval_time), "%Y-%m-%d at %H:%M")
  
  p <- ggplot2::ggplot(instance_df, ggplot2::aes(x = "", y = lmp)) +
    ggplot2::geom_violin(fill = "gray", color = "black", alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.2, fill = "#E8E8E8", color = "black", outlier.shape = NA) + 
    ggplot2::labs(
      title = paste("California LMP Frequencies on", formatted_time),
      x = "Frequency",
      y = "Locational Marginal Price ($/MWh)"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(), 
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggthemes::theme_solarized()
  
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
#' @examples
#' library(grid)
#' library(gtable)
#' 
#' # Create a simple table
#' example_table <- tableGrob(head(mtcars))
#' 
#' # Add padding to the table
#' padded_table <- gtable_add_padding(example_table, grid::unit(c(1, 1, 1, 1), "cm"))
#' 
#' # Print the padded table
#' grid::grid.newpage()
#' grid::grid.draw(padded_table)
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
