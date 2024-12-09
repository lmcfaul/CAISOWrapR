% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_single_node_prices.R
\name{visualize_node_demand}
\alias{visualize_node_demand}
\title{Visualize Node Data: Demand}
\usage{
visualize_node_demand(
  node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package =
    "CAISOWrapR"))
)
}
\arguments{
\item{node_df}{A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the \code{pulldata_node} function.}
}
\value{
A \code{ggplot2} object displaying the distribution of LMPs by demand bucket.
}
\description{
This function takes in a dataframe of a year's worth of 15-minute data for a node and visualizes the data with a box and whisker plot, using buckets of load on the x-axis and LMP on the y-axis.
}
