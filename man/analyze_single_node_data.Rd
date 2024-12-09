% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_single_node_prices.R
\name{analyze_single_node_data}
\alias{analyze_single_node_data}
\title{Analyze and Visualize Data for a Single Node}
\usage{
analyze_single_node_data(
  node_df = read.csv(system.file("extdata", "San_Francisco_node_2023.csv", package =
    "CAISOWrapR")),
  output_pdf_path = "single_node_data.pdf"
)
}
\arguments{
\item{node_df}{A dataframe of locational marginal prices for a year for one node. The default for this is San Francisco 2023, but a user can input a different dataframe by running the \code{pulldata_node} function.}

\item{output_pdf_path}{The path to save the pdf file. There is a default path of "single_instance_data.pdf" but the user can edit this path to save the file in a different location.}
}
\value{
A pdf file with the two plots
}
\description{
This function analyzes data from a single node, representing one location with locational marginal prices (LMPs) over a year (at 15-minute intervals). It calls multiple visualization functions to generate a series of plots, which are then compiled into a PDF report for easy export. The analysis covers various aspects of the node's data, including LMP distributions, congestion, loss pricing adjustments, and demand.
}
\details{
The user should first call the \code{pulldata_node} function to load the required data for the specific node. Then, they can call this function to generate a comprehensive report with visualizations that focus on different aspects of the node's data. Alternatively, the user can call the individual analysis functions separately to examine specific aspects of the data without generating the full report.

The following functions are called within this process:
\itemize{
\item \code{visualize_node}: Creates plots visualizing the distribution of LMPs for the node.
\item \code{visualize_node_seasonal}: Generates seasonal plots showing trends in LMPs over the course of the year.
\item \code{visualize_node_congestion}: Creates visualizations for congestion at the node.
\item \code{visualize_node_seasonal_congestion}: Generates seasonal plots for congestion analysis.
\item \code{visualize_node_losses}: Produces plots for loss pricing adjustments at the node.
\item \code{visualize_node_seasonal_losses}: Generates seasonal plots for loss pricing adjustments.
\item \code{visualize_node_demand}: Creates plots analyzing demand at the node over the year.
\item \code{visualize_node_demand_without_high_demand_hours}: Visualizes demand at the node, excluding high-demand hours.
\item \code{visualize_node_demand_congestion}: Visualizes the relationship between demand and congestion.
\item \code{visualize_node_demand_without_high_demand_hours_congestion}: Visualizes the relationship between demand and congestion, excluding high-demand hours.
}
}
