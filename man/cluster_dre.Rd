% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_dre.R
\name{cluster_dre}
\alias{cluster_dre}
\title{cluster_dre}
\usage{
cluster_dre(data_path, rmv, column_to_cluster)
}
\arguments{
\item{data_path}{The path to the data file, for example: "Data/if_dre_2023_06.csv"}

\item{rmv}{Number of numbers to remove at the end of the data. IF data from the BCB usually has summary variables at the end. Check the data before runing the function to verify.}

\item{column_to_cluster}{The name of the column in the data set to cluster, for example: "Resultado antes da Tributação, Lucro e Participação (g) = (e) + (f)"}
}
\value{
A tibble with a column containing the data clusters and a graph showing the mean and confidence Intervals for each cluster
}
\description{
cluster_dre
}
\examples{

clustered_data <- cluster_if(data_path = "Data/if_dre_2023_06.csv", rmv = 40, column_to_cluster = "Resultado antes da Tributação, Lucro e Participação (g) = (e) + (f)")

}
