#' cluster_if
#'
#' @param data_path The path to the data file, for example: "Data/if_2023_06.csv"
#' @param rmv Number of numbers to remove at the end of the data. IF data from the BCB usually has summary variables at the end. Check the data before runing the function to verify.
#' @param column_to_cluster The name of the column in the data set to cluster, for example: "Ativo Total"
#'
#' @return A tibble with a column containing the data clusters and a graph showing the mean and confidence Intervals for each cluster
#' @export
#'
#' @examples
#'
#' clustered_data <- cluster_if(data_path = "Data/if_2023_06.csv", rmv = 40, column_to_cluster = "Ativo Total")
#'
cluster_if <- function(data_path, rmv, column_to_cluster){

  c_name <- janitor::make_clean_names(column_to_cluster)

  data <- readr::read_delim(data_path, delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
    dplyr::slice(1:(dplyr::n() - rmv)) |>
    janitor::clean_names() |>
    dplyr::mutate(!!c_name := as.numeric(gsub("\\.", "", !!rlang::ensym(c_name)))) |>
    dplyr::filter(!is.na(!!rlang::ensym(c_name))) |>
    dplyr::arrange(desc(!!rlang::ensym(c_name)))

  initial_clusters <- c(mean(data[[c_name]][1:5]),
                        mean(data[[c_name]][5:10]),
                        mean(data[[c_name]][10:15]),
                        mean(data[[c_name]][15:20]),
                        mean(data[[c_name]][20:25]),
                        mean(data[[c_name]][25:30]))

  k_model <- kmeans(data[[c_name]], initial_clusters)

  centers <- tidyr::tibble(k_model$centers, row_names = 1:length(k_model$centers))

  sd <- data |>
    dplyr::mutate(cluster = k_model$cluster) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(sd = sqrt(var(!!rlang::ensym(c_name)))) |>
    dplyr::mutate(mean = dplyr::case_when(cluster == centers$row_names ~ centers$`k_model$centers`)) |>
    dplyr::mutate(ci_up  = mean + sd, ci_low = mean - sd)

  # plot

  plot <- sd |>
    tidyr::pivot_longer(cols = c(mean, ci_up, ci_low), names_to = "variable", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = cluster, y = value, color = variable, group = variable)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ifelse(variable == "ci_low", value, NA), ymax = ifelse(variable == "ci_up", value, NA), width = .4, )) +
    ggplot2::labs(x = "Cluster", y = NULL, color = "Variable", title = paste("Mean and Confidence Intervals for Each Cluster", column_to_cluster, sep = " - ")) +
    ggplot2::theme_minimal()

  print(plot)

  final_data <- data |>
    dplyr::mutate(cluster = k_model$cluster) |>
    dplyr::select(instituicao, codigo, !!rlang::ensym(c_name), cluster)

  return(final_data)



}
