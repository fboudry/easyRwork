#' Compute optimal cluster numbers
#'
#' @param input_data
#' @param cluster_method
#'
#' @return
#' @export
#'
#' @examples
optimal_clust <- function(input_data, cluster_method) {
  # Choose number of cluster for analysis
  elbow_graph <-
    fviz_nbclust(input_data, cluster_method, method = "wss") +
    labs(subtitle = "Elbow method") +
    ggtitle(label = "Optimal number of cluster")
  silhouette_graph <-
    fviz_nbclust(input_data, cluster_method, method = "silhouette") +
    labs(subtitle = "Silhouette method") +
    ggtitle(label = "Optimal number of cluster")
  gap_graph <-
    fviz_nbclust(
      input_data,
      cluster_method,
      nstart = 25,
      method = "gap_stat",
      nboot = 50
    ) +
    labs(subtitle = "Gap statistic method") +
    ggtitle(label = "Optimal number of cluster")

  cluster_number_graph <- # Put graphs in list
    lst(elbow_graph, silhouette_graph, gap_graph)
  names(x = cluster_number_graph) <- c("elbow", "silhouette", "gap")
  return(cluster_number_graph)
}

#' Compute k-means clusters
#'
#' @param input
#' @param cluster_number
#'
#' @return
#' @export
#'
#' @examples
compute_kclust <- function(input, cluster_number) {
  kclust_data <- lapply(X = cluster_number,
                        FUN = kmeans, x = input) %>%
    `names<-`(value = paste0("kclust_", cluster_number))
}

#' Compute hierarchical clusters
#'
#' @param input_data
#' @param nclust
#'
#' @return
#' @export
#'
#' @examples
hcl_plot <- function(input_data, nclust) {
  plot(input_data, cex = 0.6)
  rect.hclust(tree = input_data, k = nclust)
  recordPlot()
}

#' Plot cluster variables
#'
#' @param data_col
#' @param cluster_col
#'
#' @return
#' @export
#'
#' @examples
boxplots_by_clust <- function(data_col, cluster_col) {
  ggplot(plot_df, aes(x = !!sym(cluster_col), y = !!sym(paste(data_col)))) +
    geom_boxplot(aes(
      group = !!sym(cluster_col),
      fill = as.factor(!!sym(cluster_col))
    )) +
    ggtitle(label = paste(data_col, "by cluster")) +
    labs(fill = cluster_col)
}

#' Plot subject repartition in clusters
#'
#' @param data_col
#' @param cluster_col
#'
#' @return
#' @export
#'
#' @examples
subject_repartition <- function(data_col, cluster_col) {
  ggplot(data = plot_df, mapping = aes(x = !!(1:nrow(x = plot_df)),
                                       y = !!sym(paste(data_col)))) +
    geom_point(mapping = aes(color = as.factor(!!sym(
      paste(cluster_col)
    )))) +
    ggtitle(label = "Subject repartition in clusters") +
    labs(fill = cluster_col)
}
