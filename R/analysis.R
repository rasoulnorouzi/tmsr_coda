do_umap <- function(mat_vect){
# Reduce dimensionality
# we will want a larger n_neighbors value – small values will focus more on very local structure and are more prone to producing fine grained cluster structure that may be more a result of patterns of noise in the data than actual clusters.
# Second set min_dist to a very low value. Since we actually want to pack points together densely (density is what we want after all) a low value will help, as well as making cleaner separations between clusters. In this case we will simply set min_dist to be 0.

# clusterable_embedding = umap.UMAP(
#   n_neighbors=30,
#   min_dist=0.0,
#   n_components=2,
#   random_state=42,
# )
  library(umap)
  library(reticulate)
  use_virtualenv("tmsrcoda")
  pypkgs <- py_list_packages()
  if(!"umap-learn" %in% pypkgs$package) reticulate::py_install("umap-learn")
  config_umap <- umap.defaults
  config_umap$n_neighbors <- 5
  config_umap$n_components <- 10
  config_umap$min_dist <- 0.0001
  config_umap$metric <- "cosine"
  mat_vect[1:4,1:4]
  umap(d = mat_vect, config = config_umap, method = "umap-learn")
}

do_hdbscan <- function(mat, algorithm = "best",
                       allow_single_cluster = FALSE,
                       alpha = 1,
                       prediction_data = TRUE,
                       approx_min_span_tree = TRUE,
                       gen_min_span_tree = FALSE,
                       leaf_size = 40L,
                       core_dist_n_jobs = 19L,
                       metric = "euclidean",
                       min_cluster_size = 10L,
                       min_samples = 1L,
                       cluster_selection_epsilon =  .1,
                       cluster_selection_method = "leaf"){
  use_virtualenv("tmsrcoda")
  pypkgs <- py_list_packages()
  if(!"hdbscan" %in% pypkgs$package) reticulate::py_install("hdbscan")
  hdbscan <- reticulate::import('hdbscan', delay_load = TRUE)

  clusterer <- hdbscan$HDBSCAN(algorithm = algorithm,
                               allow_single_cluster = allow_single_cluster,
                               alpha = alpha,
                               prediction_data = prediction_data,
                               approx_min_span_tree = approx_min_span_tree,
                               gen_min_span_tree = gen_min_span_tree,
                               leaf_size = leaf_size,
                               core_dist_n_jobs = core_dist_n_jobs,
                               metric = metric,
                               min_cluster_size = min_cluster_size,
                               min_samples = min_samples,
                               cluster_selection_epsilon =  cluster_selection_epsilon,
                               cluster_selection_method = cluster_selection_method)

  reticulate::py_main_thread_func(clusterer$fit(mat))

  return(list(
    labels = clusterer$labels_,
    probabilities = clusterer$probabilities_,
    cluster_persistance = clusterer$cluster_persistence_,
    exemplars = clusterer$exemplars_,
    outlier_scores = clusterer$outlier_scores_))

}

remove_outliers <- function(mat_umap){

    mns_umap <- colMeans(mat_umap)
    cov_umap <- cov(mat_umap)
    dist_mah <- mahalanobis(x = mat_umap , center = mns_umap, cov = cov_umap)
    plot(density(dist_mah))

    # Cutoff value for ditances from Chi-Sqaure Dist.
    # with p = 0.95 df = 2 which in ncol(air)
    cutoff <- qchisq(p = 0.95, df = ncol(mat_umap))

    colr <- rep("black", nrow(mat_umap))
    outliers <- dist_mah > cutoff
    colr[outliers] <- "red"

    p <- ggplot(data.frame(x = mat_umap[,1], y = mat_umap[,2], col = colr), aes(x = x, y = y, color = col)) +
      geom_point(alpha = .05) +
      theme_bw()+
      labs(x = "UMAP dimension 1", y = "UMAP dimension 2")+
      scale_color_manual(values = c("black" = "black", "red"=  "red"))+
      theme(legend.position = "none")
    ggsave("umap_outliers.png", p, device = "png", width = 210, height = 200, units = "mm")
    ggsave("umap_outliers.svg", p, device = "svg")
    plot3d(x = mat_umap[,1],
           y = mat_umap[,2],
           z = mat_umap[,3],
           type="s",
           size=1,
           col = colr)
    # Check the worst words
    # tail(df$word[unique_words][outliers][order(dist_mah[outliers], decreasing = TRUE)])
    # Remove outliers
    mat_umap <- mat_umap[!outliers, ]
    plot3d(x = mat_umap[,1],
           y = mat_umap[,2],
           z = mat_umap[,3],
           type="s",
           size=1)

}
