# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

# 1. Rasoul: Extract text data (nouns and adjectives from abstracts)
# 2. Rasoul: Generate vector embeddings of text data using sciBERT
# 3. Caspar: Apply HDBSCAN to those vectors to group the terms
# 4. Caspar: Label each cluster summarizing the terms contained in it
# 5. Caspar: Visualize the results

library(worcs)
library(arrow)
library(dbscan)
library(umap)
library(data.table)
library(ggplot2)
library(reticulate)
use_virtualenv("tmsrcoda")

reticulate::py_install("hdbscan")

run_everything <- FALSE
df <- arrow::read_feather("coda_average_embeddings.feather")
unique_words = !duplicated(df$word)
# df <- df[sample.int(nrow(df), 100), ]

# Extract vector embeddings as matrix
mat_vect <- do.call(rbind, df[["embeddings"]])

# Drop duplicate words
## select unique words
mat_vect <- mat_vect[unique_words, ]

# Reduce dimensionality
# we will want a larger n_neighbors value – small values will focus more on very local structure and are more prone to producing fine grained cluster structure that may be more a result of patterns of noise in the data than actual clusters.
# Second set min_dist to a very low value. Since we actually want to pack points together densely (density is what we want after all) a low value will help, as well as making cleaner separations between clusters. In this case we will simply set min_dist to be 0.

# clusterable_embedding = umap.UMAP(
#   n_neighbors=30,
#   min_dist=0.0,
#   n_components=2,
#   random_state=42,
# )

if(run_everything){
config_umap <- umap.defaults
config_umap$n_neighbors <- 5
config_umap$n_components <- 10
config_umap$min_dist <- 0.0001
config_umap$metric <- "cosine"


tmp <- yaml::read_yaml("durations.yml")
tstart <- Sys.time()
mat_umap10d <- umap(mat_vect, config = config_umap, method = "umap-learn")
tend <- Sys.time()
tmp[["umap"]] <- tend-tstart
yaml::write_yaml(tmp, "durations.yml")

saveRDS(mat_umap_10d, "mat_umap_10d.RData")

# config_umap$n_components <- 3
# mat_umap_3d <- umap(mat_vect, config = config_umap)
# saveRDS(mat_umap_3d, "mat_umap_3d.RData")

} else {
  mat_umap <- readRDS(mat_umap_10d, "mat_umap_10d.RData")
}


# Remove outliers ---------------------------------------------------------
if(FALSE){
mat_umap <- mat_umap_10d$layout
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

# Cluster vector embeddings
tstart <- Sys.time()
res_clust <- hdbscan(mat_vect, minPts = 10)
tend <- Sys.time()
tmp <- yaml::read_yaml("durations.yml")
tmp[["hdbscan"]] <- tend-tstart
yaml::write_yaml(tmp, "durations.yml")
res_clust

hdbscan <- reticulate::import('hdbscan', delay_load = TRUE)

clusterer <- hdbscan$HDBSCAN(algorithm = "best",
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
                             cluster_selection_method = "leaf")

tstart <- Sys.time()
reticulate::py_main_thread_func(clusterer$fit(mat_vect))
tend <- Sys.time()
tmp <- yaml::read_yaml("durations.yml")
tmp[["res_hdbscan"]] <- tend-tstart
yaml::write_yaml(tmp, "durations.yml")

res_hdbscan <- list(
  labels = clusterer$labels_,
  probabilities = clusterer$probabilities_,
  cluster_persistance = clusterer$cluster_persistence_,
  exemplars = clusterer$exemplars_,
  outlier_scores = clusterer$outlier_scores_)
table(res_hdbscan$labels)

cluster_words <- split(df$word[unique_words], factor(res_hdbscan$labels))

result <- list(
  labels = clusterer$labels_,
  probabilities = clusterer$probabilities_,
  cluster_persistance = clusterer$cluster_persistence_,
  exemplars = clusterer$exemplars_,
  outlier_scores = clusterer$outlier_scores_)

