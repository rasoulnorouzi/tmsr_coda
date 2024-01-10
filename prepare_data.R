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
df <- arrow::read_feather("coda_average_embeddings.feather")
# df <- df[sample.int(nrow(df), 100), ]

# Extract vector embeddings as matrix
mat_vect <- do.call(rbind, df[["embeddings"]])

# Reduce dimensionality
# we will want a larger n_neighbors value – small values will focus more on very local structure and are more prone to producing fine grained cluster structure that may be more a result of patterns of noise in the data than actual clusters.
# Second set min_dist to a very low value. Since we actually want to pack points together densely (density is what we want after all) a low value will help, as well as making cleaner separations between clusters. In this case we will simply set min_dist to be 0.

# clusterable_embedding = umap.UMAP(
#   n_neighbors=30,
#   min_dist=0.0,
#   n_components=2,
#   random_state=42,
# )
config_umap <- umap.defaults
config_umap$n_neighbors <- 30
config_umap$n_components <- 10
config_umap$min_dist <- 0.001
config_umap$metric <- "cosine"
mat_umap <- umap(mat_vect, config = config_umap)

# Cluster vector embeddings
res_clust <- hdbscan(mat_vect, minPts = 10)
res_clust
