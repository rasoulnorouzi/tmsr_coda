###############################################################################
###############################################################################
### CODA embeddings --> clusters
###############################################################################
###############################################################################

# clear ws
rm(list=ls())

# dependencies
library(arrow)
library(stringr)

library(dbscan)
library(umap)


# prep data from feather
pathfile = './coda_average_embeddings.feather'

d_feather = arrow::read_feather(pathfile)

d.dt = setDT(d_feather)
d.dt = cbind(d.dt, t(as.data.table((d.dt$embeddings))))
names(d.dt)[6:773] = paste0('emb_', 1:768)

# save(d.dt
#      , file = './embeddings_CODA.RData')

# START
## load data
load('./embeddings_CODA.RData')

names(d.dt)
d = d.dt[, -c(5)]
rm(d.dt)

## select unique words
d_unique = d[!(duplicated(d$word)), ]

### DBSCAN with dim reduction

#### dim reduction
config_umap = umap.defaults
config_umap$n_neighbors = 30
config_umap$n_epochs = 300
config_umap$n_components = 10
config_umap$min_dist = 0.001
config_umap$metric = "cosine"

t_0 = Sys.time()
d_umap = umap(d = d_unique[, 5:772]
              , config = config_umap)
t_1 = Sys.time()
print(t_1 - t_0)

save(d_umap
     , file = './umap_after_reduction.RData')

d_umap_values = d_umap$layout

#### dbscan
set.seed(8)
dbscan_umap = dbscan(d_umap_values
                          , eps = 0.20
                          , MinPts = 10
                          , method = 'raw')
dbscan_umap
plot(dbscan_umap, d_umap_values, main = "DBSCAN", frame = FALSE)

d_reduced = cbind(d_unique[, c(1:4)], dbscan_umap$cluster)
names(d_reduced)[5] = 'cluster'

d_reduced[, .N, .(cluster, word)]
d_reduced[, .N, .(cluster)]
d_reduced[, .N, .(word)][order(-N), ]

d_reduced[cluster == 2, ]
d_reduced[cluster == 3, ]
d_reduced[cluster == 5, ]
