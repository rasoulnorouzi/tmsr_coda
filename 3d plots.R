library(rgl)

mat_umap <- mat_umap10d$layout
mns_umap <- colMeans(mat_umap)
cov_umap <- cov(mat_umap)
dist_mah <- mahalanobis(x = mat_umap , center = mns_umap, cov = cov_umap)
plot(density(dist_mah))

# Cutoff value for ditances from Chi-Sqaure Dist.
# with p = 0.95 df = 2 which in ncol(air)
cutoff <- qchisq(p = 0.95, df = 3)

colr <- rep("green", nrow(mat_umap))
outliers <- dist_mah > cutoff
colr[outliers] <- "red"

plot3d(x = mat_umap[,1],
       y = mat_umap[,2],
       z = mat_umap[,3],
       type="s",
       size=.1,
       col = colr)
tsne <- Rtsne::Rtsne(mat_vect[!duplicated(mat_vect), ], dims = 3, perplexity=1000, verbose=FALSE)
plot3d(x = tsne$Y[,1],
       y = tsne$Y[,2],
       z = tsne$Y[,3],
       type="s",
       size=.1)

# Remove outliers
mat_umap <- mat_umap[!outliers, ]
plot3d(x = mat_umap[,1],
       y = mat_umap[,2],
       z = mat_umap[,3],
       type="s",
       size=1)
