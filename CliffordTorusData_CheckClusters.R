# STA2201 Final Project
# Empirically Verifying Clusters in Clifford Torus
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov
# Requires: CliffTor4.csv

library(fossil)

set.seed(20937635)
CliffTor4 = read.csv(file = 'CliffTor4.csv', header = TRUE)

# Remove the spiral.
CliffTor4_cluster = CliffTor4[which(CliffTor4$cluster !=5), ]

# Clustering Approach
KM = kmeans(CliffTor4_cluster, centers = 4,  nstart = 50, iter.max = 100, algorithm = 'Lloyd')

# rand.index is 1 which suggests clearly separated clusters.
rand.index(KM$cluster, CliffTor4_cluster$cluster)

# Using the pairs plot and ignoring the spiral, you also see that the clusters
# are well separated in pretty much every axis. 