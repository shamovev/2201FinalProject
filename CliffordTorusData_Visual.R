# STA2201 Final Project
# Visualizing CliffTor4
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov
# Require: CliffTor4.csv

library(ggplot2)
library(dplyr)
library(GGally)
library(ggfortify)
library(tsne)

CliffTor4 = read.csv(file = 'CliffTor4.csv')

COL = c('#EC0B43', '#3FB9DE', '#FAA80F','#77D321', '#715AFF')
PALETTE = c('1' = COL[1], '2' = COL[2], '3' = COL[3], '4' = COL[4], '5' = COL[5])

# Obtain Stereographic Projection. Default pole = (0,0,0,1).
Pole = 4
X_complete = as.matrix(CliffTor4) 
X = X_complete[ ,-5]
D = 1 - X[, Pole]
Y = X/D
Y = cbind(Y[,-Pole], X_complete[,5])
colnames(Y) = c('y1', 'y2', 'y3', 'cluster')

CliffTor4$cluster = factor(CliffTor4$cluster)

################################################################################
# Pairs Plot, Coloured
plt_pairscol = ggpairs(CliffTor4, 
                    columns = 1:4, 
                    aes(color = as.factor(cluster), fill = as.factor(cluster)),
                    lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
                    diag  = list(continuous = wrap("densityDiag", alpha = 0.5)),
                    upper = list(continuous = "blank")) + theme_bw() +
  scale_color_manual(values = PALETTE, name = "cluster") +
  scale_fill_manual(values = PALETTE, name = "cluster")

plt_pairscol

################################################################################
# Plot 3D projection. Y[,1:3] = scale(Y[,1:3], center = TRUE, scale = TRUE)
plt_3d = plot_ly(x = ~y1, y = ~y2, z = ~y3, 
                 color = ~as.factor(cluster), 
                 colors = PALETTE, 
                 data = data.frame(Y),
                 type = 'scatter3d', 
                 mode = 'markers', 
                 marker = list(size = 3, opacity = 0.5))
plt_3d %>% layout(showlegend = TRUE, 
                  legend = list(font = list(size = 15), itemsizing = 'constant'))

# Nicer Generation:
plt3d_nicer = plot_ly(x = ~y1, y = ~y2, z = ~y3, 
                      color = ~as.factor(cluster), 
                      colors = PALETTE, 
                      data = data.frame(Y),
                      type = 'scatter3d', 
                      mode = 'markers', 
                      marker = list(size = 3, opacity = 0.5))

p2 = plt3d_nicer %>% 
  layout(scene = list(camera = list(eye = list(x = -1.5, y = 1.5, z = 1.5)))) %>% 
  layout(showlegend = TRUE, legend = list(font = list(size = 15), itemsizing = 'constant'))

################################################################################
# PCA
X = CliffTor4[, 1:4]
PCA1 = prcomp(X, center = TRUE, scale = TRUE)
PCA2 = prcomp(X, center = TRUE, scale = FALSE)

plt_PCA_scaled = autoplot(PCA1, data = CliffTor4, 
                            color = "cluster", 
                            loadings = TRUE, 
                            loadings.label = TRUE, 
                            loadings.label.size = 4, scale = 0) +
  scale_color_manual(values = PALETTE, name = "Cluster")

plt_PCA_unscaled = autoplot(PCA2, data = CliffTor4, 
                            color = "cluster", 
                            loadings = TRUE, 
                            loadings.label = TRUE, 
                            loadings.label.size = 4, scale = 0) +
  scale_color_manual(values = PALETTE, name = "Cluster")
plt_PCA_unscaled

################################################################################
# tSNE
set.seed(20937635)

tSNE_out = tsne(X, k = 2, perplexity = 30, whiten = FALSE)
colnames(tSNE_out) = c("tSNE1", "tSNE2")

tSNE_df = data.frame(tSNE_out, cluster = as.factor(CliffTor4$cluster))

plt_TSNE = ggplot(tSNE_df, aes(tSNE1, tSNE2, color = cluster)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = PALETTE) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(
    title = "t-SNE Embedding of the Clifford Torus",
    x = "t-SNE 1",
    y = "t-SNE 2",
    color = "Cluster"
  ) + theme_minimal()
plt_TSNE
