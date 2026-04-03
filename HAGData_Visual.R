# STA2201 Final Project
# Visualizing HAG
# Authors: Evan Shamov and Ian Zhang
# Code: Evan Shamov
# Require: HAG1.csv

library(ggplot2)
library(dplyr)
library(GGally)
library(ggfortify)


HAG1 = read.csv(file = 'HAG1.csv')

COL = c('#EC0B43', '#3FB9DE', '#FAA80F','#77D321', '#715AFF')
PALETTE = c('1' = COL[1], '2' = COL[2], '3' = COL[3], '4' = COL[4], '5' = COL[5])

HAG1$class = factor(HAG1$class)

################################################################################
# Pairs Plot, Coloured
plt_pairscol = ggpairs(HAG1, 
                       columns = 1:2, 
                       aes(color = as.factor(class), fill = as.factor(class)),
                       lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
                       diag  = list(continuous = wrap("densityDiag", alpha = 0.5)),
                       upper = list(continuous = "blank")) + theme_bw() +
  scale_color_manual(values = PALETTE, name = "class") +
  scale_fill_manual(values = PALETTE, name = "class")

plt_pairscol

################################################################################
# Perform PCA..
X = HAG1[, 1:2]
PCA1 = prcomp(X, center = TRUE, scale = TRUE)
PCA2 = prcomp(X, center = TRUE, scale = FALSE)

plt_PCA_scaled = autoplot(PCA1, data = HAG1, 
                          color = "class", 
                          loadings = TRUE, 
                          loadings.label = TRUE, 
                          loadings.label.size = 4, scale = 0) +
  scale_color_manual(values = PALETTE, name = "class")

plt_PCA_unscaled = autoplot(PCA2, data = HAG1, 
                            color = "class", 
                            loadings = TRUE, 
                            loadings.label = TRUE, 
                            loadings.label.size = 4, scale = 0) +
  scale_color_manual(values = PALETTE, name = "class")
plt_PCA_unscaled