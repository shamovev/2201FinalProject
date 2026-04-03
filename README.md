# STA2201 Final Project: SNE and t-SNE in R
## Purpose
The goal of this project is to study and compare **Stochastic Neighbour Embedding (SNE)** to **t-distributed Stochastic Neighbor Embedding (t-SNE)**. We focused on:
- implementing basic variants of the methods in `R`,
- testing their performance on synthetic datasets of various sizes and complexities,
- explore the effect of perplexity and learning rate on the quality of the embeddings,
- Compare these embeddings to those that can be obtained through simpler methods like PCA.
## Contents
This repository contains the following 8 files.
- `CliffordTorusData_GenerateFinal.R` This script generates the CliffTor4 data.
- `HAGData_GenerateFinal.R` This script generates the HAG data.
- `CliffordTorusData_Visual.R` This script produces the following visualizations of CliffTor4: a pairs plot, an interactive plot of the stereographic projection of the data, a tSNE plot using the tsne library.
- `HagData_Visual.R` This script produces the following visualizations of HAG: a pairs plot and a tSNE plot using the tsne library.
- `tSNE_implementation.R` This script is our implementation of the default t-SNE described by van der Maaten and Hinton (2008). 'Early exaggeration' was implemented for improved optimization, but adaptive gains were not.
- `SNE_implementation.R` This script is our implementation of the default SNE described by van der Maaten and Hinton (2008). 'Early exaggeration' was not implemented and neither were adaptive gains. This is the most basic implementation of SNE. 
- `GridVisuals_tSNE.R` This script generates the various parameter sweeps using our implementation of tSNE. 
- `GridVisuals_SNE.R` This script generates the various parameter sweeps using our implementation of SNE.
- `CliffordTorusData_CheckClusters.R` This script verifies empirically that the embedding map preserves the cluster structure in the image.
  
## Important Notes
- Before running `tSNE_implementation.R` or `SNE_implementation.R`, make sure to run the entire script. These scripts define helper functions with the same name, but they are not the same.
- All necessary libraries appear in the headers of the scripts. They may need to be installed before use.

## Authors
Evan Shamov and Ian Zhang
