#===============================================================================
# 06-44cluster-clustering-visualization.R
# Purpose: visualizing (using dimension reduction) the similarity of all topics
#          in the paper's RLDA object
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(extrafont) # font_import(pattern = "lmroman*")
library(ldaRobust)
library(reshape2)
library(ggplot2)
library(MASS)
library(plot3D)
library(kernlab)

# PATHS & CONSTANTS
#===============================================================================
#data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")
data_path <- paste0("~/Desktop/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================
# - load the rlda object with the list of LDA topics estamated using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

# - load a dataset with doc-level covariates
doc_covs <- read.csv(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/PressData.csv"
))


# DATA WRANGLING
#===============================================================================
# - focusing on studying the clustering with the same number of clusters and
#   topics in the "original model": 44 clusters
clustering_i <- which(r@num_of_clusters == 44)

# MAIN -- CLUSTER VISUALIZATION
#===============================================================================
# - create a topic-feature matrix (beta matrix)
beta_mat <- rbind(r@lda_u@beta, do.call(rbind, r@beta_list))
sim_mat <- dist(
  sapply(1:nrow(beta_mat), function(i) list(beta_mat[i,])),
  sapply(1:nrow(beta_mat), function(i) list(beta_mat[i,])),
  method = "cosine"
)

# - cluster them into 44 clusters
cl_obj = specc(beta_mat, 3)

# - reverse similarity matrix so 1 means totally similar
sim_mat <- 1 - sim_mat

# - reducing the similarity matrix to 3 dimensions
sim_3d <- cmdscale(1-sim_mat, eig=TRUE, k=3)
x <- sim_3d$points[,1]
y <- sim_3d$points[,2]
z <- sim_3d$points[,3]
cluster <- cl_obj@.Data

# - the plot
scatter3D(x, y, z, phi = 0, bty ="g", col = "gray30")


