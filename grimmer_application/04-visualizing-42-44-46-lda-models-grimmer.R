#===============================================================================
# 04-visualizing-42-44-46-lda-models-grimmer.R
# Purpose: visual comparison of the topic models with very similar number of
#           topics to show that these models present a very similar description
#           of the data
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ldaRobust)
library(reshape2)
library(ggplot2)

# PATHS & CONSTANTS
#===============================================================================
data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================

# - load the rlda object with the list of LDA topics estamated using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

# DATA WRANGLING
#===============================================================================

# - we initially estimated this rlda object using a .8 similarity threshold.
#   After exploring the table resulting from script 03, I believe a threshold
#   of .95 is necessary in order to consider two topics to be the same. Let's
#   try to re-use the estimated rlda object but to re-calculate all realted to
#   topic-matching across models

# - a copy of the rlda object
grimmer_rlda <- r

# - reset the similarity threshold parameter to .95
grimmer_rlda@threshold <- .95

# - empty out the `similarity_matrix` parameter of the rlda object
grimmer_rlda@similarity_mat <- list()
grimmer_rlda <- ldaRobust::compute_sim(grimmer_rlda)

# - recalculate now the document level information based on matched topics
#   across models
grimmer_rlda <- ldaRobust::getTopicInDoc(grimmer_rlda)


# MAIN -- VISUALIZE TOPIC SIMILARITY ACROSS MODELS
#===============================================================================

# A) Topic Similarity Matrix.

# - a macro similarity matrix: all original topics V. all topics form
#   alternative/comparison models
similarity_list_mat <-  do.call(cbind, grimmer_rlda@similarity_mat_list)

# A1)
# - Similarity plot "Original Model (44 topics) V. Alternative A (42 topics)"
similarity_list_mat = r@similarity_mat_list[[4]]

colnames(similarity_list_mat) = sapply(1:ncol(similarity_list_mat), function(z)
  paste0("B,", as.character(z)))

rownames(similarity_list_mat) = as.character(1:nrow(similarity_list_mat))

# - similarity matrix in long format so it can be plotted as a tile plot type
sim_melt = melt(similarity_list_mat)

pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#           "topic_sim_matrix_alternative_01.pdf"),
           "topic_sim_matrix_alternative_01_NO_SIM_NUM.pdf"),
    width = 20, height = 12)
ggplot(sim_melt,
       aes(x = factor(Var2), y = factor(Var1), fill = value, label = round(value, 2))) +
  #geom_tile(color='white', alpha = 0.5) +
  geom_tile(color='white', alpha = 0.7) +
  #geom_text(color='black', size = 4.5) +
  scale_fill_continuous(low="#56B1F7", high="#132B43") +
  #scale_colour_gradient2(high = "#132B43",
  #                       low = "#56B1F7", midpoint = .95) +
  xlab("Topics from Alternative 01 (B)\n") +
  ylab("\nTopics from Original Model\n") +
  coord_flip()+
  # - highlight the column for Topic 16 original model
  geom_hline(yintercept = 15.5, color = "red", size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = 16.5, color = "red", size = 1.5, alpha = 0.7) +
  # - highlight the row for Topic 19 alternative model
  geom_vline(xintercept = 18.5, color = "red", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 19.5, color = "red", size = 1.5, alpha = 0.7) +
  theme(panel.background = element_rect("white"),
        legend.position="none",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22))
dev.off()

# A2)
# - Similarity plot "Original Model (44 topics) V. Alternative A (42 topics)"
similarity_list_mat = r@similarity_mat_list[[7]]

colnames(similarity_list_mat) = sapply(1:ncol(similarity_list_mat), function(z)
  paste0("C,", as.character(z)))

rownames(similarity_list_mat) = as.character(1:nrow(similarity_list_mat))

# - similarity matrix in long format so it can be plotted as a tile plot type
sim_melt = melt(similarity_list_mat)

pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
           #           "topic_sim_matrix_alternative_01.pdf"),
           "topic_sim_matrix_alternative_02_NO_SIM_NUM.pdf"),
    width = 20, height = 14)
ggplot(sim_melt,
       aes(x = factor(Var2), y = factor(Var1), fill = value, label = round(value, 2))) +
  #geom_tile(color='white', alpha = 0.5) +
  geom_tile(color='white', alpha = 0.7) +
  #geom_text(color='black', size = 4.5) +
  scale_fill_continuous(low="#56B1F7", high="#132B43") +
  #scale_colour_gradient2(high = "#132B43",
  #                       low = "#56B1F7", midpoint = .95) +
  xlab("Topics from Alternative 02 (C)\n") +
  ylab("\nTopics from Original Model\n") +
  coord_flip()+
  # - highlight the rowS for Topic 3 and 6 alternative model
  geom_vline(xintercept = 2.5, color = "red", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 3.5, color = "red", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 5.5, color = "red", size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 6.5, color = "red", size = 1.5, alpha = 0.7) +
  theme(panel.background = element_rect("white"),
        legend.position="none",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22))
dev.off()


