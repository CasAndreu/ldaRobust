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
library(extrafont) # font_import(pattern = "lmroman*")
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

# - create a simplified version of the similarity variable (categorical)
sim_melt <- sim_melt %>%
  mutate(sim_cat = cut(value,
                       breaks = c(-Inf, 0.7, .8, .9, .95, Inf),
                       labels = c("Very Low", "Low", "Medium",
                                  "High", "Very High")))

p <- ggplot(sim_melt,
       aes(x = factor(Var2), y = factor(Var1), fill = sim_cat, label = round(value, 2))) +
  #geom_tile(color='white', alpha = 0.5) +
  geom_tile(color='white', alpha = 0.9) +
  #geom_text(color='black', size = 4.5) +
  scale_fill_manual(values = c("white", "gray90", "gray70", "gray50", "gray30")) +
  #scale_colour_gradient2(high = "#132B43",
  #                       low = "#56B1F7", midpoint = .95) +
  #xlab("Topics from Alternative 01 (B)\n") +
  #ylab("\nTopics from Original Model\n") +
  coord_flip()+
  # - highlight the column for Topic 16 original model
  geom_hline(yintercept = 15.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_hline(yintercept = 16.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_segment(x = 0.5, xend = 0.5, y = 15.5, yend = 16.5, color = "red",
               alpha = 0.7, size = 1) +
  geom_segment(x = 42.5, xend = 42.5, y = 15.5, yend = 16.5, color = "red",
               alpha = 0.7, size = 1) +
  # - highlight the row for Topic 19 alternative model
  geom_vline(xintercept = 17.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_vline(xintercept = 18.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_segment(y = 0.45, yend = 0.45, x = 17.5, xend = 18.5, color = "red",
               alpha = 0.7, size = 1) +
  geom_segment(y = 44.55, yend = 44.55, x = 17.5, xend = 18.5, color = "red",
               alpha = 0.7, size = 1) +
  scale_x_discrete("Topics from Alternative 01 (B)\n", expand = c(0,0)) +
  scale_y_discrete("\nTopics from Original Model\n", expand = c(0,0)) +
  theme(panel.background = element_rect("white"),
        legend.position="none",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        text = element_text(family = "LM Roman 10"),
        panel.border = element_rect(colour = "black", fill = NA))

ggsave(p, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
                             #           "topic_sim_matrix_alternative_01.pdf"),
                             "topic_sim_matrix_alternative_01_NO_SIM_NUM-02.pdf"),
       width = 20, height = 12, units = "in", device = cairo_pdf)

# A2)
# - Similarity plot "Original Model (44 topics) V. Alternative A (42 topics)"
similarity_list_mat = r@similarity_mat_list[[7]]

colnames(similarity_list_mat) = sapply(1:ncol(similarity_list_mat), function(z)
  paste0("C,", as.character(z)))

rownames(similarity_list_mat) = as.character(1:nrow(similarity_list_mat))

# - similarity matrix in long format so it can be plotted as a tile plot type
sim_melt = melt(similarity_list_mat)

# - create a simplified version of the similarity variable (categorical)
sim_melt <- sim_melt %>%
  mutate(sim_cat = cut(value,
                       breaks = c(-Inf, 0.7, .8, .9, .95, Inf),
                       labels = c("Very Low", "Low", "Medium",
                                  "High", "Very High")))

p2 <- ggplot(sim_melt,
       aes(x = factor(Var2), y = factor(Var1), fill = sim_cat, label = round(value, 2))) +
  #geom_tile(color='white', alpha = 0.5) +
  geom_tile(color='white', alpha = 0.9) +
  #geom_text(color='black', size = 4.5) +
  scale_fill_manual(values = c("white", "gray90", "gray70", "gray50", "gray30")) +
  #scale_colour_gradient2(high = "#132B43",
  #                       low = "#56B1F7", midpoint = .95) +
  xlab("Topics from Alternative 02 (C)\n") +
  ylab("\nTopics from Original Model\n") +
  coord_flip()+
  # - highlight the rowS for Topic 3 and 6 alternative model
  geom_vline(xintercept = 2.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_vline(xintercept = 3.5, color = "red", size = 1.1, alpha = 0.7) +
  #geom_vline(xintercept = 5.5, color = "red", size = 1.1, alpha = 0.7) +
  #geom_vline(xintercept = 6.5, color = "red", size = 1.1, alpha = 0.7) +
  geom_segment(y = 0.45, yend = 0.45, x = 2.5, xend = 3.5, color = "red",
               alpha = 0.7, size = 1) +
  geom_segment(y = 44.55, yend = 44.55, x = 2.5, xend = 3.5, color = "red",
               alpha = 0.7, size = 1) +
  # geom_segment(y = 0.45, yend = 0.45, x = 5.5, xend = 6.5, color = "red",
  #              alpha = 0.7, size = 1) +
  # geom_segment(y = 44.55, yend = 44.55, x = 5.5, xend = 6.5, color = "red",
  #              alpha = 0.7, size = 1) +
  theme(panel.background = element_rect("white"),
        legend.position="none",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        text = element_text(family = "LM Roman 10"),
        panel.border = element_rect(colour = "black", fill = NA))


ggsave(p2, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
                            #           "topic_sim_matrix_alternative_01.pdf"),
                            "topic_sim_matrix_alternative_02_NO_SIM_NUM-02.pdf"),
       width = 20, height = 12, units = "in", device = cairo_pdf)

