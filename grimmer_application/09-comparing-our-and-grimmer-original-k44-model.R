#===============================================================================
# 09-comparing-our-and-grimmer-original-k44-model.R
# Purpose: comparing the top topic features for our 44 topics and grimmer's 44
#           topics when fitting a 44-topic Expressed Agenda Model.
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(ldaRobust)
library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)

# PATHS & CONSTANTS
#===============================================================================
data_path <- "~/Google Drive/andreu_tia/projects/rlda/Data/03-paper-data/"


# DATA
#===============================================================================
# - loading OUR Expressed Agenda Model (EAM) results
#print(load(paste0(data_path, "Grimmer/grimmer_41-47/grimmer_41-47_raw.Rdata")))
print(load(paste0(data_path, "Grimmer_approx/new_grimmer_approx_rldagen.Rdata")))
our_betas <- r_new_approx_sim@beta_list[[4]]

# - loading grimmer's EAM results (and the word features)
print(load(paste0(data_path, "Grimmer/FinalModel.Rdata")))
grimmer_betas <- output$etas
print(load(paste0(data_path, "Grimmer/WordsUsed.Rdata")))
colnames(grimmer_betas) <- words2
colnames(our_betas) <- words2

# DATA WRANGLING -- MAIN
#===============================================================================
# - calculate cosine similarity between our and grimmer's resulting topics
# ... initialize the output matrix
sim_mat <- matrix(nrow = nrow(our_betas), ncol = nrow(grimmer_betas))

# - iterate through rows(betas--topics)
for (i in 1:nrow(our_betas)) {
  # - pull the beta vector for topic i
  topic_01 <- our_betas[i, ]
  # - iterate and compare to all other rows(betas--topics)
  for (j in 1:nrow(grimmer_betas)) {
    topic_02 <- grimmer_betas[j,]
    sim_i_j <- cosine(topic_01, topic_02)
    # - store this similarity in the initialized output matrix
    sim_mat[i, j] <- sim_i_j
  }
}

# - similarity matrix to two-column long dataframe
sim_mat_02 <- as.data.frame(sim_mat)
colnames(sim_mat_02) <- paste0("grimmer_", sprintf("%02d", 1:ncol(sim_mat)))
sim_mat_02$our_topics <- paste0("our_", sprintf("%02d", 1:nrow(sim_mat)))
plot_db <- as.data.frame(sim_mat_02) %>%
  gather(grimmer_topics, value, -our_topics)

# - add our and grimmer's top topic features into this dataframe to plot
our_top_features <-  sapply(1:nrow(our_betas), function(i)
  paste0(as.character((data.frame(pr = our_betas[i,], feature = names(our_betas[i,])) %>%
                  dplyr::arrange(desc(pr)) %>%
                  head(n = 5))$feature), collapse = ","))

our_top_features_02 <- data.frame(
  our_topics = paste0("our_", sprintf("%02d", 1:nrow(sim_mat))),
  our_features = our_top_features
)

grimmer_top_features <-  sapply(1:nrow(grimmer_betas), function(i)
  paste0(as.character((data.frame(pr = grimmer_betas[i,], feature = names(grimmer_betas[i,])) %>%
                         dplyr::arrange(desc(pr)) %>%
                         head(n = 5))$feature), collapse = ","))

grimmer_top_features_02 <- data.frame(
  grimmer_topics = paste0("grimmer_", sprintf("%02d", 1:nrow(sim_mat))),
  grimmer_features = grimmer_top_features
)

plot_db$our_topics <- as.character(plot_db$our_topics)
our_top_features_02$our_topics <- as.character(our_top_features_02$our_topics)
plot_db_02 <- left_join(plot_db, our_top_features_02)

plot_db_02$grimmer_topics <- as.character(plot_db_02$grimmer_topics)
grimmer_top_features_02$grimmer_topics <- as.character(grimmer_top_features_02$grimmer_topics)
plot_db_final <- left_join(plot_db_02, grimmer_top_features_02)

# - transform similarity values into categorical
plot_db_final$value_cat <- cut(plot_db_final$value,
                               breaks=c(-Inf,.50, .70, 0.9, 0.95, Inf),
                               labels=c("Very Low <.5", "Low [.5-.7)",
                                        "Medium [.7-.9)", "High [.9-.95)",
                                        "Very High >=.95"))
plot_db_final$value_cat <- factor(plot_db_final$value_cat,
                                  levels = c("Very Low <.5", "Low [.5-.7)",
                                             "Medium [.7-.9)", "High [.9-.95)",
                                             "Very High >=.95"))

plot_db_final <- plot_db_final %>%
  arrange(desc(value))

# - looking each of our topics to the most similar topic by Grimmer
count <- 0
aligned_db <- NULL
for (our_t in unique(plot_db_final$our_topics)) {
  count <- count + 1
#  if (count == 10) {
#    break
#  }
  if (our_t == unique(plot_db_final$our_topics)[1]) {
    our_t_db <- plot_db_final %>% filter(our_topics == our_t)
  } else {
    our_t_db <- plot_db_final %>% filter(our_topics == our_t)
    our_t_db <- our_t_db %>%
      filter(!(grimmer_features %in% aligned_db$grimmer_features))
  }
  our_t_db <- our_t_db %>% arrange(desc(value))
  new_row <- data.frame(
    our_features = our_t_db$our_features[1],
    value = our_t_db$value[1],
    grimmer_features = our_t_db$grimmer_features[1]
  )
  aligned_db <- rbind(aligned_db, new_row)
}

plot_db_final$grimmer_features <- factor(plot_db_final$grimmer_features,
                                         levels = aligned_db$grimmer_features)
plot_db_final$our_features <- factor(plot_db_final$our_features,
                                     levels = unique(plot_db_final$our_features))

# PLOT
#===============================================================================
#pdf("./images/our-grimmer-EAM-comparison-UPD.pdf", width = 18, height = 10)
ggplot(plot_db_final,
       aes(x = our_features, y = grimmer_features, fill = value_cat)) +
  geom_tile(col = "white") +
  scale_fill_manual("Topic Similarity",
                    values = c("gray90", "gray85", "gray60", "gray30", "gray20")) +
  scale_y_discrete("\nTop topic features (Grimmer's Model)") +
  scale_x_discrete("Top topic features (our Model)\n") +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_blank()
  )
dev.off()
