#===============================================================================
# 03-comparing-42-44-46-lda-models-grimmer.R
# Purpose: comparing topic models with very similar number of topics to show
#           that these models present a very similar description of the data
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ldaRobust)
library(xtable)

# PATHS & CONSTANTS
#===============================================================================
data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================

# - load the rlda object with the list of LDA topics estiamted using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

# - pulling the top topic features for the 3 models of interest

# ... first, the top topic features of the  "original" model
model44_topfeatures <- NULL
for (topic_i in 1:44) {
  topic_i_topfeatures <- data.frame(
    feature = r@lda_u@terms,
    beta = r@lda_u@beta[topic_i,]
  ) %>%
    arrange(desc(beta)) %>%
    head(n = 10)
  model44_topfeatures <- cbind(model44_topfeatures,
                               as.character(topic_i_topfeatures$feature))
}

# ... 42 and 46-topic model key features
model42_topfeatures <- r@key_features[[4]]
model46_topfeatures <- r@key_features[[7]]

# - re-arrange the order of the topics in the 42 and 46-topic models so they
#   are sorted in a way that they align well (in terms of topic similarity) with
#   the topic indeces in the original 44-topic model. The goal is to create a
#   table showing the topics of all three models, and illustrate they are quite
#   similar but still slightly different

# MAIN
#===============================================================================
# ... matching each topic in the "original" model with a topic from the 42-topic
#     model
output <- data.frame(
  m44_i = c(1:44, NA, NA),
  m44_feature = c(sapply(1:44, function(i)
    paste0(model44_topfeatures[,i], collapse = ", ")), NA, NA),
  m42_i = NA,
  m42_feature = NA,
  m42_44_i_sim = NA,
  m46_i = NA,
  m46_feature = NA,
  m46_44_i_sim = NA
)

# - iterate through topics of the 42-topic model
sim_mat_m42 <- r@similarity_mat_list[[4]]
for (i in 1:42) {
  # - find the most similar topic to this topic in the "original" model
  sim_vector <- sim_mat_m42[,i] # ... pull similarities with the m44 topic
  #sim_vector[matched_topics_m42] <- NA # ... don't consider already matched tops
  matched_topic_i <- which(sim_vector == max(sim_vector, na.rm = TRUE))
  matched_topic_i_sim <- max(sim_vector, na.rm = TRUE)
  matched_topic_features <- paste0(model42_topfeatures[,i], collapse = ", ")

  # - check if this original topic has already been matched
  if (!is.na(output$m42_i[which(output$m44_i == matched_topic_i)])) {
    done <-  FALSE
    counter <- 0
    while (!done & (counter < 42)) {
      counter <- counter + 1
      # - if so, check if the new matched similarity is highier
      if (output$m42_44_i_sim[which(output$m44_i == matched_topic_i)] <
          matched_topic_i_sim) {
        # - if so, replace the matched topic and the values in the output table
        output$m42_i[matched_topic_i] <- i
        output$m42_feature[matched_topic_i] <- matched_topic_features
        output$m42_44_i_sim[matched_topic_i] <- round(matched_topic_i_sim, 2)
        done <- TRUE
      } else {
        sim_vector[matched_topic_i] <- NA # ... don't consider this possible match
        # - consider instead the next most possible match
        matched_topic_i <- which(sim_vector == max(sim_vector, na.rm = TRUE))
        matched_topic_i_sim <- max(sim_vector, na.rm = TRUE)
        matched_topic_features <- paste0(model42_topfeatures[,i], collapse = ", ")
      }
    }
  }
  # - if no previous match, simply add match and info to output table
  output$m42_i[matched_topic_i] <- i
  output$m42_feature[matched_topic_i] <- matched_topic_features
  output$m42_44_i_sim[matched_topic_i] <- round(matched_topic_i_sim, 2)
}

# - checking we matched all 42 topics
length(unique(na.omit(output$m42_i))) # No, 1 hasn't been matched. Which means
# that the original topic we would have matched it, has a better match with
# another topic of this alternative model

# - adding this non-matched topic to the end of the table
remaining_42_i <- which(!(1:42 %in% output$m42_i))
counter <- 0
for (topic_i in remaining_42_i) {
  counter <- counter + 1
  topic_features <- paste0(model42_topfeatures[, topic_i], collapse = ", ")
  output$m42_i[(44 + counter)] <- topic_i
  output$m42_feature[(44 + counter)] <- topic_features
}

# - iterate now through the 46 topics and matching them as well
sim_mat_m46 <- r@similarity_mat_list[[7]]
for (i in 1:46) {
  # - find the most similar topic to this topic in the "original" model
  sim_vector <- sim_mat_m46[,i] # ... pull similarities with the m44 topic
  #sim_vector[matched_topics_m46] <- NA # ... don't consider already matched tops
  matched_topic_i <- which(sim_vector == max(sim_vector, na.rm = TRUE))
  matched_topic_i_sim <- max(sim_vector, na.rm = TRUE)
  matched_topic_features <- paste0(model46_topfeatures[,i], collapse = ", ")

  # - check if this original topic has already been matched
  if (!is.na(output$m46_i[which(output$m44_i == matched_topic_i)])) {
    done <-  FALSE
    counter <- 0
    while (!done & (counter < 46)) {
      counter <- counter + 1
      # - if so, check if the new matched similarity is highier
      if (output$m46_44_i_sim[which(output$m44_i == matched_topic_i)] <
          matched_topic_i_sim) {
        # - if so, replace the matched topic and the values in the output table
        output$m46_i[matched_topic_i] <- i
        output$m46_feature[matched_topic_i] <- matched_topic_features
        output$m46_44_i_sim[matched_topic_i] <- round(matched_topic_i_sim, 2)
        done <- TRUE
      } else {
        sim_vector[matched_topic_i] <- NA # ... don't consider this possible match
        # - consider instead the next most possible match
        matched_topic_i <- which(sim_vector == max(sim_vector, na.rm = TRUE))
        matched_topic_i_sim <- max(sim_vector, na.rm = TRUE)
        matched_topic_features <- paste0(model46_topfeatures[,i], collapse = ", ")
      }
    }
  }
  # - if no previous match, simply add match and info to output table
  output$m46_i[matched_topic_i] <- i
  output$m46_feature[matched_topic_i] <- matched_topic_features
  output$m46_44_i_sim[matched_topic_i] <- round(matched_topic_i_sim, 2)
}

# - checking we matched at least 44 of the 46 topics
length(unique(na.omit(output$m46_i))) # Yes!

# - and now adding the remaining two topics at the end of the table
remaining_46_i <- which(!(1:46 %in% output$m46_i))
counter <- 0
for (topic_i in remaining_46_i) {
  counter <- counter + 1
  topic_features <- paste0(model46_topfeatures[, topic_i], collapse = ", ")
  output$m46_i[(44 + counter)] <- topic_i
  output$m46_feature[(44 + counter)] <- topic_features
}

# OUTPUT -- TABLE FOR THE PAPER
#===============================================================================
# - a table showing these topics from these 3 models and their similarities and
#   disimilarities
print(xtable(output %>% dplyr::select(-m42_i, -m46_i)), include.rownames = FALSE)

