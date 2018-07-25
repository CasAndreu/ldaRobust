#==============================================================================
# 01-replication-grimmer-home-style-measure.R
# Purpose: using our approximation of Grimmer's models to predict the topics in
#           Senator's text, then the balance they give to Credit Clamin v.
#           Position Taking, and then the replicate his findings.
# Author: Andreu Casas
#==============================================================================

# PACKAGES
#==============================================================================
library(dplyr)
library(ldaRobust)
library(lsa)

# PATHS & CONSTANTS
#==============================================================================
data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA WRANGLING
#==============================================================================

# - loading our approximations of Grimmer's model using different #topics
#   (object name: "r_approx")
print(load(paste0(data_path, "03-paper-data/Grimmer/rldagen_grimmer.RData")))

# - pull the 44-topic model: our approximation of the model he uses in the paper
paper_model <- r_approx@beta_list[[6]]

# - create a data frame with the top-10 features from each topic in this model
paper_model_top_features <- NULL
for (topic_i in 1:nrow(paper_model)) {
  #  - pull the feature weights for this topic
  topic <- data.frame(
    betas = paper_model[topic_i,]
  )
  topic$features <- rownames(topic)

  # - sort them and take the top 10. Then add them to the dataframe outside loop
  top_topic_feautres <- topic %>%
    arrange(desc(betas)) %>%
    head(n = 10)
  top_topic_feautres_list <- paste0(sapply(1:nrow(top_topic_feautres), function(i)
    paste0(top_topic_feautres$features[i], " (",
           round(top_topic_feautres$betas[i], 2), ")")), collapse = ", ")
  topic_info_to_merge <- data.frame(
    topic_i = topic_i,
    top_features = top_topic_feautres_list
  )
  paper_model_top_features <- rbind(paper_model_top_features, topic_info_to_merge)
}

# DATA WRANGLING -- MANUAL MATCHING OF OUR APPROXIMATED TOPICS AND GRIMMER'S
#                   TOPICS IN TABLE 1 OF THE PAPER
#==============================================================================

# - export now this into a CSV so we can manually see how these match to Grimmer
#   topics in his paper. After manually matching them, we can know which ones
#   are about Credit Claiming v. Position Taking

#write.csv(paper_model_top_features, paste0(
#  data_path,
#  "03-paper-data/Grimmer/our_approximation_44topic_model_top_features.csv"
#), row.names = FALSE)

# - import back this dataset but now with information about how these topics
#   match to Grimmer's original topics in the paper
grimmer_matched <- read.csv(
  paste0(
    data_path,
    "03-paper-data/Grimmer/",
    "our_approximation_44topic_model_top_features_MATCH_INFO.csv")
)

# - importing as well Table 1 from Grimmer's paper, which contains information
#   about his original models
grimmer_table1 <- read.csv(
  paste0(
    data_path, "03-paper-data/Grimmer/grimmer_44topics_in_table1.csv"
  )
)

# - we have NOT been able to match 9 of the 45 original topics for Grimmer. For
#   these other 9 topics that we got, I gave them a new label and also coded
#   them for whether they should be considered as being Position Taking or
#   Credit Claiming topics (or none fo the two topic types)

# DATA WRANGLING -- AUTOMATIC MATCHING OF OUR APPROXIMATED TOPICS AND GRIMMER'S
#                   TOPICS IN THE REPLICATION OBJECT
#==============================================================================

# - loading the topic model Grimmer has in his replication material
#   (object: "output")
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/FinalModel.RData"
)))

# - loading the unique feautres in his TDM (object: words2)
print(load(paste0( # (object: words2)
  data_path,
  "01-replication-data/02-Grimmer-2012/WordsUsed.RData"
)))

# - makig sure the features are in the same order in our and his TDM
words2[c(1, 500, 1000, 1500, 2000)]
colnames(paper_model)[c(1, 500, 1000, 1500, 2000)] # yes they are!

# - computing cosine similarity between OUR and HIS topic betas
topics_sim <- as.data.frame(matrix(nrow = 44, ncol = 44))
rownames(topics_sim) <- paste0("GRIMMER_", 1:44)
colnames(topics_sim) <- paste0("OUR_", 1:44)
for (i in 1:44) {
  for (j in 1:44) {
    betas_i <- output$etas[i,]
    betas_j <- paper_model[j,]
    i_j_cos <- cosine(betas_i, betas_j)
    topics_sim[i,j] <- i_j_cos
  }
}

# - a dataset showing the index of Grimmer's topic that is most similar to each
#   of our approximations
auto_match_summary <- data.frame(
  our_topic_i = 1:44,
  grimmer_topic_i = as.numeric(sapply(1:44, function(j)
    which(topics_sim[,j] == max(topics_sim[,j])))),
  cos_sim = as.numeric(sapply(1:44, function(j)
    max(topics_sim[,j])))
)

# - adding to this match summary dataset the top5 features from our model and
#   the top 10 features from his model
auto_match_summary$out_top10 <- NULL
for (topic_j in 1:nrow(paper_model)) {
  #  - pull the feature weights for this topic
  topic <- data.frame(
    betas = paper_model[topic_j,]
  )
  topic$features <- rownames(topic)

  # - sort them and take the top 10. Then add them to the dataframe outside loop
  top_topic_feautres <- topic %>%
    arrange(desc(betas)) %>%
    head(n = 10)
  top_topic_feautres_list <- paste0(top_topic_feautres$features,
                                    collapse = ", ")
  auto_match_summary$out_top5[topic_j] <- top_topic_feautres_list
}

auto_match_summary$grimmer_top10 <- NA
for (topic_i in 1:44) {
  # - pull the betas for this topic
  topic_i_betas <- output$etas[topic_i,]
  # - merge with the features
  betas_feature_df <- data.frame(
    feature = words2,
    beta = topic_i_betas
  )
  # - sort and pull the top 10 features
  betas_feature_df <- betas_feature_df %>%
    arrange(desc(beta)) %>%
    head(n = 10)
  if (topic_i %in% auto_match_summary$grimmer_topic_i) {
    auto_match_summary$grimmer_top10[which(
      auto_match_summary$grimmer_topic_i == topic_i)] <- paste0(
        as.character(betas_feature_df$feature), collapse = ",")
  }
}

# - following this procedure, we are unable to match 8 topics
length(which(!(1:44 %in% auto_match_summary$grimmer_topic_i)))

# - and 7 topics are matched to 2 of our topics (instead of one)
length(which(table(auto_match_summary$grimmer_topic_i) > 1))

# This could be because of:
#   A) We estimated Grimmer's model slightly different? (we followed B.Stewart's
#      code, which might not exatcly be the one exactly used by Grimmer?)
#      /!\ Ask Justin Grimmer for his code!!! /!\
#   B) Different initializations and model convergence?

# For now, let's focus the anlaysis on 18 topics we are certain to be the same.
#     And let's explore how results might change depending on the amount of
#     topics we decide to originally fit.


# VALIDATION OF THE AUTO-MATCHING --
# A) CHECKING THAT WE ACTUALLY PUT ABOUT THE SAME AMOUNT OF DOCUMENTS INTO THESE
#    TOPICS
#==============================================================================
# - selecting the clearly matched topics
clear_matched_topics <- auto_match_summary %>%
  filter(cos_sim > .91)

# - for each of them, calculating the proportion of docs that have it as max
#   class (both according to us and to Grimmer)
clear_matched_topics$docs_prop_our <- NA
clear_matched_topics$docs_prop_grimmer <- NA

our_topic_preds <- round(r_approx@gamma_list[[6]], 2)
our_topic_assignments <- as.numeric(
  as.character(sapply(1:nrow(our_topic_preds), function(i)
  which(our_topic_preds[i,] == max(our_topic_preds[i,])))))

his_topic_preds <- round(output$rs, 2)
his_topic_assignments <- as.numeric(
  as.character(sapply(1:nrow(his_topic_preds), function(i)
    which(his_topic_preds[i,] == max(his_topic_preds[i,])))))

for (grimmer_i in unique(clear_matched_topics$grimmer_topic_i)) {
  grimmer_i_n <- length(which(his_topic_assignments == grimmer_i))
  grimmer_i_prop <- grimmer_i_n / length(his_topic_assignments)
  clear_matched_topics$docs_prop_grimmer[
    which(clear_matched_topics$grimmer_topic_i == grimmer_i)
  ] <- grimmer_i_prop
}

for (our_i in unique(clear_matched_topics$our_topic_i)) {
  our_i_n <- length(which(his_topic_assignments == our_i))
  our_i_prop <- our_i_n / length(our_topic_assignments)
  clear_matched_topics$docs_prop_our[
    which(clear_matched_topics$our_topic_i == our_i)
    ] <- our_i_prop
}


# MAIN -- REPLICATING GRIMMER'S PAPER USING OUR APPROXIMATED TOPICS
#==============================================================================

## Importing data from Grimmer's replication material

# ... the press releases from senators (N = 64,033)
docs <- read.csv(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/PressData.csv"
))

# ... the press releases in Sparse Matrix format (object: list.press)
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/DataFinal.RData"
)))

# ... the unique feautres in his TDM (object: words2)
print(load(paste0( # (object: words2)
  data_path,
  "01-replication-data/02-Grimmer-2012/WordsUsed.RData"
)))


## Data from our approximateions

# - pull the information about to which of our approximated-topics each of
#   the documents has been assigned (using data from the 44-topic model)
our_doc_top_pr <- as.matrix(r_approx@gamma_list[[6]])

# - create a dataframe where each document is a row and there is only one column
#   with the assigned approximated topic (out of the 44 possible topics)
our_assigned_topic <- data.frame(
  doc_i = 1:nrow(our_doc_top_pr),
  topic_i = as.numeric(sapply(1:nrow(our_doc_top_pr), function(i)
    which(our_doc_top_pr[i,] == 1)))
)

## Sanity check

# - make sure that the features for the first and e.g. 10000th document are the
#   same in our TDM and Grimmer's TDM

# - our dtm features
our_dtm_features <- r_approx@dtm$dimnames$Terms

# - the features from the first document [Akaka\\10Nov2005akaka13.txt]
first_doc_features_i <- r_approx@dtm$j[which(r_approx@dtm$i == 1)]
first_doc_features_OUR <- our_dtm_features[first_doc_features_i]

# ... and last document
last_doc_features_i <- r_approx@dtm$j[which(r_approx@dtm$i == max(
  r_approx@dtm$i
))]
last_doc_features_OUR <- our_dtm_features[last_doc_features_i]

# - the features from the first and last document in Grimmmer's TDM
first_doc_features_GRIMMER <- words2[list.press[[1]][[1]][[1]][,1]]
last_doc_features_GRIMMER <- words2[list.press[[length(list.press)]][[
  length(list.press[[length(list.press)]])]][[
    length(list.press[[length(list.press)]][[
      length(list.press[[length(list.press)]])]])
  ]][,1]]

# - checking that 100% of the features in OUR docs are in his docs as well
length(which(first_doc_features_OUR %in% first_doc_features_GRIMMER)) /
  length(first_doc_features_OUR)

length(which(last_doc_features_OUR %in% last_doc_features_GRIMMER)) /
  length(last_doc_features_OUR)


