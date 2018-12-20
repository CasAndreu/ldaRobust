#===============================================================================
# 07-strict-cluster-function-and-results.R
# Purpose: own cluster algorithm, based on a strict similarity threshold and
#           prioritizing keeping the topics in the original model.
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(extrafont) # font_import(pattern = "lmroman*") # loadfonts()
library(ldaRobust)
#library(reshape2)
library(ggplot2)
#library(MASS)
library(lme4)
library(lsa)
library(Hmisc)
library(weights)

# PATHS & CONSTANTS
#===============================================================================
#data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")
#data_path <- paste0("~/Desktop/Google Drive/andreu_tia/projects/rlda/Data/")
data_path <- paste0("~/Desktop/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================
# - load the rlda object with the list of LDA topics estamated using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  #"03-paper-data/Grimmer_lda/results.RData"
  "03-paper-data/Grimmer_lda_41-47/grimmer_clus.RData"
)))
r <- r_clus

# - load a dataset with doc-level covariates
doc_covs <- read.csv(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/PressData.csv"
))

# - author-year level covariates
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/variables.RData"
)))

# - loading aggregated measures of credit claiming and position taking from
#   Grimmer's replication data
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/DepVars.RData"
)))

# MAIN
#===============================================================================

# [ A ] CREATING SIMILARITY MATRIX: How similar all topics are to each other
#-------------------------------------------------------------------------------

# A beta (topic) matrix: size (#Topics X #Words)

# - the first rows are the topics from the original model
beta_mat <- r@lda_u@beta

# - now adding the topics from the alternative models. Starting with the
#   alternative model with the fewest topics and ending with the one with the
#   most topics
for (topic_model_i in 1:length(r@beta_list)) {
  beta_mat <- rbind(beta_mat, r@beta_list[[topic_model_i]])
}


# Creating now the similarity matrix

# - a function performing this
get_sim_matrix <- function(beta_mat, method = "cosine") {
  # - initialize the output matrix
  out <- matrix(nrow = nrow(beta_mat), ncol = nrow(beta_mat))

  # - iterate through rows(betas--topics)
  for (i in 1:nrow(beta_mat)) {
    # - pull the beta vector for topic i
    topic_01 <- beta_mat[i, ]
    # - iterate and compare to all other rows(betas--topics)
    for (j in 1:nrow(beta_mat)) {
      topic_02 <- beta_mat[j,]
      if (method == "cosine") {
        sim_i_j <- cosine(topic_01, topic_02)
      }
      # - store this similarity in the initialized output matrix
      out[i, j] <- sim_i_j
    }
  }
  return(out)
}

sim_mat <- get_sim_matrix(beta_mat)

# [ B ] CLUSTERING THE TOPICS
#-------------------------------------------------------------------------------

# A function performing the desired clustering

# - some parameters needed
# ... list of number of topics in these models: sorted in the same way they are
#     sorted in the similarity matrix ('sim_mat'). We always follow this
#     convention: firts the number of topics in the original model, then the
#     number of topics in the alternative models (ascending sorting)
k_list <- c(r@lda_u@k, r@K)
# ... similarity threshold that will be used to judge whether two topics are the
#     same
sim_threshold <- .93#r@threshold # 0.95

# - the function
get_cluster_matrix <- function(sim_mat, k_list, sim_threshold) {
  # - an integer indicating the total number of topics we are considering
  n <- sum(k_list)

  # - a vector indicating the indices of the topics in the original model,
  #   from 1 to ... k_{O}
  or_topics_indices <- (1:k_list[1])
  or_topics_final_i <- k_list[1]

  # - a matrix only with the similarity between original and alternative topics
  or_alt_sim_mat <- sim_mat[or_topics_indices, ]

  # - initialize output object: #Topic x 1
  out <- matrix(nrow = sum(n), ncol = 1)

  # - we want the topics in the original models to have all their own cluster,
  #   so giving them the first k clusters
  out[or_topics_indices,] <- or_topics_indices

  # 1) MATCHING TOPICS TO ORIGINAL MODELS
  # - now looking which topics from alternative models are very similar to these
  #   original topics and adding them to their cluster. If a topic from an
  #   alternative model meets the similarity threshold for more than 1 original
  #   topic, we assign it to the original topic/cluster to which is the most
  #   similar
  alternative_topics_i <- (or_topics_final_i + 1):n
  # ... iterating through original topics
  for (i in or_topics_indices) {
    or_topic_info <- sim_mat[i,]
    # - check which alternative models have already been assigned to clusters
    assigned_alt_indices <- which(!(is.na(out[,1])))
    # - check what remaning alternative topics meet sim threshold to this topic
    matches_indices <- which(or_topic_info > sim_threshold)
    matches_indices <- matches_indices[which(!(
      matches_indices %in% c(or_topics_indices, assigned_alt_indices)))]
    if (length(matches_indices) > 0) {
      # - now checking that these matched alternative topics are not more similar
      #   to other original topics
      for (j in matches_indices) {
        matches_to_rm <- NULL
        alt_topic_info <- or_alt_sim_mat[,j]
        if (which(alt_topic_info == max(alt_topic_info)) != i) {
          matches_to_rm <- c(matches_to_rm, j)
        }
      }
      matches_indices <- matches_indices[which(!(matches_indices %in%
                                                   matches_to_rm))]
      # - store this cluster-assingment
      out[matches_indices,] <- i
    }
  }

  # - count number of unmatched alternative models
  unmatched_alt_topics_n <- length(which(is.na(out[,1])))

  if (unmatched_alt_topics_n > 0) {
    # 2) BUILDING NEW CLUSTERS FOR THE UNMATCHED TOPICS
    # - a vector with the indeces of the unmatched topics
    unmatched_top_indices <- which(is.na(out[,1]))
    # - a similarity matrix only with info on how similar the unmatched topics are
    #   to each other
    unmatched_sim_mat <- sim_mat[unmatched_top_indices, unmatched_top_indices]
    if (length(unmatched_top_indices) > 1) {
      # - iterate through unmatched topic and create new clusters
      for (z in 1:nrow(unmatched_sim_mat)) {
        # - check if not assigned to any cluster yet
        if (is.na(out[unmatched_top_indices[z],1])){
          # - assign a new cluster number to this topic
          out[unmatched_top_indices[z],1] <- max(out[,1], na.rm = TRUE) + 1
          # - now look for very similar unmatched topic to match to this new cluster
          unmatched_top <- unmatched_sim_mat[z,]
          new_matched_indices <- which(unmatched_top > 0.95)
          new_matched_indices <- new_matched_indices[which(!(
            new_matched_indices %in% c(z, which(
              !is.na(out[unmatched_top_indices,1]))
          )))]
          # - include these very similar unmatched topics into the same new clsuter
          out[unmatched_top_indices[new_matched_indices],1] <- max(out[,1],
                                                                   na.rm = TRUE)

        }
      }
    } else {
      out[unmatched_top_indices] <- max(out[,1], na.rm = TRUE) + 1
    }
  }
  return(out)
}

cluster_mat <- get_cluster_matrix(sim_mat, k_list, sim_threshold)


# [ C ] GETTING THE TOP FEATURES OF THE CLUSTER CENTROIDS
#-------------------------------------------------------------------------------
# - A Function performing this task
rlda_obj <- r

get_cluster_top_features <- function(cluster_mat, beta_mat, k_list, rlda_obj,
                                     n = 6) {
  # - labeling the columns(features) of the beta matrix
  beta_df <- as.data.frame(beta_mat)
  colnames(beta_df) <- rlda_obj@dtm$dimnames$Terms

  # - initializing output
  out <- as.data.frame(matrix(nrow = max(cluster_mat[,1]), ncol = 2))
  colnames(out) <- c("cluster_num", "top_features")
  out$cluster_num <- 1:nrow(out)

  # - we assign the top predictive features from each original topic to the
  #   clusters of those topics: not actually calculating any centroid
  for (i in 1:rlda_obj@lda_u@k) {
    top_features <- data.frame(
      features = names(beta_df[i,]),
      betas = as.numeric(beta_df[i,])) %>%
      arrange(desc(betas)) %>%
      head(n = n)
    top_features_str <- paste0(top_features$features, collapse = ", ")
    out$top_features[i] <- top_features_str
  }

  # - For the rest of the topics, calculate the centroid first (average betas),
  #   and then pull the most predictive features according to the centroid
  for (z in ((rlda_obj@lda_u@k + 1):max(cluster_mat[,1]))) {
    # - pull the indeces of the topics in this cluster
    cluster_top_indices <- which(cluster_mat[,1] == z)

    # - pull the betas of this/these topics
    top_betas <- beta_df[cluster_top_indices,]

    # - if only 1 topic in this cluster, that's the centroid, otherwise take avg
    if (nrow(top_betas) == 1) {
      centroid <- top_betas
    } else {
      centroid <- colMeans(top_betas)
    }
    centroid_df <- data.frame(
      betas = as.numeric(centroid),
      features = names(centroid)
    ) %>%
      arrange(desc(betas)) %>%
      head(n = n)
    centroid_str <- paste0(centroid_df$features, collapse = ", ")
    out$top_features[z] <- centroid_str
  }
  return(out)
}

cluster_top_features <- get_cluster_top_features(cluster_mat, beta_mat,
                                                 k_list, rlda_obj, n = 6)


# [ D ] VISUALIZING WHICH ORIGINAL TOPICS IN ALTERNATIVE MODELS, and viscversa
#-------------------------------------------------------------------------------

# A matrix indicating whether original topics are present in alternative models

# - initialize matrix: #Original-topics x #Alternative-models
or_topics_alt_models_mat <- as.data.frame(matrix(nrow = k_list[1],
                                                 ncol = (length(k_list)-1)))

# - iterate through original topics and check whether they are in alternative
#   models
for (i in (1:k_list[1])) {
  for (j in 1:(length(k_list)-1)) {
    # - pull the number of topics of the first alternative model
    j_num_topics <- k_list[j + 1]
    # - calculate the row indices of the topics of this alternative model in the
    #   cluster matrix
    if (j == 1) {
      cluster_mat_alt_model_indices <- (k_list[1] + 1):(
        k_list[1] + j_num_topics)
    } else {
      cluster_mat_alt_model_indices <- (sum(k_list[1:j]) + 1):(
        (sum(k_list[1:j]) + j_num_topics))
    }
    # - pull the topic-clusters found in this model
    alt_model_clusters <- cluster_mat[cluster_mat_alt_model_indices,1]
    # - check if original topic i is in there
    if (i %in% alt_model_clusters) {
      # - count how many times is present in this alternative model
      x <- length(which(alt_model_clusters == i))
    } else {
      x <- 0
    }
    # - add the information to the initialized matrix
    or_topics_alt_models_mat[i,j] <- x
  }
}

# A matrix indicating whether NEW topic-clusters are present in alternative
# models
new_topics_alt_models_mat <- as.data.frame(matrix(
  nrow = length((k_list[1] + 1):max(cluster_mat[,1])),
  ncol = (length(k_list)-1)))

# - iterate through original topics and checking whether they are in alternative
#   models
for (i in ((k_list[1] + 1):max(cluster_mat[,1]))) {
  for (j in 1:(length(k_list)-1)) {
    # - pull the number of topics of the first alternative model
    j_num_topics <- k_list[j + 1]
    # - calculate the row indices of the topics of this alternative model in the
    #   cluster matrix
    if (j == 1) {
      cluster_mat_alt_model_indices <- (k_list[1] + 1):(
        k_list[1] + j_num_topics)
    } else {
      cluster_mat_alt_model_indices <- (sum(k_list[1:j]) + 1):(
        (sum(k_list[1:j]) + j_num_topics))
    }
    # - pull the topic-clusters found in this model
    alt_model_clusters <- cluster_mat[cluster_mat_alt_model_indices,1]
    # - check if original topic i is in there
    if (i %in% alt_model_clusters) {
      # - count how many times is present in this alternative model
      x <- length(which(alt_model_clusters == i))
    } else {
      x <- 0
    }
    # - add the information to the initialized matrix
    new_topics_alt_models_mat[i - (k_list[1]),j] <- x
  }
}

# Merge these two matrices
top_stability_mat <- rbind(
  or_topics_alt_models_mat,
  new_topics_alt_models_mat
)

# Add the information about each topic-cluster top features
# ... naming the alternative models
names(top_stability_mat) <- paste0("k_", k_list[2:length(k_list)])
# ... top features
top_stability_mat$top_features <- cluster_top_features$top_features
# ... numbering the clusters
top_stability_mat$top_cluster_num <- paste0(sprintf("%02d",
                                                    1:nrow(top_stability_mat)))


# /!\ EXPORT a copy of this topic-cluster stability dataset in order to:
#       * manually add a Topic-Cluster label
#       * manually add Topic-Cluster-level covariates: type of issue

# write.csv(top_stability_mat, paste0(
#   data_path,
#   "03-paper-data/Grimmer_lda_41-47/grimmer_strinct_cluster_info_41_47.csv"),
#   row.names = FALSE)

# /!\ IMPORT the dataset with the manually labeled data back in
cluster_info <- read.csv(paste0(
    data_path,
    "03-paper-data/Grimmer_lda_41-47/grimmer_strinct_cluster_info_41_47_LABELED.csv"
))

# Preparing the data to plot
plot_db <- cluster_info %>%
  dplyr::select(-type, -grimmer_clear_match) %>%
  gather(model, value, -top_features, -top_cluster_num, -original,
         -cluster_label) %>%
  mutate(labels = paste0(sprintf("%02d", top_cluster_num), ". ",
                         cluster_label)) %>%
  mutate(labels = ifelse(labels == "46. Opiod Crisis",
                         "46. Opioid Crisis",
                         as.character(labels)),
         labels = ifelse(labels == "29. Economic Development",
                         "29. Regional Development",
                         as.character(labels))) %>%
  mutate(top_cluster_num = factor(top_cluster_num,
                                  levels = sort(unique(top_cluster_num),
                                                decreasing = TRUE))) %>%
  arrange(desc(top_cluster_num))

# - calculate, for each alternative model, the proportion of original models
#   they have in them
alternative_model_weights <- NULL
for (k in r@K) {
  model_label <- paste0("k_", k)
  y <- cluster_info[,model_label]
  y_prop <- length(which(y > 0)) / length(y)
  new_row <- data.frame(
    model = model_label,
    weight = y_prop
  )
  alternative_model_weights <- rbind(alternative_model_weights, new_row)
}

plot_db <- plot_db %>%
  mutate(labels = factor(as.character(labels),
                         levels = rev(unique(plot_db$labels))),
         value_binary = ifelse(value > 0, 1, 0),
         model = gsub("_", " = ", model))

#pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#           "topic_presence_41_47-STRICT.pdf"),
#    width = 20, height = 18)
ggplot(plot_db,
       aes(y = as.numeric(as.factor(labels)), x = model,
           fill = as.character(value_binary))) +
  geom_tile(aes(alpha = as.character(original)), color = "gray20") +
  geom_hline(yintercept = 2.5, size = 1.5) +
  geom_vline(xintercept = 3.5, color = "red", alpha = 1, size = 1.5) +
  scale_x_discrete("\nAlternative Models", expand = c(0,0)) +
  scale_y_continuous("", expand = c(0,0),
                     breaks = seq(1, nrow(cluster_info), 1),
                     labels = as.character(
                       rev(unique(plot_db$labels))),
                     sec.axis = sec_axis(
                       ~.,
                       breaks = seq(1,length(cluster_info$top_features), 1),
                       labels = rev(cluster_info$top_features))) +
  scale_fill_manual(values = c("gray80", "springgreen4")) +
  scale_alpha_manual(values = c(1, 0.7)) +
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    text = element_text(family = "LM Roman 10"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.ticks = element_blank()
  )
dev.off()

# [D] PROPORTION OF DOCS ON EACH TOPICS/CLUSTER BY MODEL
#-------------------------------------------------------------------------------
# - a document-model matrix indicating into which cluster each document has been
#   classified into
orig_altern_models_k_list <- k_list
doc_mat_cluster <- as.data.frame(matrix(
  nrow = nrow(r@dtm),
  ncol = length(orig_altern_models_k_list)))
colnames(doc_mat_cluster) <- paste0("model_k_", orig_altern_models_k_list)

# - adding info about the author and timestamp of the document. This will help
#   make sure we aren't messing up the merging of the doc-level covariates
doc_mat_cluster$author <- as.character(sapply(dimnames(r@dtm)$Docs, function(x)
  strsplit(x, split = "\\\\")[[1]][3]))
doc_mat_cluster$file <- as.character(sapply(dimnames(r@dtm)$Docs, function(x)
  strsplit(x, split = "\\\\")[[1]][4]))

# - adding now the information about into which cluster each document has been
#   classified
i <- 0
# ... iterate through model K's
for (m in orig_altern_models_k_list) {
  # - pull the doc-topic gamma matrix for this model
  if (m == r@lda_u@k) {
    gamma_mat <- r@lda_u@gamma
  } else {
    gamma_mat <- r@gamma_list[[which(r@K == m)]]
  }
  # - pull doc-topic assignment from gamm matrix
  doc_topic <- data.frame(
    model_topic = sapply(1:nrow(gamma_mat), function(j)
      which(gamma_mat[j,] == max(gamma_mat[j,])))
  )

  # - find out the index of the first and last topic-cluster assignment for this
  #   model
  start_i <- i + 1
  end_i <- (start_i + m - 1)
  model_label <- paste0("model_k_", m)

  # - pull this model's topic-cluster assignment, and merge with doc-topic
  #   assignment in order to see into which cluster the doc got classified into
  topic_cluster <- data.frame(
    model_topic = 1:m,
    cluster = cluster_mat[start_i:end_i]
  )
  doc_cluster <- left_join(doc_topic, topic_cluster)

  # - add this data to the out-of-the-loop output df
  doc_mat_cluster[,model_label] <- doc_cluster$cluster

  # - update the index that indicates the start of the topic-cluster assignments
  i <- end_i
}

# - a subset of the covariate dataset, only including "Party" and "Ideology"
doc_covs_reduced <- doc_covs %>%
  dplyr::select(File, party, ideal) %>%
  rename(file_grimmer = File)

# - merging this doc-cluster assignment data to the doc covariates matrix
#   /!\ 6May2005akaka77.txt a Good Example of this topic instability
#   /!\ 3Aug2006BillNelson23.txt a Good Example of topic Stability
doc_data <- cbind(doc_mat_cluster, doc_covs_reduced) %>%
  # - the covariates and model-topic-cluster information matches: getting rid
  #   of one of the file name variables
  dplyr::select(-file_grimmer)

# - iterating through clusters and models to calculate the proportion of docs on
#   each cluster by model
docs_by_cluster_and_model <- NULL
for (cluster in unique(cluster_mat[,1])) {
  # - iterate though models
  for (model in paste0("model_k_", orig_altern_models_k_list)) {
    # - pull the proportion of message about this topic/cluster in this model
    if (length(which(doc_data[,model] == cluster)) > 0) {
        cluster_model_prop <- length(which(doc_data[,model] == cluster)) /
          nrow(doc_data)
    } else {
      cluster_model_prop <- 0
    }
    new_row <- data.frame(
      model = model,
      cluster = cluster,
      prop = cluster_model_prop
    )
    docs_by_cluster_and_model <- rbind(docs_by_cluster_and_model, new_row)
  }
}

# - merge with the alternative model weights: to calculate weighted means if
#   wanted
weights_tomerge <- alternative_model_weights %>%
  mutate(model = as.character(paste0("model_", model)))
docs_by_cluster_and_model$model <- as.character(docs_by_cluster_and_model$model)
docs_by_cluster_and_model <- left_join(docs_by_cluster_and_model,
                                       weights_tomerge)
docs_by_cluster_and_model$weight[is.na(
  docs_by_cluster_and_model$weight
)] <- 1 # this are the results of the original model: should receive full weight

# - another dataset summarizing on average the presence of each topic
docs_by_cluster <- docs_by_cluster_and_model %>%
  group_by(cluster) %>%
  summarise(pe = mean(prop),
            sd = sqrt(var(prop)),
            pe_wtd = wtd.mean(prop, weight),
            sd_wtd = sqrt(wtd.var(prop, weight)),
            lwr = ifelse(n() > 2, t.test(prop)$conf.int[1], pe),
            #lwr = pe - (1.96 * sd),
            # lwr_wtd = ifelse(n() > 2, t.test(
            #   rnorm(100, mean = pe_wtd, sd = sd_wtd)
            #   )$conf.int[1], pe),
            lwr_wtd = ifelse(n() > 2, (
              wtd.t.test(x = prop, weight = weight)$additional[1] -
                1.96 * wtd.t.test(x = prop, weight = weight)$additional[4]
            ), pe),
            #lwr_wtd = pe_wtd - (1.96 * sd_wtd),
            upr = ifelse(n() > 2, t.test(prop)$conf.int[2], pe),
            #upr = pe + (1.96 * sd),
            # upr_wtd = ifelse(n() > 2, t.test(
            #   rnorm(100, mean = pe_wtd, sd = sd_wtd)
            #   )$conf.int[2], pe)),
            #upr_wtd = pe_wtd + (1.96 * sd_wtd)
            upr_wtd = ifelse(n() > 2, (
              wtd.t.test(x = prop, weight = weight)$additional[1] +
                1.96 * wtd.t.test(x = prop, weight = weight)$additional[4]
              ), pe))

# - adding top topic/cluster featues
cluster_top_features_to_merge <- cluster_top_features %>%
  rename(cluster = cluster_num)

cluster_top_features_to_merge$cluster <- as.character(
  cluster_top_features_to_merge$cluster)
docs_by_cluster$cluster <- as.character(docs_by_cluster$cluster)
docs_by_cluster <- left_join(docs_by_cluster, cluster_top_features_to_merge)

# - adding topic labels
cluster_labels_to_merge <- cluster_info %>%
  dplyr::select(top_cluster_num, cluster_label) %>%
  rename(cluster = top_cluster_num) %>%
  mutate(cluster = as.character(cluster))
docs_by_cluster <- left_join(docs_by_cluster, cluster_labels_to_merge)
docs_by_cluster$label <- paste0(
  sprintf("%02d", as.numeric(docs_by_cluster$cluster)),
          ". ", docs_by_cluster$cluster_label)

# - sort by average proportion
docs_by_cluster <- docs_by_cluster %>%
  arrange(pe) %>%
  mutate(label = factor(label, levels = unique(as.character(label))))

# - transfer this sorting to the by cluster and model dataset
label_to_merge <- docs_by_cluster %>%
  dplyr::select(cluster, cluster_label) %>%
  mutate(cluster = as.character(cluster))
docs_by_cluster_and_model$cluster <- as.character(docs_by_cluster_and_model$cluster)
docs_by_cluster_and_model <- left_join(docs_by_cluster_and_model,
                                       label_to_merge)
docs_by_cluster_and_model$label <- paste0(
  sprintf("%02d", as.numeric(docs_by_cluster_and_model$cluster)),
  ". ", docs_by_cluster_and_model$cluster_label)

docs_by_cluster_and_model <- docs_by_cluster_and_model %>%
  mutate(label = factor(
    as.character(label),
    levels = as.character(unique(docs_by_cluster$label))))

# - mark which is the result from the original model
docs_by_cluster_and_model$Estimate <- ifelse(
  docs_by_cluster_and_model$model == "model_k_44", "Original Model",
  "Alternative Model")

# - the plot
cols_db <- data.frame(
  x = c(0,0), y = c(-1,-1), type = c("Unweighted", "Weighted")
)
# pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#            "prop_docs_in_each_cluster_by_topic_41_47-STRICT.pdf"),
#     width = 20, height = 18, family = "NYTFranklin Light")
ggplot(docs_by_cluster_and_model,
       aes(x = as.numeric(label), y = prop)) +
  geom_segment(inherit.aes = FALSE,
               data = docs_by_cluster,
               aes(x = as.numeric(label) - 0.15, xend = as.numeric(label) - 0.15,
                   y = lwr, yend = upr),
               color = "plum3", alpha = 0.8, size = 3.5) +
  geom_point(inherit.aes = FALSE,
             data = docs_by_cluster,
             aes(x = as.numeric(label) - 0.15, y = pe), pch = 16, size = 3,
             color = "gray50") +
  geom_segment(inherit.aes = FALSE,
               data = docs_by_cluster,
               aes(x = as.numeric(label) + 0.15, xend = as.numeric(label) + 0.15,
                   y = lwr_wtd, yend = upr_wtd),
               color = "palegreen3", alpha = 0.8, size = 3.5) +
  geom_point(inherit.aes = FALSE,
             data = docs_by_cluster,
             aes(x = as.numeric(label) + 0.15, y = pe_wtd),
             pch = 16, size = 3, color = "gray50") +
  geom_polygon(inherit.aes = FALSE,
               data = cols_db, aes(x = x, y = y, fill = type)) +
  coord_flip() +
  geom_point(aes(color = Estimate,
                 size = Estimate,
             alpha = Estimate), pch = 4) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
  scale_size_manual(values = c(2, 6)) +
  scale_color_manual(values = c("black", "red")) +
  scale_alpha_manual(values = c(0.6, 1)) +
  scale_fill_manual("Confidence Interval", values = c("plum3", "palegreen3")) +
  scale_x_continuous("",
                     expand = c(0.01, 0.01),
                     limits = c(-.2, length(docs_by_cluster$cluster) + 0.2),
                     breaks = seq(1, length(docs_by_cluster$cluster), 1),
                     labels = docs_by_cluster$label,
                     sec.axis = sec_axis(~.,
                                         breaks = seq(1, length(docs_by_cluster$cluster), 1),
                                         labels = docs_by_cluster$top_features)) +
  scale_y_continuous("\nProportion of Documents about each Topic Cluster",
                     limits = c(min(c(docs_by_cluster$lwr,docs_by_cluster$lwr)),
                                max(c(docs_by_cluster$upr,docs_by_cluster$upr)))) +
  #coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray80", size = 0.2),
    #text = element_text(family = "LMRoman10-Regular", color = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.key = element_rect(size = 5),
    legend.key.size = unit(2, 'lines')
  )
dev.off()


# [E] SHOWING HOW EACH AUTHOR COVARIATE IS RELATED TO EACH TOPIC CLUSTER
#-------------------------------------------------------------------------------
# - a subset of the covariate dataset, only including "Party" and "Ideology"
doc_covs_reduced <- doc_covs %>%
  dplyr::select(File, party, ideal) %>%
  rename(file_grimmer = File)

# - estimating a separate logistic regression for each cluster, topic model, and
#   covaraite; predicting the probability of a topic being about that topic as a
#   function of the covariate

# ... basic needed objects/lists
output <- NULL
topmodel_list <- paste0("model_k_", orig_altern_models_k_list)
cluster_list <- 1:max(cluster_mat[,1])
cov_list <- c("party", "ideal")

# ... output matrix
Y <- doc_data[,which(grepl("model_k_", names(doc_data)))]

# ... covariates of interest
X <- doc_data[,cov_list]

# - iterate through clusters
cluster_counter <- 0
cluster_total <- length(cluster_list)
for (cluster in cluster_list) {
  cluster_counter <- cluster_counter + 1
  print(paste0("Cluster [", cluster_counter, "/", cluster_total, "]"))
  # - create a copy of the outcome matrix
  Y_c <- Y

  # - iterate through topic models
  topmodel_counter <- 0
  topmodel_total <- length(topmodel_list)
  for (topmodel in topmodel_list) {
    topmodel_counter <- topmodel_counter + 1
    print(paste0("Topic-Model [", topmodel_counter, "/", topmodel_total, "]"))
    # - replace the values in this model's column in the copy of the outcome
    #   matrix with 0s and 1s
    y <- Y_c[,topmodel]
    y[which(y == cluster)] <- -1
    y[which(y != cluster & y != -1)] <- -2
    y <- ifelse(y == -1, 1, 0)

    # - if a topic model DOES NOT HAVE THE TOPIC CLUSTER, don't run the
    #   statistical model. At this stage we are not interesting in learning
    #   whether the topic is present, but what kind of author features are
    #   related to the probility of discussing the topic cluster when this is
    #   present.
    if (length(which(y == 1)) > 0) {

      # - iterate through document covariates of interest
      for (covariate in cov_list) {
        model_data <- data.frame(
          y = y,
          x = X[,covariate]
        )
        if (nrow(table(model_data$y)) > 1) {
          # - estimate a bivariate logistic regression
          model <- glm(y ~ x, data = model_data, family = "binomial")

          # - calculate marginal effect when going from minimum to maximum value

          # ... pull model parameters and simulate 1000 betas
          pe <- coef(model)
          vc <- vcov(model)
          se <- sqrt(diag(vc))

          sims <- 1000
          simbetas <- MASS::mvrnorm(sims, pe, vc)

          # - create two scenarios: one where the value for the covariate of
          #   interest is at its minimum value, and another one at its maximum
          min_scen <- c(1, as.numeric(min(X[,covariate]))) #... 1 for the interecept
          max_scen <- c(1, as.numeric(max(X[,covariate])))

          # - predict the Pr in each scenario to discuss this cluster/topic
          yhats_min <- exp(min_scen %*% t(simbetas))
          yhats_max <- exp(max_scen %*% t(simbetas))

          # - calculate the difference between max and min predicted values
          diff <- yhats_max - yhats_min
          pe <- mean(diff)
          lwr <- quantile(diff, probs = 0.025)
          upr <- quantile(diff, probs = 0.975)

          # - add this result to the out-of-loop results dataframe
          new_row <- data.frame(
            cluster = paste0("Cluster ", sprintf("%02d", cluster)),
            topicmodel = topmodel,
            cov = covariate,
            pe = pe, lwr = lwr, upr = upr
          )
        } else {
          new_row <- data.frame(
            cluster = paste0("Cluster ", sprintf("%02d", cluster)),
            topicmodel = topmodel,
            cov = covariate,
            pe = NA, lwr = NA, upr = NA
          )
        }
        output <- rbind(output, new_row)
      }
    }
  }
}

# - adding a column indicating that these partial (by model) results
output$type <- "partial"

# - replacing NAs with 0s
output[is.na(output)] <- 0 # /!\ I don't know really how we should treat these

# - for each of the covariates and cluster, add a summary stats-row
for (cluster in cluster_list) {
  for (covariate in cov_list) {
    cluster_label <- paste0("Cluster ", sprintf("%02d", cluster))
    cl_cov_output <- output %>%
      filter(cluster == cluster_label,
             cov == covariate)
    final_pe <- mean(cl_cov_output$pe, na.rm = TRUE)
    final_lwr <- min(cl_cov_output$lwr, na.rm = TRUE)
    final_upr <- max(cl_cov_output$upr, na.rm = TRUE)
    new_row <- data.frame(
      cluster = paste0("Cluster ", sprintf("%02d", cluster)),
      topicmodel = topmodel,
      cov = covariate,
      pe = final_pe, lwr = final_lwr, upr = final_upr,
      type = "final"
    )
    output <- rbind(output, new_row)
  }
}

# - rename the covariate labels
output <- output %>%
  mutate(cov = ifelse(cov == "ideal", "CONSERVATISM", as.character(cov)),
         cov = ifelse(cov == "party", "DEMOCRATS", as.character(cov)))

# - save a copy of the resulting ouptut
# write.csv(output, paste0(
#   data_path, "03-paper-data/Grimmer_lda/cluster_covariate_indiv_effects_41-47-STRICT.csv"
# ), row.names = FALSE)

# - adding cluster labels
output <- output %>%
  mutate(cluster_num = as.numeric(gsub("Cluster ", "", cluster)),
         cluster_num = as.character(cluster_num))

label_to_merge <- label_to_merge %>%
  rename(cluster_num = cluster) %>%
  mutate(cluster_num = as.character(cluster_num))
output_02 <- left_join(output, label_to_merge) %>%
  mutate(label = paste0(sprintf("%02d", as.numeric(cluster_num)),
                        ". ", cluster_label))

# - invert the order of the cluster labels so Cluster 01 appears first
output_02 <- output_02 %>%
  arrange(desc(as.numeric(cluster_num))) %>%
  mutate(label = factor(label, levels = unique(label))) %>%
  arrange(label)

# - sort the plot by ideological effects
out_only_final <- output_02 %>%
  filter(type == "final") %>%
  arrange(cov, pe)
output_02$label <- factor(output_02$label,
                          levels = unique(as.character(out_only_final$label)))

# - a plot
# pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#            "covariates_ensemble_first_differences_41_47-STRICT.pdf"),
#     width = 18, height = 18)
ggplot(output_02 %>%
               filter(type == "partial"),
             aes(x = label, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(
    inherit.aes = FALSE,
    data = output_02 %>% filter(type == "final"),
    aes(x = label,
        xend = label,
        y = lwr, yend = upr), color = "deepskyblue2", size = 4, alpha = 0.3) +
  geom_pointrange(alpha = 0.2, pch = 20, size = 1.1) +
  geom_point(
    inherit.aes = FALSE,
    data = output_02 %>% filter(type == "final"),
    aes(x = label, y = pe), pch = 4, size = 8) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~ cov) +
  scale_x_discrete("") +
  scale_y_continuous(
    "\nFirst Difference: Change in the Probability of Discussing a Topic Cluster when going form Minimum to Maximum value") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black", fill = "gray90"),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
                                      size = 0.5),
    text = element_text(family = "LM Roman 10"),
    #text = element_text(family = "LMRoman10-Regular"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 19),
    axis.ticks = element_blank()
  )
dev.off()
# ggsave(p2, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#                             "covariates_ensemble_first_differences_41_47-STRICT.pdf"),
#        width = 16, height = 18, units = "in", device = cairo_pdf)

# [E-02] The same but for the WEIGHTED OPTION
#-------------------------------------------------------------------------------
# - estimating a separate logistic regression for each cluster, topic model, and
#   covaraite; predicting the probability of a topic being about that topic as a
#   function of the covariate

# ... basic needed objects/lists
output <- list()
topmodel_list <- paste0("model_k_", orig_altern_models_k_list)
cluster_list <- 1:max(cluster_mat[,1])
cov_list <- c("party", "ideal")

# ... output matrix
Y <- doc_data[,which(grepl("model_k_", names(doc_data)))]

# ... covariates of interest
X <- doc_data[,cov_list]

# - iterate through clusters
cluster_counter <- 0
cluster_total <- length(cluster_list)
for (cluster in cluster_list) {
  cluster_counter <- cluster_counter + 1
  print(paste0("Cluster [", cluster_counter, "/", cluster_total, "]"))
  # - create a copy of the outcome matrix
  Y_c <- Y

  # - iterate through topic models
  topmodel_counter <- 0
  topmodel_total <- length(topmodel_list)
  for (topmodel in topmodel_list) {
    topmodel_counter <- topmodel_counter + 1
    print(paste0("Topic-Model [", topmodel_counter, "/", topmodel_total, "]"))
    # - replace the values in this model's column in the copy of the outcome
    #   matrix with 0s and 1s
    y <- Y_c[,topmodel]
    y[which(y == cluster)] <- -1
    y[which(y != cluster & y != -1)] <- -2
    y <- ifelse(y == -1, 1, 0)

    # - if a topic model DOES NOT HAVE THE TOPIC CLUSTER, don't run the
    #   statistical model. At this stage we are not interesting in learning
    #   whether the topic is present, but what kind of author features are
    #   related to the probility of discussing the topic cluster when this is
    #   present.
    if (length(which(y == 1)) > 0) {

      # - iterate through document covariates of interest
      for (covariate in cov_list) {
        model_data <- data.frame(
          y = y,
          x = X[,covariate]
        )
        if (nrow(table(model_data$y)) > 1) {
          # - estimate a bivariate logistic regression
          model <- glm(y ~ x, data = model_data, family = "binomial")

          # - calculate marginal effect when going from minimum to maximum value

          # ... pull model parameters and simulate 1000 betas
          pe <- coef(model)
          vc <- vcov(model)
          se <- sqrt(diag(vc))

          sims <- 1000
          simbetas <- MASS::mvrnorm(sims, pe, vc)

          # - create two scenarios: one where the value for the covariate of
          #   interest is at its minimum value, and another one at its maximum
          min_scen <- c(1, as.numeric(min(X[,covariate]))) #... 1 for the interecept
          max_scen <- c(1, as.numeric(max(X[,covariate])))

          # - predict the Pr in each scenario to discuss this cluster/topic
          yhats_min <- exp(min_scen %*% t(simbetas))
          yhats_max <- exp(max_scen %*% t(simbetas))

          # - calculate the difference between max and min predicted values
          diff <- yhats_max - yhats_min
        } else {
          diff <- NULL
        }
        new_out <- list(
          cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          topicmodel = topmodel,
          cov = covariate,
          diffs = diff
        )
        output[[length(output) + 1]] <- new_out
      }
    }
  }
}


# [ F ] A Replication of Grimmer's "Appropriators not Position Takers"
#-------------------------------------------------------------------------------
# - create a more complete document-level dataset by merging two pervious ones
doc_data$year <- doc_covs$Year
doc_data$date <- doc_covs$Date

# - add a var to 'cluster_covs' indicating only the cluster number (not label)
#cluster_covs$cluster_num <- gsub("cluster_", "", cluster_covs$cluster)
#cluster_covs_SAFE <- cluster_covs
cluster_covs <- cluster_info
cluster_covs$cluster_num <- cluster_covs$top_cluster_num

# - /!\ some RECODING of the cluster type
#cluster_covs$type[which(cluster_covs$cluster_num %in%
#                          c("34"))] <- "credit-claiming"

# - for each author, year and model:
#       * calculate proportion of docs with a dominant Credit-Claiming cluster
#       * calculate proportion of docs with a dominant Position-Taking cluster
#       * calculate the difference between the two proportions: Balance measure
author_year_model_balance <- NULL
authors <- unique(doc_data$author)
model_labels <- paste0("model_k_", c(r@lda_u@k, r@K))
years <- c(2005, 2006, 2007)

for (a in authors) {
  # - select all documents from this Senator
  a_docs <- doc_data %>% filter(author == a)

  # - iterate through years
  for (yr in years) {
    # - select the press releases from that year
    year_docs <- a_docs %>%
      filter(year == yr)

    if (nrow(year_docs) > 0) {

      # - iterate through the original and alternative model
      for (m in model_labels) {
        # - select the doc cluster classification according to that model
        m_cluster_doc_output <- year_docs[, m]

        # - group the Credit-Claimin and Position-Taking docs
        a_docs_n <- nrow(year_docs)
        cred_cl <- m_cluster_doc_output[which(
          m_cluster_doc_output %in%
            as.numeric(
              cluster_covs$cluster_num[cluster_covs$type == "credit-claiming"])
        )]
        cred_cl_out <- length(cred_cl) /a_docs_n
        pos_tk <- m_cluster_doc_output[which(
          m_cluster_doc_output %in%
            as.numeric(
              cluster_covs$cluster_num[cluster_covs$type == "position-taking"])
        )]
        pos_tk_out <- length(pos_tk) /a_docs_n
        balance <- cred_cl_out - pos_tk_out

        # - save the information for this author/year/model
        new_row <- data.frame(
          author = a,
          year = yr,
          model = m,
          doc_n = a_docs_n,
          credit_claiming_n = length(cred_cl),
          credit_claiming_prop = cred_cl_out,
          position_taking_n = length(pos_tk),
          position_taking_prop = pos_tk_out,
          balance = balance
        )
        author_year_model_balance <- rbind(
          author_year_model_balance, new_row
        )
      }
    }
  }
}

# - grimmer's "variables" object to dataframe. Also, adding new variable "year"
member_covs <- as.data.frame(variables)
member_covs$author <- gsub("theta.", "", attr(variables$ideal.vec, "names"))
member_covs <- member_covs %>%
  mutate(year = ifelse(party == 1 & majority == 1 |
                         party == 0 & majority == 0, 2007, NA))
index_2005_start <- 1
index_2005_end <- which(member_covs$author == "Akaka")[2] - 1
index_2006_start <- which(member_covs$author == "Akaka")[2]
index_2006_end <- which(member_covs$author == "Akaka")[3] - 1
member_covs$year[index_2005_start:index_2005_end] <- 2005
member_covs$year[index_2006_start:index_2006_end] <- 2006

# - removing from the "author_year_model_balance" dataframe the row with info
#   about Senators that weren't senators yet
relevant_obs <- paste0(member_covs$author, "-", member_covs$year)
author_year_model_balance_ready <- author_year_model_balance %>%
  mutate(obs_label = paste0(author, "-", year)) %>%
  filter(obs_label %in% relevant_obs)

# - adding to this dataset with the outcome variable, the covariates of interest
member_covs$obs_label <- paste0(member_covs$author, "-", member_covs$year)
member_covs_to_merge <- member_covs %>%
  dplyr::select(-author, -year) %>%
  mutate(obs_label = as.character(obs_label))

author_year_model_balance_ready$obs_label <- as.character(
  author_year_model_balance_ready$obs_label
)

main_author_db <- left_join(author_year_model_balance_ready,
                            member_covs_to_merge)

# - estimate now Grimmer's paper model Z times, one for each of the topic
#   models in the rlda object
options(scipen = 100)
models_output <- NULL
for (m in model_labels) {
  # - pull the data from this topic model
  model_data <- main_author_db %>%
    filter(model == m)

  # - fit linear regressions using Grimmer's covariates (he fits a diff model:
  #   a Bayesian Multilevel Linear Regression)

  # ... Outcome 1: Balance (CreditClaiming - PositionTaking)
  model_01_out <- lmer(balance ~ centered +
                         party +
                         majority +
                         on.cycle +
                         I(years/100)  +
                         house +
                         freshman +
                         I(state.pop/1e7) +
                         (1|ind.indic)  +
                         (1|state.indic),
                       data = model_data)

  # ... Outcome 2: Credit Claiming
  model_02_out <- lmer(credit_claiming_prop ~ centered +
                         party +
                         majority +
                         on.cycle +
                         I(years/100)  +
                         house +
                         freshman +
                         I(state.pop/1e7) +
                         (1|ind.indic)  +
                         (1|state.indic),
                       data = model_data)

  # ... Outcome 3: Position Taking
  model_03_out <- lmer(position_taking_prop ~ centered +
                         party +
                         majority +
                         on.cycle +
                         I(years/100)  +
                         house +
                         freshman +
                         I(state.pop/1e7) +
                         (1|ind.indic)  +
                         (1|state.indic),
                       data = model_data)

  # - pulling the statistical model coefficients and CIs
  model_outputs <- list(model_01_out, model_02_out, model_03_out)
  clean_model_outputs <- NULL
  for (mdel in model_outputs) {
    coef_tbl <- summary(mdel)$coefficients
    coef_tbl_new <- data.frame(
      terms = rownames(coef_tbl),
      pe_95 = as.numeric(coef_tbl[, "Estimate"]),
      lwr_95 = as.numeric(coef_tbl[, "Estimate"]) -
        (1.96 * coef_tbl[, "Std. Error"]),
      upr_95 = as.numeric(coef_tbl[, "Estimate"]) +
        (1.96 * coef_tbl[, "Std. Error"]),
      pe_90 = as.numeric(coef_tbl[, "Estimate"]),
      lwr_90 = as.numeric(coef_tbl[, "Estimate"]) -
        (1.64 * coef_tbl[, "Std. Error"]),
      upr_90 = as.numeric(coef_tbl[, "Estimate"]) +
        (1.64 * coef_tbl[, "Std. Error"]),
      outcome = strsplit(as.character(mdel@call)[2], split = " ~ ")[[1]][1],
      topic_model = m
    )
    rownames(coef_tbl_new) <- NULL
    clean_model_outputs <- rbind(clean_model_outputs, coef_tbl_new)
  }
  models_output <- rbind(models_output, clean_model_outputs)
}

# - aggregate statistical model results across topic models (taking the average
#   pe, and the lowest lwr CI and highest upr CI)
final_model_output <- NULL
# - iterate through statistical model types
stat_model_list <- unique(models_output$outcome)
for (out in stat_model_list) {
  # - select results only for this statistical model with this outcome variable
  outcome_res <- models_output %>%
    filter(outcome == out)

  # - average results across topic models
  outcome_res_across <- outcome_res %>%
    group_by(terms) %>%
    summarise(
      outcome = out,
      pe_95 = mean(pe_95),
      lwr_95 = min(lwr_95),
      upr_95 = max(upr_95),
      pe_90 = mean(pe_90),
      lwr_90 = min(lwr_90),
      upr_90 = max(upr_90)
    )

  final_model_output <- rbind(
    final_model_output, outcome_res_across
  )
}

# - better labels for the covariates
final_model_output$terms <- recode(final_model_output$terms,
                                   `(Intercept)` = "Intercept",
                                   `centered` = "Alignment",
                                   `party` = "Democrat",
                                   `I(years/100)` = "Years/100",
                                   `house` = "Former Repr.",
                                   `freshman` = "Freshman",
                                   `majority` = "Majority",
                                   `on.cycle` = "In Cycle",
                                   `I(state.pop/10000000)` = "State Pop.\n (Millions)")

# - better labels for the outcomes
final_model_output$outcome <- recode(final_model_output$outcome,
                                     `balance` = "Credit Claiming v. Position Taking",
                                     `credit_claiming_prop` = "Credit Claiming",
                                     `position_taking_prop` = "Position Taking")

# - the same for the partial stat model results
models_output$terms <- recode(models_output$terms,
                              `(Intercept)` = "Intercept",
                              `centered` = "Alignment",
                              `party` = "Democrat",
                              `I(years/100)` = "Years/100",
                              `house` = "Former Repr.",
                              `freshman` = "Freshman",
                              `majority` = "Majority",
                              `on.cycle` = "In Cycle",
                              `I(state.pop/10000000)` = "State Pop.\n (Millions)")

models_output$outcome <- recode(models_output$outcome,
                                `balance` = "Credit Claiming v. Position Taking",
                                `credit_claiming_prop` = "Credit Claiming",
                                `position_taking_prop` = "Position Taking")

# - merge partial and aggregated result datasets
models_output <- models_output %>%
#   dplyr::select(-topic_model) %>%
    mutate(type = "partial")

final_model_output <- final_model_output %>%
  mutate(topic_model = "aggregate")

final_model_output$type <- "aggregate"

final_res <- rbind(models_output, final_model_output)

# - a dataset with the model results from Grimmer's paper
# ... create his "balance" outcome variables
exp.apps<- list.dep[[1]]
exp.subs<- list.dep[[2]]
diff<- exp.apps - exp.subs
# ... now name the covariates the same way he does in his replication code
centered.rat<- variables[[1]]
party<- variables[[2]]
majority<- variables[[3]]
on.cycle<- variables[[4]]
house<- variables[[5]]
freshman<- variables[[6]]
state.pop<- variables[[7]]
ind.indic<- variables[[8]]
state.indic<- variables[[9]]
ideal.vec<- variables[[10]]
approp.mem<- variables[[11]]
years<- variables[[12]]
# ... estimating the models (following exactly his code)
grimmer_01 <- lmer(diff~centered.rat + party + majority + on.cycle +  I(years/100)
                   + house + freshman +  I(state.pop/1e7) + (1|ind.indic)  +
                     (1|state.indic))
grimmer_02 <- lmer(exp.apps~centered.rat + party + majority + on.cycle +
                     I(years/100)  + house +  freshman +  I(state.pop/1e7) +
                     (1|ind.indic)  + (1|state.indic))

grimmer_03<- lmer(exp.subs~centered.rat + party + majority + on.cycle +
                    I(years/100)  + house +  freshman +  I(state.pop/1e7) +
                    (1|ind.indic)  + (1|state.indic))
# ... cleaning the model outputs from grimmer
model_outputs_grimmer <- list(grimmer_01, grimmer_02, grimmer_03)
clean_model_outputs_grimmer <- NULL
for (mdel in model_outputs_grimmer) {
  coef_tbl <- summary(mdel)$coefficients
  coef_tbl_new <- data.frame(
    terms = rownames(coef_tbl),
    pe_95 = as.numeric(coef_tbl[, "Estimate"]),
    lwr_95 = as.numeric(coef_tbl[, "Estimate"]) -
      (1.96 * coef_tbl[, "Std. Error"]),
    upr_95 = as.numeric(coef_tbl[, "Estimate"]) +
      (1.96 * coef_tbl[, "Std. Error"]),
    outcome = strsplit(as.character(mdel@call)[2], split = " ~ ")[[1]][1]
  )
  rownames(coef_tbl_new) <- NULL
  clean_model_outputs_grimmer <- rbind(clean_model_outputs_grimmer, coef_tbl_new)
}
# ... rename covariates and outcome variables
clean_model_outputs_grimmer$terms <- recode(clean_model_outputs_grimmer$terms,
                                            `(Intercept)` = "Intercept",
                                            `centered.rat` = "Alignment",
                                            `party` = "Democrat",
                                            `I(years/100)` = "Years/100",
                                            `house` = "Former Repr.",
                                            `freshman` = "Freshman",
                                            `majority` = "Majority",
                                            `on.cycle` = "In Cycle",
                                            `I(state.pop/10000000)` = "State Pop.\n (Millions)")

clean_model_outputs_grimmer$outcome <- recode(clean_model_outputs_grimmer$outcome,
                                              `diff` = "Credit Claiming v. Position Taking",
                                              `exp.apps` = "Credit Claiming",
                                              `exp.subs` = "Position Taking")

# - transform some large coefficients to fit a secondary axis
vars_to_transf <- c("Alignment", "Years/100")
scalar <- 4
for (v in vars_to_transf) {
  clean_model_outputs_grimmer$pe_95[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$pe_95[clean_model_outputs_grimmer$terms == v] / scalar
  clean_model_outputs_grimmer$lwr_95[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$lwr_95[clean_model_outputs_grimmer$terms == v] / scalar
  clean_model_outputs_grimmer$upr_95[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$upr_95[clean_model_outputs_grimmer$terms == v] / scalar

  final_res$pe_95[final_res$terms == v] <-
    final_res$pe_95[final_res$terms == v] / scalar
  final_res$lwr_95[final_res$terms == v] <-
    final_res$lwr_95[final_res$terms == v] / scalar
  final_res$upr_95[final_res$terms == v] <-
    final_res$upr_95[final_res$terms == v] / scalar

  final_res$pe_90[final_res$terms == v] <-
    final_res$pe_90[final_res$terms == v] / scalar
  final_res$lwr_90[final_res$terms == v] <-
    final_res$lwr_90[final_res$terms == v] / scalar
  final_res$upr_90[final_res$terms == v] <-
    final_res$upr_90[final_res$terms == v] / scalar

}

final_res$scalar <- 0
final_res$scalar[which(as.character(final_res$terms) %in% vars_to_transf)] <- 1

clean_model_outputs_grimmer$scalar <- 0
clean_model_outputs_grimmer$scalar[
  which(as.character(clean_model_outputs_grimmer$terms) %in% vars_to_transf)] <- 1


# - get rid of the intercept
final_res <- final_res %>%
  filter(terms != "Intercept") %>%
  mutate(terms = as.factor(as.character(terms)))

clean_model_outputs_grimmer <- clean_model_outputs_grimmer %>%
  filter(terms != "Intercept") %>%
  mutate(terms = as.factor(as.character(terms)))

# - sorting the results so Alignment and Years/100 (re-scaled coefficients) are
#   at the bottom
final_res <- final_res %>%
  mutate(terms = factor(terms, levels = rev(c(
    "Alignment", "Years/100", "Democrat", "Former Repr.",
    "Freshman", "Majority", "In Cycle", "State Pop.\n (Millions)"
  ))))

clean_model_outputs_grimmer <- clean_model_outputs_grimmer %>%
  mutate(terms = factor(terms, levels = rev(c(
    "Alignment", "Years/100", "Democrat", "Former Repr.",
    "Freshman", "Majority", "In Cycle", "State Pop.\n (Millions)"
  ))))

# - PLOT comparing our "robust" results to Grimmer's original results.
# pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#           "grimmer_models_41_47-STRICT.pdf"), width = 18, height = 12)
ggplot(final_res %>%
               filter(type == "partial"),
             aes(x = as.numeric(terms), y = pe_95,
                 ymin = lwr_95, ymax = upr_95)) +
  # - our aggregated topic model results (point estimates)
  geom_point(
    inherit.aes = FALSE,
    data = final_res %>% filter(type == "aggregate"),
    aes(x = terms, y = pe_95), pch = 4, size = 7) +
  # - our aggregated topic model results (confidence intervals)
  geom_segment(
    inherit.aes = FALSE,
    data = final_res %>% filter(type == "aggregate"),
    aes(x = terms,
        xend = terms,
        y = lwr_95, yend = upr_95), color = "deepskyblue2", size = 4, alpha = 0.3) +
  # - our separate topic model results
  geom_pointrange(alpha = 0.1, pch = 20, size = 1.1) +
  # - grimmer's results (confidence intervals)
  geom_pointrange(inherit.aes = FALSE,
                  data = clean_model_outputs_grimmer,
                  aes(
                    x = as.numeric(terms) + 0.2, y = pe_95,
                    ymin = lwr_95, ymax = upr_95,
                    shape = as.character(scalar)), color = "red") +
  # - grimmer's point estimates
  geom_point(inherit.aes = FALSE,
            data = clean_model_outputs_grimmer,
            aes(x = as.numeric(terms) + 0.2, y = pe_95, size = 3,
                shape = as.character(scalar)), color = "red") +
  # - the results of one of our models that approximates grimmer's results the
  #   most (our k = 43 model)
  # ... the 95% CIs
  geom_segment(
    inherit.aes = FALSE,
    data = final_res %>%
      filter(type == "partial", topic_model == "model_k_43"),
    aes(x = as.numeric(terms) - 0.2, xend = as.numeric(terms) - 0.2,
        y = lwr_95, yend = upr_95)) +
  # ... the 90% CIs
  geom_segment(
    inherit.aes = FALSE,
    data = final_res %>%
      filter(type == "partial", topic_model == "model_k_43"),
    aes(x = as.numeric(terms) - 0.2, xend = as.numeric(terms) - 0.2,
        y = lwr_90, yend = upr_90), size = 2, alpha = 0.6) +
  # ... the point estiamtes
  geom_point(
    inherit.aes = FALSE,
    data = final_res %>%
      filter(type == "partial", topic_model == "model_k_43"),
    aes(x = as.numeric(terms) - 0.2, y = pe_90, shape = as.character(scalar)),
    size = 5, alpha = 0.9) +
  scale_shape_manual(values = c(16, 17)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~outcome) +
  scale_x_discrete("") +
  scale_y_continuous(
    "\nAggregated coefficients (+95% confidence intervals) from Bayesian Multilevel Linear Regression",
    sec.axis = sec_axis(trans = ~.*4, breaks = seq(-1.2, 1.2, .3)),
    expand = c(0,0),
    breaks = seq(-.45, .5, .1),
    limits = c(-.4, .4)) +
  geom_polygon(inherit.aes = FALSE,
               data = data.frame(x = c(6.5, 6.5, 8.75, 8.75),
                                 y = c(-.4, 0.4, 0.4, -0.4)),
               aes(x = x , y = y),
               fill = "gray70", alpha = 0.3) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black", fill = "gray90"),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
                                      size = 0.5),
    #text = element_text(family = "LMRoman10-Regular"),
    #text = element_text(family = "LM Roman 10"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 20),
    #axis.ticks = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(2, "lines"),
    legend.position = "none"
  )
dev.off()

# ggsave(p3, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#                              "grimmer_models_41_47.pdf"),
#        width = 16, height = 10, units = "in", device = cairo_pdf)


