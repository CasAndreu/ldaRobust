#' get_cluster_matrix_general
#'
#' Compute clustering of topics across different models
#'
#' @include compute_sim_general.R
#' @param r a rlda_general object
#' @param sim_threshold similarity threshold
#' @exportMethod get_cluster_matrix_general
#'

setGeneric("get_cluster_matrix_general", function(r, sim_threshold)standardGeneric("get_cluster_matrix_general"))
setMethod("get_cluster_matrix_general",
          signature(r = "rlda_general", sim_threshold = "numeric"),
          function (r, sim_threshold) {
            k_list <- r@K
            beta_mat <- r@beta_list[[1]]
            for (topic_model_i in 2:length(r@beta_list)) {
              beta_mat <- rbind(beta_mat, r@beta_list[[topic_model_i]])
            }
            #sim_mat <- get_sim_matrix(beta_mat)
            sim_mat <- cos_sim(beta_mat, beta_mat)

            cluster_mat <- get_cluster_matrix_sub(sim_mat, k_list, sim_threshold)


            cluster_top_features <- get_cluster_top_features_gen(cluster_mat, beta_mat,
                                                             k_list, r, n = 6)

            r@topic_cluster_assignment <- cluster_mat
            r@cluster_center_key_words_list <- cluster_top_features

            # get top_stability_mat
            #k_list <- c(r@lda_u@k, r@K)
            or_topics_alt_models_mat <- as.data.frame(matrix(nrow = k_list[1],
                                                             ncol = (length(k_list)-1)))

            # - iterate through original topics and checking whether they are in alternative
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
            names(top_stability_mat) <- paste0(paste0("k_", k_list[2:length(k_list)]), r@model_type[2:length(k_list)])
            # ... top features
            top_stability_mat$top_features <- cluster_top_features$top_features
            # ... numbering the clusters
            top_stability_mat$top_cluster_num <- paste0(sprintf("%02d",
                                                                1:nrow(top_stability_mat)))

            r@top_stability_mat = top_stability_mat


            # create doc_by_cluster_and_model
            docs_by_cluster_and_model <- as.data.frame(matrix(
              nrow = nrow(r@dtm),
              ncol = length(k_list)))
            colnames(docs_by_cluster_and_model) <- paste0(paste0("model_k_", k_list), r@model_type)

            # - adding now the information about into which cluster each
            #   document has beenclassified
            i <- 0
            # ... iterate through model K's
            for (idx in 1:length(k_list)) {
              # - pull the doc-topic gamma matrix for this model
              m = k_list[idx]
              gamma_mat <- r@gamma_list[[idx]]
              #if (idx == 1) {
              #  gamma_mat <- r@lda_u@gamma
              #}
              #else
              #{
              #  gamma_mat <- r@gamma_list[[idx-1]]
              #}
              # - pull doc-topic assignment from gamm matrix
              doc_topic <- data.frame(
                model_topic = sapply(1:nrow(gamma_mat), function(j)
                  which(gamma_mat[j,] == max(gamma_mat[j,])))
              )

              # - find out the index of the first and last topic-cluster assignment for this
              #   model
              start_i <- i + 1
              end_i <- (start_i + m - 1)
              model_label <- paste0(paste0("model_k_", m), r@model_type[idx])
              print(model_label)

              # - pull this model's topic-cluster assignment, and merge with doc-topic
              #   assignment in order to see into which cluster the doc got classified into
              topic_cluster <- data.frame(
                model_topic = 1:m,
                cluster = cluster_mat[start_i:end_i]
              )
              doc_cluster <- suppressMessages(
                left_join(doc_topic, topic_cluster))

              # - add this data to the out-of-the-loop output df
              docs_by_cluster_and_model[,model_label] <- doc_cluster$cluster

              # - update the index that indicates the start of the topic-cluster assignments
              i <- end_i
            }

            # - adding this information into the `docs_by_cluster_and_model`
            #   @slot
            r@docs_by_cluster_and_model <- docs_by_cluster_and_model
            return(r)
          })


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





# [ B ] CLUSTERING THE TOPICS
#-------------------------------------------------------------------------------

# A function performing the desired clustering
get_cluster_matrix_sub <- function(sim_mat, k_list, sim_threshold) {
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
    #print(i)
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
        #print(j)
        matches_to_rm <- NULL
        alt_topic_info <- or_alt_sim_mat[,j]
        if (which(alt_topic_info == max(alt_topic_info)) != i) {
          matches_to_rm <- c(matches_to_rm, j)
        }
      }
      #print(1)
      matches_indices <- matches_indices[which(!(matches_indices %in%
                                                   matches_to_rm))]
      #print(2)
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




# [ C ] GETTING THE TOP FEATURES OF THE CLUSTER CENTROIDS
#-------------------------------------------------------------------------------
# - A Function performing this task
get_cluster_top_features_gen <- function(cluster_mat, beta_mat, k_list, rlda_obj,
                                     n = 6) {
  # - labeling the columns(features) of the beta matrix
  ## change vals=0 to min
  min_val = min(beta_mat)
  zero_idx = (beta_mat == 0)
  beta_mat[zero_idx] = min_val-1

  beta_df <- as.data.frame(beta_mat)
  colnames(beta_df) <- rlda_obj@terms

  # - initializing output
  out <- as.data.frame(matrix(nrow = max(cluster_mat[,1]), ncol = 2))
  colnames(out) <- c("cluster_num", "top_features")
  out$cluster_num <- 1:nrow(out)

  # - we assign the top predictive features from each original topic to the
  #   clusters of those topics: not actually calculating any centroid
  for (i in 1:rlda_obj@K[rlda_obj@idx]) {
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
  for (z in ((rlda_obj@K[rlda_obj@idx] + 1):max(cluster_mat[,1]))) {
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
