#' plot_cluster_proportions
#'
#' Plot showing proportion of documents about each topic-cluster.
#'
#' @param r a rlda object
#' @param dir directory to save plot
#' @exportMethod plot_cluster_proportion
#'

setGeneric("plot_cluster_proportion", function(r, dir)standardGeneric("plot_cluster_proportion"))
setMethod("plot_cluster_proportion",
          signature(r = "rlda", dir = "character"),
          function (r, dir) {
            # - making sure the user has already run the 'get_cluster_matrix'
            #   function in order to get the topic clusters
            if (is.null(r@topic_cluster_assignment) |
                (nrow(r@topic_cluster_assignment) == 0 &
                 ncol(r@topic_cluster_assignment) == 0)) {
              stop("Run 'get_cluster_matrix()' first to create the topic clusters.\n e.g.: ``r <- get_cluster_matrix(r = r, sim_threshold = .95)''")
            }

            # - pulling from the rlda object some of the information we'll need
            orig_altern_models_k_list <- c(r@lda_u@k, r@K)
            cluster_mat <- r@topic_cluster_assignment
            cluster_top_features <- r@cluster_center_key_words_list
            top_stability_mat <- r@top_stability_mat

            # - check if the `@doc_by_cluster_and_model` slot is empty; if so,
            #   create it.
            if (is.null(r@docs_by_cluster_and_model) |
                (nrow(r@docs_by_cluster_and_model) == 0 &
                 ncol(r@docs_by_cluster_and_model) == 0)) {
              docs_by_cluster_and_model <- as.data.frame(matrix(
                nrow = nrow(r@dtm),
                ncol = length(orig_altern_models_k_list)))
              colnames(docs_by_cluster_and_model) <- paste0("model_k_", orig_altern_models_k_list)

              # - adding now the information about into which cluster each
              #   document has beenclassified
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
            }

            # - iterating through clusters and models to calculate the prop.
            #   of docs on each cluster by model
            prop_doc_by_cluster_and_model <- NULL
            for (cluster in unique(cluster_mat[,1])) {
              # - iterate though models
              for (model in paste0("model_k_", orig_altern_models_k_list)) {
                # - pull the proportion of message about this topic/cluster in this model
                if (length(
                  which(docs_by_cluster_and_model[,model] == cluster)) > 0) {
                  cluster_model_prop <- length(which(
                    docs_by_cluster_and_model[,model] == cluster)) /
                    nrow(docs_by_cluster_and_model)
                } else {
                  cluster_model_prop <- 0
                }
                new_row <- data.frame(
                  model = model,
                  cluster = cluster,
                  prop = cluster_model_prop
                )
                prop_doc_by_cluster_and_model <- rbind(
                  prop_doc_by_cluster_and_model, new_row)
              }
            }

            # - now summarizing on average the presence of each topic
            docs_by_cluster <- prop_doc_by_cluster_and_model %>%
              group_by(cluster) %>%
              summarise(pe = mean(prop),
                        lwr = ifelse(n() > 2, t.test(prop)$conf.int[1], pe),
                        upr = ifelse(n() > 2, t.test(prop)$conf.int[2], pe))

            # - adding top topic/cluster featues
            cluster_top_features_to_merge <- cluster_top_features %>%
              rename(cluster = cluster_num)

            cluster_top_features_to_merge$cluster <- as.character(
              cluster_top_features_to_merge$cluster)
            docs_by_cluster$cluster <- as.character(docs_by_cluster$cluster)
            docs_by_cluster <- left_join(docs_by_cluster, cluster_top_features_to_merge)

            # - adding topic labels
            cluster_labels_to_merge <- top_stability_mat %>%
              dplyr::select(top_cluster_num) %>%
              rename(cluster = top_cluster_num) %>%
              mutate(cluster = as.character(cluster))

            docs_by_cluster <- left_join(docs_by_cluster, cluster_labels_to_merge)
            docs_by_cluster$label <- paste("cluster", docs_by_cluster$cluster)

            # - sort by average proportion
            docs_by_cluster <- docs_by_cluster %>%
              arrange(pe) %>%
              mutate(label = factor(label, levels = unique(as.character(label))))

            # - transfer this sorting to the by cluster and model dataset
            label_to_merge <- docs_by_cluster %>%
              dplyr::select(cluster) %>%
              mutate(cluster = as.character(cluster))
            prop_doc_by_cluster_and_model$cluster <- as.character(
              prop_doc_by_cluster_and_model$cluster)
            prop_doc_by_cluster_and_model <- left_join(
              prop_doc_by_cluster_and_model, label_to_merge)
            prop_doc_by_cluster_and_model$label <- paste(
              "cluster",prop_doc_by_cluster_and_model$cluster)

            prop_doc_by_cluster_and_model <- prop_doc_by_cluster_and_model %>%
              mutate(label = factor(
                as.character(label),
                levels = as.character(unique(docs_by_cluster$label))))

            # - the plot
            p <- ggplot2::ggplot(prop_doc_by_cluster_and_model,
                   aes(x = as.numeric(label), y = prop)) +
              geom_pointrange(inherit.aes = FALSE,
                              data = docs_by_cluster,
                              aes(x = as.numeric(label),
                                  y = pe, ymin = lwr, ymax = upr),
                              size = 1.1, alpha = 0.8) +
              coord_flip() +
              geom_point(pch = 4, alpha = 0.6, size = 3) +
              geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
              scale_x_continuous("",
                                 expand = c(0.01, 0.01),
                                 limits = c(1, length(docs_by_cluster$cluster)),
                                 breaks = seq(1, length(docs_by_cluster$cluster), 1),
                                 labels = docs_by_cluster$label,
                                 sec.axis = sec_axis(~.,
                                                     breaks = seq(1, length(docs_by_cluster$cluster), 1),
                                                     labels = docs_by_cluster$top_features)) +
              scale_y_continuous("\nProportion of Documents about each Topic Cluster") +
              theme(
                panel.background = element_blank(),
                panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
                panel.grid.major.y = element_line(color = "gray80", size = 0.2),
                axis.ticks = element_blank()
              )
            print(p)
            return(r)
          })

