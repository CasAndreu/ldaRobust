#' plot_cluster_proportions
#'
#' compute clustering of topics across different models
#'
#' @param r a rlda object
#' @param dir directory to save plot
#' @param cluster_info
#' @param doc_covs
#' @exportMethod plot_cluster_proportion
#'

setGeneric("plot_cluster_proportion", function(r, dir, cluster_info, doc_covs)standardGeneric("plot_cluster_proportion"))
setMethod("plot_cluster_proportion",
          signature(r = "rlda", dir = "character", cluster_info = "data.frame", doc_covs = "data.frame"),
          function (r, dir, cluster_info, doc_covs) {
            # [D] PROPORTION OF DOCS ON EACH TOPICS/CLUSTER BY MODEL
            #-------------------------------------------------------------------------------
            # - a document-model matrix indicating into which cluster each document has been
            #   classified into
            k_list <- c(r@lda_u@k, r@K)
            cluster_mat <- r@topic_cluster_assignment
            cluster_top_features <- r@cluster_center_key_words_list
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

            # - another dataset summarizing on average the presence of each topic
            docs_by_cluster <- docs_by_cluster_and_model %>%
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

            # - the plot
            # pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
            #            "prop_docs_in_each_cluster_by_topic_41_47-STRICT.pdf"),
            #     width = 20, height = 18)
            ggplot(docs_by_cluster_and_model,
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
              #coord_flip() +
              theme(
                panel.background = element_blank(),
                panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
                panel.grid.major.y = element_line(color = "gray80", size = 0.2),
                #text = element_text(family = "LMRoman10-Regular", color = "black"),
                #text = element_text(family = "LM Roman 10", color = "black"),
                #axis.text = element_text(size = 18),
                #axis.title = element_text(size = 18),
                axis.ticks = element_blank()
              )
            #dev.off()
          })

