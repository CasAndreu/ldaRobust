#' plot_author_topic_cov
#'
#' compute clustering of topics across different models
#'
#' @param r a rlda object
#' @param dir directory to save plot
#' @param cluster_info
#' @param doc_covs
#' @exportMethod plot_author_topic_cov
#'

setGeneric("plot_author_topic_cov", function(r, dir, cluster_info, doc_covs)standardGeneric("plot_author_topic_cov"))
setMethod("plot_author_topic_cov",
          signature(r = "rlda", dir = "character", cluster_info = "data.frame", doc_covs = "data.frame"),
          function (r, dir, cluster_info, doc_covs) {
            # [E] SHOWING HOW EACH AUTHOR COVARIATE IS RELATED TO EACH TOPIC CLUSTER
            #-------------------------------------------------------------------------------
            # - a subset of the covariate dataset, only including "Party" and "Ideology"
            k_list <- c(r@lda_u@k, r@K)
            cluster_mat <- r@topic_cluster_assignment
            cluster_top_features <- r@cluster_center_key_words_list
            orig_altern_models_k_list <- k_list



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
            # - merging this doc-cluster assignment data to the doc covariates matrix
            #   /!\ 6May2005akaka77.txt a Good Example of this topic instability
            #   /!\ 3Aug2006BillNelson23.txt a Good Example of topic Stability
            doc_data <- cbind(doc_mat_cluster, doc_covs_reduced) %>%
              # - the covariates and model-topic-cluster information matches: getting rid
              #   of one of the file name variables
              dplyr::select(-file_grimmer)

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
                #text = element_text(family = "LM Roman 10"),
                #text = element_text(family = "LMRoman10-Regular"),
                #axis.text = element_text(size = 18),
                #axis.title = element_text(size = 18),
                #strip.text = element_text(size = 19),
                axis.ticks = element_blank()
              )
            #dev.off()
            # ggsave(p2, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
            #                             "covariates_ensemble_first_differences_41_47-STRICT.pdf"),
            #        width = 16, height = 18, units = "in", device = cairo_pdf)

          })
