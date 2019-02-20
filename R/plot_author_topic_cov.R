#' plot_author_topic_cov
#'
#' This function plots the bivariate relationship between a set of author and/or document-level covariates and the likelihood of a document to be about each of the topic clusters.
#'
#' @param r An rlda object.
#' @param dir Directory to save the resulting plot.
#' @param doc_covs A dataset with author and/or document-level covariates. The dataset needs to be indexed exactly as the documents in the Document Term Matrix provided during fitting.
#' @param covs A vector with the covariates to analayze: 5 covariates maximum.
#' @param cat_covs categorical covariates among covs
#' @exportMethod plot_author_topic_cov

setGeneric("plot_author_topic_cov", function(
  r, dir, doc_covs, covs, cat_covs)standardGeneric("plot_author_topic_cov"))
setMethod("plot_author_topic_cov",
          signature(r = "rlda", dir = "character", doc_covs = "data.frame",
                    covs = "character", cat_covs = "character"),
          function (r, dir, doc_covs, covs, cat_covs) {
            # - making sure that the user provided <= 5 covariates, and that they
            #   are all present in the dataset
            if (length(covs) > 5) {
              stop("Too many covariates provided: please provide fewer than 6.")
            }
            if (length(which(covs %in% names(doc_covs))) != length(covs)) {
              stop("Not all provided covariates are in the covariate dataset")
            }
            # - making sure the user has already run the 'get_cluster_matrix'
            #   function in order to get the topic clusters
            if (is.null(r@topic_cluster_assignment) |
                (nrow(r@topic_cluster_assignment) == 0 &
                 ncol(r@topic_cluster_assignment) == 0)) {
              stop("Run first 'get_cluster_matrix()' in order to create the topic clusters.")
            }

            # - pulling from the rlda object some of the information we'll need
            orig_altern_models_k_list <-  r@K
            cluster_mat <- r@topic_cluster_assignment
            cluster_top_features <- r@cluster_center_key_words_list
            docs_by_cluster_and_model <- r@docs_by_cluster_and_model
            top_stability_mat <- r@top_stability_mat

            # [E] SHOWING HOW EACH AUTHOR COVARIATE IS RELATED TO EACH TOPIC CLUSTER
            #-------------------------------------------------------------------------------
            # - a subset of the covariate dataset, only including "Party" and "Ideology"
            doc_covs_reduced <- doc_covs[,covs]

            # - estimating a separate logistic regression for each cluster, topic model, and
            #   covaraite; predicting the probability of a topic being about that topic as a
            #   function of the covariate

            # ... basic needed objects/lists
            output <- NULL
            topmodel_list <- paste0(paste0("model_k_", orig_altern_models_k_list), r@model_type)
            cluster_list <- 1:max(cluster_mat[,1])
            #cov_list <- c("party", "ideal")

            # create doc data
            doc_data <- cbind(docs_by_cluster_and_model, doc_covs_reduced)

            # [E-02] The same but for the WEIGHTED OPTION
            #-------------------------------------------------------------------------------
            # - estimating a separate logistic regression for each cluster, topic model, and
            #   covaraite; predicting the probability of a topic being about that topic as a
            #   function of the covariate

            #doc_data = doc_data %>%
            #  mutate(party = ifelse(party == 1, "Democrat", "Republican"))


            # ... basic needed objects/lists
            #output <- NULL
            #topmodel_list <- paste0("model_k_", orig_altern_models_k_list)
            #cluster_list <- 1:max(cluster_mat[,1])
            #cov_list <- c("party", "ideal")

            # ... output matrix
            Y <- doc_data[,which(grepl("model_k_", names(doc_data)))]

            # ... covariates of interest
            X <- doc_data[,covs]

            # ... categorical variables
            #cat_covs <- c("party")

            # initialize weighted average
            alternative_model_weights <- NULL
            for (k in 2:length(r@K)) {
              model_label <- paste0(paste0("k_", r@K[k]), r@model_type[k])
              y <- top_stability_mat[,model_label]
              y_prop <- length(which(y > 0)) / length(y)
              new_row <- data.frame(
                model = model_label,
                weight = y_prop
              )
              alternative_model_weights <- rbind(alternative_model_weights, new_row)
            }

            # - iterate through clusters
            cluster_counter <- 0
            cluster_total <- length(cluster_list)
            for (cluster in cluster_list) {
              cluster_counter <- cluster_counter + 1
              print(paste0("Cluster [", cluster_counter, "/", cluster_total, "]"))
              # - create a copy of the outcome matrix
              Y_c <- Y

              # - iterate through document covariates of interest
              cov_counter <- 0
              cov_total <- length(covs)
              for (covariate in covs) {
                cov_counter <- cov_counter + 1
                print(paste0("... Covariate [", cov_counter, "/", cov_total, "]"))

                # - check if it's a categorical variable
                if (covariate %in% cat_covs) {
                  # - make sure it's categorical in the data frame
                  X[,covariate] <- factor(X[,covariate])

                  # - check what's the reference category/class
                  all_cats <- levels(X[,covariate])
                  ref_cat <- all_cats[1]
                  other_cats <- all_cats[2:length(all_cats)]

                  # - iterate through non-reference category and calculate marginal effects
                  for (ocat in other_cats) {
                    all_diffs <- NULL
                    all_weights <- NULL
                    topmodel_counter <- 0
                    topmodel_total <- length(topmodel_list)
                    for (topmodel in topmodel_list) {
                      topmodel_counter <- topmodel_counter + 1
                      print(paste0("... ... topic-model [", topmodel_counter, "/", topmodel_total, "]"))
                      # - replace the values in this model's column in the copy of the outcome
                      #   matrix with 0s and 1s
                      y <- Y_c[,topmodel]
                      y[which(y == cluster)] <- -1
                      y[which(y != cluster & y != -1)] <- -2
                      y <- ifelse(y == -1, 1, 0)

                      model_data <- data.frame(
                        y = y,
                        x = factor(X[,covariate])
                      )

                      # - if a topic model DOES NOT HAVE THE TOPIC CLUSTER, don't run the
                      #   statistical model. At this stage we are not interesting in learning
                      #   whether the topic is present but the kind of author features that are
                      #   related to the probility of discussing the topic cluster when this IS
                      #   present.
                      if (length(which(y == 1)) > 0) {
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
                          min_scen_pre <- data.frame(intercept = 1, cov = factor(
                            ref_cat, levels = all_cats))
                          max_scen_pre <- data.frame(intercept = 1, cov = factor(
                            ocat, levels = all_cats))
                          min_scen <- model.matrix(~cov, data = min_scen_pre)
                          max_scen <- model.matrix(~cov, data = max_scen_pre)

                          # - predict the Pr in each scenario to discuss this cluster/topic
                          yhats_min <- exp(min_scen %*% t(simbetas))
                          yhats_max <- exp(max_scen %*% t(simbetas))

                          # - calculate the difference between max and min predicted values
                          diff <- yhats_max - yhats_min
                        } else {
                          diff <- NULL
                        }

                        # - calculate and save the cluster-topicmodel-covariate results
                        # - calculate the difference between max and min predicted values
                        pe <- mean(diff)
                        lwr <- quantile(diff, probs = 0.025)
                        upr <- quantile(diff, probs = 0.975)

                        # - add this PARTIAL result to the out-of-loop results dataframe
                        new_row <- data.frame(
                          cluster = paste0("Cluster ", sprintf("%02d", cluster)),
                          topicmodel = topmodel,
                          cov = paste0(covariate, ":", ocat),
                          pe = pe, lwr = lwr, upr = upr,
                          weighted = "none",
                          type = "partial"
                        )
                        output <- rbind(output, new_row)

                        # - pull the weight for this topic model
                        if (topmodel != paste0(paste0("model_k_", r@lda_u@k), "or")) {
                          model_weight <- alternative_model_weights$weight[
                            which(paste0("model_", alternative_model_weights$model) == topmodel)]
                        } else {
                          model_weight <- 1
                        }
                        # - save weights and differences
                        all_diffs <- c(all_diffs, diff)
                        all_weights <- c(all_weights, rep(model_weight, length(diff)))
                      } else {

                      }
                    }
                    all_weights_std <- DMwR::ReScaling(all_weights, 0.01, 1)
                  }
                  # - calculate average effects across models for each cluster-covariate
                  for (av_type in c("regular", "weighted")) {
                    if (av_type == "regular") {
                      final_diffs <- sample(x = all_diffs, size = 1000, replace = FALSE)
                      weighted <- "no"
                    } else {
                      #set.seed(1)
                      final_diffs <- sample(x = all_diffs, size = 1000, replace = FALSE,
                                            prob = all_weights_std)
                      weighted <- "yes"
                    }
                    final_pe <- mean(final_diffs)
                    final_lwr <- quantile(final_diffs, 0.025)
                    final_upr <- quantile(final_diffs, 0.975)
                    # - save the result
                    new_row <- data.frame(
                      cluster = paste0("Cluster ", sprintf("%02d", cluster)),
                      topicmodel = "cluster-result",
                      cov = paste0(covariate, ":", ocat),
                      pe = final_pe, lwr = final_lwr, upr = final_upr,
                      weighted = weighted,
                      type = "final"
                    )
                    output <- rbind(output, new_row)
                  }
                } else {
                  # - iterate through topic models
                  all_diffs <- NULL
                  all_weights <- NULL
                  topmodel_counter <- 0
                  topmodel_total <- length(topmodel_list)
                  for (topmodel in topmodel_list) {
                    topmodel_counter <- topmodel_counter + 1
                    print(paste0("... ... topic-model [", topmodel_counter, "/", topmodel_total, "]"))
                    # - replace the values in this model's column in the copy of the outcome
                    #   matrix with 0s and 1s
                    y <- Y_c[,topmodel]
                    y[which(y == cluster)] <- -1
                    y[which(y != cluster & y != -1)] <- -2
                    y <- ifelse(y == -1, 1, 0)

                    model_data <- data.frame(
                      y = y,
                      x = X[,covariate]
                    )

                    # - if a topic model DOES NOT HAVE THE TOPIC CLUSTER, don't run the
                    #   statistical model. At this stage we are not interesting in learning
                    #   whether the topic is present but the kind of author features that are
                    #   related to the probility of discussing the topic cluster when this IS
                    #   present.
                    if (length(which(y == 1)) > 0) {
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

                      # - calculate and save the cluster-topicmodel-covariate results
                      # - calculate the difference between max and min predicted values
                      pe <- mean(diff)
                      lwr <- quantile(diff, probs = 0.025)
                      upr <- quantile(diff, probs = 0.975)

                      # - add this PARTIAL result to the out-of-loop results dataframe
                      new_row <- data.frame(
                        cluster = paste0("Cluster ", sprintf("%02d", cluster)),
                        topicmodel = topmodel,
                        cov = covariate,
                        pe = pe, lwr = lwr, upr = upr,
                        weighted = "none",
                        type = "partial"
                      )
                      output <- rbind(output, new_row)

                      # - pull the weight for this topic model
                      if (topmodel != paste0(paste0("model_k_", r@lda_u@k), "or")) {
                        model_weight <- alternative_model_weights$weight[
                          which(paste0("model_", alternative_model_weights$model) == topmodel)]
                      } else {
                        model_weight <- 1
                      }
                      # - save weights and differences
                      all_diffs <- c(all_diffs, diff)
                      all_weights <- c(all_weights, rep(model_weight, length(diff)))
                    }
                  }
                  all_weights_std <- DMwR::ReScaling(all_weights, 0.01, 1)
                  # - calculate average effects across models for each cluster-covariate
                  for (av_type in c("regular", "weighted")) {
                    if (av_type == "regular") {
                      final_diffs <- sample(x = all_diffs, size = 1000, replace = FALSE)
                      weighted <- "no"
                    } else {
                      #set.seed(1)
                      final_diffs <- sample(x = all_diffs, size = 1000, replace = FALSE,
                                            prob = all_weights_std)
                      weighted <- "yes"
                    }
                    final_pe <- mean(final_diffs)
                    final_lwr <- quantile(final_diffs, 0.025)
                    final_upr <- quantile(final_diffs, 0.975)
                    # - save the result
                    new_row <- data.frame(
                      cluster = paste0("Cluster ", sprintf("%02d", cluster)),
                      topicmodel = "cluster-result",
                      cov = covariate,
                      pe = final_pe, lwr = final_lwr, upr = final_upr,
                      weighted = weighted,
                      type = "final"
                    )
                    output <- rbind(output, new_row)
                  }
                }
              }
            }

            # - rename the covariate labels /!\ DON'T ADD TO PACKAGE (they must do it before
            #   hand)
            # output <- output %>%
            #   mutate(cov = ifelse(cov == "ideal", "CONSERVATISM", as.character(cov)),
            #          cov = ifelse(cov == "party", "DEMOCRATS", as.character(cov)))

            # - save a copy of the resulting ouptut
            # write.csv(output, paste0(
            #   data_path, "03-paper-data/Grimmer_lda/cluster_covariate_indiv_effects_41-47-STRICT.csv"
            # ), row.names = FALSE)

            # - adding cluster labels
            output <- output %>%
              mutate(cluster_num = as.numeric(gsub("Cluster ", "", cluster)),
                     cluster_num = as.character(cluster_num))

            label_to_merge_02 <- label_to_merge %>%
              mutate(cluster = paste0("Cluster ", sprintf("%02d", as.numeric(cluster))))
            output$cluster <- as.character(output$cluster)
            output_02 <- left_join(output, label_to_merge_02) %>%
              rename(label = cluster_label)

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
            #            "covariates_ensemble_first_differences_41_47-STRICT-WEIGHTED.pdf"),
            #     width = 18, height = 18)
            ggplot(output_02 %>%
                     filter(type == "partial"),
                   aes(x = label, y = pe, ymin = lwr, ymax = upr)) +
              geom_segment(
                inherit.aes = FALSE,
                data = output_02 %>% filter(type == "final", weighted == "yes"),
                aes(x = as.numeric(label) + 0.2,
                    xend = as.numeric(label) + 0.2,
                    y = lwr, yend = upr), color = "plum3", size = 4, alpha = 0.5) +
              geom_segment(
                inherit.aes = FALSE,
                data = output_02 %>% filter(type == "final", weighted == "no"),
                aes(x = as.numeric(label) - 0.2,
                    xend = as.numeric(label) - 0.2,
                    y = lwr, yend = upr), color = "palegreen3", size = 4, alpha = 0.5) +
              geom_pointrange(alpha = 0.2, pch = 20, size = 1.1) +
              geom_point(
                inherit.aes = FALSE,
                data = output_02 %>% filter(type == "final", weighted == "yes"),
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


          #   # - check if the `@doc_by_cluster_and_model` slot is empty; if so,
          #   #   create it.
          #   # if (is.null(r@docs_by_cluster_and_model) |
          #   #     (nrow(r@docs_by_cluster_and_model) == 0 &
          #   #      ncol(r@docs_by_cluster_and_model) == 0)) {
          #   #   docs_by_cluster_and_model <- as.data.frame(matrix(
          #   #     nrow = nrow(r@dtm),
          #   #     ncol = length(orig_altern_models_k_list)))
          #   #   colnames(docs_by_cluster_and_model) <- paste0("model_k_", orig_altern_models_k_list)
          #   #
          #   #   # - adding now the information about into which cluster each
          #   #   #   document has beenclassified
          #   #   i <- 0
          #   #   # ... iterate through model K's
          #   #   for (m in orig_altern_models_k_list) {
          #   #     # - pull the doc-topic gamma matrix for this model
          #   #     if (m == r@lda_u@k) {
          #   #       gamma_mat <- r@lda_u@gamma
          #   #     } else {
          #   #       gamma_mat <- r@gamma_list[[which(r@K == m)]]
          #   #     }
          #   #     # - pull doc-topic assignment from gamm matrix
          #   #     doc_topic <- data.frame(
          #   #       model_topic = sapply(1:nrow(gamma_mat), function(j)
          #   #         which(gamma_mat[j,] == max(gamma_mat[j,])))
          #   #     )
          #   #
          #   #     # - find out the index of the first and last topic-cluster assignment for this
          #   #     #   model
          #   #     start_i <- i + 1
          #   #     end_i <- (start_i + m - 1)
          #   #     model_label <- paste0("model_k_", m)
          #   #
          #   #     # - pull this model's topic-cluster assignment, and merge with doc-topic
          #   #     #   assignment in order to see into which cluster the doc got classified into
          #   #     topic_cluster <- data.frame(
          #   #       model_topic = 1:m,
          #   #       cluster = cluster_mat[start_i:end_i]
          #   #     )
          #   #     doc_cluster <- suppressMessages(
          #   #       left_join(doc_topic, topic_cluster))
          #   #
          #   #     # - add this data to the out-of-the-loop output df
          #   #     docs_by_cluster_and_model[,model_label] <- doc_cluster$cluster
          #   #
          #   #     # - update the index that indicates the start of the topic-cluster assignments
          #   #     i <- end_i
          #   #   }
          #   #
          #   #   # - adding this information into the `docs_by_cluster_and_model`
          #   #   #   @slot
          #   #   r@docs_by_cluster_and_model <- docs_by_cluster_and_model
          #   # } else {
          #   #   docs_by_cluster_and_model <- r@docs_by_cluster_and_model
          #   # }
          #
          #   # - a subset of the covariate dataset including only the provided
          #   #   covariates
          #   doc_covs_reduced <- doc_covs[,covs]
          #
          #   # - estimating a separate logistic regression for each cluster,
          #   #   topic model, and covaraite; predicting the probability of a given
          #   #   document to be about that topic as a function of the covariate
          #
          #   # ... basic needed objects/lists
          #   output <- NULL
          #   topmodel_list <- paste0("model_k_", orig_altern_models_k_list)
          #   cluster_list <- 1:max(cluster_mat[,1])
          #
          #   # - merging the doc-cluster assignment data to the doc covariates matrix
          #   doc_data <- cbind(docs_by_cluster_and_model, doc_covs_reduced)
          #
          #   # - estimating the bivariate effects
          #   # ... outcome variable
          #   Y <- doc_data[,which(grepl("model_k_", names(doc_data)))]
          #
          #   # ... covariates of interest
          #   X <- doc_data[,covs]
          #
          #   # ... checking for categorical covariates
          #   cat_covs <- as.character(na.omit(sapply(covs, function(x)
          #     ifelse(is.factor(X[,x]) | is.character(X[,x]), x, NA))))
          #   noncat_covs <- covs[which(!(covs %in% cat_covs))]
          #
          #   # - iterate through clusters
          #   cluster_counter <- 0
          #   cluster_total <- length(cluster_list)
          #   print("Estimating bivariate effects for...")
          #   for (cluster in cluster_list) {
          #     cluster_counter <- cluster_counter + 1
          #     print(paste0("cluster [", cluster_counter, "/", cluster_total, "]"))
          #
          #     # - create a copy of the outcome matrix
          #     Y_c <- Y
          #
          #     # - iterate through topic models
          #     topmodel_counter <- 0
          #     topmodel_total <- length(topmodel_list)
          #     for (topmodel in topmodel_list) {
          #       topmodel_counter <- topmodel_counter + 1
          #       print(paste0("... topic-model [", topmodel_counter, "/", topmodel_total, "]"))
          #       # - replace the values in this model's column in the copy of the outcome
          #       #   matrix with 0s and 1s
          #       y <- Y_c[,topmodel]
          #       y[which(y == cluster)] <- -1
          #       y[which(y != cluster & y != -1)] <- -2
          #       y <- ifelse(y == -1, 1, 0)
          #
          #       # - if a topic model DOES NOT HAVE THE TOPIC CLUSTER, don't run
          #       #   the statistical model. We are not interesting in learning
          #       #   whether the topic is present, but what kind of features are
          #       #   related to the probility of discussing the topic cluster
          #       #   when this is present.
          #       if (length(which(y == 1)) > 0) {
          #
          #         # - iterate through NON-CATEGORICAL covariates
          #         for (covariate in noncat_covs) {
          #           model_data <- data.frame(
          #             y = y,
          #             x = X[,covariate]
          #           )
          #           if (nrow(table(model_data$y)) > 1) {
          #             # - estimate a bivariate logistic regression
          #             model <- glm(y ~ x, data = model_data, family = "binomial")
          #
          #             # - calculate marginal effect when going from minimum to maximum value
          #
          #             # ... pull model parameters and simulate 1000 betas
          #             pe <- coef(model)
          #             vc <- vcov(model)
          #             se <- sqrt(diag(vc))
          #
          #             sims <- 1000
          #             simbetas <- MASS::mvrnorm(sims, pe, vc)
          #
          #             # - create two scenarios: one where the value for the covariate of
          #             #   interest is at its minimum value, and another one at its maximum
          #             min_scen <- c(1, as.numeric(min(X[,covariate]))) #... 1 for the interecept
          #             max_scen <- c(1, as.numeric(max(X[,covariate])))
          #
          #             # - predict the Pr in each scenario to discuss this cluster/topic
          #             yhats_min <- exp(min_scen %*% t(simbetas))
          #             yhats_max <- exp(max_scen %*% t(simbetas))
          #
          #             # - calculate the difference between max and min predicted values
          #             diff <- yhats_max - yhats_min
          #             pe <- mean(diff)
          #             lwr <- quantile(diff, probs = 0.025)
          #             upr <- quantile(diff, probs = 0.975)
          #
          #             # - add this result to the out-of-loop results dataframe
          #             new_row <- data.frame(
          #               cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          #               topicmodel = topmodel,
          #               cov = covariate,
          #               pe = pe, lwr = lwr, upr = upr
          #             )
          #           } else {
          #             new_row <- data.frame(
          #               cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          #               topicmodel = topmodel,
          #               cov = covariate,
          #               pe = NA, lwr = NA, upr = NA
          #             )
          #           }
          #           output <- rbind(output, new_row)
          #         }
          #
          #         # - iterate through CATEGORICAL covariates
          #         for (covariate in cat_covs) {
          #           model_data <- data.frame(
          #             y = y,
          #             x = X[,covariate]
          #           )
          #
          #           # - estimate a bivariate logistic regression
          #           model <- glm(y ~ x, data = model_data, family = "binomial")
          #
          #           # - calculate marginal effect: when going from reference
          #           #   class to the other classes
          #           ref_class <- levels(as.factor(model_data$x))[1]
          #           other_classes <- levels(
          #             as.factor(model_data$x))[2:length(levels(
          #               as.factor(model_data$x)
          #             ))]
          #
          #           # ... pull model parameters and simulate 1000 betas
          #           pe <- coef(model)
          #           vc <- vcov(model)
          #           se <- sqrt(diag(vc))
          #
          #           sims <- 1000
          #           simbetas <- MASS::mvrnorm(sims, pe, vc)
          #
          #           # - create a reference scenario and estimate the reference
          #           #   effects
          #           ref_scen <- model_data[1,]
          #           ref_scen$x <- ref_class
          #           ref_scen$x <- factor(ref_scen$x,
          #                                levels = levels(model_data$x))
          #           ref_scen <- model.matrix(model$formula, data = ref_scen)
          #
          #           yhats_ref <- exp(ref_scen %*% t(simbetas))
          #
          #           # - iterate through non-reference classes and calculate
          #           #   and save marginal effects
          #           for (other_class in other_classes) {
          #             if (nrow(table(model_data$y)) > 1) {
          #               # - create a scenario for this class and estimate its
          #               #   marginal effect
          #               other_scen <- model_data[1,]
          #               other_scen$x <- other_class
          #               other_scen$x <- factor(other_scen$x,
          #                                    levels = levels(model_data$x))
          #               other_scen <- model.matrix(model$formula, data = other_scen)
          #               yhats_other <- exp(other_scen %*% t(simbetas))
          #               diff <- yhats_other - yhats_ref
          #               pe <- mean(diff)
          #               lwr <- quantile(diff, probs = 0.025)
          #               upr <- quantile(diff, probs = 0.975)
          #
          #               # - add this categorical marginal effects to the results
          #               # dataset
          #               new_row <- data.frame(
          #                 cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          #                 topicmodel = topmodel,
          #                 cov = paste0(covariate, ": ",
          #                              ref_class, " >> ", other_class),
          #                 pe = pe, lwr = lwr, upr = upr
          #               )
          #             } else {
          #               new_row <- data.frame(
          #                 cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          #                 topicmodel = topmodel,
          #                 cov = paste0(covariate, ": ",
          #                              ref_class, " >> ", other_class),
          #                 pe = pe, lwr = lwr, upr = upr
          #               )
          #             }
          #             output <- rbind(output, new_row)
          #           }
          #         }
          #       }
          #     }
          #   }
          #
          #   # - adding a column indicating that these are partial (by model)
          #   #   results
          #   output$type <- "partial"
          #
          #   # - replacing NAs with 0s
          #   #output[is.na(output)] <- 0
          #
          #   # - for each of the covariates and cluster, add a summary stats-row
          #   for (cluster in cluster_list) {
          #     for (covariate in unique(output$cov)) {
          #       cluster_label <- paste0("Cluster ", sprintf("%02d", cluster))
          #       cl_cov_output <- output %>%
          #         filter(cluster == cluster_label,
          #                cov == covariate)
          #       final_pe <- mean(cl_cov_output$pe, na.rm = TRUE)
          #       final_lwr <- min(cl_cov_output$lwr, na.rm = TRUE)
          #       final_upr <- max(cl_cov_output$upr, na.rm = TRUE)
          #       new_row <- data.frame(
          #         cluster = paste0("Cluster ", sprintf("%02d", cluster)),
          #         topicmodel = topmodel,
          #         cov = covariate,
          #         pe = final_pe, lwr = final_lwr, upr = final_upr,
          #         type = "final"
          #       )
          #       output <- rbind(output, new_row)
          #     }
          #   }
          #
          #   # - adding the top cluster features to the cluster labels
          #   output_to_merge <- output %>%
          #     mutate(cluster_num = as.numeric(gsub("Cluster ", "", cluster)),
          #            cluster_num = as.character(cluster_num))
          #   cluster_top_features$cluster_num <- as.character(
          #     cluster_top_features$cluster_num
          #   )
          #
          #   output_02 <- suppressMessages(left_join(output_to_merge, cluster_top_features)) %>%
          #     mutate(label = paste0(cluster, ": ", top_features))
          #
          #   # - sort the plot by the effects of the first covariate given
          #   out_only_final <- output_02 %>%
          #     filter(type == "final") %>%
          #     arrange(cov, pe)
          #   output_02$label <- factor(output_02$label,
          #                             levels = unique(as.character(out_only_final$label)))
          #
          #   # - the plot
          #   ggplot(output_02 %>%
          #            filter(type == "partial"),
          #          aes(x = label, y = pe, ymin = lwr, ymax = upr)) +
          #     geom_segment(
          #       inherit.aes = FALSE,
          #       data = output_02 %>% filter(type == "final"),
          #       aes(x = label,
          #           xend = label,
          #           y = lwr, yend = upr), color = "deepskyblue2", size = 4, alpha = 0.3) +
          #     geom_pointrange(alpha = 0.2, pch = 20, size = 1.1) +
          #     geom_point(
          #       inherit.aes = FALSE,
          #       data = output_02 %>% filter(type == "final"),
          #       aes(x = label, y = pe), pch = 4, size = 8) +
          #     geom_hline(yintercept = 0, color = "red", alpha = 0.7) +
          #     coord_flip() +
          #     facet_wrap(~ cov) +
          #     scale_x_discrete("") +
          #     scale_y_continuous(
          #       "\nFirst Difference: Change in the Probability of Discussing a Topic Cluster when going form Minimum to Maximum value") +
          #     theme(
          #       panel.background = element_blank(),
          #       panel.border = element_rect(colour = "black", fill = NA),
          #       strip.background = element_rect(colour = "black", fill = "gray90"),
          #       panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
          #       panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
          #                                         size = 0.5),
          #       axis.ticks = element_blank()
          #     )
          #
          })
