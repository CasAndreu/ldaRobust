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

            # - a subset of the covariate dataset, only including "Party" and "Ideology"
            doc_covs_reduced <- doc_covs[,covs]

            # - estimating a separate logistic regression for each cluster, topic model, and
            #   covaraite; predicting the probability of a topic being about that topic as a
            #   function of the covariate

            # ... basic needed objects/lists
            output <- NULL
            topmodel_list <- paste0(paste0("model_k_", orig_altern_models_k_list), r@model_type)
            cluster_list <- 1:max(cluster_mat[,1])

            # create doc data
            doc_data <- cbind(docs_by_cluster_and_model, doc_covs_reduced)


            # ... output matrix
            Y <- doc_data[,which(grepl("model_k_", names(doc_data)))]

            # ... covariates of interest
            X <- doc_data[,covs]

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
                      # statistical model. At this stage we are not interesting in learning
                      # whether the topic is present but the kind of author features that are
                      # related to the probility of discussing the topic cluster when this IS
                      # present.
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
                        }
                      }
                    }
                    if (length(table(all_weights)) == 1) {
                      all_weights_std <- all_weights
                    } else {
                      all_weights_std <- DMwR::ReScaling(all_weights, 0.01, 1)
                    }

                    # - calculate average effects across models for each cluster-covariate
                    for (av_type in c("regular", "weighted")) {
                      if (av_type == "regular") {
                        final_diffs <- sample(x = all_diffs, size = 1000, replace = FALSE)
                        weighted <- "no"
                      } else {
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
                  }
                } else {
                  # - do the same for Non-Categorical variables
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
                  }

                  if (length(table(all_weights)) == 1) {
                    all_weights_std <- all_weights
                  } else {
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

            output$cluster_num <- as.character(output$cluster_num)
            cluster_top_features$cluster_num <- as.character(cluster_top_features$cluster_num)
            output_02 <- left_join(output, cluster_top_features)

            # - invert the order of the cluster labels so Cluster 01 appears first
            output_02 <- output_02 %>%
              arrange(desc(as.numeric(cluster_num))) %>%
              mutate(top_features = factor(top_features, levels = unique(top_features))) %>%
              arrange(top_features)

            # - sort the plot by ideological effects
            out_only_final <- output_02 %>%
              filter(type == "final") %>%
              arrange(cov, pe)
            output_02$label <- factor(output_02$top_features,
                                      levels = unique(as.character(out_only_final$top_features)))


            plotcols <- data.frame(
              Unweighted = "plum3",
              Weighted = "palegreen3"
            )

            # - the plot
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
              scale_fill_manual("Interval", values = plotcols) +
              theme(
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                strip.background = element_rect(colour = "black", fill = "gray90"),
                panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
                panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
                                                  size = 0.5),
                axis.text = element_text(size = 18),
                axis.title = element_text(size = 18),
                strip.text = element_text(size = 19),
                axis.ticks = element_blank()
              )
          })
