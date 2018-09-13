#' or_topic_in_alt_plot
#'
#' compute clustering of topics across different models
#'
#' @param r a rlda object
#' @param dir directory to save plot
#' @param cluster_info
#' @exportMethod or_topic_in_alt_plot
#'

setGeneric("or_topic_in_alt_plot", function(r, dir, cluster_info)standardGeneric("or_topic_in_alt_plot"))
setMethod("or_topic_in_alt_plot",
          signature(r = "rlda", dir = "character", cluster_info = "data.frame"),
          function (r, dir, cluster_info) {
            # [ D ] VISUALIZING WHICH ORIGINAL TOPICS IN ALTERNATIVE MODELS, and viscversa
            #-------------------------------------------------------------------------------

            # A matrix indicating whether original topics are present in alternative models

            # - initialize matrix: #Original-topics x #Alternative-models
            k_list <- c(r@lda_u@k, r@K)
            cluster_mat <- r@topic_cluster_assignment
            cluster_top_features <- r@cluster_center_key_words_list
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
            #cluster_info <- read.csv(paste0(
            #  data_path,
            #  "03-paper-data/Grimmer_lda_41-47/grimmer_strinct_cluster_info_41_47_LABELED.csv"
            #))

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
                #text = element_text(family = "LM Roman 10"),
                #axis.text = element_text(size = 18),
                #axis.title = element_text(size = 18),
                axis.ticks = element_blank()
              )
            #dev.off()
            #ggsave(p0, filename = paste0(dir,
            #                            "/topic_presence.pdf"),
            #       width = 16, height = 18, units = "in", device = cairo_pdf)

          })
