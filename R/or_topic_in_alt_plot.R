#' or_topic_in_alt_plot
#'
#' compute clustering of topics across different models
#'
#' @param r a rlda object
#' @param dir directory to save plot
#' @exportMethod or_topic_in_alt_plot
#'

setGeneric("or_topic_in_alt_plot", function(r, dir)standardGeneric("or_topic_in_alt_plot"))
setMethod("or_topic_in_alt_plot",
          signature(r = "rlda", dir = "character"),
          function (r, dir) {
                    # A matrix indicating whether original topics are present in alternative models

                    # - initialize matrix: #Original-topics x #Alternative-models
                    cluster_mat <- r@topic_cluster_assignment
                    cluster_top_features <- r@cluster_center_key_words_list

                    top_stability_mat <- r@top_stability_mat

                    # Preparing the data to plot
                    plot_db <- top_stability_mat %>%
                      dplyr::select(everything()) %>%
                      gather(model, value, -top_features, -top_cluster_num) %>%
                      mutate(labels = paste("cluster", top_cluster_num)) %>%
                      mutate(top_cluster_num = factor(top_cluster_num,
                                                      levels = sort(unique(top_cluster_num),
                                                                    decreasing = TRUE))) %>%
                      arrange(desc(top_cluster_num))

                    plot_db <- plot_db %>%
                      mutate(labels = factor(as.character(labels),
                                             levels = rev(unique(plot_db$labels))),
                             value_binary = ifelse(value > 0, 1, 0),
                             model = paste0(gsub("diff_", "(diff-", gsub("k_", "k = ", model)), ")"),
                             original = ifelse(as.numeric(as.character(top_cluster_num)) > max(r@K), 
                                               "or", "new"))

                    ggplot(plot_db,
                           aes(y = as.numeric(as.factor(labels)), x = model,
                               fill = as.character(value_binary))) +
                      geom_tile(color = "gray20", aes(alpha = original)) +
                      scale_x_discrete("\nAlternative Models", expand = c(0,0)) +
                      scale_y_continuous("", expand = c(0,0),
                                         breaks = seq(1, nrow(top_stability_mat), 1),
                                         labels = as.character(
                                           rev(unique(plot_db$labels))),
                                         sec.axis = sec_axis(
                                           ~.,
                                           breaks = seq(1,length(top_stability_mat$top_features), 1),
                                           labels = rev(top_stability_mat$top_features))) +
                      scale_fill_manual(values = c("gray80", "springgreen4")) +
                      scale_alpha_manual("", values = c(1, 0.7)) +
                      theme(
                        panel.background = element_blank(),
                        legend.position = "none",
                        axis.ticks = element_blank()
                      )
          })
