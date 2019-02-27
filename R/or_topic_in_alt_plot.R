#' or_topic_in_alt_plot
#'
#' compute clustering of topics across different models
#'
#' @param r a rlda or rstm object
#' @param dir directory to save plot
#' @exportMethod or_topic_in_alt_plot
#'

setGeneric("or_topic_in_alt_plot", function(r, dir)standardGeneric("or_topic_in_alt_plot"))
setMethod("or_topic_in_alt_plot",
          signature(r = "rlda", dir = "character"),
          function (r, dir) {
            # [ D ] VISUALIZING WHICH ORIGINAL TOPICS IN ALTERNATIVE MODELS, and viscversa
            #-------------------------------------------------------------------------------

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
                     model = gsub("_", " = ", model))

            #pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
            #           "topic_presence_41_47-STRICT.pdf"),
            #    width = 20, height = 18)
            ggplot(plot_db,
                   aes(y = as.numeric(as.factor(labels)), x = model,
                       fill = as.character(value_binary))) +
              geom_tile( color = "gray20") +
              #geom_hline(yintercept = 2.5, size = 1.5) +
              #geom_vline(xintercept = 3.5, color = "red", alpha = 1, size = 1.5) +
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
setMethod("or_topic_in_alt_plot",
          signature(r = "rstm", dir = "character"),
          function (r, dir = NULL) {
            # [ D ] VISUALIZING WHICH ORIGINAL TOPICS IN ALTERNATIVE MODELS, and viscversa
            #-------------------------------------------------------------------------------

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
                     model = gsub("_", " = ", model))

            #pdf(paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
            #           "topic_presence_41_47-STRICT.pdf"),
            #    width = 20, height = 18)
            ggplot(plot_db,
                   aes(y = as.numeric(as.factor(labels)), x = reorder(as.factor(model), as.numeric(gsub('\\D+','', plot_db$model))),
                       fill = as.character(value_binary))) +
              geom_tile( color = "gray20") +
              #geom_hline(yintercept = 2.5, size = 1.5) +
              #geom_vline(xintercept = 3.5, color = "red", alpha = 1, size = 1.5) +
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

          }
)
