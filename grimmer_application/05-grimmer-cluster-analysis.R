#===============================================================================
# 05-grimmer-cluster-analysis.R
# Purpose: using the clustered topics to study topic and result robustness
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(extrafont) # font_import(pattern = "lmroman*")
library(ldaRobust)
library(reshape2)
library(ggplot2)
library(MASS)

# PATHS & CONSTANTS
#===============================================================================
data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================
# - load the rlda object with the list of LDA topics estamated using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

# - load a dataset with doc-level covariates
doc_covs <- read.csv(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/PressData.csv"
))


# DATA WRANGLING
#===============================================================================
# - focusing on studying the clustering with the same number of clusters and
#   topics in the "original model": 44 clusters
clustering_i <- which(r@num_of_clusters == 44)

# MAIN
#===============================================================================


# [A] Showing whether docs get classified into the same clusters across models
#-------------------------------------------------------------------------------

# - for the clustering of interest (C = 44), pull the list providing information
#   about the proportion of docs classified into topics belonging to each cluster
#   in each of the models
clustering <-r@perc_document_belong_cluster_list[clustering_i]

# - transforming this information from nested list to data frame format
model_cluster_df <- as.data.frame(matrix(nrow = 11, ncol = 44))
colnames(model_cluster_df) <- paste0("cluster_", sprintf("%02d", 1:44))
rownames(model_cluster_df) <- paste0("model_", sprintf("%02d", c(44,
                                                                 39:43,
                                                                 45:49)))
for (i in 1:length(clustering[[1]])) {
  model_cluster_df[i,] <- round(clustering[[1]][[i]], 4)
}

# - preparing two dataset to visualize to visualize the data
plot_db_01_01 <- model_cluster_df %>%
  mutate(model = rownames(.)) %>%
  gather(cluster, value, - model)

plot_db_01_02 <- model_cluster_df %>%
  mutate(model = rownames(.)) %>%
  gather(cluster, value, - model) %>%
  group_by(cluster) %>%
  summarise(pe = mean(value),
            lwr = t.test(value)$conf.int[1],
            upr = t.test(value)$conf.int[2])

# - adding information about the top features of each cluster centroid
features_nested_list <- r@cluster_center_key_words_list[clustering_i]
features_list_str <- NULL
for (j in 1:ncol(features_nested_list[[1]])) {
  new_str <- paste0(features_nested_list[[1]][1:6,j], collapse = ", ")
  features_list_str <- c(features_list_str, new_str)
}


plot_db_01_02$top_features <- features_list_str

# - a plot showing the proportion that get classified into the same cluster by
#   model
p <- ggplot(plot_db_01_02 %>%
         mutate(cluster = as.numeric(gsub("cluster_", "", cluster))),
       aes(x = cluster, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange(size = 1.1) +
  geom_point(inherit.aes = FALSE,
             data = plot_db_01_01 %>%
               mutate(cluster = as.numeric(gsub("cluster_", "", cluster))),
             aes(x = cluster, y = value),
             pch = 4,
             alpha = 0.6,
             size = 3) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
  scale_x_continuous("",
                     expand = c(0.01, 0.01),
                     limits = c(1, length(plot_db_01_02$cluster)),
                     breaks = seq(1, length(plot_db_01_02$cluster), 1),
                     labels = paste0("Cluster ",
                                     seq(1, length(plot_db_01_02$cluster), 1)),
    sec.axis = sec_axis(~.,
                        breaks = seq(1, length(plot_db_01_02$cluster), 1),
                        labels = plot_db_01_02$top_features)) +
  scale_y_continuous("\nProportion of Documents about each Topic-Cluster") +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray80", size = 0.2),
    text = element_text(family = "LM Roman 10", color = "black"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.ticks = element_blank()
  )

ggsave(p, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
                            "prop_docs_in_each_cluster_by_topic.pdf"),
       width = 16, height = 18, units = "in", device = cairo_pdf)


# [B] Showing how each doc covariate is associated to each cluster, by model
#-------------------------------------------------------------------------------

# - pulling the doc-cluster assignments for the 44-cluster clustering
topic_cluster_assign <- r@topic_cluster_assignment[[clustering_i]]

# - a subset of the covariate dataset, only including "Party" and "Ideology"
doc_covs_reduced <- doc_covs %>%
  dplyr::select(File, party, ideal) %>%
  rename(file_grimmer = File)

# - a document-model matrix indicating into which cluster each document has been
#   classified into
orig_altern_models_k_list <- c(r@lda_u@k, r@K)
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
    cluster = topic_cluster_assign[start_i:end_i]
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

# - estimating a separate logistic regression for each cluster, topic model, and
#   covaraite; predicting the probability of a topic being about that topic as a
#   function of the covariate

# ... basic needed objects/lists
output <- NULL
topmodel_list <- paste0("model_k_", orig_altern_models_k_list)
cluster_list <- 1:max(topic_cluster_assign)
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

# - adding a column indicating that these partial (by model) results
output$type <- "partial"

# - replacing NAs with 0s
#output[is.na(output)] <- 0 # /!\ I don't know really how we should treat these

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

# - save a copy of the resulting ouptut
# write.csv(output, paste0(
#   data_path, "03-paper-data/Grimmer_lda/cluster_covariate_indiv_effects.csv"
# ), row.names = FALSE)

# - rename the covariate labels
output <- output %>%
  mutate(cov = ifelse(cov == "ideal", "CONSERVATISM", as.character(cov)),
         cov = ifelse(cov == "party", "DEMOCRATS", as.character(cov)))

# - a plot
p2 <- ggplot(output %>%
         filter(type == "partial"),
       aes(x = cluster, y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(
    inherit.aes = FALSE,
    data = output %>% filter(type == "final"),
    aes(x = cluster,
        xend = cluster,
        y = lwr, yend = upr), color = "deepskyblue2", size = 4, alpha = 0.3) +
  geom_pointrange(alpha = 0.1, pch = 20, size = 1.1) +
  geom_point(
    inherit.aes = FALSE,
    data = output %>% filter(type == "final"),
    aes(x = cluster, y = pe), pch = 4, size = 6) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~ cov) +
  scale_x_discrete("") +
  scale_y_continuous(
    "\nFirst Difference: Change in the Probability of Discussing a Topic-Cluster when going form Minimum to Maximum value") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black", fill = "gray90"),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
                                      size = 0.5),
    text = element_text(family = "LM Roman 10"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    axis.ticks = element_blank()
  )

ggsave(p2, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
                            "covariates_ensemble_first_differences.pdf"),
       width = 16, height = 18, units = "in", device = cairo_pdf)

