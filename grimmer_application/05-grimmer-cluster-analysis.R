#===============================================================================
# 05-grimmer-cluster-analysis.R
# Purpose: using the clustered topics to study topic and result robustness
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(extrafont) # font_import(pattern = "lmroman*") # loadfonts()
library(ldaRobust)
library(reshape2)
library(ggplot2)
library(MASS)
library(lme4)

# PATHS & CONSTANTS
#===============================================================================
#data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")
data_path <- paste0("~/Desktop/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================
# - load the rlda object with the list of LDA topics estamated using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  #"03-paper-data/Grimmer_lda/results.RData"
  "03-paper-data/Grimmer_lda_41-47/grimmer_clus.RData"
)))
r <- r_clus

# - load a dataset with doc-level covariates
doc_covs <- read.csv(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/PressData.csv"
))

# - author-year level covariates
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/variables.RData"
)))

# - import a dataset with TOPIC/CLUSTER-level covariates: whether a cluster is
#   about Credit-Claiming / Position-Taking / Other. Also, whether it matches
#   a topic in Grimmer's Table 1, and the topic label for the match.
cluster_covs <- read.csv(paste0(
  #data_path, "03-paper-data/Grimmer_lda/grimmer_lda_cluster_info_LABELED.csv"
  data_path, "03-paper-data/Grimmer_lda/grimmer_lda_cluster_info_41_47_LABELED.csv"
))

# - loading aggregated measures of credit claiming and position taking from
#   Grimmer's replication data
print(load(paste0(
  data_path,
  "01-replication-data/02-Grimmer-2012/DepVars.RData"
)))


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
#model_cluster_df <- as.data.frame(matrix(nrow = 11, ncol = 44))
model_cluster_df <- as.data.frame(matrix(nrow = 7, ncol = 44))
colnames(model_cluster_df) <- paste0("cluster_", sprintf("%02d", 1:44))
rownames(model_cluster_df) <- paste0("model_", sprintf("%02d", c(44,
                                                                 #39:43,
                                                                 41:43,
                                                                 #45:49,
                                                                 45:47)))
for (i in 1:length(clustering[[1]])) {
  model_cluster_df[i,] <- round(clustering[[1]][[i]], 4)
}

# - preparing two dataset to visualize the data
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

# - write out a copy of this dataset showing only the cluster number and the
#   most predictive features of each cluster
# write.csv(plot_db_01_02 %>% dplyr::select(cluster, top_features),
#           paste0(data_path,
#                  "03-paper-data/Grimmer_lda_41-47/grimmer_lda_cluster_info.csv"),
#           row.names = FALSE) # - Manually labeling the cluster for whether
#                                  they're about Credit Claiming / Position Taking
#                                  or something else.

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
                                     sprintf("%02d",
                                             seq(1,
                                                 length(plot_db_01_02$cluster),
                                                 1))),
    sec.axis = sec_axis(~.,
                        breaks = seq(1, length(plot_db_01_02$cluster), 1),
                        labels = plot_db_01_02$top_features)) +
  scale_y_continuous("\nProportion of Documents about each Topic-Cluster") +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray80", size = 0.2),
    text = element_text(family = "LMRoman10-Regular", color = "black"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.ticks = element_blank()
  )

# ggsave(p, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#                            "prop_docs_in_each_cluster_by_topic_41_47.pdf"),
#       width = 16, height = 18, units = "in", device = cairo_pdf)


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
#   data_path, "03-paper-data/Grimmer_lda/cluster_covariate_indiv_effects_41-47.csv"
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
    #text = element_text(family = "LM Roman 10"),
    text = element_text(family = "LMRoman10-Regular"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    axis.ticks = element_blank()
  )

# ggsave(p2, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
#                             "covariates_ensemble_first_differences_41_47.pdf"),
#        width = 16, height = 18, units = "in", device = cairo_pdf)


# [C] A Replication of Grimmer's "Appropriators not Position Takers"
#-------------------------------------------------------------------------------
# - create a more complete document-level dataset by merging two pervious ones
doc_data$year <- doc_covs$Year
doc_data$date <- doc_covs$Date

# - add a var to 'cluster_covs' indicating only the cluster number (not label)
cluster_covs$cluster_num <- gsub("cluster_", "", cluster_covs$cluster)

# - /!\ some RECODING of the cluster type
cluster_covs$type[which(cluster_covs$cluster_num %in%
                    c("34"))] <- "credit-claiming"

# - for each author, year and model:
#       * calculate proportion of docs with a dominant Credit-Claiming cluster
#       * calculate proportion of docs with a dominant Position-Taking cluster
#       * calculate the difference between the two proportions: Balance measure
author_year_model_balance <- NULL
authors <- unique(doc_data$author)
model_labels <- paste0("model_k_", c(r@lda_u@k, r@K))
years <- c(2005, 2006, 2007)

for (a in authors) {
  # - select all documents from this Senator
  a_docs <- doc_data %>% filter(author == a)

  # - iterate through years
  for (yr in years) {
    # - select the press releases from that year
    year_docs <- a_docs %>%
      filter(year == yr)

    if (nrow(year_docs) > 0) {

      # - iterate through the original and alternative model
      for (m in model_labels) {
          # - select the doc cluster classification according to that model
          m_cluster_doc_output <- year_docs[, m]

          # - group the Credit-Claimin and Position-Taking docs
          a_docs_n <- nrow(year_docs)
          cred_cl <- m_cluster_doc_output[which(
            m_cluster_doc_output %in%
              as.numeric(
                cluster_covs$cluster_num[cluster_covs$type == "credit-claiming"])
            )]
          cred_cl_out <- length(cred_cl) /a_docs_n
          pos_tk <- m_cluster_doc_output[which(
            m_cluster_doc_output %in%
              as.numeric(
                cluster_covs$cluster_num[cluster_covs$type == "position-taking"])
          )]
          pos_tk_out <- length(pos_tk) /a_docs_n
          balance <- cred_cl_out - pos_tk_out

          # - save the information for this author/year/model
          new_row <- data.frame(
            author = a,
            year = yr,
            model = m,
            doc_n = a_docs_n,
            credit_claiming_n = length(cred_cl),
            credit_claiming_prop = cred_cl_out,
            position_taking_n = length(pos_tk),
            position_taking_prop = pos_tk_out,
            balance = balance
          )
          author_year_model_balance <- rbind(
            author_year_model_balance, new_row
          )
      }
    }
  }
}

# - grimmer's "variables" object to dataframe. Also, adding new variable "year"
member_covs <- as.data.frame(variables)
member_covs$author <- gsub("theta.", "", attr(variables$ideal.vec, "names"))
member_covs <- member_covs %>%
  mutate(year = ifelse(party == 1 & majority == 1 |
                         party == 0 & majority == 0, 2007, NA))
index_2005_start <- 1
index_2005_end <- which(member_covs$author == "Akaka")[2] - 1
index_2006_start <- which(member_covs$author == "Akaka")[2]
index_2006_end <- which(member_covs$author == "Akaka")[3] - 1
member_covs$year[index_2005_start:index_2005_end] <- 2005
member_covs$year[index_2006_start:index_2006_end] <- 2006

# - removing from the "author_year_model_balance" dataframe the row with info
#   about Senators that weren't senators yet
relevant_obs <- paste0(member_covs$author, "-", member_covs$year)
author_year_model_balance_ready <- author_year_model_balance %>%
  mutate(obs_label = paste0(author, "-", year)) %>%
  filter(obs_label %in% relevant_obs)

# - adding to this dataset with the outcome variable, the covariates of interest
member_covs$obs_label <- paste0(member_covs$author, "-", member_covs$year)
member_covs_to_merge <- member_covs %>%
  dplyr::select(-author, -year) %>%
  mutate(obs_label = as.character(obs_label))

author_year_model_balance_ready$obs_label <- as.character(
  author_year_model_balance_ready$obs_label
)

main_author_db <- left_join(author_year_model_balance_ready,
                            member_covs_to_merge)

# - estimate now Grimmer's paper model Z times, one for each of the topic
#   models in the rlda object
models_output <- NULL
for (m in model_labels) {
  # - pull the data from this topic model
  model_data <- main_author_db %>%
    filter(model == m)

  # - fit linear regressions using Grimmer's covariates (he fits a diff model:
  #   a Bayesian Multilevel Linear Regression)

  # ... Outcome 1: Balance (CreditClaiming - PositionTaking)
  model_01_out <- lmer(balance ~ centered +
                      party +
                      majority +
                      on.cycle +
                      I(years/100)  +
                      house +
                      freshman +
                      I(state.pop/1e7) +
                      (1|ind.indic)  +
                      (1|state.indic),
                    data = model_data)

  # ... Outcome 2: Credit Claiming
  model_02_out <- lmer(credit_claiming_prop ~ centered +
                         party +
                         majority +
                         on.cycle +
                         I(years/100)  +
                         house +
                         freshman +
                         I(state.pop/1e7) +
                         (1|ind.indic)  +
                         (1|state.indic),
                       data = model_data)

  # ... Outcome 3: Position Taking
  model_03_out <- lmer(position_taking_prop ~ centered +
                         party +
                         majority +
                         on.cycle +
                         I(years/100)  +
                         house +
                         freshman +
                         I(state.pop/1e7) +
                         (1|ind.indic)  +
                         (1|state.indic),
                       data = model_data)

  # - pulling the statistical model coefficients and CIs
  model_outputs <- list(model_01_out, model_02_out, model_03_out)
  clean_model_outputs <- NULL
  for (mdel in model_outputs) {
    coef_tbl <- summary(mdel)$coefficients
    coef_tbl_new <- data.frame(
      terms = rownames(coef_tbl),
      pe = as.numeric(coef_tbl[, "Estimate"]),
      lwr = as.numeric(coef_tbl[, "Estimate"]) -
        (1.96 * coef_tbl[, "Std. Error"]),
      upr = as.numeric(coef_tbl[, "Estimate"]) +
        (1.96 * coef_tbl[, "Std. Error"]),
      outcome = strsplit(as.character(mdel@call)[2], split = " ~ ")[[1]][1],
      topic_model = m
    )
    rownames(coef_tbl_new) <- NULL
    clean_model_outputs <- rbind(clean_model_outputs, coef_tbl_new)
  }
  models_output <- rbind(models_output, clean_model_outputs)
}

# - aggregate statistical model results across topic models (taking the average
#   pe, and the lowest lwr CI and highest upr CI)
final_model_output <- NULL
# - iterate through statistical model types
stat_model_list <- unique(models_output$outcome)
for (out in stat_model_list) {
  # - select results only for this statistical model with this outcome variable
  outcome_res <- models_output %>%
    filter(outcome == out)

  # - average results across topic models
  outcome_res_across <- outcome_res %>%
    group_by(terms) %>%
    summarise(
      outcome = out,
      pe = mean(pe),
      lwr = min(lwr),
      upr = max(upr)
    )

  final_model_output <- rbind(
    final_model_output, outcome_res_across
  )
}

# - better labels for the covariates
final_model_output$terms <- recode(final_model_output$terms,
                                   `(Intercept)` = "Intercept",
                                   `centered` = "Alignment",
                                   `party` = "Democrat",
                                   `I(years/100)` = "Years/100",
                                   `house` = "Former House Member",
                                   `freshman` = "Freshman",
                                   `majority` = "Majority",
                                   `on.cycle` = "In Cycle",
                                   `I(state.pop/1e+07)` = "State Pop. (Millions)")

# - better labels for the outcomes
final_model_output$outcome <- recode(final_model_output$outcome,
                                   `balance` = "Credit Claiming - Position Taking",
                                   `credit_claiming_prop` = "Credit Claiming",
                                   `position_taking_prop` = "Position Taking")

# - the same for the partial stat model results
models_output$terms <- recode(models_output$terms,
                                   `(Intercept)` = "Intercept",
                                   `centered` = "Alignment",
                                   `party` = "Democrat",
                                   `I(years/100)` = "Years/100",
                                   `house` = "Former House Member",
                                   `freshman` = "Freshman",
                                   `majority` = "Majority",
                                   `on.cycle` = "In Cycle",
                                   `I(state.pop/1e+07)` = "State Pop. (Millions)")

models_output$outcome <- recode(models_output$outcome,
                                `balance` = "Credit Claiming - Position Taking",
                                `credit_claiming_prop` = "Credit Claiming",
                                `position_taking_prop` = "Position Taking")

# - merge partial and aggregated result datasets
models_output <- models_output %>%
  dplyr::select(-topic_model) %>%
  mutate(type = "partial")

final_model_output$type <- "aggregate"

final_res <- rbind(models_output, final_model_output)

# - a dataset with the model results from Grimmer's paper
# ... create his "balance" outcome variables
exp.apps<- list.dep[[1]]
exp.subs<- list.dep[[2]]
diff<- exp.apps - exp.subs
# ... now name the covariates the same way he does in his replication code
centered.rat<- variables[[1]]
party<- variables[[2]]
majority<- variables[[3]]
on.cycle<- variables[[4]]
house<- variables[[5]]
freshman<- variables[[6]]
state.pop<- variables[[7]]
ind.indic<- variables[[8]]
state.indic<- variables[[9]]
ideal.vec<- variables[[10]]
approp.mem<- variables[[11]]
years<- variables[[12]]
# ... estimating the models (following exactly his code)
grimmer_01 <- lmer(diff~centered.rat + party + majority + on.cycle +  I(years/100)
               + house + freshman +  I(state.pop/1e7) + (1|ind.indic)  +
                 (1|state.indic))
grimmer_02 <- lmer(exp.apps~centered.rat + party + majority + on.cycle +
                     I(years/100)  + house +  freshman +  I(state.pop/1e7) +
                     (1|ind.indic)  + (1|state.indic))

grimmer_03<- lmer(exp.subs~centered.rat + party + majority + on.cycle +
                 I(years/100)  + house +  freshman +  I(state.pop/1e7) +
                 (1|ind.indic)  + (1|state.indic))
# ... cleaning the model outputs from grimmer
model_outputs_grimmer <- list(grimmer_01, grimmer_02, grimmer_03)
clean_model_outputs_grimmer <- NULL
for (mdel in model_outputs_grimmer) {
  coef_tbl <- summary(mdel)$coefficients
  coef_tbl_new <- data.frame(
    terms = rownames(coef_tbl),
    pe = as.numeric(coef_tbl[, "Estimate"]),
    lwr = as.numeric(coef_tbl[, "Estimate"]) -
      (1.96 * coef_tbl[, "Std. Error"]),
    upr = as.numeric(coef_tbl[, "Estimate"]) +
      (1.96 * coef_tbl[, "Std. Error"]),
    outcome = strsplit(as.character(mdel@call)[2], split = " ~ ")[[1]][1]
  )
  rownames(coef_tbl_new) <- NULL
  clean_model_outputs_grimmer <- rbind(clean_model_outputs_grimmer, coef_tbl_new)
}
# ... rename covariates and outcome variables
clean_model_outputs_grimmer$terms <- recode(clean_model_outputs_grimmer$terms,
                              `(Intercept)` = "Intercept",
                              `centered.rat` = "Alignment",
                              `party` = "Democrat",
                              `I(years/100)` = "Years/100",
                              `house` = "Former House Member",
                              `freshman` = "Freshman",
                              `majority` = "Majority",
                              `on.cycle` = "In Cycle",
                              `I(state.pop/1e+07)` = "State Pop. (Millions)")

clean_model_outputs_grimmer$outcome <- recode(clean_model_outputs_grimmer$outcome,
                                `diff` = "Credit Claiming - Position Taking",
                                `exp.apps` = "Credit Claiming",
                                `exp.subs` = "Position Taking")

# - transform some large coefficients to fit a secondary axis
vars_to_transf <- c("Alignment", "Years/100")
scalar <- 4
for (v in vars_to_transf) {
  clean_model_outputs_grimmer$pe[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$pe[clean_model_outputs_grimmer$terms == v] / scalar
  clean_model_outputs_grimmer$lwr[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$lwr[clean_model_outputs_grimmer$terms == v] / scalar
  clean_model_outputs_grimmer$upr[clean_model_outputs_grimmer$terms == v] <-
    clean_model_outputs_grimmer$upr[clean_model_outputs_grimmer$terms == v] / scalar

  final_res$pe[final_res$terms == v] <-
    final_res$pe[final_res$terms == v] / scalar
  final_res$lwr[final_res$terms == v] <-
    final_res$lwr[final_res$terms == v] / scalar
  final_res$upr[final_res$terms == v] <-
    final_res$upr[final_res$terms == v] / scalar

}

final_res$scalar <- 0
final_res$scalar[which(as.character(final_res$terms) %in% vars_to_transf)] <- 1

clean_model_outputs_grimmer$scalar <- 0
clean_model_outputs_grimmer$scalar[
  which(as.character(clean_model_outputs_grimmer$terms) %in% vars_to_transf)] <- 1


# - get rid of the intercept
final_res <- final_res %>%
  filter(terms != "Intercept") %>%
  mutate(terms = as.factor(as.character(terms)))

clean_model_outputs_grimmer <- clean_model_outputs_grimmer %>%
  filter(terms != "Intercept") %>%
  mutate(terms = as.factor(as.character(terms)))

# - sorting the results so Alignment and Years/100 (re-scaled coefficients) are
#   at the bottom
final_res <- final_res %>%
  mutate(terms = factor(terms, levels = rev(c(
    "Alignment", "Years/100", "Democrat", "Former House Member",
    "Freshman", "Majority", "In Cycle", "State Pop. (Millions)"
  ))))

clean_model_outputs_grimmer <- clean_model_outputs_grimmer %>%
  mutate(terms = factor(terms, levels = rev(c(
    "Alignment", "Years/100", "Democrat", "Former House Member",
    "Freshman", "Majority", "In Cycle", "State Pop. (Millions)"
  ))))

# - PLOT comparing our "robust" results to Grimmer's original results.

p3 <- ggplot(final_res %>%
         filter(type == "partial"),
       aes(x = as.numeric(terms), y = pe, ymin = lwr, ymax = upr)) +
  geom_segment(
    inherit.aes = FALSE,
    data = final_res %>% filter(type == "aggregate"),
    aes(x = terms,
        xend = terms,
        y = lwr, yend = upr), color = "deepskyblue2", size = 4, alpha = 0.3) +
  geom_pointrange(alpha = 0.1, pch = 20, size = 1.1) +
  geom_pointrange(inherit.aes = FALSE,
                  data = clean_model_outputs_grimmer,
                  aes(
                    x = as.numeric(terms) + 0.2, y = pe, ymin = lwr, ymax = upr,
                    shape = as.character(scalar)), color = "red") +
  scale_shape_manual(values = c(16, 17)) +
  geom_point(
    inherit.aes = FALSE,
    data = final_res %>% filter(type == "aggregate"),
    aes(x = terms, y = pe), pch = 4, size = 6) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.7) +
  coord_flip() +
  facet_wrap(~outcome) +
  scale_x_discrete("") +
  scale_y_continuous(
    "\nAggregated coefficients (+95% confidence intervals) from Bayesian Multilevel Linear Regression",
    sec.axis = sec_axis(trans = ~.*4, breaks = seq(-1.2, 1.2, .5)),
    expand = c(0,0),
    breaks = seq(-.45, .5, .1),
    limits = c(-.4, .4)) +
  geom_polygon(inherit.aes = FALSE,
               data = data.frame(x = c(6.5, 6.5, 8.75, 8.75),
                                 y = c(-.4, 0.4, 0.4, -0.4)),
               aes(x = x , y = y),
               fill = "gray70", alpha = 0.3) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black", fill = "gray90"),
    panel.grid.major.x = element_line(color = "gray60", linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray60", linetype = "dotted",
                                      size = 0.5),
    text = element_text(family = "LMRoman10-Regular"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18 ),
    #axis.ticks = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(2, "lines"),
    legend.position = "none"
  )

ggsave(p3, filename = paste0(data_path, "03-paper-data/Grimmer_lda/figures/",
                            "grimmer_models_41_47.pdf"),
       width = 16, height = 10, units = "in", device = cairo_pdf)


