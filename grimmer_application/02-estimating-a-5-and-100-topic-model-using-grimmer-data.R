#==============================================================================
# 02-estimating-a-5-and-100-topic-model-using-grimmer-data.R
# Purpose: fitting a 5 and 100 topic model (LDA) to grimmer's data. The goal is
#           to show how topic scope shrinks as we increase the number of topics.
# Author: Andreu Casas
#==============================================================================

# PACKAGES
#==============================================================================
library(dplyr)
library(ldaRobust)
library(topicmodels)

# PATHS & CONSTANTS
#==============================================================================
# - DEVELOPMENT
#data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# - PRODUCTION
data_path <- paste0("/scratch/acs706/rlda/data/")

# DATA WRANGLING
#==============================================================================

# - loading the RLDA object with the list of LDA models estimated using Grimmer
#   data (object: "r"). In this rlda object we have a copy of Grimmer's TDM
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

dtm_grimmer <- r@dtm

# MAIN -- ESTIMATE 2 MORE LDAs: 5 and 100 topic models
#==============================================================================
grimmer_5topic_model <- topicmodels::LDA(dtm_grimmer, k = 5)
save(grimmer_5topic_model,
     paste0(data_path,
            "03-paper-data/Grimmer_lda/grimmer_5topic_model.RData"))

grimmer_100topic_model <- topicmodels::LDA(dtm_grimmer, k = 100)
save(grimmer_100topic_model,
     paste0(data_path,
            "03-paper-data/Grimmer_lda/grimmer_100topic_model.RData"))


