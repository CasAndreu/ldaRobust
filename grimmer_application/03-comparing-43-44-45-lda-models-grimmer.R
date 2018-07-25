#===============================================================================
# 03-comparing-43-44-45-lda-models-grimmer.R
# Purpose: comparing topic models with very similar number of topics to show
#           that these models present a very similar description of the data
# Author: Andreu Casas
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(ldaRobust)

# PATHS & CONSTANTS
#===============================================================================
data_path <- paste0("~/Google Drive/andreu_tia/projects/rlda/Data/")

# DATA
#===============================================================================

# - load the rlda object with the list of LDA topics estiamted using Grimmer's
#   data (object name: "r")
print(load(paste0(
  data_path,
  "03-paper-data/Grimmer_lda/results.RData"
)))

# - pulling the 3 models of interest

