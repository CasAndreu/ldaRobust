#' rlda
#' main class
#' methods: LDA wrapper, fit
#' 
#' 
#' @import topicmodels
#' @importClassesFrom topicmodels LDA
#' @import tm
#' @param dtm document term matrix
#' @param LDA_u lda object(in topicmodels)
#' @param K numeric or vector, if numeric, number of k to try, if vector, k's to try
#' @param threshold, sim_threshold, threshold for return2, between [0,1]
#' @param same_k_estimation, integer, number of initial states to try, if 0, do not try with diff state
#' @param similarity_measure string, similarity measure. Default: cosine
#' @param seed_list
#' @param model_list
#' @param model_topic_mat
#' @param similarity_mat
#' @param init_states 
#' 
#' fit
#' @param x tmd
#' @param method ??? VEM, Gibbs
#' @param k number of topics
#' @param ???? LDA control list
#' 
#' 
#' 
#' @return A matrix with models vs topics (from the users lda model result). Each entry gives the probability of a user’s topic shows up in a tried model’s resulting topics (by computing similarity scores between topics of model outputs and user’s given topic, take max). Also, return same number of topics with different initial state (first few row, depending on which)
#' @return A matrix with models vs topics. Each entry gives proportion of documents that dominated by the given topics that map to the topic given by user (need a threshold for similarity)
#' 
#' 
#' 
#' 
#' 
devtools::use_package("topicmodels")
setClass("rlda",
         representation(dtm="DocumentTermMatrix",
                        lda_u = "LDA",
                        K = "numeric",
                        threshold="numeric",
                        same_k_estimation="numeric",
                        similarity_measure="character",
                        seed_list = "matrix", #?????,
                        model_list = "LDA",
                        model_topic_mat = "numeric",
                        similarity_mat = "numeric",
                        init_states = "numeric"),
         prototype(K=5, 
                   threshold=0.5, 
                   same_k_estimation=0, 
                   similarity_measure="cosine",
                   seed_list = NULL,
                   model_list = NULL,
                   model_topic_mat = NULL,
                   similarity_mat = NULL,
                   init_states = NULL)
)










