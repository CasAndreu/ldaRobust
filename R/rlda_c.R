#' rlda_c
#' main class
#' methods: LDA wrapper, fit
#'
#'
#' @import topicmodels
#' @import ggplot2
#' @import reshape2
#' @importClassesFrom topicmodels LDA
#' @import tm
#' @slot  dtm document term matrix
#' @slot LDA_u lda object(in topicmodels)
#' @slot K numeric or vector, if numeric, number of k to try, if vector, k's to try (will be overwrite with list of k's tried once fit has been run)
#' @slot threshold, sim_threshold, threshold for return2, between [0,1]
#' @slot same_k_estimation, integer, number of initial states to try, if 0, do not try with diff state
#' @slot similarity_measure string, similarity measure (so far cosine or hellinger). Default: cosine
#' @slot num_of_clusters vector, number of clusters used when performing spectral clustering
#' @slot seed_list seeds tried (exists only when same_k_estimation is true)
#' @slot beta_list list of beta matrices in LDA objects of all K tried (ordered same as K)
#' @slot gamma_list list of gamma matrices in LDA objects of all K tried (ordered same as K)
#' @slot model_topic_mat percentage of documents dominated by the given topic
#' @slot similarity_mat maximum similarity (given choice of similarity functions) of a given topic compare to any topics in the original lda model (to give the probability of a user’s topic shows up in a tried model’s resulting topics)
#' @slot sim_matrix_list list of similarity matrices that gives us similarity between
#' @slot init_states # not used yet, probably gonna delete
#' @slot key_features top 10 features of a given topic in each model tried
#' @slot topic_dom_perc_list percentage of documents dominated by the given topic out of documents originally dominated by similar topic in the original model
#' @slot dominant_topic_cluster_list clusters correponding to dominant topics of each document in each model
#' @slot cluster_center_key_words_list top 10 keywords for each center found by the cluster algorithn (so far only support spectral clustering)
#' @slot perc_document_belong_cluster_list percentage of documents belong to a given cluster in a given model
#' @slot topic_cluster_assignment cluster number a given topic belongs to
#' @exportClass rlda
#'
#'
#'
#'
#'
devtools::use_package("topicmodels")
#setClassUnion("Nul_meric", c("numeric", "NULL"))
#setClassUnion("Nul_DA", c("LDA", "NULL"))

setClass("rlda",
         representation(dtm="DocumentTermMatrix",
                        lda_u = "LDA",
                        K = "numeric",
                        threshold="numeric",
                        same_k_estimation="numeric",
                        similarity_measure="character",
                        num_of_clusters = "numeric",
                        seed_list = "numeric", #?????,
                        #model_list = "list",
                        beta_list = "list",
                        gamma_list = "list",
                        model_topic_mat = "list",
                        similarity_mat = "list",
                        similarity_mat_list = "list",
                        init_states = "numeric",
                        topic_dom_perc_list = "list",
                        dominant_topic_cluster_list = "list",
                        key_features = "list",
                        cluster_center_key_words_list = "list",
                        perc_document_belong_cluster_list = "list",
                        topic_cluster_assignment = "list"
                        ),
         prototype(K=5,
                   threshold=0.5,
                   same_k_estimation=0,
                   similarity_measure="cosine",
                   #seed_list = NULL,
                   #model_list = NULL,
                   #model_topic_mat =NULL,
                   #similarity_mat = NULL,
                   #similarity_mat_list = NULL,
                   init_states = 0)
)









