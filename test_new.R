#test

setwd("./ldaRobust")
data <- rjson::fromJSON(file = "data.json")
#set.seed(123) # - we set the seed so we can get reproducible results
#data <- data[sample(x = 1:length(data), size = 1000, replace = FALSE)]
df<- plyr::ldply(data, data.frame)
x = tm::SimpleCorpus(tm::VectorSource(df$speech), control=list(language ="en"))
dtm = tm::DocumentTermMatrix(x, control=list(language="en",removePunctuation=TRUE, stopwords=TRUE, removeNumbers=TRUE,stemming=TRUE, tolower=TRUE))

lda = topicmodels::LDA(dtm, 40)
now = Sys.time()
r_testfull <- new("rlda",
         dtm=dtm,
         lda_u=lda,
         threshold = 0.8,
         num_of_clusters = c(8,9),
         K = 10,
         compute_parallel = TRUE)
r_testfull <- ldaRobust::fit(r_testfull)
diff = Sys.time()-now
print("parallel full fit time is:")
print(diff)
fit_list = list()
for (i in 1:10)
{
  r <- new("rlda",
           dtm=C_dtm,
           lda_u=lda,
           threshold = 0.8,
           similarity_measure = "cosine",
           K = 10)
  r <- ldaRobust::fit(r)
  r <- ldaRobust::getTopicInDoc(r)
  fit_list[[i]] = r

}
#r <- ldaRobust::fit(r)
r <- ldaRobust::getTopicInDoc(r)
#r <- ldaRobust::cluster_topic(r)

test <- function()
{
  now = Sys.time()
  #lda = topicmodels::LDA(dtm, 10)
  print(Sys.time()-now)
  now = Sys.time()
  r_test_new = ldaRobust::fit(r)
  print(Sys.time()-now)
}












count1 = c()
count2 = c()
prev = ""
for( i in 1:length(author)){
  if (author[i] != prev){
    count1 = c(count1, i)
    count2 = c(count2, i-1)
    prev = author[i]
  }
}




# find top 10 key words for the approximated grimmer results
for (i in 1:length(lda_list))
{
  # think about better implementation
  #idx_ord = apply(model_i@beta, 1, order, decreasing=TRUE)

  # each column is top 10 feature for each topic in model_i
  mod = lda_list[[i]]
  top_f = apply(mod@beta, 1, function(x){terms_u[order(x, decreasing = TRUE)][1:10]})
  feature_list[[i]] = top_f
  beta_list[[i]] = mod@beta
  gamma_list[[i]] = mod@gamma
}


# generate features;
gri_feature = apply(output$etas, 1, function(x){words2[order(x, decreasing = TRUE)][1:10]})
approx_feature = apply(t(list_of_results[[6]]$mus), 1, function(x){words2[order(x, decreasing = TRUE)][1:10]})


# generate beta list
beta_list = list()
gamma_list = list()
for (i in 1:length(list_of_results))
{
  beta_list[[i]] = t(list_of_results[[i]]$mus)
  gamma_list[[i]] = list_of_results[[i]]$rs
}

r_approx <- new("rlda_general", dtm = r_test_new@dtm, beta_list=beta_list,
         gamma_list=gamma_list, idx=6, terms=words2, threshold = 0.8, K=c(39,40,41,42,43,44,45,46,47,48,49))

## new rlda_c, with self supplyting beta and gamma


#' rlda_c
#' main class
#' methods: LDA wrapper, fit
#'
#'
#' @import topicmodels
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import ggplot2
#' @import reshape2
#' @importClassesFrom topicmodels LDA
#' @import tm
#' @slot  dtm document term matrix
#' @slot LDA_u lda object(in topicmodels)
#' @slot K numeric or vector, if numeric, number of k to try, if vector, k's to try (will be overwrite with list of k's tried once fit has been run)
#' @slot threshold, sim_threshold, threshold for return2, between [0,1]
#' @slot compute_parallel bool, if true, will parallel when fitting lda models; otherwise, sequential for fitting
#' @slot same_k_estimation, integer, number of initial states to try, if 0, do not try with diff state
#' @slot fitted bool, TRUE if supplying own betalist and gammalist, FALSE otherwise
#' @slot similarity_measure string, similarity measure (so far cosine or hellinger). Default: cosine
#' @slot num_of_clusters numeric or vector, number of clusters used when performing spectral clustering
#' @slot seed_list seeds tried (exists only when same_k_estimation is true)
#' @slot beta_list list of beta matrices in LDA objects of all K tried (ordered same as K)
#' @slot terms vector, unique words in corpus, only needed when supplying own beta, gamma
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
                        fitted="logical",
                        compute_parallel = "logical",
                        similarity_measure="character",
                        num_of_clusters = "numeric",
                        seed_list = "numeric", #?????,
                        #model_list = "list",
                        beta_list = "list",
                        gamma_list = "list",
                        terms = "character",
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
                   compute_parallel = TRUE,
                   #seed_list = NULL,
                   #model_list = NULL,
                   #model_topic_mat =NULL,
                   #similarity_mat = NULL,
                   #similarity_mat_list = NULL,
                   init_states = 0)
)
@slot compute_parallel bool, if true, will parallel when fitting lda models; otherwise, sequential for fitting


original rlda:
  #' rlda_c
  #' main class
  #' methods: LDA wrapper, fit
  #'
  #'
  #' @import topicmodels
  #' @import foreach
  #' @import doParallel
  #' @import parallel
  #' @import ggplot2
  #' @import reshape2
#' @importClassesFrom topicmodels LDA
#' @import tm
#' @slot  dtm document term matrix
#' @slot LDA_u lda object(in topicmodels)
#' @slot K numeric or vector, if numeric, number of k to try, if vector, k's to try (will be overwrite with list of k's tried once fit has been run)
#' @slot threshold, sim_threshold, threshold for return2, between [0,1]
#' @slot compute_parallel bool, if true, will parallel when fitting lda models; otherwise, sequential for fitting
#' @slot same_k_estimation, integer, number of initial states to try, if 0, do not try with diff state
#' @slot similarity_measure string, similarity measure (so far cosine or hellinger). Default: cosine
#' @slot num_of_clusters numeric or vector, number of clusters used when performing spectral clustering
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
                        compute_parallel = "logical",
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
                   compute_parallel = TRUE,
                   #seed_list = NULL,
                   #model_list = NULL,
                   #model_topic_mat =NULL,
                   #similarity_mat = NULL,
                   #similarity_mat_list = NULL,
                   init_states = 0)
)
