#' compute_sim
#'
#' compute similarities between topics generated by new models and topics in original models
#'
#' @include rlda_c.R
#' @param r a rlda object
#' @exportMethod compute_sim
#'

setGeneric("compute_sim", function(r)standardGeneric("compute_sim"))
setMethod("compute_sim",
          signature(r = "rlda"),
          function (r) {
            if(length(r@beta_list) == 0)
            {
              stop("must run fit first")
            }


            words_in_topics = (r@lda_u)@beta
            sim_measure = r@similarity_measure
            if(sim_measure == "cosine")
              sim_func = cos_sim
            else if(sim_measure == "hellinger"){
              sim_func = topicmodels::distHellinger
            }
            else
            {
              # if change to function, must check it is a function before hand and accept matrices
              # or change it to apply afterwards
              stop("do not support it yet")
            }

            sim_list = list()
            sim_mat_list = list()
            i = 0
            for( i in 1:length(r@K) )
            {
                # similarity matrix result, each row is similarity between A_i with B
                topic_mod_beta = r@beta_list[[i]]
                sim_mat = sim_func(words_in_topics, topic_mod_beta)
                sim_list[[i]] = apply(sim_mat, 1, max)
                sim_mat_list[[i]] = sim_mat
            }

            r@similarity_mat = sim_list
            r@similarity_mat_list = sim_mat_list
            return(r)

            })
# might be more convenient with proxy
cos_sim <- function(A,B){
  # row of a matrix is a instance(i.e. topic) (with tcrosspod), otherwise use crosspod
  numerator = tcrossprod(A, B)
  denom_1 = sqrt(apply(A,1,crossprod))
  denom_2 = sqrt(apply(B, 1, crossprod))
  return(numerator/outer(denom_1,denom_2))
}
