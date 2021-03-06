#' compute_sim_general
#'
#' compute similarities between topics generated by new models and topics in original models
#'
#' @include rlda_general.R
#' @param r a rlda_general object
#' @exportMethod compute_sim_general
#'

setGeneric("compute_sim_general", function(r)standardGeneric("compute_sim_general"))
setMethod("compute_sim_general",
          signature(r = "rlda_general"),
          function (r) {
            if(length(r@beta_list) == 0)
            {
              stop("must have nonempty list")
            }


            words_in_topics = r@beta_list[[r@idx]]
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
            feature_list = list()
            i = 0
            j = 1
            feature_list[[1]] = apply(r@beta_list[[r@idx]], 1, function(x){r@terms[order(x, decreasing = TRUE)][1:10]})
            for( i in 1:length(r@K) )
            {
              j=j+1
              if (i == r@idx)
              {
                j = j-1
                next
              }
              # similarity matrix result, each row is similarity between A_i with B
              topic_mod_beta = r@beta_list[[i]]
              sim_mat = sim_func(words_in_topics, topic_mod_beta)
              sim_list[[j+1]] = apply(sim_mat, 1, max)
              #print(sim_list[[j]])
              sim_mat_list[[j]] = sim_mat
              ot_dtm_ct = 1
              if(model_type[i] == "diff_dtm")
              {
                diff_term = r@other_dtms[[ot_dtm_ct]]$dimnames$Terms
                feature_list[[j]] = apply(r@beta_list[[i]], 1, function(x){diff_term[order(x, decreasing = TRUE)][1:10]})
                ot_dtm_ct = ot_dtm_ct+1
              }
              else
              {
                feature_list[[j]] = apply(r@beta_list[[i]], 1, function(x){r@terms[order(x, decreasing = TRUE)][1:10]})
              }
            }

            r@similarity_mat = sim_list
            r@similarity_mat_list = sim_mat_list
            r@key_features = feature_list
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
