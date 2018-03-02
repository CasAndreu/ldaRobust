#' fit_rlda
#' 
#' methods: LDA wrapper, fit
#' 
#' @include rlda.R
#' @param dtm document term matrix
#' @param LDA_u lda object(in topicmodels)
#' @param K numeric or vector, if numeric, number of k to try, if vector, k's to try
#' @param same_k_estimation, integer, number of initial states to try, if 0, do not try with diff state; <= [0, 5]; set see before
#' 
#' 
setGeneric("fit", function(r)standardGeneric("fit"))
setMethod("fit",
    signature(r = "rlda"),
    function (r) 
    {
        # get all variables from rlda object
        lda_list = NULL
        k_list = NULL
        seed_list=NULL
        dtm = r@dtm
        LDA_u = r@lda_u
        K = r@K
        same_k_estimation = r@same_k_estimation
        
        # initialize list of k to try and run lda on all
        if (length(K) == 1){
          # if k is a number
          or_topic_number=LDA_u@k
          half = K%/%2
          # if number of K to try is more than half of original topics
          if (half > or_topic_number%/%2){
            k_list = seq(1, K)
          }
          else{
            start_num = or_topic_num-K%/%2
            end_num =  start_num+K-1
            k_list = seq(start_num, end_num)
          }
          
          lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
        }
        else{
          # if k is a vector
          lda_list = lda_wrapper_k(dtm, K,LDA_u@control)
        }
        
        # if need to try with different seeds
        if (same_k_estimation){
          #???????seed?
          #save seed list, run lda
        }
        #return (need to add seed list!!!!)
        r@model_list = lda_list
        r@K = k_list  #overwrite K?
        return(r)
      
    }
)

lda_wrapper_k <- function(dtm, list_of_k, control_list){
  lda_l = NULL
  for (k in list_of_k){
    lda_k=topicmodels::LDA(dtm, k, control = control_list)
    lda_l=c(lda_l, lda_k)
  }
  return(lda_l)
}

lda_wrapper_init <- function(dtm, list_of_init, control_list){
  lda_l = NULL
  for(s in list_of_init){
    lda_s = topicmodels::LDA(dtm, s, control_list)
    lda_l = c(lda_l, lda_s)
  }
  return(lda_l)
}
