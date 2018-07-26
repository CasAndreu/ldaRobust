#' fit_rlda
#'
#' This function fits different LDA models with different number of clusters for a rlda object
#'
#' @include rlda_c.R
#' @param r a rlda object
#' @exportMethod "fit"
#'
#'

setGeneric("fit", function(r)standardGeneric("fit"), package = "rlda")
setMethod("fit",
          signature(r = "rlda"),
          function (r)
          {
            # get all variables from rlda object
            lda_list = NULL
            k_list = r@K
            seed_list=NULL
            beta_list = list()
            gamma_list = list()
            feature_list = list()
            dtm = r@dtm
            LDA_u = r@lda_u
            K = r@K
            same_k_estimation = r@same_k_estimation
            terms_u = LDA_u@terms

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
                if(K == 1)
                  k_list = c(or_topic_number + 1)
                else
                {
                  start_num = or_topic_number - half
                  end_num =  start_num+K
                  #if(end_num > or_topic_number)
                  k_list = c(seq(start_num, or_topic_number-1), seq(or_topic_number+1, end_num))
                  #else
                  #k_list = seq(start_num, or_topic_number-1)
                }
              }
              print("list of K to try: ")
              print(k_list)
              if(r@compute_parallel == TRUE){
                lda_list = lda_wrapper_k_para(dtm, k_list,LDA_u@control)
              }
              else{
                lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
              }
              r@K = k_list
            }
            else{
              # if k is a vector
              if(r@compute_parallel == TRUE){
                lda_list = lda_wrapper_k_para(dtm, k_list,LDA_u@control)
              }
              else{
                lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
              }
            }

            # if need to try with different seeds
            if (same_k_estimation){
              if(same_k_estimation > 5)
                stop("Number of initial states to try should be less than 5")
              set.seed(NULL)
              seeds = .Random.seed[1:same_k_estimation]   #
              r@seed_list = seeds
              lda_list2 = lda_wrapper_init(dtm, seeds,LDA_u@k,LDA_u@control)
              lda_list = c(lda_list2, lda_list)
              #save seed list, run lda
            }

            # get top 10 features for each model
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

            #return (need to add seed list!!!!)

            #overwrite K?
            r@key_features = feature_list
            r@beta_list = beta_list
            r@gamma_list = gamma_list
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

lda_wrapper_init <- function(dtm, list_of_init, k,control_list){
  lda_l = NULL
  for(s in list_of_init){
    set.seed(s)
    lda_s = topicmodels::LDA(dtm, k, control = control_list)
    lda_l = c(lda_l, lda_s)
  }
  return(lda_l)
}

lda_wrapper_k_para <- function(dtm, list_of_k, control_list){
  lda_l = NULL
  print("yeah")
  no_cores <- parallel::detectCores() - 1
  cl<-parallel::makeCluster(no_cores)
  parallel::clusterSetRNGStream(cl, 123)
  doParallel::registerDoParallel(cl)
  lda_l = foreach::foreach( k = list_of_k,
                            .combine = c)  %dopar% {
                              topicmodels::LDA(dtm, k, control = control_list)
                            }

  parallel::stopCluster(cl)
  return(lda_l)
}

#'
#' #' fit_rlda
#' #'
#' #' This function fits different LDA models with different number of clusters for a rlda object
#' #'
#' #' @include rlda_c.R
#' #' @param r a rlda object
#' #' @exportMethod "fit"
#' #'
#' #'
#'
#' setGeneric("fit", function(r)standardGeneric("fit"), package = "rlda")
#' setMethod("fit",
#'           signature(r = "rlda"),
#'           function (r)
#'           {
#'             # get all variables from rlda object
#'             lda_list = NULL
#'             k_list = NULL
#'             seed_list=NULL
#'             beta_list = list()
#'             gamma_list = list()
#'             feature_list = list()
#'             dtm = r@dtm
#'
#'             if(!r@fitted)
#'             {
#'               LDA_u = r@lda_u
#'               K = r@K
#'               same_k_estimation = r@same_k_estimation
#'               terms_u = LDA_u@terms
#'               # initialize list of k to try and run lda on all
#'               if (length(K) == 1){
#'                 # if k is a number
#'                 or_topic_number=LDA_u@k
#'                 half = K%/%2
#'                 # if number of K to try is more than half of original topics
#'                 if (half > or_topic_number%/%2){
#'                   k_list = seq(1, K)
#'                 }
#'                 else{
#'                   if(K == 1)
#'                     k_list = c(or_topic_number + 1)
#'                   else
#'                   {
#'                     start_num = or_topic_number - half
#'                     end_num =  start_num+K
#'                     #if(end_num > or_topic_number)
#'                     k_list = c(seq(start_num, or_topic_number-1), seq(or_topic_number+1, end_num))
#'                     #else
#'                     #k_list = seq(start_num, or_topic_number-1)
#'                   }
#'                 }
#'                 print("list of K to try: ")
#'                 print(k_list)
#'                 if(r@compute_parallel == TRUE){
#'                   lda_list = lda_wrapper_k_para(dtm, k_list,LDA_u@control)
#'                 }
#'                 else{
#'                   lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
#'                 }
#'                 r@K = k_list
#'               }
#'               else{
#'                 # if k is a vector
#'                 if(r@compute_parallel == TRUE){
#'                   lda_list = lda_wrapper_k_para(dtm, k_list,LDA_u@control)
#'                 }
#'                 else{
#'                   lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
#'                 }
#'               }
#'
#'               # if need to try with different seeds
#'               if (same_k_estimation){
#'                 if(same_k_estimation > 5)
#'                   stop("Number of initial states to try should be less than 5")
#'                 set.seed(NULL)
#'                 seeds = .Random.seed[1:same_k_estimation]   #
#'                 r@seed_list = seeds
#'                 lda_list2 = lda_wrapper_init(dtm, seeds,LDA_u@k,LDA_u@control)
#'                 lda_list = c(lda_list2, lda_list)
#'                 #save seed list, run lda
#'               }
#'
#'               # get top 10 features for each model
#'               for (i in 1:length(lda_list))
#'               {
#'                 # think about better implementation
#'                 #idx_ord = apply(model_i@beta, 1, order, decreasing=TRUE)
#'
#'                 # each column is top 10 feature for each topic in model_i
#'                 mod = lda_list[[i]]
#'                 top_f = apply(mod@beta, 1, function(x){terms_u[order(x, decreasing = TRUE)][1:10]})
#'                 feature_list[[i]] = top_f
#'                 beta_list[[i]] = mod@beta
#'                 gamma_list[[i]] = mod@gamma
#'               }
#'               r@beta_list = beta_list
#'               r@gamma_list = gamma_list
#'             }
#'             else
#'             {
#'               for (i in 1:length(r@beta_list))
#'               {
#'                 feature_list[[i]] = apply(r@beta_list[[i]], 1, function(x){r@terms[order(x, decreasing = TRUE)][1:10]})
#'               }
#'             }
#'
#'             #return (need to add seed list!!!!)
#'
#'             #overwrite K?
#'             r@key_features = feature_list
#'             return(r)
#'
#'           }
#' )
#'
#' lda_wrapper_k <- function(dtm, list_of_k, control_list){
#'   lda_l = NULL
#'   for (k in list_of_k){
#'     lda_k=topicmodels::LDA(dtm, k, control = control_list)
#'     lda_l=c(lda_l, lda_k)
#'   }
#'   return(lda_l)
#' }
#'
#' lda_wrapper_init <- function(dtm, list_of_init, k,control_list){
#'   lda_l = NULL
#'   for(s in list_of_init){
#'     set.seed(s)
#'     lda_s = topicmodels::LDA(dtm, k, control = control_list)
#'     lda_l = c(lda_l, lda_s)
#'   }
#'   return(lda_l)
#' }
#'
#' lda_wrapper_k_para <- function(dtm, list_of_k, control_list){
#'   lda_l = NULL
#'   print("yeah")
#'   no_cores <- parallel::detectCores() - 1
#'   cl<-parallel::makeCluster(no_cores)
#'   parallel::clusterSetRNGStream(cl, 123)
#'   doParallel::registerDoParallel(cl)
#'   lda_l = foreach::foreach( k = list_of_k,
#'                             .combine = c)  %dopar% {
#'                               topicmodels::LDA(dtm, k, control = control_list)
#'                             }
#'
#'   parallel::stopCluster(cl)
#'   return(lda_l)
#' }


