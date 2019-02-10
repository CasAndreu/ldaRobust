#' fit_rlda
#'
#' This function fits different LDA models with different number of clusters for a rlda object
#'
#' @include rlda_c.R
#' @param r a rlda or rstm object
#' @exportMethod fit
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
            other_dtms = r@other_dtms
            model_type = NULL

            # initialize list of k to try and run lda on all
            if (length(K) == 1){
              if (K != 0)
              {
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
              #r@K = k_list
              model_type = rep("diff_K", length(k_list))
              }
            }
            else{
              # if k is a vector
              if(r@compute_parallel == TRUE){
                lda_list = lda_wrapper_k_para(dtm, k_list,LDA_u@control)
              }
              else{
                lda_list = lda_wrapper_k(dtm, k_list,LDA_u@control)
              }
              model_type = rep("diff_K", length(k_list))
            }

            # try with other dtms
            if(length(other_dtms) > 0)
            {
             lda_list1 = lda_wrapper_dtm(other_dtms, LDA_u@k, LDA_u@control)
             if (is.null(lda_list)){
               lda_list = lda_list1
               model_type = rep("diff_dtm", length(other_dtms))
               k_list = rep(LDA_u@K, length(other_dtms))
             }
             else{
             lda_list = c(lda_list1, lda_list)
             model_type = c(rep("diff_dtm", length(other_dtms)), model_type)
             k_list = c(rep(LDA_u@K, length(other_dtms)),k_list)
             }
            }

            # if need to try with different seeds
            if (length(same_k_estimation) > 1 | same_k_estimation){
              if(length(same_k_estimation) == 1)
              {
              if(same_k_estimation > 5)
                stop("Number of initial states to try should be less than 5")
              set.seed(NULL)
              seeds = .Random.seed[1:same_k_estimation]   #
              rm(.Random.seed, envir=.GlobalEnv)
              r@seed_list = seeds
              lda_list2 = lda_wrapper_init(dtm, seeds,LDA_u@k,LDA_u@control)
              if (is.null(lda_list)){
                lda_list = lda_list2
                model_type = rep("diff_seed", length(seeds))
                k_list = rep(LDA_u@K, length(seeds))
              }
              else{
                lda_list = c(lda_list2, lda_list)
                model_type = c(rep("diff_seed", length(seeds)), model_type)
                k_list = c(rep(LDA_u@K, length(seeds)),k_list)
              }
              #save seed list, run lda
              }
              else
              {
                lda_list2 = lda_wrapper_init(dtm, same_k_estimation,LDA_u@k,LDA_u@control)
                if (is.null(lda_list)){
                  lda_list = lda_list2
                  model_type = rep("diff_seed", length(same_k_estimation))
                  k_list = rep(LDA_u@K, length(same_k_estimation))
                }
                else{
                  lda_list = c(lda_list2, lda_list)
                  model_type = c(rep("diff_seed", length(same_k_estimation)), model_type)
                  k_list = c(rep(LDA_u@K, length(same_k_estimation)),k_list)
                }
              }
            }

            feature_list[[1]] = apply(LDA_u@beta, 1, function(x){terms_u[order(x, decreasing = TRUE)][1:10]})

            # get top 10 features for each model
            ot_dtm_ct = 1
            for (i in 1:length(lda_list))
            {
              # think about better implementation
              #idx_ord = apply(model_i@beta, 1, order, decreasing=TRUE)

              # each column is top 10 feature for each topic in model_i
              mod = lda_list[[i]]
              if(model_type[i] == "diff_dtm")
              {
                ot_terms = mod@terms
                top_f = apply(mod@beta, 1, function(x){ot_terms[order(x, decreasing = TRUE)][1:10]})
                ot_dtm_ct = ot_dtm_ct+1
              }
              else
              {
                top_f = apply(mod@beta, 1, function(x){terms_u[order(x, decreasing = TRUE)][1:10]})
              }
              feature_list[[i+1]] = top_f
              beta_list[[i]] = mod@beta
              gamma_list[[i]] = mod@gamma
            }

            # add new terms and union terms
            if(length(other_dtms) > 0)
            {
              new_beta_tuple = union_terms(terms_u, LDA_u@beta, other_dtms, beta_list, mod_type)
              beta_list = new_beta_tuple[[1]]
              new_terms = new_beta_tuple[[2]]
            }
            #return (need to add seed list!!!!)

            #overwrite K?
            r@key_features = feature_list
            r@beta_list = c(list(LDA_u@beta),beta_list)
            r@gamma_list = c(list(LDA_u@gamma), gamma_list)
            r@model_type = c("or", model_type)
            r@K = c(LDA_u@k, k_list)
            return(r)

          }
)

setMethod("fit",
          signature(r = "rstm"),
          function (r)
          {
            stm_u <- r@stm_u
            K <- r@K
            documents <- r@documents
            vocab <- r@vocab
            stm_list = NULL
            k_list <- K
            seed_list=NULL
            beta_list = list()
            theta_list = list()
            feature_list = list()
            model_type = NULL

            # initialize list of k to try and run lda on all
            if (length(K) == 1){
              if (K != 0){
                # if k is a number
                or_topic_number=stm_u$settings$dim$K
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
                  stm_list = stm_wrapper_k_para(documents, vocab, k_list)
                }
                else{
                  stm_list = stm_wrapper_k(documents, vocab, k_list)
                }
                #r@K = k_list
                model_type = rep("diff_K", length(k_list))
              }
            }
            else{
              # if k is a vector
              if(r@compute_parallel == TRUE){
                stm_list = stm_wrapper_k_para(documents, vocab, k_list)
              }
              else{
                stm_list = stm_wrapper_k(documents, vocab, k_list)
              }
              model_type = rep("diff_K", length(k_list))
            }


            feature_list[[1]] = apply(stm_u$beta$logbeta[[1]], 1, function(x){vocab[order(x, decreasing = TRUE)][1:10]})

            # get top 10 features for each model
            for (i in 1:length(stm_list)){
              # think about better implementation
              #idx_ord = apply(model_i@beta, 1, order, decreasing=TRUE)

              # each column is top 10 feature for each topic in model_i
              mod = stm_list[[i]]
              top_f = apply(mod$beta$logbeta[[1]], 1, function(x){vocab[order(x, decreasing = TRUE)][1:10]})
              feature_list[[i+1]] = top_f
              beta_list[[i]] = mod$beta$logbeta[[1]]
              theta_list[[i]] = mod$theta
            }

            #overwrite K?
            r@key_features <- feature_list
            r@beta_list <- c(list(stm_u$beta$logbeta[[1]]),beta_list)
            r@theta_list <- c(list(mod$theta), theta_list)
            r@model_type <- c("or", model_type)
            r@K <- c(stm_u$settings$dim$K, k_list)
            return(r)

          }
)

# utility functions
lda_wrapper_k <- function(dtm, list_of_k, control_list){
  lda_l = NULL
  for (k in list_of_k){
    lda_k=topicmodels::LDA(dtm, k, control = control_list)
    lda_l=c(lda_l, lda_k)
  }
  return(lda_l)
}

lda_wrapper_dtm <- function(list_of_dtms, k, control_list){
  lda_l = NULL
  for (other_dtm in list_of_dtms){
    lda_k=topicmodels::LDA(other_dtm, k, control = control_list)
    lda_l=c(lda_l, lda_k)
  }
  return(lda_l)
}

lda_wrapper_init <- function(dtm, list_of_init, k,control_list){
  lda_l = NULL
  for(s in list_of_init){
    #set.seed(s)
    control_list@seed = s
    lda_s = topicmodels::LDA(dtm, k, control = control_list)
    lda_l = c(lda_l, lda_s)
    #rm(.Random.seed, envir=.GlobalEnv)
  }
  return(lda_l)
}

lda_wrapper_k_para <- function(dtm, list_of_k, control_list){
  lda_l = NULL
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

stm_wrapper_k <- function(documents, vocab, list_of_k){
  stm_l = NULL
  print(length(vocab))
  for (k in list_of_k){
    stm_k=stm::stm(documents, vocab, K=k)
    stm_l[[k]] <- stm_k
  }
  return(stm_l)
}

stm_wrapper_k_para <- function(documents, vocab, list_of_k){
  stm_l = NULL
  no_cores <- parallel::detectCores() - 1
  cl<-parallel::makeCluster(no_cores)
  parallel::clusterSetRNGStream(cl, 123)
  doParallel::registerDoParallel(cl)
  stm_l = foreach::foreach( k = list_of_k)  %dopar% {
    stm::stm(documents, vocab, K=k)
  }

  parallel::stopCluster(cl)
  return(stm_l)
}


# utility functions for other dtms
union_terms <- function(dtm_terms, or_beta, list_of_dtms, beta_list, mod_type)
{
  # get union of terms
  list_of_dtm_terms = lapply(list_of_dtms, function(x) x$dimnames$Terms)
  list_of_dtm_terms[[length(list_of_dtms) + 1]] = dtm_terms
  all_terms = purrr::reduce(list_of_dtm_terms, function(x,y) union(x,y))

  additional_cols = matrix(0, nrow(or_beta), length(all_terms) - length(dtm_terms))
  term_order = c(dtm_terms, setdiff(all_terms, dtm_terms))
  or_beta = cbind(or_beta, additional_cols)
  dtm_ct = 1
  new_beta_list = list()
  for(i in 1:length(beta_list))
  {
    if(mod_type[i] == "diff_dtm")
    {
      alt_dtm_terms = list_of_dtm_terms[[dtm_ct]]
      new_words = setdiff(all_terms, alt_dtm_terms)
      sort_idx = match(term_order, c(alt_dtm_terms, new_words))
      additional_col_dtm = matrix(0, nrow(beta_list[[i]]), length(new_words))
      new_beta = cbind(beta_list[[i]], additional_col_dtm)[,sort_idx]
      new_beta_list[[i]] = new_beta
      dtm_ct = dtm_ct+1
    }
    else
    {
      additional_col_mat = matrix(0, nrow(beta_list[[i]]), length(all_terms) - length(dtm_terms))
      new_beta_list[[i]] = cbind(beta_list[[i]], additional_col_mat)
    }
  }
  return(list(new_beta_list,term_order))
}


