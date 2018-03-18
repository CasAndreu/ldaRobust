#' getTopicInDoc
#'
#' compute percentage of old documents dominated by a given old topic in the new model
#'
#' @param r a rlda object
#' @include compute_sim.R
#' @exportMethod getTopicInDoc

setGeneric("getTopicInDoc", function(r)standardGeneric("getTopicInDoc"))
setMethod("getTopicInDoc",
          signature(r = "rlda"),
          function (r) {
            thresh = r@threshold
            doc_num = r@dtm$nrow

            if(length(r@gamma_list) == 0)
            {
              stop("must run fit first")
            }


            topic_in_docs = (r@lda_u)@gamma
            words_in_topic = (r@lda_u)@beta
            sim_measure = r@similarity_measure
            if(sim_measure == "cosine")
              sim_func = cos_sim
            else if(sim_measure == "hellinger"){
              sim_func = topicmodels::distHellinger
            }
            else
             {
                # if change to function, must check it is a function before hand
              stop("do not support it yet")
            }

            create_sim_list = FALSE
            if(length(r@similarity_mat_list) == 0)
              create_sim_list = TRUE

            topic_dom_list = list() # topic vs doc T/F value
            topic_dom_perc_list = list() # topic vs doc dominimance in percentage
            sim_list2 = list() # topic similarity list
            sim_mat_list = list() # topic similarity matrix list
            topic_mod_gamma_or = (r@lda_u)@gamma
            dom_top_ind_old = apply(topic_mod_gamma_or, 1, function(x){order(x, decreasing = TRUE)[1]})
            for( i in 1:length(r@K) )
            {
              # similarity matrix result, each row is similarity between A_i with B
                #topic_mod_gamma = r@gamma_list[[i]]
                #max_ind = argmax(topic_mod_gamma, rows = TRUE)
                #sim_mat = sim_func(topic_in_docs, topic_mod@gamma)
                #sim_list = c(sim_list, apply(sim_mat, 1, max))
                if (create_sim_list)
                {
                  topic_mod_beta = r@beta_list[[i]]
                  sim_mat = sim_func(words_in_topic, topic_mod_beta)
                  sim_list2[[i]] = apply(sim_mat, 1, max)#????????
                  sim_mat_list[[i]] = sim_mat
                }
                else
                {
                  sim_mat = r@similarity_mat_list[[i]]
                }
                topic_mod_gamma = r@gamma_list[[i]]
                ##max_ind = argmax(topic_mod_gamma, rows = TRUE)
                #sim_vals_for_maxind = apply(topic_mod_gamma, 1, function(x){sim_mat[order(x, decreasing = TRUE)[1],]})

                ##sim_vals_for_maxind = sim_mat(c(0:(nrow(topic_mod_beta)-1))*ncol(topic_mod_beta)+max_ind)
                #top_dom_mat = (sim_vals_for_maxind)>thresh
                #topic_dom_perc_list[[i]] = colSums(top_dom_mat)/doc_num
                #topic_dom_list[[i]] = top_dom_mat


                # new_code
                dom_top_ind_new = apply(topic_mod_gamma, 1, function(x){order(x, decreasing = TRUE)[1]})
                sim_vals_for_maxind = sim_mat[(dom_top_ind_new-1)*nrow(sim_mat)+dom_top_ind_old]
                # whether similarity values exceeds threshold
                same_bool_list = sim_vals_for_maxind > thresh
                total_doc_dom = tapply(rep(1, length(dom_top_ind_old)), dom_top_ind_old, FUN = sum)
                dom_in_new_and_old = tapply(same_bool_list, dom_top_ind_old, FUN = sum)
                topic_dom_perc_list[[i]] = dom_in_new_and_old/total_doc_dom
            }

            #r@model_topic_mat = topic_dom_list
            r@topic_dom_perc_list = topic_dom_perc_list
            if(create_sim_list)
            {
              r@similarity_mat = sim_list2
              r@similarity_mat_list = sim_mat_list
            }
            return(r)
        })
