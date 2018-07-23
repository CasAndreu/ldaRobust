#' getTopicInDoc_general
#'
#' compute percentage of old documents dominated by a given old topic in the new model
#'
#' @param r a rlda_general object
#' @include compute_sim_general.R
#' @exportMethod getTopicInDoc_general

setGeneric("getTopicInDoc_general", function(r)standardGeneric("getTopicInDoc_general"))
setMethod("getTopicInDoc_general",
          signature(r = "rlda_general"),
          function (r) {
            thresh = r@threshold
            doc_num = dim(r@dtm)[1]

            if(length(r@gamma_list) == 0)
            {
              stop("must hasve nonempty gamma")
            }


            topic_in_docs = r@gamma_list[[r@idx]]
            words_in_topic = r@beta_list[[r@idx]]
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
            topic_mod_gamma_or = r@gamma_list[[r@idx]]
            # find dominant topic for each document in the original model
            dom_top_ind_old = apply(topic_mod_gamma_or, 1, function(x){order(x, decreasing = TRUE)[1]})
            # Count number of documents dominated by each old topic
            total_doc_dom = tapply(rep(1, doc_num), dom_top_ind_old, FUN = sum)
            topic_dom_list[[1]]= total_doc_dom/doc_num
            j = 0
            for( i in 1:length(r@K) )
            {
              j=j+1
              if (i == r@idx)
              {
                j = j-1
                next
              }
              # similarity matrix result, each row is similarity between A_i with B
              #topic_mod_gamma = r@gamma_list[[i]]
              #max_ind = argmax(topic_mod_gamma, rows = TRUE)
              #sim_mat = sim_func(topic_in_docs, topic_mod@gamma)
              #sim_list = c(sim_list, apply(sim_mat, 1, max))
              if (create_sim_list)
              {
                topic_mod_beta = r@beta_list[[i]]
                sim_mat = sim_func(words_in_topic, topic_mod_beta)
                sim_list2[[j]] = apply(sim_mat, 1, max)#????????
                sim_mat_list[[j]] = sim_mat
              }
              else
              {
                sim_mat = r@similarity_mat_list[[j]]
              }
              topic_mod_gamma = r@gamma_list[[i]]
              ##max_ind = argmax(topic_mod_gamma, rows = TRUE)
              #sim_vals_for_maxind = apply(topic_mod_gamma, 1, function(x){sim_mat[order(x, decreasing = TRUE)[1],]})

              ##sim_vals_for_maxind = sim_mat(c(0:(nrow(topic_mod_beta)-1))*ncol(topic_mod_beta)+max_ind)
              #top_dom_mat = (sim_vals_for_maxind)>thresh
              #topic_dom_perc_list[[i]] = colSums(top_dom_mat)/doc_num
              #topic_dom_list[[i]] = top_dom_mat


              # new_code
              # find the dominant topic for each document in the new model and the similarity value between the new dominant topic and the old dominant topic
              dom_top_ind_new = apply(topic_mod_gamma, 1, function(x){order(x, decreasing = TRUE)[1]})
              sim_vals_for_maxind = sim_mat[(dom_top_ind_new-1)*nrow(sim_mat)+dom_top_ind_old]
              # whether similarity values exceeds threshold
              same_bool_list = sim_vals_for_maxind > thresh
              # count number of documents dominated by new topic similar to a given old topic
              dom_in_new_and_old = tapply(same_bool_list, dom_top_ind_old, FUN = sum)
              # percentage of document dominated by new topic similar to old out of documents dominated by old topic
              topic_dom_perc_list[[j]] = matrix(dom_in_new_and_old/total_doc_dom)
              # percentage of documents dominated by new topic similar to old out of all documents
              topic_dom_list[[j+1]] = matrix(dom_in_new_and_old/doc_num)
            }

            r@model_topic_mat = topic_dom_list
            r@topic_dom_perc_list = topic_dom_perc_list
            if(create_sim_list)
            {
              r@similarity_mat = sim_list2
              r@similarity_mat_list = sim_mat_list
            }
            return(r)
          })
