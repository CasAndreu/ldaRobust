#' cluster_topic
#'
#' compute percentage of old documents dominated by a given cluster
#'
#' @param r a rlda object
#' @import kernlab
#' @include rlda_c.R
#' @exportMethod cluster_topic

setGeneric("cluster_topic", function(r)standardGeneric("cluster_topic"))
setMethod("cluster_topic",
          signature(r = "rlda"),
          function (r) {

            # create clusters of topics using beta matrix (words in topic)
            num_topics_or = r@lda_u@k
            num_topics_list = r@K
            terms_u = r@lda_u@terms
            doc_num = dim(r@dtm)[1]
            beta_mat = rbind(r@lda_u@beta, do.call(rbind, r@beta_list))
            cl_obj = kernlab::specc(beta_mat, r@num_of_clusters)

            # initialize dominant cluster list and percentage of documents in a given model dominated by a cluster
            gamma_original = (r@lda_u)@gamma
            gamma_list = r@gamma_list
            dominant_topic_cluster_list = cl_obj@.Data[apply(gamma_original, 1, function(x){order(x, decreasing = TRUE)[1]})]
            # might be better with dplyr
            cluster_value = rep(1, doc_num)
            perc_document_tab = tapply(rep(1, doc_num), dominant_topic_cluster_list, FUN=sum)/doc_num
            perc_document_belong_cluster_list = list()
            perc_document_belong_cluster = rep(0, r@num_of_clusters)
            perc_document_belong_cluster[as.integer(names(perc_document_tab))]=as.vector(perc_document_tab)
            #print(perc_document_tab)
            #print(perc_document_belong_cluster)
            perc_document_belong_cluster_list[[1]] = perc_document_belong_cluster
            #print(perc_document_belong_cluster_list[[1]])
            #print(sum(perc_document_belong_cluster_list[[1]]))
            # go through all models
            starting_idx = num_topics_or
            for(i in 1:length(num_topics_list))
                {
                  topic_num = num_topics_list[i]
                  cluster_value = rep(1, doc_num)
                  new_dom_top_cluster = cl_obj@.Data[apply(gamma_list[[i]], 1, function(x){order(x, decreasing = TRUE)[1]})+starting_idx]
                  dominant_topic_cluster_list = rbind(dominant_topic_cluster_list, new_dom_top_cluster)

                  perc_document_tab = tapply(rep(1, doc_num), new_dom_top_cluster, FUN=sum)/doc_num
                  perc_document_belong_cluster = rep(0, r@num_of_clusters)
                  perc_document_belong_cluster[as.integer(names(perc_document_tab))]=as.vector(perc_document_tab)
                  perc_document_belong_cluster_list[[i+1]] = perc_document_belong_cluster
                  #print(perc_document_belong_cluster_list[[i+1]])
                  #print(sum(perc_document_belong_cluster_list[[i+1]]))

                  starting_idx = starting_idx + num_topics_list[i]
            }
            r@perc_document_belong_cluster_list = perc_document_belong_cluster_list
            r@topic_cluster_assignment = cl_obj@.Data
            r@cluster_center_key_words_list = apply(cl_obj@centers, 1, function(x){terms_u[order(x, decreasing = TRUE)][1:10]})
            r@dominant_topic_cluster_list = dominant_topic_cluster_list

            return(r)
          })
