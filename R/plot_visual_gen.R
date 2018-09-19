#' plot_visual_gen
#'
#' generate visualizations for rlda_general
#'
#' @include rlda_general.R
#' @param r a rlda_general object
#' @param dir directory to save generated plots (no "/" at the end)
#' @exportMethod plot_visual_gen
#'

setGeneric("plot_visual_gen", function(r, dir)standardGeneric("plot_visual_gen"))
setMethod("plot_visual_gen",
          signature(r = "rlda_general", dir = "character"),
          function (r, dir) {
            k = r@K[-r@idx]
            dir.create(paste0(dir,"/sim_gen"))
            dir.create(paste0(dir, "/topDoc_gen"))

            # plot similarity matrix
            similarity_list_mat = do.call(cbind, r@similarity_mat_list)
            mod_idx = rep(1:length(k), k)
            #idx_mat = cbind(mod_idx, unlist(lapply(k, function(x) 1:x)))
            #colnames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[2])
            #rownames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[1])
            #colnames(similarity_list_mat) = paste('t[',1:dim(similarity_list_mat)[2],']',sep='')
            #colnames(similarity_list_mat) = paste('t[',idx_mat[,1],idx_mat[,2],']',sep='')
            #rownames(similarity_list_mat) = paste('t[',1:dim(similarity_list_mat)[1],']',sep='')
            #sim_melt = melt(similarity_list_mat)
            #sim_melt$mod_i = paste('m[',rep(1:length(k), k*44),']',sep=)

            #x_tick_lab = parse(text = levels(as.factor(sim_melt$Var2)))
            #y_tick_lab = parse(text = levels(as.factor(sim_melt$Var1)))


            for (i in 1:length(r@similarity_mat_list)){
              similarity_list_mat = r@similarity_mat_list[[i]]
              #colnames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[2])
              #rownames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[1])
              #colnames(similarity_list_mat) = paste('t[',1:dim(similarity_list_mat)[2],']',sep='')
              colnames(similarity_list_mat) = paste('t[',i,'*","*',1:dim(similarity_list_mat)[2],']',sep='')
              rownames(similarity_list_mat) = paste('t[',1:dim(similarity_list_mat)[1],']',sep='')
              sim_melt = melt(similarity_list_mat)
              #sim_melt$mod_i = rep(1:length(r@K), r@K*10)

              x_tick_lab = parse(text = levels(as.factor(sim_melt$Var2)))
              y_tick_lab = parse(text = levels(as.factor(sim_melt$Var1)))

              myplot<-ggplot(sim_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
                geom_tile(aes(fill=value), color='white')+
                geom_text(aes(label=round(value, 2)), color='beige', size=2.5)+
                ggtitle("topic similarities")+
                #scale_fill_gradient2(low="white", high="black", midpoint = 0.1)+
                scale_fill_continuous(low="#56B1F7", high="#132B43", guide = guide_legend(title = "similarity"))+
                scale_x_continuous(breaks=1:length(x_tick_lab),labels = rev(x_tick_lab), expand = c(0,0))+
                scale_y_continuous(breaks = 1:length(y_tick_lab),
                                   labels = y_tick_lab,
                                   sec.axis = dup_axis(),
                                   expand = c(0,0))+
                xlab("Comparison Model Topics\n")+
                ylab("\nOriginal Model Topics\n")+
                #facet_grid(rev(mod_i)~., scales = "free", space = "free")+
                coord_flip()+
                theme(panel.background = element_rect("white"),
                      legend.position="bottom",
                      axis.text = element_text(size = 14))

              ggsave(paste0("sim", i, ".jpg"), plot=myplot,
                     path = paste0(dir,"/sim_gen"),
                     width = 12, height = 12)

            }

            # model topic matrix ( percentage of matrix dominated by given topic)
            r@model_topic_mat[[1]] = matrix(r@model_topic_mat[[1]])
            topic_doc_perc = do.call(cbind, r@model_topic_mat)
            #colnames(topic_doc_perc) = paste(0:(dim(topic_doc_perc)[2]-1))
            colnames(topic_doc_perc) = paste('m[',0:(dim(topic_doc_perc)[2]-1),']','*","*','n','*"="*',c(r@K[r@idx], k),sep='')
            #rownames(topic_doc_perc) = paste(1:dim(topic_doc_perc)[1])
            rownames(topic_doc_perc) = paste('t[',1:dim(topic_doc_perc)[1],']',sep='')
            topic_doc_melt = melt(topic_doc_perc)

            x_tick_lab = parse(text = levels(as.factor(topic_doc_melt$Var2)))
            y_tick_lab = parse(text = levels(as.factor(topic_doc_melt$Var1)))

            #dir.create("./Rep_images/Grimmer/topDoc/")
            myplot <- ggplot(topic_doc_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
              geom_tile(aes(fill=value), color='white')+
              geom_text(aes(label=round(value, 4)), color='beige',size=2)+
              ggtitle("Proportion of documents dominated by a topic from the original model")+
              #scale_fill_gradient2(low="white", high="black", midpoint = 0.1)+
              scale_fill_continuous(high = "#132B43", low = "#56B1F7", guide = guide_legend(title = "proportion"))+
              scale_x_continuous(breaks=1:length(x_tick_lab),labels = x_tick_lab, expand = c(0,0))+
              scale_y_continuous(breaks = 1:length(y_tick_lab),
                                 labels = y_tick_lab,
                                 sec.axis = dup_axis(),
                                 expand = c(0,0))+
              xlab("Comparison Models\n")+
              ylab("\nTopics In Original Model\n")+
              coord_flip()+
              theme(panel.background = element_rect("white"),
                    legend.position="bottom",
                    axis.text = element_text(size = 14))
            ggsave(paste0("mod_top.jpg"), plot=myplot,
                   path = paste0(dir, "/topDoc_gen"),
                   width = 15, height = 15)


            # topic dom perc list (Prop of docs that have a topic0 as max class that also have as max class in a new model a topicX that maps to topic0)
            topic_dom = do.call(cbind, r@topic_dom_perc_list)
            colnames(topic_dom) = paste('m[',1:dim(topic_dom)[2],'*","*','n','*"="*',k,']',sep='')
            rownames(topic_dom) = paste('t[',1:dim(topic_dom)[1],']',sep='')
            topic_dom_melt = melt(topic_dom)

            x_tick_lab = parse(text = levels(as.factor(topic_dom_melt$Var2)))
            y_tick_lab = parse(text = levels(as.factor(topic_dom_melt$Var1)))

            myplot <- ggplot(topic_dom_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
              geom_tile(aes(fill=value), color='white')+
              geom_text(aes(label=round(value, 2)), color='beige',size=3)+
              ggtitle("proportions of documents classified into the same topic")+
              #scale_fill_gradient2(low="lightblue", high="darkblue", midpoint = 0.1,guide = guide_legend(title = "proportion"))+
              scale_fill_continuous(high = "#132B43", low = "#56B1F7",guide = guide_legend(title = "proportion"))+
              scale_x_continuous(breaks=1:length(x_tick_lab),labels = x_tick_lab, expand = c(0,0))+
              scale_y_continuous(breaks = 1:length(y_tick_lab),
                                 labels = y_tick_lab,
                                 sec.axis = dup_axis(),
                                 expand = c(0,0))+
              #scale_y_discrete(labels = parse(text = levels(as.factor(topic_dom_melt$Var1))))+
              xlab("Comparison Models\n")+
              ylab("\nTopics In Original Model\n")+
              coord_flip()+
              theme(panel.background = element_rect("white"),
                    legend.position="bottom",
                    axis.text = element_text(size = 10))

            ggsave(paste0("top_mod_perc.jpg"), plot=myplot,
                   path = paste0(dir, "/topDoc_gen"),
                   width = 15, height = 15)
          })
