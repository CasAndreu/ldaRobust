#' plot_topDoc
#'
#' generate visualizations for rlda object
#'
#' @include rlda_c.R
#' @param r a rlda object
#' @param dir directory to save generated plots (no "/" at the end)
#' @exportMethod plot_topDoc
#'

setGeneric("plot_topDoc", function(r, dir)standardGeneric("plot_topDoc"))
setMethod("plot_topDoc",
          signature(r = "rlda", dir = "character"),
          function (r, dir) {
            k = r@K
            dir.create(paste0(dir, "/topDoc"))

            # model topic matrix ( percentage of matrix dominated by given topic)
            r@model_topic_mat[[1]] = matrix(r@model_topic_mat[[1]])
            topic_doc_perc = do.call(cbind, r@model_topic_mat)
            #colnames(topic_doc_perc) = paste(0:(dim(topic_doc_perc)[2]-1))
            colnames(topic_doc_perc) = paste('m[',0:(dim(topic_doc_perc)[2]-1),'*","*','n','*"="*',c(r@lda_u@k, k),']',sep='')
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
                   path = paste0(dir, "/topDoc"),
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
              geom_text(aes(label=round(value, 4)), color='beige',size=2)+
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
                   path = paste0(dir, "/topDoc"),
                   width = 15, height = 15)
          })
