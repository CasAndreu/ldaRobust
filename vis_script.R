library(ggplot2)
library(reshape2)

# similarity matrices visualization
similarity_list_mat = do.call(cbind, r@similarity_mat_list)
#colnames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[2])
#rownames(similarity_list_mat) = paste(1:dim(similarity_list_mat)[1])
colnames(similarity_list_mat) = paste('T[',1:dim(similarity_list_mat)[2],']',sep='')
rownames(similarity_list_mat) = paste('T[',1:dim(similarity_list_mat)[1],']',sep='')
sim_melt = melt(similarity_list_mat)

x_tick_lab = parse(text = levels(as.factor(sim_melt$Var2)))
y_tick_lab = parse(text = levels(as.factor(sim_melt$Var1)))

ggplot(sim_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
  geom_tile(aes(fill=value), color='white')+
  geom_text(aes(label=round(value, 2)), color='beige', size=2.5)+
  ggtitle("topic similarities")+
  #scale_fill_gradient2(low="white", high="black", midpoint = 0.1)+
  scale_fill_continuous(guide = guide_legend(title = "proportion"))+
  scale_x_continuous(breaks=1:length(x_tick_lab),labels = x_tick_lab)+
  scale_y_continuous(breaks = 1:length(y_tick_lab),
                     labels = y_tick_lab,
                     sec.axis = dup_axis())+
  xlab("new model topics")+
  ylab("original model topics")+
  coord_flip()+
  theme(panel.background = element_rect("white"),
        legend.position="bottom",
        axis.text = element_text(size = 14))

# model topic matrix ( percentage of matrix dominated by given topic)
r@model_topic_mat[[1]] = matrix(r@model_topic_mat[[1]])
topic_doc_perc = do.call(cbind, r@model_topic_mat)
#colnames(topic_doc_perc) = paste(0:(dim(topic_doc_perc)[2]-1))
colnames(topic_doc_perc) = paste('M[',0:(dim(topic_doc_perc)[2]-1),']',sep='')
#rownames(topic_doc_perc) = paste(1:dim(topic_doc_perc)[1])
rownames(topic_doc_perc) = paste('T[',1:dim(topic_doc_perc)[1],']',sep='')
topic_doc_melt = melt(topic_doc_perc)

x_tick_lab = parse(text = levels(as.factor(topic_doc_melt$Var2)))
y_tick_lab = parse(text = levels(as.factor(topic_doc_melt$Var1)))

ggplot(topic_doc_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
  geom_tile(aes(fill=value), color='white')+
  geom_text(aes(label=round(value, 2)), color='beige',size=3)+
  ggtitle("proportion of documents dominated by a topic from the original model")+
  #scale_fill_gradient2(low="white", high="black", midpoint = 0.1)+
  scale_fill_continuous(guide = guide_legend(title = "proportion"))+
  scale_x_continuous(breaks=1:length(x_tick_lab),labels = x_tick_lab)+
  scale_y_continuous(breaks = 1:length(y_tick_lab),
                     labels = y_tick_lab,
                     sec.axis = dup_axis())+
  xlab("MODEL NUMBER")+
  ylab("TOPICS IN ORIGINAL MODEL")+
  coord_flip()+
  theme(panel.background = element_rect("white"),
        legend.position="bottom",
        axis.text = element_text(size = 14))


# topic dom perc list (Prop of docs that have a topic0 as max class that also have as max class in a new model a topicX that maps to topic0)
topic_dom = do.call(cbind, r@topic_dom_perc_list)
colnames(topic_dom) = paste('M[',1:dim(topic_dom)[2],']',sep='')
rownames(topic_dom) = paste('T[',1:dim(topic_dom)[1],']',sep='')
topic_dom_melt = melt(topic_dom)

x_tick_lab = parse(text = levels(as.factor(topic_dom_melt$Var2)))
y_tick_lab = parse(text = levels(as.factor(topic_dom_melt$Var1)))

ggplot(topic_dom_melt, aes(x=as.numeric(as.factor(Var2)), y=as.numeric(as.factor(Var1))))+
  geom_tile(aes(fill=value), color='white')+
  geom_text(aes(label=round(value, 2)), color='beige',size=3)+
  ggtitle("proportions of documents classified into the same topic")+
  #scale_fill_gradient2(low="lightblue", high="darkblue", midpoint = 0.1,guide = guide_legend(title = "proportion"))+
  scale_fill_continuous(guide = guide_legend(title = "proportion"))+
  scale_x_continuous(breaks=1:length(x_tick_lab),labels = x_tick_lab)+
  scale_y_continuous(breaks = 1:length(y_tick_lab),
                     labels = y_tick_lab,
                     sec.axis = dup_axis())+
  #scale_y_discrete(labels = parse(text = levels(as.factor(topic_dom_melt$Var1))))+
  xlab("MODEL NUMBER")+
  ylab("TOPICS IN ORIGINAL MODEL")+
  coord_flip()+
  theme(panel.background = element_rect("white"),
        legend.position="bottom",
        axis.text = element_text(size = 10))


# visualizing cluster
beta_mat = rbind(r@lda_u@beta, do.call(rbind, r@beta_list))
dissE <- cluster::daisy(beta_mat)
topic_clusters = r@topic_cluster_assignment
dissimilarity_dist = dissE^2
scl <- cluster::silhouette(topic_clusters, dissimilarity_dist)
plot(scl)








