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

