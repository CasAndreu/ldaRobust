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
