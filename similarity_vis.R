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
