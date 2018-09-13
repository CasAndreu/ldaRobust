#' plot_sim
#'
#' generate visualizations for rlda object
#'
#' @include rlda_c.R
#' @param r a rlda object
#' @param dir directory to save generated plots (no "/" at the end)
#' @exportMethod plot_sim
#'

setGeneric("plot_sim", function(r, dir)standardGeneric("plot_sim"))
setMethod("plot_sim",
          signature(r = "rlda", dir = "character"),
          function (r, dir) {
            k = r@K
            dir.create(paste0(dir,"/sim"))

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
                     path = paste0(dir,"/sim"),
                     width = 12, height = 12)

            }
          })
