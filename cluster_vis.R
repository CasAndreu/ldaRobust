# visualizing cluster
beta_mat = rbind(r@lda_u@beta, do.call(rbind, r@beta_list))
dissE <- cluster::daisy(beta_mat)
topic_clusters = r@topic_cluster_assignment
dissimilarity_dist = dissE^2
scl <- cluster::silhouette(topic_clusters, dissimilarity_dist)
plot(scl)
