#This example script performes clustering on the iris dataset using k-means, hierarchical, model-based and non negative matrix factorization (nmf). 
#The results are visualized using Principal Component Analysis (PCA).

library(mclust) # Package for mobel-based clustering
library(NMF) # Package for nmf
library(cluster)

#Preprocessing the iris dataset.
data(iris)
iris2=iris[,-5]
iris.scale=scale(iris2)
iris.scale.dist=dist(iris.scale)
iris.t=t(iris2)
iris.dist=dist(iris2)
k_range=seq(2,15)

#Performes k-means clustering. The optimal number of clusters is determined using the silhouette value. 
sil_kmeans=sapply(k_range,function (k) {mean(silhouette(kmeans(iris.scale,k,nstart=25)$cluster,iris.scale.dist)[,3])})
sil_kmeans=c(c(0),sil_kmeans)
kmeans_kmax=kmeans(iris.scale,which.max(sil_kmeans),nstart=25)
sil_kmeans_kmax=silhouette(kmeans_kmax$cluster,iris.scale.dist)

#Performes hierarchical clustering using 1-pearson correlation as distance measure. The optimal number of clusters is determined using the silhouette value.
dissim=as.dist(1-cor(t(iris.scale)))
hclust=hclust(dissim,method="ward.D2")
sil_hclust=sapply(seq(2,15),function (x){mean(silhouette(cutree(hclust,k=x),dissim)[,3])})
sil_hclust=c(c(0),sil_hclust)
hclust_kmax=cutree(hclust,k=which.max(sil_hclust))
sil_hclust_kmax=silhouette(hclust_kmax,dissim)

#Performes model-based clustering. The optimal number of clusters is determined using the bayesian information criterion
MBclust=Mclust(iris.scale)

#Performes non negative matrix factorization. The optimal number of clusters is determined using the silhouette value.
k.max=15
sil_nmf = rep(0,k.max)
#Iterates through each k
for (k in 2:k.max)
{
	#Performes nmf.
	nmf_res<-nmf(iris.t, k , nrun=20)
	#Calculates the silhouette value.
	ss=silhouette(apply(coef(nmf_res), 2, which.max),iris.dist)
	sil_nmf[k] = mean(ss[,3])

}
#Performes nmf and uses the k with the higest silhouette value.
nmf_kmax=nmf(iris.t, which.max(sil_nmf) , nrun=20)
#Extracts the cluster numbers.
nmf_kmax_cluster=apply(coef(nmf_kmax), 2, which.max)
sil_nmf_kmax=silhouette(nmf_kmax_cluster,iris.dist)

#Performes PCA on the iris dataset
pca=prcomp(iris.scale)

#Combines the original iris data, the clustering results and the 1st and 2nd Principal Components.
iris_cluster_pca=cbind(ID=c(1:nrow(iris.scale)),iris2,no_clust=rep(1,nrow(iris.scale)),kmeans=kmeans_kmax$cluster,hclust=hclust_kmax,MBclust=MBclust$classification,nmf=nmf_kmax_cluster,PC1=pca$x[,1],PC2=pca$x[,3])

#Plots the clustering results using the 1st and 2nd Principal Components.
pdf("clustering_vis.pdf")
par(mfrow=c(4,3))
plot(sil_kmeans,type='b')
plot(sil_kmeans_kmax)
plot(iris_cluster_pca$PC1,iris_cluster_pca$PC2,col=iris_cluster_pca$kmeans)

plot(sil_hclust,type='b')
plot(sil_hclust_kmax)
plot(iris_cluster_pca$PC1,iris_cluster_pca$PC2,col=iris_cluster_pca$hclust)

plot(MBclust,'BIC')
plot(sil_MBclust)
plot(iris_cluster_pca$PC1,iris_cluster_pca$PC2,col=iris_cluster_pca$MBclust)

plot(sil_nmf,type='b')
plot(sil_nmf_kmax)
plot(iris_cluster_pca$PC1,iris_cluster_pca$PC2,col=iris_cluster_pca$nmf)
#dev.copy2pdf(file="clustering_vis.pdf")

dev.off()
