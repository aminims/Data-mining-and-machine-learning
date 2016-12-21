library(mclust)
library(NMF)

data(iris)
iris2=iris[,-5]
iris.scale=scale(iris2)
iris.scale.dist=dist(iris.scale)
iris.t=t(iris2)
iris.dist=dist(iris2)
k_range=seq(2,15)

sil_kmeans=sapply(k_range,function (k) {mean(silhouette(kmeans(iris.scale,k,nstart=25)$cluster,iris.scale.dist)[,3])})
sil_kmeans=c(c(0),sil_kmeans)
kmeans_kmax=kmeans(iris.scale,which.max(sil_kmeans),nstart=25)
sil_kmeans_kmax=silhouette(kmeans_kmax$cluster,iris.scale.dist)

dissim=as.dist(1-cor(t(iris.scale)))
hclust=hclust(dissim,method="ward.D2")
sil_hclust=sapply(seq(2,15),function (x){mean(silhouette(cutree(hclust,k=x),dissim)[,3])})
sil_hclust=c(c(0),sil_hclust)
hclust_kmax=cutree(hclust,k=which.max(sil_hclust))
sil_hclust_kmax=silhouette(hclust_kmax,dissim)

MBclust=Mclust(iris.scale)
sil_MBclust=silhouette(MBclust$classification,iris.scale.dist)

k.max=15
sil_nmf = rep(0,k.max)
for (k in 2:k.max)
{
	nmf_res<-nmf(iris.t, k , nrun=20)
	ss=silhouette(apply(coef(nmf_res), 2, which.max),iris.dist)
	sil_nmf[k] = mean(ss[,3])

}
plot(sil_nmf,type='b')
nmf_kmax=nmf(iris.t, which.max(sil_nmf) , nrun=20)
nmf_kmax_cluster=apply(coef(nmf_kmax), 2, which.max)
sil_nmf_kmax=silhouette(nmf_kmax_cluster,iris.dist)

pca=prcomp(iris.scale)

iris_cluster_pca=cbind(ID=c(1:nrow(iris.scale)),iris2,no_clust=rep(1,nrow(iris.scale)),kmeans=kmeans_kmax$cluster,hclust=hclust_kmax,MBclust=MBclust$classification,nmf=nmf_kmax_cluster,PC1=pca$x[,1],PC2=pca$x[,3])

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

write.table(iris_cluster_pca,file="clusterData.txt",sep="\t",quote=F,row.names=F)
write.table(iris_cluster_pca[,4],file="testData.txt",sep="\t",quote=F,row.names=F,col.names=F)
write.table(iris_cluster_pca[,1:5],file="newData.txt",sep="\t",quote=F,row.names=F)
