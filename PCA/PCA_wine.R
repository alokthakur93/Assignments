my_data <- read.csv(file.choose())
View(my_data)

wine_data <- my_data[,-1]#consider only numeric values for PCA
attach(wine_data)
cor(wine_data)

pca_obj <- princomp(wine_data,cor=TRUE,scores = TRUE,covmat = NULL)
str(pca_obj)
loadings(pca_obj)
plot(pca_obj)# graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)
biplot(pca_obj)

plot(cumsum(pca_obj$sdev*pca_obj$sdev)*100/(sum(pca_obj$sdev*pca_obj$sdev)),type="b")

pca_obj$scores[,1:3]

my_data<-cbind(my_data,pca_obj$scores[,1:3])
View(my_data)

#preparing data for clustering(considering only top 3 PCA scores as they represent the entire data)

clustdata <- my_data[,15:17]
View(clustdata)

#Normalizing the data
#####Hierarchial Clustering#######
normclus <- scale(clustdata)
d<-dist(normclus,method = "euclidean")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang = -1)
rect.hclust(fit, k=5, border="red")
group<-cutree(fit,5)

membership.1<-as.matrix(group)
View(membership.1)


final.1<-cbind(membership.1,clustdata) # binding column wise with orginal data
View(final.1)
View(aggregate(final.1[1:4],by=list(membership.1),FUN=mean))
#####K-Means Clustering#########
View(normclus)
norm_k <- normclus

wss = (nrow(norm_k)-1)*sum(apply(norm_k, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:6) wss[i] = sum(kmeans(norm_k, centers=i)$withinss)
plot(1:6, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit.k <- kmeans(norm_k, 5) # 5 cluster solution
final_1<- data.frame(norm_k, fit.k$cluster) # append cluster membership
final_1

aggregate(norm_k, by=list(fit.k$cluster),mean)


#####By performing clustering on PCA values we get 5 cluster by bot H-clust method
#and k means method
#Now performing clustering on original data to check 
#H-clustering method
normal_Ori <- scale(wine_data)
View(normal_Ori)
d.ori <- dist(normal_Ori, method = "euclidean") # distance matrix
d.ori

fit.o <- hclust(d.ori, method="complete")
plot(fit.o) # display dendrogram
plot(fit.o, hang=-1)
rect.hclust(fit.o, k=5, border="red")
groups.o <- cutree(fit.o, k=5)

membership.o<-as.matrix(groups.o) # groups or cluster numbers
final.o <- data.frame(wine_data, membership.o)

View(final.o)

aggregate(wine_data,by=list(final.o$membership.o),mean)


###K-means clustering####

View(normal_Ori)
norm_k_o <- normal_Ori

wss = (nrow(norm_k_o)-1)*sum(apply(norm_k_o, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:6) wss[i] = sum(kmeans(norm_k_o, centers=i)$withinss)
plot(1:6, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit.k.o <- kmeans(norm_k_o, 5) # 5 cluster solution
final_o<- data.frame(norm_k_o, fit.k.o$cluster) # append cluster membership
final_o

aggregate(wine_data, by=list(fit.k.o$cluster),mean)



##We get same no. of clusters




