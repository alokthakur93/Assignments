crime <- read.csv(file.choose())

normalized_data <- scale(crime[,2:5])
d <- dist(normalized_data, method = "euclidean") # distance matrix
d

fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)
rect.hclust(fit, k=4, border="red")
groups <- cutree(fit, k=4)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)

aggregate(crime[,-1],by=list(final$membership),mean)
#Cluster 1 has the maximum number of Murder out of all clusters which to me looks like it is more vulnerable
#with Assault on second top in the list.
#Custer 2 has the maximum number of Rape,Assault and urban Pop and stands second in Murder.
#Cluster 3 is a little vulnerable in terms of the Murder(Less). However it still stands 2nd in
#UrbanPop Crime
#Cluster 4 has got the less vulnerable to all the Crime categories.

#---------------K-means clustering--------------#

crime_k <- crime
str(crime_k)
normalized_datak<-scale(crime_k[,2:5])

wss = (nrow(normalized_datak)-1)*sum(apply(normalized_datak, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:5) wss[i] = sum(kmeans(normalized_datak, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_datak, 4) # 4 cluster solution
final2<- data.frame(crime_k, fit$cluster) # append cluster membership
final2

aggregate(crime_k[,2:5], by=list(fit$cluster),mean)

#The Cluster with maximum # of murders seems to be a major threat to Live.
#The 1st cluster with a maximum on Assault, Rape and other factors takes the 2nd rank
#on the most dangerous state to live.
#The Next two clusters would take those rankings based on the # of Crime rate categories
#on rape, murder, assault and urbanpop metrics.