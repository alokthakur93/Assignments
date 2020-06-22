####-------------H-CLustering--------------###
library(readxl)
ewair <- read_excel("C://Users//LENOVO//Desktop//ExcelR//Material//EastWestAirlines (1).xlsx",sheet = 2)
sum(is.na(ewair)) #no NA values
normalized_data <- scale(ewair[,2:12])
d <- dist(normalized_data,method = "euclidean")

fit <- hclust(d, method="ward.D2")
plot(fit) # display dendrogram
plot(fit, hang=-1)
rect.hclust(fit, k=3, border="red")
groups <- cutree(fit, k=3)
table(groups)
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(ewair, membership)

View(final)

aggregate(ewair[,-1],by=list(final$membership),mean)

#By performing Hclustering, It can be seen that Cluster 3 has the premium membership
#having more balance miles, Maximum number of Non-flight Trans in the last , Maximum number of
#flight miles in the last 12 months, Maximum number of flight transactions in the last 12 months and also
#awarded.
#Cluster 2 - This customer forms the second best priority customer with 2nd highest balance hours, First top
#qualifying miles,20000+ miles earned using frequent flier program and received Awards.
#The Least priority of customers were from Cluster 1.




###------------------Kmeans CLustering------------###

ewair_k <- ewair
str(ewair_k)
normalized_datak<-scale(ewair_k[,2:12])

wss = (nrow(normalized_datak)-1)*sum(apply(normalized_datak, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_datak, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_datak, 3) # 3 cluster solution
final2<- data.frame(ewair_k, fit$cluster) 
final2

aggregate(ewair_k[,2:12], by=list(fit$cluster),mean)
table(fit$cluster)

#Here cluster 1 is more premium followed by cluster 2 and cluster 3


