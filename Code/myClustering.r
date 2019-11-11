#45155291 Ankit Sharma
#Appling K-Means and Hierarchical clustering to cluster the data

#--Task 2.1--#
#Load the preprocessed data into a data frame 
processed_data <- readRDS(file="./data/bcw_processed.Rda")
#Loading preprocessed data without Class column
processed_data_without_class <- subset(processed_data, select = -Class)

#--Task 2.2--#
#Cluster the data into 2 clusters using K-Means clustering, using the default parameters for the kmeans function.
#Set seed
set.seed(5291)

#initialising an empty vectors to store within sum of squares and between sum of squares
wss <- vector()
bss <- vector()

#Calculating and storing total within sum of squares and between sum of squares of k = 1 clustering for later use
kmeans.result <- kmeans(processed_data_without_class,centers = 1)
wss <- c(wss,kmeans.result$tot.withinss)
bss <- c(bss,kmeans.result$betweenss)

#Cluster with K-means into 2 clusters
nclust = 2
(kmeans.result <- kmeans(processed_data_without_class,nclust))

#To visuallise the two clusters we can add a column colour based on the cluster assigned
#Cluster 1 is assigned red colour, Cluster 2 is assigned black colour
kmeans_result <- kmeans.result$cluster
kmeans_result<-as.data.frame(kmeans_result)
kmeans_result$color[kmeans_result$kmeans_result=="1"] <- "black"
kmeans_result$color[kmeans_result$kmeans_result=="2"] <- "red"

#Storing total within sum of squares and between sum of squares of k = 2 clustering
wss <- c(wss,kmeans.result$tot.withinss)
bss <- c(bss,kmeans.result$betweenss)

#Plotting the results of the clusters as a 2D plot where the x-axis is Clump Thickness and the y-axis is Uniformity of Cell Size.
#The plot is saved in the plot folder using png command
png(filename=paste("./Plot/Task2.2 Cluster Plot",".png"), width = 480, height = 480)
plot(processed_data_without_class[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans_result$color, ylim=c(0,15))
title(paste("k = ", nclust, sep=""))
points(kmeans.result$centers, col = 1:nclust, pch = 8, cex = 3)
legend(1,15,pch =c(1,1), legend=c("Cluster1", "Cluster2"), col=c("red", "black"), cex=1)
dev.off()
      
#--Task 2.3--#
#Plotting another 2D plot with the same dimensions above, but coloring the points according to the Class column.
png(filename=paste("./Plot/Task2.3 Distribution by Class",".png"), width = 480, height = 480)
plot(processed_data[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = processed_data$Class, ylim=c(0,15))
title(paste("Distribution by Class"))
points(processed_data$Class[c("Clump.Thickness","Uniformity.of.Cell.Size")],col = 1:2, pch =2, cex = 2)
legend(1,15,pch =c(1,1), legend=c("Benign", "Malignant"), col=c("black", "red"), cex=1)
dev.off()

#--Task 2.4--#
#Compare the 2 plots obtained in the previous two tasks – do the clusters visually represent the benign vs malignant classes?.
png(filename=paste("./Plot/Task2.4 Comparison of clustering with given class",".png"), width = 960, height = 480)
par(mfrow=c(1,2))
plot(processed_data_without_class[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans_result$color, ylim=c(0,15))
title(paste("k = ", nclust, sep=""))
points(kmeans.result$centers, col = 1:nclust, pch = 8, cex = 3)
legend(1,15,pch =c(1,1), legend=c("Cluster1", "Cluster2"), col=c("red", "black"), cex=1)
plot(processed_data[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = processed_data$Class, ylim=c(0,15))
title(paste("Distribution by Class"))
points(processed_data$Class[c("Clump.Thickness","Uniformity.of.Cell.Size")],col = 1:2, pch =2, cex = 2)
legend(1,15,pch =c(1,1), legend=c("Benign", "Malignant"), col=c("black", "red"), cex=1)
dev.off()
#We observe that there are differences in classes obtained by clustering from the given classes(benign and malignant).

#--Task 2.5--#
#Clustering the data into 3 clusters using K-Means clustering and plotting the clustering results.
set.seed(5291)
nclust = 3
png(filename=paste("./Plot/Task2.5 k=3 clustering",".png"), width = 480, height = 480)
(kmeans.result <- kmeans(processed_data_without_class,nclust))
plot(processed_data_without_class[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep=""))
points(kmeans.result$centers, col = 1:nclust, pch = 8, cex = 3)
dev.off()

#Storing total within sum of squares and between sum of squares of k = 3 clustering
wss <- c(wss,kmeans.result$tot.withinss)
bss <- c(bss,kmeans.result$betweenss)


#Clustering the data into 4 clusters using K-Means clustering and ploting the clustering results.
nclust = 4
png(filename=paste("./Plot/Task2.5 k=4 clustering",".png"), width = 480, height = 480)
(kmeans.result <- kmeans(processed_data_without_class,nclust))
plot(processed_data_without_class[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep=""))
points(kmeans.result$centers, col = 1:nclust, pch = 8, cex = 3)
dev.off()

#Storing total within sum of squares and between sum of squares of k = 4 clustering
wss <- c(wss,kmeans.result$tot.withinss)
bss <- c(bss,kmeans.result$betweenss)

#Clustering the data into 5 clusters using K-Means clustering and ploting the clustering results.
nclust = 5
png(filename=paste("./Plot/Task2.5 k=5 clustering",".png"), width = 480, height = 480)
(kmeans.result <- kmeans(processed_data_without_class,nclust))
plot(processed_data_without_class[,c("Clump.Thickness","Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = ", nclust, sep=""))
points(kmeans.result$centers, col = 1:nclust, pch = 8, cex = 3)
dev.off()

#Storing total within sum of squares and between sum of squares of k = 5 clustering
wss <- c(wss,kmeans.result$tot.withinss)
bss <- c(bss,kmeans.result$betweenss)

#--Task 2.6--#
#Compare the plots and SSEs obtained in the previous task, and provide your comments on the quality of clustering.

#k = 2
#2 clusters of sizes 453, 230
#Within cluster sum of squares by cluster: 4384.565 14938.609
#(between_SS / total_SS =  60.1 %)

#k = 3
#3 clusters of sizes 260, 203, 220
#Within cluster sum of squares by cluster: 1092.923  2986.404 14040.868
#(between_SS / total_SS =  62.6 %)

#k = 4
#4 clusters of sizes 93, 111, 50, 429
#Within cluster sum of squares by cluster: 3597.161 6294.649 2307.980 2753.301
#(between_SS / total_SS =  69.1 %)

#k = 5
#5 clusters of sizes 261, 97, 188, 59, 78
#Within cluster sum of squares by cluster: 1124.628 3812.577 1873.266 3188.576 3705.628
#(between_SS / total_SS =  71.7 %)

#We observe that the average distance to centroid falls rapidly until right k, then changes little
#With k = 2, the clusters are too few with long distances to centroid
#With k = 3, the clusters are just right with distances rather short
#With k = 4,5 , the clusters are too many with little improvement in average distance 

#Plotting WSS across clusters
sse = data.frame(c(1:5), c(wss))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'

png(filename=paste("./Plot/Task2.6 SSE across clusters",".png"), width = 480, height = 480)
plot(sse,type="b",col="red", pch =1)
title("WSS across clusters")
lines(sse)
dev.off()

#Plotting BSS across clusters
bss = data.frame(c(1:5), c(bss))
names(bss)[1] = 'Clusters'
names(bss)[2] = 'BSS'

png(filename=paste("./Plot/Task2.6 BSS across clusters",".png"), width = 480, height = 480)
plot(bss,type="b",col="red", pch =1)
title("BSS across clusters")
lines(bss)
dev.off()

#--Task 2.7--#
#Applying colour based on class for better visualisation
processed_data_class <- processed_data$Class

#Converting to dataframe format before assigning values
processed_data_class <- as.data.frame(processed_data_class)

processed_data_class$color[processed_data_class$processed_data_class=="2"] <- "green"
processed_data_class$color[processed_data_class$processed_data_class=="4"] <- "red"
processed_data_class$label[processed_data_class$processed_data_class=="2"] <- "Benign"
processed_data_class$label[processed_data_class$processed_data_class=="4"] <- "Malignant"

#Install package to have coloured branches 
#install.packages('dendextend'); 
library('dendextend')

#Applying hierarchical clustering to the data using the hclust function with default parameters and plotting the corresponding dendrogram. 
hc <- hclust(dist(processed_data))
hc$labels <- as.character(processed_data_class$label)

#Plotting the default dendogram
hcd <- as.dendrogram(hc)
png(filename="./Plot/Task2.7 Dendogram initial.png", width = 1080, height = 720)
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Dendogram"))
dev.off()

#Clustering the dendrogram into 2 clusters
hcd <- as.dendrogram(hc)
png(filename="./Plot/Task2.7 Dendogram nclust = 2.png", width = 1080, height = 720)
hcd = color_branches(hc,2,groupLabels = FALSE)
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Dendogram with 2 clusters"))
rect.hclust(hc,k=2,border = "black")
dev.off()

#Clustering the dendrogram into 3 clusters
hcd <- as.dendrogram(hc)
png(filename="./Plot/Task2.7 Dendogram nclust = 3.png", width = 1080, height = 720)
hcd = color_branches(hc,3,groupLabels = FALSE)
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Dendogram with 3 clusters"))
rect.hclust(hc,k=3,border = "black")
dev.off()

#Clustering the dendrogram into 4 clusters
hcd <- as.dendrogram(hc)
png(filename="./Plot/Task2.7 Dendogram nclust = 4.png", width = 1080, height = 720)
hcd = color_branches(hc,4,groupLabels = FALSE)
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Dendogram with 4 clusters"))
rect.hclust(hc,k=4,border = "black")
dev.off()

#Clustering the dendrogram into 5 clusters
hcd <- as.dendrogram(hc)
png(filename="./Plot/Task2.7 Dendogram nclust = 5.png", width = 1080, height = 720)
hcd = color_branches(hc,5,groupLabels = FALSE)
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Dendogram with 5 clusters"))
rect.hclust(hc,k=5,border = "black")
dev.off()

#--Task 2.8--#
#Compare the plots obtained in the previous task and provide your observations on the achieved clusters - should we have a new subtype of diseases?.
#Yes, on comparison of dendogram created, we should have 3 subtypes of diseases
#At height of 3, we see some entry points malignant class clustered as a different category
#Splitting the dataset into just 2 clusters is overgeneralisation of attributes
#Keeping nclust = 3, we see that there is scope of having two subtypes to the malignant class   

#--Task 2.9--#
#Try different agglomeration methods in hierarchical clustering (i.e.,“single”, “complete”, and “average”).
set.seed(5921)

png(filename="./Plot/Task2.9 Dendogram single.png", width = 1080, height = 720)
hc <- hclust(dist(processed_data), method = "single")
hc$labels <- as.character(processed_data_class$label)
hcd <- as.dendrogram(hc)
hcd = color_branches(hc,2,groupLabels = TRUE,col=c("blue","orange"))
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Clustering using single method"))
dev.off()

png(filename="./Plot/Task2.9 Dendogram complete.png", width = 1080, height = 720)
hc <- hclust(dist(processed_data), method = "complete")
hc$labels <- as.character(processed_data_class$label)
hcd <- as.dendrogram(hc)
hcd = color_branches(hc,2,groupLabels = TRUE,col=c("blue","orange"))
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Clustering using complete method"))
dev.off()

png(filename="./Plot/Task2.9 Dendogram average.png", width = 1080, height = 720)
hc <- hclust(dist(processed_data), method = "average")
hc$labels <- as.character(processed_data_class$label)
hcd <- as.dendrogram(hc)
hcd = color_branches(hc,2,groupLabels = TRUE,col=c("blue","orange"))
labels_colors(hcd) <- processed_data_class[order.dendrogram(hcd),]$color
plot(hcd)
title(paste("Clustering using average method"))
dev.off()

#Quality of clustering - is the data sensitive to the used agglomeration method?
#Yes the data is sensitive to agglomeration method used as can be seen from different dengograms created on using different techniques
#All three techniques are based on grouping clusters in bottom-up fashion (agglomerative clustering)

#In single-linkage clustering similarity of two clusters is based on the two most similar (closest) points in the different clusters
#Drawback of this method is its sensitive to noise. Also, there is a chaining phenomenon where clusters may be forced together due to single elements being close to each other, even though many of the elements in each cluster may be very distant to each other

#For Complete linkage clustering, Similarity of two clusters is based on the two least similar (most distant) points in the different clusters
#Limitation of this method is that it tends to break large clusters

#In average linkage hierarchical clustering, the average pairwise distance between points in the two clusters is used. It is a compromise between the single-linkage and complete linkage clustering methods 

#Based on your results, what do you think is the default agglomeration method used in Task 2.7?
#The default agglomeration method used is "complete" 