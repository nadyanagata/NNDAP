#Clustering Model
library(readr)
library(dplyr)
library(cluster)
library(factoextra)

#Import dataset (post-CB, 2020 dataset)
merli_new = read_csv("merli_new_cleaneddata.csv")
merli_norm = sapply(merli_new, scale) #Normalization of data
rownames(merli_norm) = rownames(merli_new)

#Elbow to identify the best no. of clusters for k-means clustering
set.seed(1)
k.max = 15
wss = (nrow(merli_norm)-1)*sum(apply(merli_norm, 2, var))
for (i in 2:k.max) wss[i] = sum(kmeans(merli_norm,
                                       centers = i,
                                       iter.max = 15,
                                       algorithm = "Hartigan-Wong")$withinss)
# Plot Elbow
plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters",
     pch=19, cex=1)

# Select optimal clusters no. at 5.
abline(v = 5, lty = 2)

# Assign the "optimal" number to a variable
Ci<-5

# K-Means Clustering
km = kmeans(merli_norm, Ci)

km$size #Shows that cluster 5 has the least amount of data, but still substantial enough.
km$centers

# Assign Cluster Numbers to dataset
merli.clustn <- merli_new %>% # Binded with the original dataset
  bind_cols(data.frame(km$cluster))

# Cluster Plot
fviz_cluster(km, data=merli_norm,
             ellipse.type="convex",
             outlier.color = "black",
             outlier.shape = 23)

# Outlier detection
km.centers = km$centers[km$cluster, ]
km.dist = sqrt(rowSums((merli_norm - km.centers)^2))

# Calculating mean distance by cluster
mdist.km = tapply(km.dist, km$cluster, mean)

# Divide each distance by the mean for its cluter
distscore.km = km.dist/(mdist.km[km$cluster])
distfact.km = data.frame(distscore.km)
colnames(distfact.km) = "DIST.FACTOR"

# Min-Max Normalisation
minmaxscore.km <- data.frame((distscore.km - min(distscore.km))/(max(distscore.km)
                                                                 -min(distscore.km)))
colnames(minmaxscore.km) <- "SCORE.MINMAX"

# dataframe of dataset with cluster#,distance score,distance score percentile
merli.clustdn <- merli.clustn %>%
  bind_cols(distfact.km, minmaxscore.km)

# Predetermine Threshold MINMAX SCORE
# Choosing those above 70%
P <- 0.7

# Plot Outliers
plot(x = merli.clustdn$SCORE.MINMAX,
     y = merli.clustdn$DIST.FACTOR,
     main = "Outliers based on Distance Score %",
     xlab = "MinMax Score",
     ylab = "Distance Score",
     col = ifelse(minmaxscore.km > P,"tomato","black"),
     pch = 13)

# Determine number of Outliers based on Distance Score % Criterion
x <- length(which(merli.clustdn$SCORE.MINMAX > P) )

# Outliers (Above distance score Percentile of 70%)
merli.perc.order <- order(distscore.km, decreasing = TRUE)[1:x]
# Coerce outliers into dataframe
merli.perc.outl <- data.frame(merli.clustdn[merli.perc.order,])
View(merli.perc.outl)
#write_csv(merli.perc.outl, "percoutliers.csv")

cluster5 = filter(merli.clustdn, km.cluster == "5")
mean(cluster5$amt_loan) #229863.6
mean(cluster5$veh_cost) #347073.7
# Outlier in cluster 5 likely because amt_loan and veh_cost higher than normal.

cluster1 = filter(merli.clustdn, km.cluster == "1")
mean(cluster1$out_loanamt) #872831.6
mean(cluster1$app_loanamt) #1097661
mean(cluster1$dis_loanamt) #1108690
# Outlier in cluster 1 likely because values for these three variables higher than the mean values.
