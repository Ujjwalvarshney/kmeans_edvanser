getwd()
library(dplyr)
setwd("C:/Users/ujjwa/Downloads")
wq=read.csv("winequality-white.csv",sep = ";")
glimpse(wq)
normalized_data=scale(wq)
View(normalise_data)
#elbow curve & k ~ sqrt(n/2) to decide the k value( total no of clusters)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit=kmeans(normalized_data,5)# 5 cluster selection
fit
final2=data.frame(wq,fit$cluster)
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
write.csv(final3,"kmeans")
