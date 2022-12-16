library(tidyverse)
library(cluster)
library(NbClust)

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')

d <- read.csv('SOEP_depriv_17.csv', sep = ';')
d <- d %>% 
  dplyr::select ('heat', 'meat', 'clothes', 'friends', 'furniture', 'holiday')
d <- d %>% drop_na

d$heat <- as.factor(d$heat)
d$meat <- as.factor(d$meat)
d$clothes <- as.factor(d$clothes)
d$friends <- as.factor(d$friends)
d$furniture <- as.factor(d$furniture)
d$holiday <- as.factor(d$holiday)

gower.diss <- daisy(d, metric = 'gower')

res.single <- hclust(d = gower.diss, method = 'single')
res.complete <- hclust(d = gower.diss, method = 'complete')
res.average <- hclust(d = gower.diss, method = 'average')
res.median <- hclust(d = gower.diss, method = 'median')

plot(res.single, labels = FALSE, main = 'Single')
plot(res.complete, labels = FALSE, main = 'Complete')
plot(res.average, labels = FALSE, main = 'Average')
plot(res.median, labels = FALSE, main = 'Median')

dendr.single <- as.dendrogram(res.single)
dendr.complete <- as.dendrogram(res.complete)
dendr.average <- as.dendrogram(res.average)
dendr.median <- as.dendrogram(res.median)

plot(cut(dendr.average, h = 0.3)$upper,
     main = 'Dendrogram with clusters k = 2,..., 8')
for (i in 2:8){
  rect.hclust(res.average, k = i, border = i)
}

dendr.hclust <- as.dendrogram(res.ward)
plot(cut(dendr.hclust, h = 5)$upper, 
     main = 'Upper Dendrogram of Cut at Height h = 5')

plot(cut(dendr.hclust, h = 5)$lower[[2]], 
     main = '2nd Branch of Lower Dendrogram with cut at h = 5')

res.nbclust <- NbClust(data = d,
                       diss = gower.diss,
                       distance = NULL,
                       method = 'complete', 
                       min.nc = 2, max.nc = 15)

par(mfrow = c(1, 1))
k <- 50
mean.cluster <- t(sapply(X = 1:k,
                           FUN = function(nc)
                           apply(d[cutree(res.average, k = k) == nc, ], 2, mean)))

plot(mean.cluster[1, ], type = 'o', pch = 19, lwd = 2,
     xlab = 'Item', ylab = 'Cluster Means',
     ylim = c(0,1), axes = FALSE, col = 1,
     main = 'Cluster Means')
abline(h = 1/3, lty= 2 , lwd = 1.5)
abline(h = 2/3, lty= 2 , lwd = 1.5)
labels = NULL
for (n in 1:k) {
  lines (mean.cluster[n, ], type = 'o', pch = 19, col = n)
  labels = append(labels, paste('Clus', as.character(n),
                                paste('(', 
                                as.character (table(cutree(res.average, k = k))[[n]]),
                                ')', sep = ''),
                                sep = ' '))
}
axis (1, at = 1:ncol(mean.cluster),
      lab = colnames(mean.cluster), cex.axis = 0.75)
axis (2, at = 0:1, las = 1)
legend(1, 1, labels, pch = 19, cex = 0.75, col = c(1:k))

summary.average <- as.data.frame(cbind(mean.cluster,
                                       as.integer(as.character(table(cutree(res.average, k = k))[1:n]))))
colnames(summary.average)[colnames(summary.average) == "V7"] <- "clust_size"
summary.average <- summary.average[order(-summary.average$clust_size), ]

summary.average


