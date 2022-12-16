library(tidyverse)
library(cluster)
library(reshape2)
library(viridis)

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')

d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')
d <- d %>%
  dplyr::select ('heating', 'meat', 'clothes','friends',
  'car',  'furniture', 'vacation', 'emergency') 

#d <- d %>%
#  dplyr::select ('heat', 'meat', 'clothes', 'friends', 'furniture', 'vacation')

d <- d %>% drop_na()

    #d$heat <- as.factor(d$heat)
    #d$meat <- as.factor(d$meat)
    #d$clothes <- as.factor(d$clothes)
    #d$friends <- as.factor(d$friends)
    #d$furniture <- as.factor(d$furniture)
    #d$holiday <- as.factor(d$holiday)

res.mona <- mona(x = d)

## Assigns a cluster to each household, 
## and print the number of households in each cluster	
cluster.analysis = function(cluster.obj, k){
  indices = c()
  results.vector = c()
  
  if(class(cluster.obj)[1] == "diana"){
    cluster.count = 2^k
    indices =  order(cluster.obj$height, decreasing = T)[1:(cluster.count-1)]}
  
  ## Find all splitting points in the data set. 
  if(class(cluster.obj)[1] == "mona"){
    for(i in 1:k){indices = append(indices, which(cluster.obj$step == i))}}
  sorted.indices = sort(indices)
  
  ## Add the total number of patients to the vector
  sorted.indices = append(sorted.indices, length(cluster.obj$order))
  
  n = 1
  clust.size = NULL
  n_new = NULL
  for (i in 1:k){
    n_new =  n - length(which(cluster.obj$step == i)) + length(which(cluster.obj$step == i))*2
    n = n_new
    n_new = NULL
  }
  
  ## Start with 1
  low = 1
  ## Make a vector with the same length of n, in which each cluster is specified.
  for(j in 1:n){
    high = sorted.indices[j]
    cluster.size = length(cluster.obj$order[low:high])
    results.vector = append(results.vector, rep(j, cluster.size))
    ## Print the cluster  = tsize
    print(paste("Cluster = ", j, ", count = ", cluster.size))
    low = high +1 }
  df = data.frame(order = cluster.obj$order, results.vector = results.vector)
  df = df[order(df$order),]
  return(df$results.vector)}

## Get the variables on which the data was divided
mona.variables = function(mona.obj, k){  
  return(data.frame(step = mona.obj$step[mona.obj$step > 0 & mona.obj$step <=k], 
                    variable = mona.obj$variable[which(mona.obj$step > 0 & mona.obj$step<=k)]))}

step <- 1

#labs <-  mona.variables(res.mona, 8)
#table(filter(mona.variables(res.mona, 8), 
#             mona.variables(res.mona, 8)[, 1]==8)[,2])


clust <-cluster.analysis(res.mona, step)
clust.mona <- cbind(d, clust)

clust.mean <- data.frame(matrix(ncol = dim(d)[2]+2, nrow = 2^step))
colnames(clust.mean) <- c(colnames(d), 'size', 'var')

for (c in 1:(2^step)) {
  for (v in 1:dim(d)[2]) {
    clust.mean[c, v]<- mean((clust.mona %>% dplyr::filter(clust == c))[ ,v])
    if (v == dim(d)[2]) {
      clust.mean[c, (dim(d)[2]+2)] <- var((clust.mona %>% dplyr::filter(clust == c))[ ,v])
      clust.mean[c, dim(d)[2]+1]<- count((clust.mona%>% dplyr::filter(clust == c)))
    }
  }
}

# Clusts to show
n <- 4

clust.mean.ord <- tibble::rowid_to_column(clust.mean, "Cluster")
clust.mean.ord <- clust.mean.ord[order(-clust.mean.ord$size), ]
#clust.mean.ord <- clust.mean.ord[3:4, ]



melt_clust <- melt(tail(clust.mean.ord[, 0:dim(d)[2]+1], n), id.vars  = 'Cluster')
colnames(melt_clust) <- c('Cluster', 'Item', 'Mean')
melt_clust$Size <-rep(clust.mean.ord$size, 8)  

for (i in 1:dim(melt_clust)[1]){
  melt_clust$Cluster[i] <- paste('Cluster',  melt_clust$Cluster[i])
}

plot_ly(colors = 'GnBu') %>%
  layout(autosize = TRUE, 
         xaxis=list(title = "",
                    tickfont = list(size = 17)
                    ),
         yaxis=list(title = '',
                    ticktext = list(NULL),
                    tickvals = list(NULL))) %>%
  add_trace(data = melt_clust, x = ~Item, y = ~Cluster, z = ~Mean, 
            type = "heatmap",
            hoverinfo = 'text',
            text = ~paste( melt_clust$Cluster,
                          '<br>Size:', melt_clust$Size,
                          "<br>Item:", melt_clust$Item,
                          "<br>Mean:", round(melt_clust$Mean, 3)))



hm

melt_clust <- melt(tail(clust.mean.ord, n), id.vars  = 'Cluster')


plot_ly(data = melt_clust, x = ~Item, y = ~Mean, 
        color = ~Cluster, colors = viridis,
        mode = 'lines+markers')

labls <- NULL
for (j in 1:2^(step)){
  dta<- tail(clust.mean.ord$size, n)
  nam <- paste(j, ' (', dta[j], ')', sep ='')
  labls <- c(labls, nam)
}

melt_clust$ID <- as.factor(melt_clust$ID)   



lp <- ggplot(melt_clust, aes(x = Item, y = Mean)) +
  theme_bw() +
  geom_line(aes(group = ID, col = ID), size = 2) +
scale_y_continuous(expand = c(0.05, 0), limits = c(0,1),
                   breaks=c(0, 0.5, 1),
                   labels=c(as.character(c(0, 0.5, 1)))) +
  scale_color_viridis(discrete = TRUE, 
                      labels = labls, 
                      ' ') 
ggplotly(lp)

melt_clust$ID <- as.factor(melt_clust$Cluster)

ggplot(melt_clust, aes(Item, ID))+
  theme(panel.background = element_rect(fill = 'white'),
        axis.text.y = element_text(size = 15, face = 'bold'),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 13, vjust = 1),
        axis.line.x = element_line(colour = 'black', size = 0),
        axis.ticks.x = element_line(size = 0),
        axis.title.x = element_text(size = 0, hjust= 0.5,
                                    margin = margin(t = 20)),
        plot.margin=unit(c( 0.25, .25, .25, .25), 'cm')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 8.5),
                     breaks=seq(1, 8),
                     labels=c(as.character(colnames(clust.mean.ord)[2:9]))) +
  geom_tile(aes(fill = Mean), color = "white",  lwd = 0.5,  linetype = 1)  +
  scale_fill_gradient(low = 'seashell2', high = "steelblue3") +
  geom_text(aes(label = round(Mean, 2) ), color = "black", size = 4.5) +
  xlab('') +
  ylab('') +
  theme(legend.position = "none") +
  labs(title = "Cluster means by item after 3 partition steps", 
       subtitle = '10 largest clusters')


### 2 CLUSTERS 
plot( 1:dim(d)[2], clust.mean[1, 1:dim(d)[2]], type = 'o', pch = 16, lwd = 3,
      xlab = '', 
      ylab = '',
      ylim = c(0,1), axes = FALSE, col = viridis(6, )[4],
      main = 'Cluster means by Item after 1 partition step',
      cex.lab = 1.3,
      cex.main = 1.5,)
title(xlab="Item", line=3.5, cex.lab=1.3)
title(ylab = 'Mean', line = 1.5, cex.lab = 1.3)

for (n in 2:(2^step)) {
  lines (1:dim(d)[2], clust.mean[n, 1:dim(d)[2]], 
         lwd = 3, 
         type = 'o', pch = 16, 
         col = viridis(6, )[1])
}

#axes = FALSE

axis (1, at = 1:dim(d)[2], labels = FALSE, 
      cex.axis = 1.2, )
text(seq(1, 8, by=1), par("usr")[3] - 0.2, offset = -1.5, 
     labels = colnames(clust.mean)[1:8], srt = 45, pos = 1, xpd = TRUE,
     col = c('black', 'black', 'black', 'black', 'black', 'black', 'red', 'black'),)

axis (2, at = 0:1 , las = 1)

legend(1.1, 0.25, col = c(viridis(6, )[1], viridis(6, )[4]),
        pch = 16, lwd = 3,
       c('Cluster 2 (73.87%)', 'Cluster 1 (16.13%)'),
       bty="n", y.intersp=1.3)

### 4 CLUSTERS 
plot( 1:6, clust.mean[1, 1:6], type = 'o', pch = 16, lwd = 3,
      xlab = 'Item', ylab = 'Cluster Means',
      ylim = c(0,1), axes = FALSE, col = 'steelblue1',
      main = 'Cluster means by Item after 2 partition steps',
      cex.lab = 1.3,
      cex.main = 1.5,)

COLS <- c('steelblue4', 'darkorange4', 'darkorange1')
for (n in 2:(2^step)) {
  lines (1:6, clust.mean[n, 1:6], 
         lwd = 3, 
         type = 'o', pch = 19, 
         col = COLS[n-1])
}

axis (1, at = 1:ncol(clust.mean), labels = FALSE, 
      cex.axis = 1.2, )
mtext(colnames(clust.mean),
      side = 1 , line = 1, at = 1:ncol(clust.mean),
      col = c('black', 'black', 'black', 'red', 'black', 'red'))
axis (2, at = 0:1, las = 1)

abline(h = 1/3, lty= 2 , lwd = 1.5)
abline(h = 2/3, lty= 2 , lwd = 1.5) 

legend(1,0.3, col = c('steelblue1', 'steelblue4', 
                    'darkorange4', 'darkorange1'),
       pch = 19, lwd = 3,
       c('Clust 1 (804)', 'Clust 2 (5800)', 
         'Clust 3 (1332)', 'Clust 4 (780)'))



######################### TOTAL SUM OF SQUARES

ss_info<- data.frame(matrix(ncol = 3, nrow = 8))
colnames(ss_info) <- c('step', 'TSS', 'WSS')

for (s in 1:8){
  clust <-cluster.analysis(res.mona, s)
  clust.mona <- cbind(d, clust)
  
  clust.mean <- data.frame(matrix(ncol = dim(d)[2]+2, nrow = 2^s))
  colnames(clust.mean) <- c(colnames(d), 'size', 'var')
  
  for (c in 1:(2^s)) {
    for (v in 1:dim(d)[2]) {
      clust.mean[c, v]<- mean((clust.mona %>% dplyr::filter(clust == c))[ ,v])
      if (v == dim(d)[2]) {
        clust.mean[c, (dim(d)[2]+2)] <- var((clust.mona %>% dplyr::filter(clust == c))[ ,v])
        clust.mean[c, dim(d)[2]+1]<- count((clust.mona%>% dplyr::filter(clust == c)))
      }
    }
  }
  
  vars <- NULL
  for (i in 1:8){
    vars <- c(vars, var(clust.mean[, i], na.rm = T))
  }
  
  ss_info[s, 1] <- s
  ss_info[s, 2] <- sum(vars, na.rm = T)
  ss_info[s, 3] <- mean(vars, na.rm = T)
  
}

plot(ss_info$step, ss_info$TSS, type = 'b', col = 'blue')
plot(ss_info$step, ss_info$WSS, type = 'b', col = 'black')


min(ss_info$TSS)/max(ss_info$WSS)

ggplot(ss_info, aes(step, TSS)) +
  geom_line(colour = viridis(4)[2], 
            size = 1.25) +
  geom_point(colour = viridis(4)[2], 
             size = 1.75) +
  scale_x_continuous(breaks= c(1:8), expand = c(0.025, 0.025),
                     sec.axis = sec_axis(~ .*1,
                                         name = "Clusters",
                                         breaks = c(1:8),
                                         labels = c(2, 4, 8, 16, 32, 62, 105, 141))) +
  scale_y_continuous(expand = c(0.05, 0.05), limits = c(min(ss_info$TSS), max(ss_info$TSS))
                     ) +
  theme_linedraw()+
  theme(#panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x.top = element_text(size = 13, vjust = 3),
        axis.title.x.bottom = element_text(size = 13, vjust = -1.5),
        axis.title.y = element_text(size = 13, vjust = 2),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = margin(b = 0.3, t = 0.25, l = 0.25, r = 0.1, unit = "cm")) +
  labs( x = 'Partition step', y = 'TSS')


######################### 2 CLUSTERS GGPLOT

dta <- melt_clust[1:16, ]
dta$Cluster <- as.factor(dta$Cluster)

ggplot(dta, aes(variable, value, color = Cluster,group = Cluster)) +
  geom_line(size = 1.25) +
  geom_point(size = 1.75) +
  theme_linedraw() +
  theme(axis.text = element_text(size=11),
        axis.title.x = element_text(size = 13, vjust = -2),
        axis.title.y = element_text(size = 13, vjust = 3),
        legend.position = c(0.2, 0.17),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.key.width = unit(1.15, 'cm'),
        legend.key.height = unit(0.6, 'cm'),
        plot.margin = margin(b = 0.4, t = 0.25, l = 0.35, r = 0.25, unit = "cm")) +
  labs (x = 'Item', y = 'Mean') +
  scale_colour_manual(values=c(viridis(10)[2], viridis(10)[7]),
                      labels = c('Cluster 1 (26.13%)',
                                 'Cluster 2 (73.87%)'))





