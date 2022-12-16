library(tidyverse)
library(Rmixmod)

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')

d <- read.csv('SOEP_depriv.csv', sep = ';')
d <- d %>% 
  dplyr::select ('heat', 'meat', 'clothes', 'friends', 'furniture', 'holiday')
d <- d %>% drop_na

d$heat <- as.factor(d$heat)
d$meat <- as.factor(d$meat)
d$clothes <- as.factor(d$clothes)
d$friends <- as.factor(d$friends)
d$furniture <- as.factor(d$furniture)
d$holiday <- as.factor(d$holiday)

strategy1 <-mixmodStrategy(nbTry = 100)
depriv.out1 <- mixmodCluster(d, nbCluster = 1:10,
                             dataType = 'qualitative',
                             model = mixmodMultinomialModel(), 
                             strategy = strategy1,
                             criterion = c('BIC', 'ICL', 'NEC'))
strategy2 <-mixmodStrategy(initMethod = 'random',
                           nbTry = 100)
depriv.out2 <- mixmodCluster(d, nbCluster = 1:10,
                             dataType = 'qualitative',
                             model = mixmodMultinomialModel(), 
                             strategy = strategy2,
                             criterion = c('BIC', 'ICL', 'NEC'))

depriv.out1@bestResult@nbCluster
depriv.out1@bestResult@model

BIC1 <- sortByCriterion(depriv.out1, 'BIC')
BIC1@bestResult@nbCluster
BIC1@bestResult@model

ICL1 <- sortByCriterion(depriv.out1, 'ICL')
ICL1@bestResult@nbCluster
ICL1@bestResult@model

NEC1 <- sortByCriterion(depriv.out1, 'NEC')
NEC1@bestResult@nbCluster
NEC1@bestResult@model

depriv.out2@bestResult@nbCluster
depriv.out2@bestResult@model

ICL2 <- sortByCriterion(depriv.out2, 'ICL')
ICL2@bestResult@nbCluster
ICL2@bestResult@model

NEC2 <- sortByCriterion(depriv.out2, 'NEC')
NEC2@bestResult@nbCluster
NEC2@bestResult@model

round(depriv.out1@bestResult@criterionValue, 2)
round(depriv.out2@bestResult@criterionValue, 2)

round(ICL1@bestResult@criterionValue, 2)
round(ICL2@bestResult@criterionValue, 2)

round(NEC1@bestResult@criterionValue, 2)
round(NEC2@bestResult@criterionValue, 2)

G <- attr(depriv.out1, 'nbCluster')
modelN <- attr(depriv.out1, 'models')[1]
Criteria <- data.frame(G = rep(G, 3*length(modelN)),
                       model = rep(modelN, 
                                   each= length(G)),
                       BIC = NA, ICL = NA, NEC = NA, 
                       stringsAsFactors = FALSE)
for (i in 1:length(depriv.out1@results)) {
  Criteria[i, 1] = depriv.out1@results[[i]]@nbCluster
  Criteria[i, 2] = depriv.out1@results[[i]]@model
  Criteria[i, 3:5] = depriv.out1@results[[i]]@criterionValue 
}
library(gridExtra)
lx <- xlab('Number of CLusters')
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, 
                      function(x) x$name) == 'guide-box')
  legend <- tmp$grobs[[leg]]
  return(legend)
}
xscale <- scale_x_discrete(limits = c('1', '2', '3', '4', '5', 
                                      '6', '7', '8', '9', '10'))
A <- ggplot(Criteria, aes(Criteria[ ,1],
                          Criteria[ ,3], 
                          color = Criteria[ ,2])) +
  stat_summary(aes(group = Criteria[ ,2]),
               fun = mean, geom = 'line') + 
  ylab('BIC') + 
  lx + 
  labs(color = 'Models') + 
  xscale
B <- ggplot(Criteria, aes(Criteria[ ,1],
                          Criteria[ ,4], 
                          color = Criteria[ ,2])) +
  stat_summary(aes(group = Criteria[ ,2]),
               fun = mean, geom = 'line') + 
  ylab('ICL') + 
  lx + 
  labs(color = 'Models') + 
  xscale
C <- ggplot(Criteria, aes(Criteria[ ,1],
                          Criteria[ ,5],
                          color = Criteria[ ,2])) +
  stat_summary(aes(group = Criteria[ ,2]),
               fun = mean, geom = 'line') + 
  ylab('NEC') + 
  lx + 
  labs(color = 'Models') + 
  xscale
mylegend <- g_legend(A)
grid.arrange(A + theme(legend.position = 'none'), B +
               theme(legend.position = 'none'), C +
               theme(legend.position = 'none'),
             nrow = 2, top = 'Strategy 1', mylegend)

plot(BIC1)
legend('bottomleft', c('Cluster1', 'Cluster2', 'Cluster3', 'Cluster4'), 
       col = c(2,3, 4, 5), pch = c(1, 2, 3, 4))

plot(ICL1)
legend('bottomleft', c('Cluster1', 'Cluster2'), 
       col = c(2,3), pch = c(1, 2))
barplot(BIC1)
