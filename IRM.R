library(tidyverse)
library(ltm)
library(mirt)
library(Amelia)
library(viridis)

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')
d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')

d[, 2:13] %>% 
  Amelia::missmap(main= 'Missing Values Map', 
                  rank.order = TRUE, 
                  col = c('lightyellow', 'dodgerblue3'),
                  main.cex = 1.5,
                  x.cex = 1,
                  y.cex = 0.9,
                  y.labels = c("1","1000", "2000", "3000","4000","5000", 
                               "6000", "7000", "8000", "9000", "9838"),
                  y.at = c(9838, 8838, 7838, 6838, 5838, 
                           4838, 3838, 2838, 1838, 838, 1),
                  margins = c(2.5, 2.5),
                  x.las = 2)
dim(d)
dim(drop_na(d))

pct_not_na <- data.frame(friends = nrow(d[!is.na(d$friends),])/nrow(d)*100,
                         furniture = nrow(d[!is.na(d$furniture),])/nrow(d)*100,
                         leisure = nrow(d[!is.na(d$meat),])/nrow(d)*100,
                         internet = nrow(d[!is.na(d$internet),])/nrow(d)*100,
                         vacation = nrow(d[!is.na(d$vacation),])/nrow(d)*100,
                         car = nrow(d[!is.na(d$car),])/nrow(d)*100, 
                         emergency = nrow(d[!is.na(d$emergency),])/nrow(d)*100,
                         meat = nrow(d[!is.na(d$meat),])/nrow(d)*100, 
                         clothes = nrow(d[!is.na(d$clothes),])/nrow(d)*100, 
                         money = nrow(d[!is.na(d$money),])/nrow(d)*100,
                         heating = nrow(d[!is.na(d$heating),])/nrow(d)*100,
                         shoes = nrow(d[!is.na(d$shoes),])/nrow(d)*100)
                           
max(pct_not_na)

hist(rowSums(is.na(d)), main = 'Households by Missing Values', 
     xlab = 'Missing Values', breaks = -0.5:12.5)
axis(side=1, at=seq( 0, 12, 1), labels=seq(0, 12, 1))

#         *************************
#         ***  ALL INDICATORS   ***
#         *************************

d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')
d <- d[, 2:13]

d %>% 
  Amelia::missmap(main= 'Missing Values Map', 
                  rank.order = TRUE, 
                  col = c('lightyellow', 'dodgerblue3'),
                  main.cex = 1.5,
                  x.cex = 0.8,
                  y.cex = 0.9,
                  y.labels = c("1","1000", "2000", "3000","4000","5000", 
                               "6000", "7000", "8000", "9000", "9838"),
                  y.at = c(9838, 8838, 7838, 6838, 5838, 
                           4838, 3838, 2838, 1838, 838, 1),
                  margins = c(2.5, 2.5),
                  x.las = 2)

d <- d %>% drop_na

pct_depriv <- data.frame (shoes     = (1 - sum(d$shoes)/dim(d)[1])*100,
                          meat      = (1 -sum(d$meat)/dim(d)[1])*100,
                          heating      = (1 -sum(d$heating)/dim(d)[1])*100,
                          internet  = (1 - sum(d$internet)/dim(d)[1])*100,
                          clothes   = (1 -sum(d$clothes)/dim(d)[1])*100,
                          money     = (1 -sum(d$money)/dim(d)[1])*100,
                          car       = (1 -sum(d$car)/dim(d)[1])*100,
                          leisure   = (1 -sum(d$leisure)/dim(d)[1])*100,
                          friends   = (1 -sum(d$friends)/dim(d)[1])*100,
                          furniture = (1 -sum(d$furniture)/dim(d)[1])*100,
                          vacation   = (1 -sum(d$vacation)/dim(d)[1])*100,
                          emergency = (1 -sum(d$emergency)/dim(d)[1])*100
)


d$sum <- 12 - apply(d, 1, function(x) sum(x))

h <- hist(d$sum, 
          breaks = seq(-0.5, 12.5),
          main = 'Households by Number of Deprived Items', 
          xlab = 'Deprived items',
          cex.lab = 1.3,
          cex.main = 1.5,
          col = 'dodgerblue3',
          freq = FALSE,
          ylim=c(0,0.8))
text(h$mids[1:3], h$density[1:3], h$counts[1:3], adj = c(.5, -.5), 
     col = "black", cex = 0.8)
text(h$mids[1:3], h$density[1:3], paste('(',round(h$density[1:3]*100,2), '%)', sep = ''),
     adj = c(.5, 1.4), col = "white", cex = 0.7)
text(h$mids[4:13], h$density[4:13], h$counts[4:13], adj = c(.5, -1.8), 
     col = "black", cex = 0.8)
text(h$mids[4:13], h$density[4:13], paste('(',round(h$density[4:13]*100,2), '%)', sep = '')
     , adj = c(.5, -0.5), col = "black", cex = 0.7)

d <- d %>% dplyr::select (-c('sum'))

for (c in 1:dim(d)[2]) {
  d[, c] <- as.factor(d[ ,c])
}


##         **************************
##         *** OPTIMAL INDICATORS ***
##         **************************
#
#d$na_count <- apply(d, 1, function(x) sum(is.na(x)))
#d <- subset(d, na_count < 4)
#nrow(d)
#
#d <- d %>% 
#  dplyr::select ('car', 'emergency', 'vacation',  
#          'meat', 'clothes', 'heating', 'money', 'shoes')
#d <- d %>% drop_na
#
#pct_depriv <- data.frame (shoes = sum(d$shoes)/dim(d)[1]*100,
#                          heating = sum(d$heating)/dim(d)[1]*100,
#                          meat = sum(d$meat)/dim(d)[1]*100,
#                          clothes = sum(d$clothes)/dim(d)[1]*100,
#                          money = sum(d$money)/dim(d)[1]*100,
#                          car = sum(d$car)/dim(d)[1]*100,
#                          vacation = sum(d$vacation)/dim(d)[1]*100,
#                          emergency = sum(d$emergency)/dim(d)[1]*100)
#pct_depriv
#
#d$sum <- apply(d, 1, function(x) sum(x))
#hist(d$sum, main = 'Households by Number of Items Deprived', 
#     xlab = 'Items')
#d <- d %>% dplyr::select (-c('sum'))
#
#for (c in 1:dim(d)[2]) {
#  d[, c] <- as.factor(d[ ,c])
#}

#         *************************
#         *** PHRASE INDICATORS ***
#         *************************

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')
d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')

d <- d %>% 
  dplyr::select ('heating', 'meat',  'clothes', 
                 'friends', 'car', 'furniture', 'vacation', 'emergency')

d %>% 
  Amelia::missmap(main= 'Missing Values Map', 
                  rank.order = TRUE, 
                  col = c('lightyellow', 'dodgerblue3'),
                  main.cex = 1.5,
                  x.cex = 0.8,
                  y.cex = 0.9,
                  y.labels = c("1","1000", "2000", "3000","4000","5000", 
                               "6000", "7000", "8000", "9000", "9838"),
                  y.at = c(9838, 8838, 7838, 6838, 5838, 
                           4838, 3838, 2838, 1838, 838, 1),
                  margins = c(2.5, 2.5),
                  x.las = 1)


d <- d %>% drop_na

pct_depriv <- data.frame (heating = (1-sum(d$heating)/dim(d)[1])*100,
                          meat = (1-sum(d$meat)/dim(d)[1])*100, 
                          clothes = (1-sum(d$clothes)/dim(d)[1])*100,
                          friends = (1-sum(d$friends)/dim(d)[1])*100,
                          car = (1-sum(d$car)/dim(d)[1])*100,
                          furniture = (1-sum(d$furniture)/dim(d)[1])*100,
                          vacation = (1-sum(d$vacation)/dim(d)[1])*100,
                          emergency = (1-sum(d$emergency)/dim(d)[1])*100)
pct_depriv

d$sum <- 8 - apply(d, 1, function(x) sum(x))

ht <- ggplot(d, aes(x=sum)) + 
  geom_histogram(aes(y=..density..), 
                 colour="black", fill= viridis(20)[8], binwidth=1) +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 15),
        axis.title.y = element_text(vjust = 3, size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.margin=unit(c(0.25, 0.25, 0.5, 0.5), 'cm')) +
  scale_x_continuous(expand = c(0, 0.5),
                     breaks = seq(0, 8, by = 2)) +
  xlab('Deprived items') +
  ylab ('Density')

ht_d <- ggplot_build(ht)$data[[1]]

ht + geom_text(data = ht_d[1:6, ], 
               aes(x = x, y = y-0.02, 
                   label = paste(round((density*100),2), '%', sep = '')),
               size = 3.8, 
               colour = 'white') +
  geom_text(data = ht_d[1:6, ], 
            aes(x = x, y = y+0.02, label = count),
            size = 5) +
  geom_text(data = ht_d[7:9, ], 
            aes(x = x, y = y+0.02, 
                label = paste(round((density*100),2), '%', sep = '')),
            size = 3.8) +
  geom_text(data = ht_d[7:9, ], 
            aes(x = x, y = y+0.06, label = count),
            size = 5) 

h <- hist(d$sum, 
     breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), 
     main = '', 
     xlab = 'Deprived items',
     cex.lab = 1.3,
     cex.main = 1.5,
     col = 'dodgerblue3',
     freq = FALSE,
     ylim=c(0,0.6))
#curve (dnorm(x, mean=mean(d$sum), sd=sd(d$sum)),
#       add=TRUE, col="red")
text(h$mids[1:5], h$density[1:5], h$counts[1:5], adj = c(.5, -.5), 
     col = "black", cex = 1)
text(h$mids[1:5], h$density[1:5], paste(round(h$density[1:5]*100,2), '%', sep = ''),
     adj = c(.5, 1.4), col = "white", cex = 0.9)
text(h$mids[6:9], h$density[6:9], h$counts[6:9], adj = c(.5, -2.2), 
     col = "black", cex = 1)
text(h$mids[6:9], h$density[6:9], paste(round(h$density[6:9]*100,2), '%', sep = ''),
     adj = c(.5, -0.5), col = "black", cex = 0.9)

d <- d %>% dplyr::select (-c('sum'))

for (c in 1:dim(d)[2]) {
  d[, c] <- as.factor(d[ ,c])
}


# P(x_im = 1 | z_m) = a_i(z_m - b_i)
# x_im is the dichotomous variable (i: item, m: hh)
# z_m denotes each household's latent variable

# Difficulty parameters (b) are expressed in terms of std. dev. of a normal 
# distribution. A b param of -2 indicates the item is very easy to achieve, 
# whereas an b param of 2 indicates item is hard to achieve. 

# Discrimination parameters (a) express the extent to which an item describes 
# the person/household. a higher a parameter results in an steeper ICC slope
# and indicates that the item can differentiate between very poor and other hh

# Rizopoulos, 2006
# The functions included in the ltm package fit under the logit link
# Package ltm fits the models using Marginal Maximum Likelihood Estimation (MMLE)
# Parameter estimation under MMLE assumes that the respondents represent a 
# random sample from a population and their ability is distributed according to 
# a distribution function F(z). The model parameters are estimated by maximizing 
# the observed data log-likelihood obtained by integrating out the latent variables

#1 Parameter IRM 
# Rasch Model assumes that all items discriminate in the same way
# i.e. a parameter is equal for all
d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')
d <- d[, 2:13]
d <- d %>% drop_na

irm <- rasch(d, constraint = cbind(length(d) + 1, 1))
summary(irm)
coef(irm, order = TRUE)

item.fit(irm)
GoF.rasch(irm, B = 199)
margins(irm)
margins(irm, type = "three-way", nprint = 2)

#Item Characteristic Curve 
# Difficulty (b param.): More to the right, item is harder to achieve
par(mfrow=c(1,1))
plot(irm, type = 'ICC', xlab = 'Deprivation',
     ylim = c(-0.2, 1))

par(mfrow=c(2,1))
plot(irm, type ='IIC', xlab = 'Deprivation', 
     main = 'Item Information Function')
plot(irm, type ='IIC', items = 0, xlab = 'Deprivation', 
     main = 'Joint Information Function')

#2 Parameter IRM
irm2 <- ltm(d ~ z1, IRT.param = TRUE)
summary(irm2)
coef(irm2, order = TRUE)

scores2 <-factor.scores(irm2)
scores2$coef


margins(irm2, type = c("two-way", "three-way"))
margins(irm2, type = "three-way")

item_irm2 <- item.fit(irm2, G = 7, simulate.p.value = T, B = 100)
person_irm2<- person.fit(irm2, simulate.p = T,
                         alternative = 'two.sided')

count = 0
for (i in 1:dim(person_irm2$p.values)[1]){
  if (person_irm2$p.values[i ,2] < 0.001 &
      person_irm2$Tobs[i, 2] < 0 ){
    count = count + 1 
  }
}
print(count/dim(person_irm2$p.values)[1])

scores_EB <- factor.scores(irm)$score.dat$z1
scores_EAP <- factor.scores(irm, method = 'EAP')$score.dat$z1
scores_MI <- factor.scores(irm, method = 'MI', B = 20)$scores.dat$z1

info_EB <- c(min(scores_EB), mean(scores_EB), max(scores_EB), sd(scores_EB))
info_EAP <- c(min(scores_EAP), mean(scores_EAP), max(scores_EAP), sd(scores_EAP))
info_MI <- c(min(scores_MI), mean(scores_MI), max(scores_MI), sd(scores_MI))

rbind(info_EB, info_EAP, info_MI)

res <- cbind(scores_EB1, scores_EB1)

delta <- NULL
for (i in 1:length(scores_EB1)){
  delta[i] <- scores_EB1[i] - scores_EB1[i]
}

#Item Characteristic Curve 
# Difficulty (b param.):  More to the right, item is harder to achieve
# Discrimination (a param.): Steeper slope, item discriminates more
par(mfrow=c(1,1))
plot(irm2, type = 'ICC',
     col = (viridis(8, )),
     lwd = 3,
     lty = rep(c(2,1), 4),
     xlab = 'Purchasing ability',
     legend = TRUE, 
     cx = "bottomright", 
     cex.main = 1.5, 
     cex.lab = 1.3, 
     cex = 1.1)


plot(irm2, type = 'ICC', items = c(1, 4), xlab = 'Deprivation') 

tpm(d, max.guessing = 0)

#Test Information Function
# Shows how much information each item/combination of items gives in terms of 
# std. dev. of a normal distribution. 
par(mfrow=c(2,1))
plot(irm2, type ='IIC', xlab = 'Deprivation', 
     main = 'Item Information Function')
plot(irm2, type = 'IIC', items = 0, xlab = 'Deprivation',
     main = 'Joint Information Function') 

anova(irm, irm2)


fit3 <- ltm(d ~ z1 + z2)
summary(fit3)
coef(fit3)
par(mfrow=c(1,1))
plot(fit3, type = 'ICC', xlab = 'Deprivation', ylab = 'Item Bias',
     col = 'seagreen2') 



x<- c(-6:2)
df<-data.frame(x)

dta <- summary(irm2)$coefficients
diff <- dta[1:8, 1]
disc <- dta[9:16, 1]

ggplot(df,aes(x))  +
  geom_segment(x = 0, xend = 0, y = -0.5, yend = 1.5, size = 0.1) +
  stat_function(fun=function(x) 1/(1+exp(-disc[[2]]*((x)-diff[[2]]))),
                aes(colour = 'heating'), size = 1, linetype = "dashed") +
  stat_function(fun=function(x) 1/(1+exp(-disc[[1]]*((x)-diff[[1]]))),
                aes(colour = 'meat'), size = 1) +
  stat_function(fun=function(x) 1/(1+exp(-disc[[3]]*((x)-diff[[3]]))),
                aes(colour = 'clothes'), size = 1, linetype = "dashed") +
  stat_function(fun=function(x) 1/(1+exp(-disc[[4]]*((x)-diff[[4]]))),
                aes(colour = 'friends'), size = 1) +
  stat_function(fun=function(x) 1/(1+exp(-disc[[5]]*((x)-diff[[5]]))),
                aes(colour = 'car'), size = 1, linetype = "dashed") +
  stat_function(fun=function(x) 1/(1+exp(-disc[[6]]*((x)-diff[[6]]))),
                aes(colour = 'furniture'), size = 1) +
  stat_function(fun=function(x) 1/(1+exp(-disc[[7]]*((x)-diff[[7]]))),
                aes(colour = 'vacation'), size = 1, linetype = "dashed") +
  stat_function(fun=function(x) 1/(1+exp(-disc[[8]]*((x)-diff[[8]]))),
                aes(colour = 'emergency'), size = 1) +
  scale_colour_manual(values=c(heating= viridis(8)[1], meat= viridis(8)[2],
                               clothes = viridis(8)[3], friends = viridis(8)[4],
                               car = viridis(8)[5], furniture = viridis(8)[6],
                               vacation = viridis(8)[7], emergency = viridis(8)[8]),
                      guide=guide_legend(override.aes=list(linetype=rep(c(2,1), 4),
                                                           lwd=rep(1, 8)))) +
  theme_linedraw() +
  theme(axis.text = element_text(size=11),
        axis.title.x = element_text(size = 13, vjust = -2),
        axis.title.y = element_text(size = 13, vjust = 3),
        legend.position = c(0.15, 0.65),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.key.width = unit(1.15, 'cm'),
        legend.key.height = unit(0.6, 'cm'),
        plot.margin = margin(b = 0.4, t = 0.25, l = 0.35, r = 0.25, unit = "cm")) +
  labs (x = 'Purchasing ability', y = 'Probability')
