library(poLCA)
library(tidyverse)
library(latex2exp)
library(ggplot2)
library(gridExtra)
library(viridis)

#install.packages("devtools")
#devtools::install_github("davismcc/scater", build_vignettes = TRUE)

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')

d <- read.csv('SOEP_depriv_17 copy.csv', sep = ';')
d <- d %>%
  dplyr::select ('heating', 'meat', 'clothes','friends',
                 'car', 'furniture', 'vacation','emergency')
d <- d %>% drop_na()
d <- data.frame(lapply(d, as.factor))

f <- cbind( meat, heating, clothes, 
            friends, car, furniture, vacation, emergency) ~ 1

#loglike_15 <- -Inf
m_iter  <- 200000
for (i in 1:5){
  model <-  poLCA(f, data = d, nclass = 15, 
                  maxiter = m_iter,
                  graphs = FALSE,
                  verbose = FALSE)
  if (model$llik > loglike_15) {
    probs_15 <- model$probs
    loglike_15 <- model$llik
  }
  print(paste('Iteration', i))
  print(model$numiter)
  print(model$llik)
  print(model$time)
}

start.p <- list(probs_2, probs_3, probs_4, probs_5, probs_6, probs_7, 
                probs_8, probs_9, probs_10)

loglikes <- c(loglike_2, loglike_3, loglike_4,  loglike_5,  loglike_6, 
              loglike_7, loglike_8, loglike_9, loglike_10)

lca_info <- data.frame( matrix(nrow = length(start.p), ncol = 8))

colnames(lca_info) = c('N_class', 'log_like', 'AIC', 'BIC',
                                'Chi_sqr', 'G_sqr', 'df', 'Resid_df')

max_iter  <- 250000
n = 2
for (i in 1:length(start.p)){
  lca <-  poLCA(f, data = d, nclass = n,
                maxiter = max_iter,
                probs.start = start.p[[i]],
                graphs = F, verbose = F)
  lca_info[i, 1:dim(lca_info)[2]] <- c(n, lca$llik, lca$aic, lca$bic, lca$Chisq,
                        lca$Gsq, lca$npar, lca$resid.df)
  if(lca$probs.start.ok == F){
    print(paste('LCA', n ))
    print('ERROR: Unable to read probabilites')
  } else if (lca$numiter == max_iter) {
    print(paste('LCA', n ))
    print('ERROR: No convergence')
  }
  else {
  print(paste('LCA', n ))
  print(paste('   Iterations:',lca$numiter))
  print(paste('   loglike diff:', loglikes[i]-lca$llik)) # Negative is better
  print(paste('   ', lca$time, sep = ''))
  }
  n = n + 1
}  

lca_4 <-  poLCA(f, data = d, nclass = 4,
              maxiter = max_iter,
              probs.start = probs_4)
lca_5$



lca_info <- drop_na(lca_info)

p1 <- ggplot(lca_info, aes(N_class, log_like)) +
  geom_segment(y=min(lca_info$log_like)-1000, yend=max(lca_info$log_like)+1000,
               x = 3, xend = 3, linetype = "dashed", color = 'red',
               size = 0.5) +
  geom_line(colour = viridis(6)[3], 
            size = 1) +
  geom_point(colour = viridis(6)[3], 
             size = 1.5) +
  theme_linedraw() +
  theme(plot.title = element_text(size=10),
        #panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank()
        ) +
  labs( title = 'log likelihood',
    y = "", x = "") +
  scale_x_continuous(expand = c(0, 0.5),
                     breaks = seq(2, 10, by = 2)) 

p2 <- ggplot(lca_info, aes(N_class, BIC)) +
  geom_segment(y=min(lca_info$BIC)-1000, yend=max(lca_info$BIC)+1000,
               x = 3, xend = 3, linetype = "dashed", color = 'red',
               size = 0.5) +
  geom_line(colour = viridis(6)[3], 
            size = 1) +
  geom_point(colour = viridis(6)[3], 
             size = 1.5) +
  theme_linedraw() +
  theme(plot.title = element_text(size=10),
        legend.position = c(0.8, 0.35),
        legend.title = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank()
        ) +
  labs( title = 'BIC',
    y = "", x = "") +
  #scale_colour_manual(values=c(BIC= viridis(10)[2],
  #                             AIC= viridis(10)[7])) +  
  scale_x_continuous(expand = c(0, 0.5),
                     breaks = seq(2, 10, by = 2))

p3 <- ggplot(lca_info, aes(N_class, Chi_sqr )) +
  geom_segment(y=min(lca_info$Chi_sqr)-1000, yend=max(lca_info$Chi_sqr)+1000,
               x = 4, xend = 4, linetype = "dashed", color = 'red',
               size = 0.5) +
  geom_line(colour = viridis(4)[2],
            size = 1) +
  geom_point(colour = viridis(4)[2],
             size = 1.5) +
  theme_linedraw() + 
  theme(axis.title.y = element_text( angle= 0, vjust = 0.45),
        plot.title = element_text(size=10),
        #panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank()
        ) +
  labs( title = TeX('Goodness of fit') ,
        y = TeX('$\\chi^{2}'),  x = "Latent classes") +
  scale_x_continuous(expand = c(0, 0.5),
                     breaks = seq(2, 10, by = 2))

p4 <- ggplot(lca_info, aes(N_class, G_sqr )) +
  geom_segment(y=min(lca_info$G_sqr)-1000, yend=max(lca_info$G_sqr)+1000,
               x = 3, xend = 3, linetype = "dashed", color = 'red',
               size = 0.5) +
  geom_line(color = viridis(6)[3],
            size = 1)+
  geom_point(color = viridis(6)[3],
             size = 1.5) +
  theme_linedraw() +
  theme(axis.title.y = element_text(angle= 0, vjust= 0.45),
        plot.title = element_text(size=10),
        #panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank()
        ) +
  labs( title = 'Likehood ratio/deviance statistic',
    y = TeX('$\\G^{2}'), x = "Latent classes") +
  scale_x_continuous(expand = c(0, 0.5),
                     breaks = seq(2, 10, by = 2)) 

grid.arrange(p1, p2, p3, p4, ncol=2)



