setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/Network equating/Code/network_equate")
source("genData.R")
source("genLinkage.R")
source("linearEquate.R")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(igraph)
library(ggpubr)


numTests = 3
nt = c(50, 100, 100)
np= c(1000, 1000, 1000)
avg_diff = c(1, 0, -1) # Easy, Medium, Hard
Sigma= diag(numTests)
#Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
x = gendata(numTests, nt=nt, np=np, avg_diff, 
            diff_dist="iid", ability_type= "normal")
data=x[[1]] ## Item and respondent level response data for each test
true.ability= x[[2]]
sum_scores= x[[3]]

x=sum_scores[,-1]
id=sum_scores[,1]
x_equated= lineq(x,id)
boot.se= link.boot.se(x, rep=500)
boot.se
#### Error of equating -  bootstrap
#0.2928948   0.6179196   0.5526686 


## Correlations before equating vs recovered correlations

c(true.cor= cor(sum_scores$totalScore1, true.ability$th), 
  recovered.cor= cor(x_equated[[1]]$totalScore1, true.ability$th))

c(true.cor= cor(sum_scores$totalScore2, true.ability$th), 
  recovered.cor= cor(x_equated[[1]]$totalScore2, true.ability$th))

c(true.cor= cor(sum_scores$totalScore3, true.ability$th), 
  recovered.cor= cor(x_equated[[1]]$totalScore3, true.ability$th))

## New scores
colMeans(sum_scores[,-1], na.rm = T)
colMeans(x_equated[[1]][,-1], na.rm = T)

## See how convergence went
info= x_equated[[2]]
plot(NULL,xlim=c(0,12),ylim=c(0,50))
for (i in 1:(length(x)-1)) {
  sapply(info,'[[',i)->z
  z<-t(z)
  lines(z[,1],z[,2],type='l')
}

### Plots
### Check results make sense


### Truth is known
plot(x_equated[[1]]$totalScore1, true.ability$th)
plot(sum_scores$totalScore1, true.ability$th)
plot(x_equated[[1]]$totalScore2, true.ability$th)
plot(x_equated[[1]]$totalScore3, true.ability$th)



g1= x |>
  pivot_longer(cols= starts_with("totalScore"), 
               names_to= "test",
               values_to= "totalScore") |>
  ggplot(aes(x=totalScore, group=test, fill=test, color=test)) + 
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,0.05)) +
  theme(legend.position = "bottom")

g2= x_equated[[1]] |>
  pivot_longer(cols= starts_with("totalScore"), 
               names_to= "test",
               values_to= "totalScore") |>
  ggplot(aes(x=totalScore, group=test, fill=test, color=test)) + 
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,0.05)) +
  theme(legend.position = "bottom")


g= ggarrange(g1, g2, common.legend = TRUE)
ggsave(g,"Plots/linked_scores_network.png")


plot(x[,3],x_equated[[1]][,4])
plot(x[,2],x_equated[[1]][,3])
