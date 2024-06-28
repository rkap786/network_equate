---
title: "network data generation"
format: html
editor: visual
---

# Data generation for linking

```{r}

setwd("/Users/radhika/Library/CloudStorage/GoogleDrive-rkap786@stanford.edu/My Drive/0. Projects - Stanford/Network equating/Code/network_equate")
source("genData.R")
source("genLinkage.R")
source("linearEquate.R")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(igraph)

```

## Setting 1: Common population different tests

### Example 1

-   Generates test score data (at the item and total score level) for T tests with nt items, given student ability drawn for NP students

-   Student population paramters:Student ability is simulated for given

    -   N(mean,sd) or

    -   skewed (rbeta distribution)

-   Test parameters

    -   Easy, medium, or hard tests:

        -   Easiness independently drawn from N(0,1), N(-2,1), N(2,1) - increasing mean signifies increasing easiness

        -   test difficulty from multivariate normal distribution with correlation between tests set at 0.5 ***\[this might not be important?\]***

-   Function returns:

-   For each respondent, test score for test t. Test score is NA is test has fewer respondents. This can be used to calculate sum scores for each test for available respondents.

    -   Changing number of respondents can automatically put in a linkage structure

-   True ability for each respondent

Linkage structure can be specified more explicitly (next section)

```{r}
# Overall respondent population paramters ~ N(10,000, 1) or rbeta(10,000)
# T is number of tests
# nt is a vector with length = nt, contains number of items for each test T
# np is a vector with length = np, contains number of respondents for each test T
###### np respondents are drawn at random from the true population

#### Generation of difficulty paramters
# avg_diff is a vector of average easiness level(easy=2, medium=0, hard=-2) of length nt
# difficulties are iid or are correlated with each other - given as Sigma




numTests = 3
nt = c(50, 100, 100)
np= c(1000, 1000, 1000)
avg_diff = c(1, 0, -1) # Easy, Medium, Hard
Sigma= diag(numTests)
#Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
x = gendata(T, nt=nt, np=np, avg_diff, 
               diff_dist="iid", ability_type= "normal")
data=x[[1]] ## Item and respondent level response data for each test
true.ability= x[[2]]
sum_scores= x[[3]]





```

Plot difficulty

```{r}
#### Only observed sum scores for observed score equating

data.frame(sum_scores) |> 
  pivot_longer(cols= starts_with("totalScore"), 
               names_to= "test",
               values_to= "totalScore") |>
  ggplot(aes(x=totalScore, group=test, fill=test, color=test)) + 
  geom_density(alpha = 0.1)



## Common students for overlap
#sum_scores |> dplyr::select(totalScore1, totalScore2) |> drop_na() |> nrow()

```

### Example 2

Bigger network structure

```{r}
#### Example 2: bigger network
numT = 10
nt = 50
np= 1000
avg_diff = sample(c(-1,0,1), numT, replace=T) # Easy, Medium, Hard
Sigma= diag(numT)
#Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
x = gendata(T=numT, nt=nt, np=np, avg_diff, 
               diff_dist="iid", ability_type= "normal")
data=x[[1]] ## Item and respondent level response data for each test
true.ability= x[[2]]
sum_scores= x[[3]]

#### Only observed sum scores for observed score equating


data.frame(sum_scores) |> 
  pivot_longer(cols= starts_with("totalScore"), 
               names_to= "test",
               values_to= "totalScore") |>
  ggplot(aes(x=totalScore, group=test, fill=test, color=test)) + 
  geom_density(alpha = 0.1)

## Common students for overlap
#sum_scores |> dplyr::select(totalScore1, totalScore2) |> drop_na() |> nrow()

```

# Linkage structure

Input is:

-   Data frame where each row is a student, and each column is a test

    -   Cell has test score for a student if available, otherwise NA

-   Linkage structure

    -   Edges between tests, defined as:

        -   Exists or not (1/0)

        -   Number of overlapping students

Output is:

-   A new data frame, where students have overlapping test scores as specified in input structure

-   Overall mean test scores and SD for each test

Example of simulated linkage

-   Default structures with Normal distribution of ability and test scores, with following linkage

    -   Two tests/ nodes

    -   Three tests/ nodes, all interconnected

    -   3 tests connected in a straight line

    -   Two triangles connected with one node overlap

### Example 1: 3 tests all interconnected

```{r}


numT = 3
nt = 50
np= 2400
avg_diff = sample(c(-1,0,1), numT, replace=T) # Easy, Medium, Hard
Sigma= diag(numT)
#Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
x = gendata(T=numT, nt=nt, np=np, avg_diff, 
               diff_dist="iid", ability_type= "normal")
data=x[[1]] ## Item and respondent level response data for each test
true.ability= x[[2]]
sum_scores= x[[3]]

### Specify overlap, specified as (1) Existence of edge (2) number of respondents overlapping 
####### Case 1, entire population is overlapping
edges<-matrix(rep(1,9),3,3)
noverlap= matrix(c(600, 200, 200, 
                   200, 600, 200, 
                   200, 200, 600),3,3, byrow=T)


data_3nodes= add_missing_3nodes(sum_scores, data, edges, noverlap)



```

### Example 2: straight line connections

```{r}


numT=5 
nt = 50
np= 100


avg_diff = sample(c(-1,0,1), numT, replace=T) # Easy, Medium, Hard
Sigma= diag(numT)
#Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
x = gendata(T=numT, nt=nt, np=np, avg_diff, 
               diff_dist="iid", ability_type= "normal")




data=x[[1]] ## Item and respondent level response data for each test
true.ability= x[[2]]
sum_scores= x[[3]]

### Specify overlap, specified as (1) Overlap between adjacent nodes (2) number of respondents only in one test 

### Example 1
#### 5 tests, 10 country overlaps, 12 countries only took each test (no overlap)
cty_overlap= 10
ncountries_test= 12
ntests= 5
data_missing= addmissing_single_link(data[,-1], cty_overlap, ncountries_test,ntests)
data_missing=bind_cols(id=1:nrow(data_missing),data_missing)
   
   
cty_overlap= 5
ncountries_test= 16
ntests= 5
data_missing= addmissing_single_link(data[,-1], cty_overlap, ncountries_test,ntests)
data_missing=bind_cols(id=1:nrow(data_missing),data_missing)

   
cty_overlap= 2
ncountries_test= c(18,18,18,18,20)
ntests= 5
data_missing= addmissing_single_link(data[,-1], cty_overlap, ncountries_test,ntests)
data_missing=bind_cols(id=1:nrow(data_missing),data_missing)

cty_overlap= 1
ncountries_test= c(19,19,19,19,20)
ntests= 5
data_missing= addmissing_single_link(data[,-1], cty_overlap, ncountries_test,ntests)
data_missing=bind_cols(id=1:nrow(data_missing),data_missing)




```

```{r}




# 
# 
# 
# ####### Case 5 Two tests are interconnected to all tests
# edges<-diag(6)
# edges[outer(1:6, 1:6, function(i,j) i==1 | j==1 | i==2 | j==2)] <- 1

```

## Plot the network

```{r}

## Get adjacency matrix
adj.matrix= getadjmatrix(data_3nodes[,-1])

edgeNetwork <- graph_from_adjacency_matrix(adj.matrix, mode="undirected", weighted = T)
plot(edgeNetwork)
  
  


```

## Estimate performance of equating

```{r}

### Variable name has to be total score 1,2,3
### Input is dataframe with id in first column, and scores in remaining columns
### Name of score variables is totalScore
x=data_3nodes[,-1]
id=data_3nodes[,1]
x_equated= lineq(x,id)

## New scores
colMeans(x_equated[[1]][,-1], na.rm = T)


info= x_equated[[2]]
plot(NULL,xlim=c(0,12),ylim=c(0,35))
for (i in 1:(length(x)-1)) {
  sapply(info,'[[',i)->z
  z<-t(z)
  lines(z[,1],z[,2],type='l')
}


```
