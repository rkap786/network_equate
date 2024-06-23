# Overall respondent population paramters ~ N(10,000, 1) or rbeta(10,000)
# T is number of tests
# nt is a vector with length = nt, contains number of items for each test T
# np is a vector with length = np, contains number of respondents for each test T
###### np respondents are drawn at random from the true population

#### Generation of difficulty paramters
# avg_diff is a vector of average difficulty level(easy, medium, hard) of length nt
# difficulties are all iid normally distributed


gendata = function(T, nt, np, avg_diff=0, diff_dist="iid", ability_type="normal", 
                    Sigma) {
  library(mirt)
  library(MASS)
  library(dplyr)
  
  #avg_diff = 0 if med, 2 if easy and -2 if hard
  if(length(avg_diff)==1) {avg_diff = rep(avg_diff, T)}
  if(length(nt)==1) {nt = rep(nt, T)}
  if(length(np)==1) {np = rep(np, T)}
  
  ## Simulate ability for full population
  # sample population
  np.max= max(np)
  if(ability_type=="normal") {th_overall = rnorm(np.max, 0, 1)}
  if(ability_type=="skew") {th_overall = rbeta(np.max, 5, 2)}
  th = bind_cols(id=1:np.max, th=th_overall)
  th.output=th
  
  ## Simulate test difficulty
  b_tests= list()
  if(diff_dist=="iid") {
    for (test in 1:T) {
      b_tests[[test]] = rnorm(nt[test], avg_diff[test],1)   
    }
  }
  
  if(diff_dist=="mvn") {
    # Sigma= diag(T)
    # Sigma[outer(1:T, 1:T, function(i,j) i!=j)] <- 0.5
    nt.max= max(nt)
    b.mvn= mvrnorm(n=nt.max, avg_diff, Sigma= Sigma)
    for (test in 1:T) {
      b_tests[[test]] = sample(x=b.mvn[,test], size=nt[test])
    }
  }
  
  ### Now, simulate scores for different tests 
  ### Different tests might have different number of respondents
  resp=list()
  for (i in 1:T) {
    th.sample = sample_n(th, np[i])
    th.input= as.matrix(th.sample[,2])
    df= mirt::simdata(a= rep(1,nt[i]),d=b_tests[[i]], N=np[i], 
                                           itemtype = 'dich', 
                                           Theta = th.input )
    df = bind_cols(id=th.sample[,1], df) |>
      arrange(id)
    resp[[i]] = df
    
  }
  
  ## sumscores from full data
  sum_scores=data.frame(id=1:max(np))
  for (i in 1:T) {
    x= bind_cols(resp[[i]]$id, rowSums(resp[[i]][,-1]))
    names(x) = c("id", paste0("totalScore",i))
    sum_scores= left_join(sum_scores, x, by="id")
  }
  return(list(resp, th.output,sum_scores))
}
