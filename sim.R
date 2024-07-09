getparam= function(d, activeNode=1) {
  d1=list()
  c=1
  for (i in 1:length(d)) {
    if(i==activeNode) {d2= d[[i]]}
    if(i!=activeNode) {
      d1[[c]]= d[[i]]
      c=c+1
      }
  }
  
  #d1,d2
  l= length(d1)
  alpha=c()
  beta=c()
  for (i in 1:l) {
    b=sd(d1[[i]],  na.rm=TRUE)/ sd(d2,  na.rm=T)
    a = mean(d1[[i]], na.rm = TRUE) - b*mean(d2, na.rm = TRUE)  
    alpha=c(alpha,a)
    beta=c(beta,b)
  }
  param=list(alpha, beta)
  
  return(param)
}

# kld_base = function(x,y,...){
#   integrand = function(x,y,t){
#     f.x =  approx(density(x)$x,density(x)$y,t)$y
#     f.y =  approx(density(y)$x,density(y)$y,t)$y
#     tmpRatio = f.x *(log2(f.x) - log2(f.y))
#     tmpRatio = ifelse(is.infinite(tmpRatio),0,ifelse(is.na(tmpRatio),0,tmpRatio))
#     return(tmpRatio)
#   }
#   return(stats::integrate(integrand,-Inf,Inf,x = x,y = y,stop.on.error=FALSE)$value)
# }

### 

## Two nodes
### Generate means and SDs of test scores
nodes= 3
## Generate test scores
s1= rnorm(500, 1, 1)
s2= rnorm(100, 2, 2)
s3= rnorm(500, 3, 3)
mu= c(mean(s1), mean(s2), mean(s3))  # mean varies between  0 & 1
sd= c(sd(s1), sd(s2), sd(s3))  # sd varies between  1 & 3
# d= kld_base(s1,s2)

# linlink_12 = getparam(s1,s2) # y=1 x=2 Constants A & B when we are using s2 to predict s1
# linlink_21 = getparam(s2,s1)
#Check: works as predicted 
#s1pred = linlink_12[1] + linlink_12[2]*s2 
#mean(linlink_12[1] + linlink_12[2]*s2) = mean(s1)
# mean(linlink_21[1] + linlink_21[2]*s1) = mean(s2)
# Mean s1 = 0.95, mean s2= 1.92



x<- list(s1,s2, s3) # node positions are mean test scores and sd
original= list(rnorm(100,2,1), rnorm(100), rnorm(100, -3,1))
x=original

v<-cbind(rep(0,100),rep(0,100),rep(0,100)) # this will change when there is force, based on difference in means
e<-matrix(rep(1,9),3,3) # connections between nodes, all nodes are connected here
# k_a<-matrix(c(1,linlink_12[1],linlink_21[1],1), 2,2)
# k_b<-matrix(c(1,linlink_12[2],linlink_21[2],1),2,2)


t<-0
dt<-.01
info<-list()
nodes=3
diff=1
while (t<12) {
  t<-t+dt
  tmp<-list()
  #nodels = length(x[[1]])
  for (i in 1:nodes) {
    edges<-e[i,-i]
  #using equating coefficients as constants doesnt make sense
    # kka<-k_a[i,-i]
    # kkb<-k_b[i,-i]
    kka= getparam(x,i)[[1]]
    kkb= getparam(x,i)[[2]]
    newx = kka + outer(x[[i]], kkb, FUN = "*") #new estimated means from all directions
    del= newx - x[[i]]  #Matrix of length N * nodes, where nodes is connected nodes
    f= del%*%diag(edges)  #Column multiplication
    f= rowMeans(f) # Can weight this by SD or N
    # mean(newx)
    # mean(x[,i])
    # mean(f)
  # direct difference between mean of x does not account for SD of distributions
    #del<-abs(x[[1]][-i]-x[[1]][i]) #No L here 
    # del= abs(newx-x[[1]][i])
    #sign<-x[[1]][i]<x[[1]][-i]
    #del<-ifelse(sign,del,-1*del)
  # Try to minimize KL distance between distributions
    # sign<-mean(x[i])< mean(x[-i])
    # f= sum(d*edges*sign)
    
    #f<-sum(del*sign*edges) #Changing to mean to average effects of two nodes?
    v0<-v[,i]
    
    #f<-f-sign(v0)*0.1*v0^2 #drag  - we can make this a small fraction
    # mean(f)
    a<-f #let's just say m=1 for all nodes. This might change based on sample size or test error as weight
    v1<-v0+dt*a
    v[,i]<-v1 #update velocity
    vl<-(v0+v1)/2 #linear approximation
    x[[i]]<-x[[i]]+vl*dt #update position
    # plot(NULL,xlim=c(-5,5),ylim=c(-1,1),yaxt='n')
    # abline(h=0,col='gray')
    # points(x,rep(0,length(x)),pch=19)
    # for (j in 1:length(x)) segments(x[j],-10,x[j],0,col='gray')
    # text(x,rep(.3,length(x)),1:length(x))
    tmp[[i]]<-c(t,mean(x[[i]]),mean(a))
    # legend("topright",bty='n',legend=as.character(t))
  }
  #x<-x-mean(x) #conservation
  
  info[[as.character(t)]]<-tmp
  m= rbind(unlist(lapply(x,FUN=mean)))
  diff_prev=diff
  diff=0
  for (i in 1:(ncol(m)-1)) {
    diff=diff + abs(m[i]-m[i+1])
  }
  print(diff)
  if(diff>diff_prev & diff_prev<0.01) break
  x_prev=x
}

rbind(unlist(lapply(x,FUN=mean)))
rbind(unlist(lapply(x,FUN=sd)))
rbind(unlist(lapply(x_prev,FUN=mean)))
rbind(unlist(lapply(x_prev,FUN=sd)))


plot(NULL,xlim=c(0,15),ylim=c(-5,5))
for (i in 1:(length(x)-1)) {
  sapply(info,'[[',i)->z
  z<-t(z)
  lines(z[,1],z[,2],type='l')
}

plot(original[[1]],x_prev[[1]])



#####################################################################
#####################################################################
#####################################################################

t<-0
dt<-.01
info<-list()
nodes=3
diff=1
while (t<12) {
  t<-t+dt
  tmp<-list()
  #nodels = length(x[[1]])
  for (i in 1:nodes) {
    edges<-e[i,-i]
    #using equating coefficients as constants doesnt make sense
    # kka<-k_a[i,-i]
    # kkb<-k_b[i,-i]
    kka= getparam(x,i)[[1]]
    kkb= getparam(x,i)[[2]]
    newx = kka + outer(x[[i]], kkb, FUN = "*") #new estimated means from all directions
    del= newx - x[[i]]  #Matrix of length N * nodes, where nodes is connected nodes
    f= del%*%diag(edges)  #Column multiplication
    f= rowMeans(f) # Can weight this by SD or N
    # mean(newx)
    # mean(x[,i])
    # mean(f)
    # direct difference between mean of x does not account for SD of distributions
    #del<-abs(x[[1]][-i]-x[[1]][i]) #No L here 
    # del= abs(newx-x[[1]][i])
    #sign<-x[[1]][i]<x[[1]][-i]
    #del<-ifelse(sign,del,-1*del)
    # Try to minimize KL distance between distributions
    # sign<-mean(x[i])< mean(x[-i])
    # f= sum(d*edges*sign)
    
    #f<-sum(del*sign*edges) #Changing to mean to average effects of two nodes?
    v0<-v[,i]
    f<-f-sign(v0)*0.1*v0^2 #drag  - we can make this a small fraction
    # mean(f)
    a<-f #let's just say m=1 for all nodes. This might change based on sample size or test error as weight
    v1<-v0+dt*a
    v[,i]<-v1 #update velocity
    vl<-(v0+v1)/2 #linear approximation
    x[[i]]<-x[[i]]+vl*dt #update position
    # plot(NULL,xlim=c(-5,5),ylim=c(-1,1),yaxt='n')
    # abline(h=0,col='gray')
    # points(x,rep(0,length(x)),pch=19)
    # for (j in 1:length(x)) segments(x[j],-10,x[j],0,col='gray')
    # text(x,rep(.3,length(x)),1:length(x))
    tmp[[i]]<-c(t,mean(x[[i]]),mean(a))
    # legend("topright",bty='n',legend=as.character(t))
  }
  #x<-x-mean(x) #conservation
  
  info[[as.character(t)]]<-tmp
  m= rbind(unlist(lapply(x,FUN=mean)))
  diff_prev=diff
  diff=0
  for (i in 1:(ncol(m)-1)) {
    diff=diff + abs(m[i]-m[i+1])
  }
  print(diff)
  if(diff>diff_prev & diff_prev<0.01) break
  x_prev=x
}

