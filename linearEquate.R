getparam= function(d, activeNode=1) {
  d2= d[[activeNode]] ## X
  d1=d[-activeNode] ## Y
  
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

getadjmatrix= function(x){
  ### input only contains scores, no ID column
  #x= data_3nodes[,-1]
  #head(x)
  nnodes= ncol(x)
  names(x) = paste0("totalScore", 1:nnodes)
  adj.matrix= diag(nrow=nnodes, ncol=nnodes)
  #colMeans(x, na.rm=T)
  
  for (i in 1:nnodes) {
    for (j in 1:nnodes) {
      if(i!=j) {
        name1= paste0("totalScore",i)
        name2= paste0("totalScore",j)
        edge= data_3nodes |> 
          filter(!is.na(!!as.symbol(name1)) & !is.na(!!as.symbol(name2))) |> 
          nrow()
        adj.matrix[i,j]= edge
      }
    }
  }
  
  
return(adj.matrix)  
  # edgeNetwork <- graph_from_adjacency_matrix(adj.matrix, mode="undirected", weighted = T)
  # plot(edgeNetwork)
  # 
}


lineq= function(x, id){
library("purrr")
#x=data_3nodes
adj.matrix= getadjmatrix(x)
e= ifelse(adj.matrix>1,1,0)
oldnames= names(x)
nodes=ncol(x)
names(x) = paste0("totalScore", 1:nodes)
## function takes inputs as dataframe with total score in each test; AND
### IDs for each student
x=bind_cols(id=id,x)

t<-0
dt<-.01
info<-list()
diff=1000
v=list()

while (t<12) {
  t<-t+dt
  tmp<-list()
  
  for (i in 1:nodes) {
    varname= paste0("totalScore",i)
    xfilter= x |> filter(!is.na(!!sym(varname))) 
    idlist= xfilter$id
    xfilter= xfilter |> dplyr::select(-id) |> as.list()
    if(t==0.01) {
        v[[i]]= rep(0, length(idlist))
    }
    
    edges<-e[i,-i]
    # Note that equating coeffients update at each iteration
    
    kka= getparam(xfilter,i)[[1]]
    n= length(xfilter[[i]])
    kka= do.call("rbind", replicate( n, kka, simplify = FALSE)) 
    kkb= getparam(xfilter,i)[[2]]
    #xtemp= x[[i]] |> filter(!is.na)
    
    newx = kka + outer(xfilter[[i]], kkb, FUN = "*") #Project score from new estimated scores from all directions
    
    del= newx - xfilter[[i]]  #Matrix of length N * nodes, where nodes is connected nodes
    f= del%*%diag(edges)  #Column multiplication
    f= rowMeans(f, na.rm = T) # Can weight this by SD or N
    #f= ifelse(is.na(f) | is.nan(f),0,f
    
    v0<-v[[i]]
    
    a<-f #let's just say m=1 for all nodes. This might change based on sample size or test error as weight
    v1<-v0+dt*a
    v[[i]]<-v1 #update velocity
    vl<-(v0+v1)/2 #linear approximation
    xfilter[[i]]<-xfilter[[i]]+vl*dt #update position
    xfilter[[i]]= cbind(id=idlist, score=xfilter[[i]])
    xupdated = suppressMessages(left_join(data.frame(id=x$id), xfilter[[i]], copy=T))
    x[,(i+1)]=xupdated[,2]
    
    tmp[[i]]<-c(t,mean(x[,(i+1)], na.rm=TRUE),mean(a, na.rm=T))
  }
  
  info[[as.character(t)]]<-tmp
  m= rbind(unlist(lapply(x[,-1],FUN=mean, na.rm=TRUE)))
  diff_prev=diff
  diff=0
  for (i in 1:(ncol(m)-1)) {
    diff=diff + abs(m[i]-m[i+1])
  }
  #print(diff)
  if(diff>diff_prev) {
    x_prev=x
    break 
    }
  
}
names(x_prev)= c("id", oldnames)
return(list(x_prev, info))
}
# 
# rbind(unlist(lapply(x[,-1],FUN=mean, na.rm=T)))
# rbind(unlist(lapply(x[,-1],FUN=sd, na.rm=T)))
# rbind(unlist(lapply(x_prev[,-1],FUN=mean, na.rm=T)))
# rbind(unlist(lapply(x_prev[,-1],FUN=sd, na.rm=T)))
# 

# 
# lineq= function(x, id){
#   #x=data_3nodes
#   adj.matrix= getadjmatrix(x)
#   e= ifelse(adj.matrix>1,1,0)
#   oldnames= names(x)
#   nodes=ncol(x)
#   names(x) = paste0("totalScore", 1:nodes)
#   ## function takes inputs as dataframe with total score in each test; AND
#   ### IDs for each student
#   x=bind_cols(id=id,x)
#   
#   t<-0
#   dt<-.01
#   info<-list()
#   diff=1000
#   v=list()
#   
#   while (t<12) {
#     t<-t+dt
#     tmp<-list()
#     
#     for (i in 1:nodes) {
#       varname= paste0("totalScore",i)
#       xfilter= x |> filter(!is.na(!!sym(varname))) 
#       idlist= xfilter$id
#       xfilter= xfilter |> dplyr::select(-id) |> as.list()
#       if(t==0.01) {
#         v[[i]]= rep(0, length(idlist))
#       }
#       
#       edges<-e[i,-i]
#       # Note that equating coeffients update at each iteration
#       kka= getparam(xfilter,i)[[1]]
#       kkb= getparam(xfilter,i)[[2]]
#       #xtemp= x[[i]] |> filter(!is.na)
#       newx = kka + outer(xfilter[[i]], kkb, FUN = "*") #new estimated means from all directions
#       ## X converted to other scales
#       del= newx - xfilter[[i]]  #Matrix of length N * nodes, where nodes is connected nodes
#       f= del%*%diag(edges)  #Column multiplication
#       f= rowMeans(f, na.rm = T) # Can weight this by SD or N
#       #f= ifelse(is.na(f) | is.nan(f),0,f
#       
#       v0<-v[[i]]
#       
#       a<-f #let's just say m=1 for all nodes. This might change based on sample size or test error as weight
#       v1<-v0+dt*a
#       v[[i]]<-v1 #update velocity
#       vl<-(v0+v1)/2 #linear approximation
#       xfilter[[i]]<-xfilter[[i]]+vl*dt #update position
#       xfilter[[i]]= cbind(id=idlist, score=xfilter[[i]])
#       xupdated = suppressMessages(left_join(data.frame(id=x$id), xfilter[[i]], copy=T))
#       x[,(i+1)]=xupdated[,2]
#       
#       tmp[[i]]<-c(t,mean(x[,(i+1)], na.rm=TRUE),mean(a, na.rm=T))
#     }
#     
#     info[[as.character(t)]]<-tmp
#     m= rbind(unlist(lapply(x[,-1],FUN=mean, na.rm=TRUE)))
#     diff_prev=diff
#     diff=0
#     for (i in 1:(ncol(m)-1)) {
#       diff=diff + abs(m[i]-m[i+1])
#     }
#     #print(diff)
#     if(diff>diff_prev) {
#       x_prev=x
#       break 
#     }
#     
#   }
#   names(x_prev)= c("id", oldnames)
#   return(list(x_prev, info))
# }
# # 
# # rbind(unlist(lapply(x[,-1],FUN=mean, na.rm=T)))
# # rbind(unlist(lapply(x[,-1],FUN=sd, na.rm=T)))
# # rbind(unlist(lapply(x_prev[,-1],FUN=mean, na.rm=T)))
# # rbind(unlist(lapply(x_prev[,-1],FUN=sd, na.rm=T)))
# # 
# 
# 
# 
# 
# 
# 
# 
