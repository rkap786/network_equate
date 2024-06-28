add_missing_3nodes= function(sum_scores, data) {
  id_selected=c()
  data=sum_scores
  data_missing= data
  Nr= nrow(data_missing)
  Nc= ncol(data_missing)-1
  list=data_missing$id
  
  ### Only in nodes 
  for (i in 1:Nc) {
    ## Individual nodes
    m0= sample(list, noverlap[i,i], replace = F)
    data_missing[m0,-c(1,(i+1))]= NA
  }
  
  
  ### First node with node 2 
  overlap.count=noverlap[1,2] 
  m.new= data_missing[,-4] # total score 2 and ID
  m.new= m.new |> 
    filter(!is.na(totalScore1) & !is.na(totalScore2)) |>
    pull(id)
  m12_overlap= sample(m.new, overlap.count, replace=F)
  data_missing[m12_overlap, 4]= NA
  
  ### First node with node3
  overlap.count=noverlap[1,3] 
  m.new= data_missing[,-3] # total score 3 and ID
  m.new= m.new |> filter(!is.na(totalScore1) &
                           !is.na(totalScore3)) |> pull(id)
  m13_overlap= sample(m.new, overlap.count, replace=F)
  data_missing[m13_overlap, 3]= NA
  
  
  ### Node 2 & node 3
  overlap.count=noverlap[2,3] 
  m.new= data_missing[,-2] # total score 3 and ID
  m.new= m.new |> filter(!is.na(totalScore2) &
                           !is.na(totalScore3)) |> pull(id)
  m23_overlap= sample(m.new, overlap.count, replace=F)
  data_missing[m23_overlap, 2]= NA
  return(data_missing)
  
}






addmissing_single_link = function(data, cty_overlap, 
                                  ncountries_test, ntests) {
  #Generates missing data matrix given number of countries that took test 2 & 3
  # And country overlap
  # Adjacent tests have some overlap with each other, and no overlap with other tests
  #ncountries_test=c(10,10)
  #data = data_missing
  if(length(cty_overlap==1) & ntests>2) 
  {cty_overlap=rep(cty_overlap,ntests-1)}
  ncountry= nrow(data)
  ctylist=1:ncountry
  cty_selected=c()
  data_missing=data
  
  
  #length(m0)
  # Select sample for test 1
  m0=sample(ctylist, ncountries_test[1]+cty_overlap[1]) # Number of countries in test i
  # Set data according to country selection
  m0_na= rep(NA, ncountry)
  m0_na[m0]=1 #selected countries are not set to NA
  data_missing[,1]= data_missing[,1]*m0_na # test i 
  cty_selected=c(cty_selected, m0)
  
  for (i in 2:ntests) {
    ctylist = setdiff(ctylist, m0)
    # Setup for next country
    #cty_selected= c(cty_selected, m0)
    m1_overlap= sample(m0, cty_overlap[i-1])
    if(i==ntests) {m1_nooverlap= sample(ctylist, ncountries_test[i]) } else
    {m1_nooverlap= sample(ctylist, ncountries_test[i]+cty_overlap[i-1]) }
    m1=sort(c(m1_overlap,m1_nooverlap))
    m1_na= rep(NA, ncountry)
    m1_na[m1]=1
    cty_selected= c(cty_selected, m1_nooverlap)
    m0= m1_nooverlap
    # set scores for test 1 to missing except for country overlap
    data_missing[,i]= data_missing[,i]*m1_na # generate scores for test 3
    
  }
  return(data_missing)
}
