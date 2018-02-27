
less80<- function(data, ppl=14){
  
  dataN<- NULL
  
  data$getOut<- 0
  #cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    #cat(paste(i, " ", sep=""));
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
      o<- subset(n, fix_dur<80)
      op<- which(n$fix_dur<80)
      
      if(nrow(o)>0){
        for(k in 1:nrow(o)){
          p<- which(n$xPos>o$xPos[k] & n$xPos<o$xPos[k]+ppl | n$xPos<o$xPos[k] & n$xPos>o$xPos[k]-ppl)
          if(length(p)>1){
            p<- p[2]
          }
          if(length(p)>0){
            if(n$intrasent_regr[p]==0){ # only if during first-pass reading
              n$fix_dur[p]<- n$fix_dur[p] + n$fix_dur[op[k]]
              cat(sprintf("Subject %i trial %i: merged fixation %i into fixation %i", i, j, n$fix_num[op[k]], n$fix_num[p]))
              cat(sprintf("\n\n"));
              n$getOut[op[k]]<-1
            }

          }
        }
      }
     
      dataN<- rbind(dataN,n) 
    }
  }
  
  dataN<- subset(dataN, getOut==0)
  dataN$getOut<- NULL
  return(dataN)
  
  cat(toupper(sprintf("%f percent of fixations less than 80 ms were merged", 
                  round(((nrow(data)- nrow(dataN))/length(which(data$fix_dur<80)))*100,2))))
  
}