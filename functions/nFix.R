nFix<- function(data){
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL;
  dataN<- NULL; dataT<- NULL; 
  nfix1<- NULL; nfix2<-NULL; nfixAll<- NULL; sound<- NULL
  
  cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
        
      word<- n$word[1] # critical word  
      sub<- n$sub[1]
      item<- n$item[1]
      seq<- n$seq[1]
      cond<- n$cond[1]
      sound<- n$sound[1]
          
          
      # first-pass fixations:
      p1<- subset(n, intrasent_regr==0)
      # second-pass:
      p2<- subset(n, intrasent_regr==1)
            
          
      ## code fixations
      nfix1<- nrow(p1)
      nfix2<- nrow(p2)
      nfixAll<- nrow(p1)+ nrow(p2)
          
        
      dataT<- data.frame(sub, item, word, seq, cond, nfix1, nfix2, nfixAll, sound)
      sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL;
      p1<- NULL; p2<- NULL; sound<- NULL
      nfix1<- NULL; nfix2<- NULL; nfixAll<-NULL; 
        
      dataN<- rbind(dataN, dataT)

      
    } # end of j
    
  } # end of i
  
  return(dataN)
}