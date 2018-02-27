nFixRG<- function(data){
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; o<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL;
  dataN<- NULL; dataT<- NULL; q<- NULL; r<- NULL; sent<- NULL
  FFD<- NULL; nfix1<- NULL; nfix2<-NULL; nfixAll<- NULL; sound<- NULL
  
  library(readr)
  
  cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    design<- read_delim(paste("design/P", toString(i), ".txt", sep= ""), 
                        " ", escape_double = FALSE, trim_ws = TRUE)
    design<- subset(design, item<121)
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
      o<- sort(unique(n$sent))
      
      for(k in 1:length(o)){
        q<- subset(n, sent==o[k])
        #r<- sort(unique(q$word))
        r<- c(3,5,7,9,11)
        w<- c(1,2,3,4,5)
        
        for(l in 1:length(r)){ # for each word in sentence k
          word[l]<- r[l] # critical word  
          sub[l]<- n$sub[1]
          item[l]<- n$item[1]
          seq[l]<- n$seq[1]
          cond[l]<- n$cond[1]
          sent[l]<- o[k]
          
          
          ### Refixation conditional:
          p<- subset(q, word==r[l])
          sound[l]<- p$sound[1]
          
          if(is.na(sound[l])){ # target was skipped, but next word was fixated
            if(cond[l]==1){
              sound[l]<- 'SLC'
            }
            if(cond[l]==2){
              sound[l]<- 'STD'
            }
            
            if(cond[l]==3){
              a<- which(design$item==item[l] & design$pos== w[l])
              if(length(a)==0){
                sound[l]<- 'STD'
              } else{
                sound[l]<- 'DEV'
              }
              
            }
            
          }
          
          p<- subset(q, word==r[l] | word==r[l]+1)
          
          # first-pass fixations:
          p1<- subset(p, intrasent_regr==0)
          p2<- subset(p, intrasent_regr==1)
            
          
          ## code fixations
           nfix1[l]<- nrow(p1)
           nfix2[l]<- nrow(p2)
           nfixAll[l]<- nrow(p1)+ nrow(p2)
          
        } # end of l
        
        dataT<- data.frame(sub, item, word, seq, sent, cond, nfix1, nfix2, nfixAll, sound)
        sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
        p1<- NULL; p2<- NULL; q<- NULL; r<- NULL; sound<- NULL
        nfix1<- NULL; nfix2<- NULL; nfixAll<-NULL; TVT<- NULL
        
        dataN<- rbind(dataN, dataT)
        
      } # end of k
      
    } # end of j
    
  } # end of i
  
  return(dataN)
}