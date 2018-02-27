
map_by_pos<- function(data, remove=T){
  library(readr)
  dataN<- NULL
  
  for(i in 1:length(unique(data$sub))){
    design<- read_delim(paste("design/P", toString(i), ".txt", sep= ""), 
                        " ", escape_double = FALSE, trim_ws = TRUE)
    design<- subset(design, item<121)
    
    design$word<- NULL
    for(j in 1:nrow(design)){
      if(design$pos[j]==1){
        design$word[j]<- 3
      }
      if(design$pos[j]==2){
        design$word[j]<- 5
      }
      if(design$pos[j]==3){
        design$word[j]<- 7
      }
      if(design$pos[j]==4){
        design$word[j]<- 9
      }
      if(design$pos[j]==5){
        design$word[j]<- 11
      }
      
    }
    
    s<- subset(data, sub==i)
    s$keep<- NA
    
    for(j in 1:nrow(s)){
      a<- which(design$item== s$item[j])
      if(s$word[j]== design$word[a]){
        s$keep[j]=1
      }else{
        s$keep[j]=0
      }
    }
    
    if(remove){
      s<- subset(s, keep==1)
    }
    
    dataN<- rbind(dataN, s)
    
  }
  
  return(dataN)
  
}
