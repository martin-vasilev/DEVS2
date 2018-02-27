
re_map<- function(data, remove=T){
  library(readr)
  dataN<- NULL
  
  for(i in 1:length(unique(data$sub))){
    design<- read_delim(paste("design/P", toString(i), ".txt", sep= ""), 
                        " ", escape_double = FALSE, trim_ws = TRUE)
    design<- subset(design, item<121)
    
    
    s<- subset(data, sub==i)
    s$keep<- NA
    
    for(j in 1:nrow(s)){
      a<- which(design$pos== s$sound[j] & design$item== s$item[j])
      if(length(a)>0){
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
