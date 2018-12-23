
re_map<- function(data, remove=T){
  library(readr)
  dataN<- NULL
  
  for(i in 1:length(unique(data$sub))){
    design<- read_delim(paste("Experiment/design/P", toString(i), ".txt", sep= ""), 
                        " ", escape_double = FALSE, trim_ws = TRUE)
    design<- subset(design, item<121)
    
    # Dev order:
    design$trial<- NA
    dPos<- which(design$sound=="DEV")
    design$trial[dPos]= 1:length(dPos)
    
    # Std order:
    dPos<- which(design$sound=="STD")
    design$trial[dPos]= 1:length(dPos)
    
    s<- subset(data, sub==i)
    s$keep<- NA
    s$order<- NA
    
    for(j in 1:nrow(s)){
      a<- which(design$pos== s$sound[j] & design$item== s$item[j])
      if(length(a)>0){
        s$keep[j]=1
        s$order[j]= design$trial[a]
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
