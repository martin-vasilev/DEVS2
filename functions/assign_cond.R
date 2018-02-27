
assign_cond<- function(sc, rf){

  message('Assigning sound conditions to fixations...')
  

  sc$word<- NULL
  for(i in 1:nrow(sc)){
    if(sc$sound[i]==1){
      sc$word[i]<- 3
    }
    
    if(sc$sound[i]==2){
      sc$word[i]<- 5
    }
    
    if(sc$sound[i]==3){
      sc$word[i]<- 7
    }
    
    if(sc$sound[i]==4){
      sc$word[i]<- 9
    }
    
    if(sc$sound[i]==5){
      sc$word[i]<- 11
    }
  }
  
  rf$out<- 0
  rf$sound<- NA
  for(i in 1:nrow(rf)){
    
    if(is.element(rf$word[i], c(3,5,7,9,11))){
      a<- which(sc$sub== rf$sub[i] & sc$item== rf$item[i] & sc$word== rf$word[i])
      
      if(length(a)==0){
        rf$out[i]<-1
      } else{
        rf$sound[i]<- sc$sound_type[a]
      }
    }
    
    if(is.element(rf$word[i], c(3,5,7,9,11)) & rf$cond[i]==1){
      rf$sound[i]<- 'SLC'
    }
    
  }
  
  return(rf)
  
}