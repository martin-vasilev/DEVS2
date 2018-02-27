
reAlign<- function(rawfix, coords, map, ResX, ResY){
  
  old<- rawfix
  
  #---------------------------------------
  # get some info about position of text:
  #---------------------------------------
  
  ystart<- coords$y1[1] # start of first line on y-axis
  yend<- coords$y2[nrow(coords)] # end of last line on y-axis
  nlines<- max(coords$line) # number of lines in trial
  
  # x start position of each line
  xstart<- matrix(nrow = nlines, ncol = 2, data = 0)
  xstart[1:nlines,1]<- 1:nlines
  
  # x end position of each line
  xend<- matrix(nrow = nlines, ncol = 2, data = 0)
  xend[1:nlines,1]<- 1:nlines
  
  for(i in 1:nlines){
    a<- subset(coords, line==i)
    xstart[i,2]<- a$x1[1]
    xend[i,2]<- a$x2[nrow(a)]
  }
  
  
  #----------------------------------------------------
  #     Re-alignment of fixations outside the text box:
  #----------------------------------------------------
  
  rawfix$reAligned<- "No"
  rawfix$prevX<- NA
  rawfix$prevY<- NA
  rawfix$reason<-NA
  
  for(i in 1:nrow(rawfix)){
    
    if(is.na(rawfix$sent[i])){ # if fixation needs to be re-aligned
      
      fillIn<- FALSE
      
      #------------#
      # Problem 1: # Fixation is above the first line of text
      #------------#
      #-->  Solution: Bring it down to the first line of text (y pos)
      
      if(rawfix$yPos[i]<ystart & rawfix$yPos[i]> 1){
        
        rawfix$prevX[i]<- rawfix$xPos[i]
        rawfix$prevY[i]<- rawfix$yPos[i]
        rawfix$reAligned[i]<- "Yes"
        rawfix$yPos[i]<- ystart+1
        fillIn<- TRUE
        rawfix$reason[i]<- 'P1'
      } # end of Problem 1
      
      
      
      #------------#
      # Problem 2: # Fixation is below the last line of text
      #------------#
      #-->  Solution: Bring it up to the last line of text (y pos)
      
      if(rawfix$yPos[i]>yend & rawfix$yPos[i]< ResY){
        rawfix$prevX[i]<- rawfix$xPos[i]
        rawfix$prevY[i]<- rawfix$yPos[i]
        rawfix$reAligned[i]<- "Yes"
        rawfix$yPos[i]<- yend-1
        fillIn<- TRUE
        rawfix$reason[i]<- 'P2'
      } # end of Problem 2
      
      
      #-----------------------#
      # Fill in missing info: #
      #-----------------------#
      
      if(fillIn){
        loc<- map[rawfix$yPos[i], rawfix$xPos[i]]
        rawfix$sent[i]<- coords$sent[loc]
        rawfix$word[i]<- coords$word[loc]
        rawfix$line[i]<- coords$line[loc]
        if(i==1){
          rawfix$max_word[i]<- rawfix$word[i]
          rawfix$max_sent[i]<- rawfix$sent[i]
        } else{
          if(rawfix$max_sent[i-1]< rawfix$sent[i]){
            rawfix$max_sent[i]<- rawfix$sent[i]
          } else{
            rawfix$max_sent[i]<- rawfix$max_sent[i-1]
          }
          
          if(!is.na(rawfix$max_word[i-1])){
            if(rawfix$max_word[i-1]< rawfix$word[i]){
              rawfix$max_word[i]<- rawfix$word[i]
            } else{
              rawfix$max_word[i]<- rawfix$max_word[i-1]
            }
          }
          
        }
      } # end of fill in
      
      
    }
  }
  
  return(rawfix)
  
}