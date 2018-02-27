
# Martin R. Vasilev, 2017

paraFix<- function(data_list= "preproc/files.txt", ResX= 1920, ResY=1080, maxtrial= 120, align=TRUE, plot=TRUE, keepLastFix=TRUE){
  
  # Load functions:
  source("functions/utility.R")
  #source("functions/plot_fix.R")
  
  data<- readLines(data_list, warn=F)

  raw_fix<- NULL; RFalign<- NULL
  
  for (i in 1:length(data)){ # for each subject..
    #  i=1; # temporary
    
    cat(sprintf("Processing subject %i", i)); cat("\n")  
    cat(sprintf("Loading data %s ...", data[i])); 
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n") 
    trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      text<- get_text(file[trial_db$ID[j]:trial_db$start[j]])
      
      if(text[1]!=0){ # if trial contained text
        coords<- get_coord(text)
        map<- coord_map(coords, x=ResX, y= ResY)
        raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i, ResX, ResY, keepLastFix)
        
        # Align fixations:
        if(align){
          RFalignTemp<- reAlign(raw_fix_temp, coords, map, ResX, ResY)
          RFalign<- rbind(RFalign, RFalignTemp)
        }
        
        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial  
          # create picture of fixations:
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY)
        } 
      } else{ # if there was no text in trial, just extract fixations
        raw_fix_temp<- parse_fix(file, map=0, coords=0, trial_db[j,], i, ResX, ResY, keepLastFix, hasText=FALSE)
        # create picture of fixations:
        
        if(length(raw_fix_temp)>1 & plot==TRUE){ # if data was extracted from trial 
          plot_fix(coords, raw_fix_temp, i, j, ResX, ResY, hasText = FALSE)
        }
      }
      

      raw_fix<- rbind(raw_fix, raw_fix_temp) 
      
      cat(toString(j)); cat(" ")
    } # end of item loop
    
    cat("\n DONE \n \n"); 
  } # end of subject loop
  
  
  #text<- get_text(file)
  #coords<- get_coord(text)
  #map<- coord_map(coords, x=ResX, y= ResY)
  
  #raw_fix<- parse_fix(file, map, coords)
  
  #cat("Saving data."); 
  #save(raw_fix, file= "data/raw_fix.Rda"); cat(".")
  #write.csv(raw_fix, file= "data/raw_fix.csv"); cat(".")
  cat("\n \n All Done!"); 
  
  if(align){
    return(RFalign)
  }else{
    return(raw_fix)
  }
  
  
}
