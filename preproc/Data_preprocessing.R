
# Martin R. Vasilev, 2017

rm(list=ls())


# Question accuracy:
library(EMreading)

if(!file.exists("Data/quest.Rda")){
  q<- Question(data_list = "D:/Data/DEVS2", maxtrial = 120)
  save(q, file= "Data/quest.Rda")
}else{
  load("Data/quest.Rda")
}


library(reshape)
DesQuest<- melt(q, id=c('sub', 'item', 'cond'), 
              measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
min(mQuest$accuracy_M)

sprintf("All participants had comprehension accuracy greater than %g percent", min(mQuest$accuracy_M))
sprintf("Mean accuracy: %g percent", mean(mQuest$accuracy_M))


if(!file.exists("preproc/sound_check_t.Rda")){
  # First fixation:
  source("functions/soundCheck.R")
  
  sound_check<- soundCheck()
  
  #sound_check$flagDiff<- sound_check$
  
  sound_check<- subset(sound_check, sound!=1)
  
  source("functions/re_map.R")
  sound_check<- re_map(sound_check)
  
  sound_check$word<- NA
  for(i in 1:nrow(sound_check)){
    if(sound_check$sound[i]==2){
      sound_check$word[i]<- 5
    }
    if(sound_check$sound[i]==3){
      sound_check$word[i]<- 7
    }
    if(sound_check$sound[i]==4){
      sound_check$word[i]<- 9
    }
    if(sound_check$sound[i]==5){
      sound_check$word[i]<- 11
    }
  }
  
  
  sound_check$Trialt<- sound_check$trialEnd- sound_check$trialStart
  
  save(sound_check, file= "preproc/sound_check_t.Rda")
  write.csv(sound_check, "preproc/sound_check_t.csv")
  rm(sound_check)
}



##########################
#      filter data:
##########################

load("preproc/sound_check_t.Rda")
nobs<- nrow(sound_check)

# remove blinks on critical words:
blinks2<- which(sound_check$blink2=='Yes')

nblinks<- length(blinks2)
sound_check<- sound_check[-blinks2,]

# remove sounds played after fixation has started:
infix<- which(sound_check$delFix>14 | sound_check$delFix< -100) 
infixn<- length(infix)
sound_check<- sound_check[-infix,]

nhook<- nrow(sound_check)
sound_check<- subset(sound_check, hook=="No")
nhook<- ((nrow(sound_check)-nhook)/nobs)*100

outliers<- which(sound_check$N1<80 | sound_check$N1>1000)
#outliers<- which(sound_check$N1>1000)

outTab<- sound_check[outliers,]
sound_check<- sound_check[-outliers,]
noutliers<- nrow(outTab)

# saccade outliers:
outPeak<- which(sound_check$sacc_peak>1000)
sound_check<- sound_check[-outPeak,]

outAmpl<- which(sound_check$sacc_ampl>15)
sound_check<- sound_check[-outAmpl,]


cat(sprintf("%f percent of data excluded due to blinks", (nblinks/nobs)*100))
cat(sprintf("%f percent of data excluded due to in-fixations", (infixn/nobs)*100))
cat(sprintf("%f percent of data excluded due to hooks", abs(nhook)))
cat(sprintf("%f percent of data excluded as outliers (<80; > 1000ms)",  (noutliers/nobs)*100))
cat(sprintf("%f percent of data excluded as outliers (> 1000 deg/s peak saccade)",  (length(outPeak)/nobs)*100))
cat(sprintf("%f percent of data excluded as outliers (> 15 deg saccade)",  (length(outAmpl)/nobs)*100))
cat(sprintf("%f percent of data remains for analysis", (nrow(sound_check)/nobs)*100))


sound_check$next_sacc<- abs(sound_check$nextFix- sound_check$N1x)/14

dat<- sound_check

# remove some columns we don't need during analysis:
dat$blink<- NULL
dat$blink2<- NULL
dat$prevGood<- NULL
dat$inRegion<- NULL
dat$hook<- NULL
dat$keep<- NULL
dat$N1len<- NULL
dat$N2len<- NULL
dat$skip<- ifelse(dat$onTarget=="Yes", 0, 1)
dat$next_sacc_type<- ifelse(dat$N1x>= dat$regionE, "inter-word", 'intra-word')



### subset data for DPA analysis:

DPA<- sound_check[,c("sub", "N1", "cond", "del")]

colnames(DPA)<- c("subject", "duration", "condition", "delay")
DPA$condition<- DPA$condition-1

DPA_0ms<- subset(DPA, delay==0)
DPA_0ms$delay<- NULL
DPA_0ms<- DPA_0ms[with(DPA_0ms, order(condition,subject)), ]


DPA_120ms<- subset(DPA, delay==120)
DPA_120ms$delay<- NULL
DPA_120ms<- DPA_120ms[with(DPA_120ms, order(condition,subject)), ]

# save files:
write.table(DPA_0ms, file= "DPA/DPA_0ms_keep80s.txt", sep = "\t", quote = F, row.names = F)

write.table(DPA_120ms, file= "DPA/DPA_120ms.txt", sep = "\t", quote = F, row.names = F)


###############################
#   Pre-process fixations:    #
###############################

# extract data from asc files:
if(!file.exists("preproc/dataN.Rda")){
  
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
  
  library(EMreading)
  
  raw_fix<- SingleLine(data_list = "D:/Data/DEVS2", ResX = 1920, ResY = 1080, 
                       maxtrial = 120, tBlink = 50)
  save(raw_fix, file= "preproc/raw_fix_temp.Rda")
  write.csv(raw_fix, "preproc/raw_fix_temp.csv")
  
  # clean up data:
  dataN<- cleanData(raw_fix, removeOutsideText = F, removeBlinks = F, removeSmallFix = F)
  dataN<- dataN[-which(dataN$fix_dur<80), ]
  #dataN<- dataN[-which(dataN$blink==1 | dataN$prev_blink==1 | dataN$after_blink==1 ),]
  
  save(dataN, file= "preproc/dataN.Rda")
  write.csv(dataN, file= "preproc/dataN.csv")
}else{
  load("preproc/dataN.Rda")
}


FD<- wordMeasures(dataN)
FD<- FD[-which(FD$blinks_1stPass==1), ]

fix<- dataN

save(fix, file= "data/fix.Rda")
write.csv2(fix, "data/fix.csv")

save(FD, file= "preproc/FD.Rda")
write.csv(FD, "preproc/FD.csv")

# remove outliers:
out<- which(FD$FFD>800 | FD$GD>2000 | FD$TVT>4000)
a<- FD[out,]

if(length(out)>0){
  FD<- FD[-out,]
}

FD$keep<- 0
FD$keepN1<- 0
FD$sound<- NA
FD$del<- NA
FD$order<- NA

sound_check$sound_type<- as.character(sound_check$sound_type)
sound_check$del<- as.character(sound_check$del)

for(i in 1:nrow(FD)){
  a<- which(sound_check$sub== FD$sub[i] & sound_check$item== FD$item[i] & sound_check$word== FD$word[i])
  
  if(length(a)>0){
    FD$keep[i]<- 1
    FD$sound[i]<- sound_check$sound_type[a]
    FD$del[i]<- sound_check$del[a]
    FD$order[i]<- sound_check$order[a]
    
    b<- which(FD$item== FD$item[i] & FD$sub== FD$sub[i] & FD$word== FD$word[i]+1)
    if(length(b>0)){
      FD$keepN1[b]<- 1
      FD$sound[b]<- sound_check$sound_type[a]
    }
    
  }
}

TW<- subset(FD, keep==1)
N1<- subset(FD, keepN1==1)

save(TW, file= 'preproc/TW.Rda')
write.csv(TW, file= 'preproc/TW.csv')
#save(N1, file='data/N1.Rda')

# code 1st-pass refixation probability:
TW$refix<- ifelse(TW$nfix1>1, 1, 0)
TW$refix[which(TW$nfix1==0)]<- NA

dat$refix_prob<- NA

for(i in 1:nrow(dat)){
  a<- which(TW$sub== dat$sub[i] & TW$item== dat$item[i])
  
  if(length(a)>0){
    dat$refix_prob[i]<- TW$refix[a]
  }
  
}

# save main data
save(dat, file= "data/dat.Rda")
write.csv2(dat, file= "data/dat.csv")


# library(reshape)
# DesFix<- melt(TW, id=c('sub', 'item', 'cond', 'sound', 'del'), 
#               measure=c("FFD", "SFD", "GD", "TVT"), na.rm=TRUE)
# m<- cast(DesFix, sound+del ~ variable
#              ,function(x) c(M=signif(mean(x),3)
#                             , SD= sd(x) ))
# 
# 
# TW$sound<- as.factor(TW$sound)
# TW$sound<- factor(TW$sound, levels= c("STD", "DEV"))
# 
# contrasts(TW$sound)
# 
# TW$del<- as.factor(TW$del)
# contrasts(TW$del)
# 
# library(lme4)
# 
# TW$altGaze= TW$GD- TW$FFD
# TW$altTVT= TW$TVT- TW$GD
# 
# for(i in 1:nrow(TW)){
#   if(!is.na(TW$altGaze[i])){
#     if(TW$altGaze[i]==0){
#       TW$altGaze[i]= NA
#     }
#   }
#   if(!is.na(TW$altTVT[i])){
#     if(TW$altTVT[i]==0){
#       TW$altTVT[i]= NA
#     }
#   }
# }
# 
# # FFD: 
# summary(LM1<-lmer(log(FFD) ~ sound*del + (sound|sub)+ (1|item), data= TW, REML=T))
# 
# 
# # GD:
# summary(LM1<-lmer(log(altGaze) ~ sound*del + (del|sub)+ (1|item), data= TW, REML=T))
# 
# # TVT:
# summary(LM1<-lmer(log(altTVT) ~ sound*del + (sound+del|sub)+ (sound+del|item), data= TW, REML=T))
# 


# sound_check$sound_type<- as.character(sound_check$sound_type)
# sound_check$del<- as.character(sound_check$del)
# 
# fix$keep<- 0
# fix$keepN1<- 0
# fix$sound<- NA
# fix$del<- NA
# 
# for(i in 1:nrow(fix)){
#   a<- which(sound_check$sub== fix$sub[i] & sound_check$item== fix$item[i] & sound_check$word== fix$word[i])
#   
#   if(length(a)>0){
#     fix$keep[i]<- 1
#     fix$sound[i]<- sound_check$sound_type[a]
#     fix$del[i]<- sound_check$del[a]
#     
#     b<- which(fix$item== fix$item[i] & fix$sub== fix$sub[i] & fix$word== fix$word[i]+1)
#     if(length(b>0)){
#       fix$keepN1[b]<- 1
#       fix$sound[b]<- sound_check$sound_type[a]
#     }
#     
#   }
# }
# 
# TWraw<- subset(fix, keep==1)


# source("functions/nFix.R")
# nFix<- nFix(TWraw)
# 
# DesFix<- melt(TWraw, id=c('sub', 'item', 'cond', 'sound', 'del'), 
#               measure=c("sacc_dur", "sacc_len"), na.rm=TRUE)
# m<- cast(DesFix, sound+del ~ variable
#          ,function(x) c(M=signif(mean(x),3)
#                         , SD= sd(x) ))
# 
# 
# DesnFix<- melt(nFix, id=c('sub', 'item', 'cond', 'sound', 'delay'), 
#               measure=c("nfix1", "nfixAll"), na.rm=TRUE)
# mnFix<- cast(DesnFix, sound+delay ~ variable
#          ,function(x) c(M=signif(mean(x),3)
#                         , SD= sd(x) ))
# 
# 
# DesFix<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
#               measure=c("Trialt"), na.rm=TRUE)
# m<- cast(DesFix, sound_type ~ variable
#          ,function(x) c(M=signif(mean(x),3)
#                         , SD= sd(x) ))
