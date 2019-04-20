
# Martin R. Vasilev, 2019

rm(list= ls())


### Note:

# In the data, condition 2 is the standard sound and condition 3 is the novel sound


# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2") # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}


######################
# Question accuracy: #  
######################

load("Data/quest.Rda")

q$cond<- as.factor(q$cond)
levels(q$cond)<- c("standard", "novel")

library(reshape)
DesQuest<- melt(q, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
# min accuracy:
min(mQuest$accuracy_M)*100


DesQuest2<- melt(q, id=c('sub', 'item', 'cond'), 
                 measure=c("accuracy"), na.rm=TRUE)
mQuest2<- cast(DesQuest2, cond ~ variable
               ,function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

# Convert to percetages:
mQuest2[,2:3]<- mQuest2[,2:3]*100


contrasts(q$cond)


if(!file.exists("Models/GLM1.Rda")){
  GLM1<- glmer(accuracy ~ cond + (cond|sub)+ (1|item), family= binomial, data= q)
  save(GLM1, file= "Models/GLM1.Rda")
} else{
  load("Models/GLM1.Rda")
}

SGM1<- coef(summary(GLM1))
SGM1


###########################
# FIXATION DURATION DATA: #
###########################

load("data/sound_check.Rda")

DesFix<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
              measure=c("N1"), na.rm=TRUE)
mFix<- cast(DesFix, sound_type+del ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

colnames(mFix)<- c("Sound", "Delay", "Mean", "SD")
mFix$SE<- mFix$SD/sqrt(64)
mFix$Delay<- as.factor(mFix$Delay)
mFix$Sound<- as.factor(mFix$Sound)
levels(mFix$Sound)<- c("Novel", "Standard")

Plot <-ggplot(mFix, aes(x= Delay, y= Mean, group= Sound, colour= Sound, shape= Sound,
                        ymin= Mean- SE, ymax= Mean+SE)) +
  theme_bw(16) +geom_point(size=5)+ geom_line(size=2)+ 
  scale_colour_brewer(palette="Accent")+ xlab("Sound onset delay (in ms)")+
  ylab("First fixation duration (in ms)")+ geom_ribbon(alpha=0.1, colour=NA)+
  theme(legend.position="bottom", legend.key.width=unit(1.5,"cm")); Plot

ggsave(Plot, filename = "Plots/FFD_mainFig.pdf", width = 5, height = 5)


###### LMM analysis:

# Note, for historical reasons (i.e., ease of programming) I'm still using "DEV" to denote the different (deviant) sound.
# However, in this experiment a novel sound was used, so "DEV"= Novel.

# check contrast coding
sound_check$sound_type<- as.factor(sound_check$sound_type)
sound_check$sound_type<- factor(sound_check$sound_type, levels= c("STD", "DEV"))
contrasts(sound_check$sound_type)#<- c(1, 0)


sound_check$del<- as.factor(sound_check$del)
contrasts(sound_check$del)

if(!file.exists("Models/LM1.Rda")){
  LM1<-lmer(log(N1) ~ sound_type*del + (sound_type+del|sub)+ (sound_type+del|item),
            data= sound_check)
  save(LM1, file= "Models/LM1.Rda")
}else{
  load("Models/LM1.Rda")
}

SM1<- round(coef(summary(LM1)),3)

rownames(SM1)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SM1, file = "Models/")



