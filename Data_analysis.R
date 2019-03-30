
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




