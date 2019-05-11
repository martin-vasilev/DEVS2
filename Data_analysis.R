
# Martin R. Vasilev, 2019


rm(list= ls())


#######################################################
#            Main data column explanation:            #
#######################################################

# sub:        subject number
# item:       item (sentence) number from the corpus file
# cond:       condition number (2= standard, 3= novel)
# seq:        trial sequence, or the order in which trials were presented in the experiment
# trialStart: start of trial time stamp in raw asc file
# trialEnd:   end of trial time stamp in raw asc file
# sound:      target word number on which the sound is played
# sound_type: sound condition (STD= Standard; DEV= Novel sound); NB: DEV is used for historical reasons only (convenience)
# regionS:    start of target word region (empty space before target word), in pixels (x pos)
# regionE:    end of target word region (end of target word), in pixels (x pos)
# tBnd:       timestamp in the raw data showing when the invisible boundary was crossed
# tSFIX:      timestamp corresponding to the start of the next fixation after crossing the boundary
# tPlaySound: timestamp when the command to play the sound was executed
# ISI:        inter-stimulus interval between playing the current and the previous sound (in ms)
# nextFlag:   what's the next flag in the data file after the sound is played (ESACC= end of saccade; EFIX= end of fixation)
# delBnd:     delay (in ms) between crossing the invisible boundary and playing the sound
# delFix:     delay (in ms) in playing the sound relative to the start of the next fixation
# del:        Experimental delay condition (from experiment design)
# SOD:        stimulus onsed delay of playing the sound relative to start of fixation
# prevFix:    x position of previous fixation before crossing the boundary
# nextFix:    x position of next fixation after crossing the boundary
# N1:         duration of next fixation after crossing boundary (i.e., this is when the sound is played)
# N2:         duration of the second fixation after boundary (i.e., fixation after the one when the sound is played)
# sacc_dur:   duration of next saccae after sound is played (in ms)
# sacc Sflag: timestamp of the start of saccade after sound is played
# sacc_peak:  peak saccade velocity for next saccade after sound is played (in deg/s)
# sacc_vel:   average saccade velocity for next saccade after sound is played (in deg/s)
# sacc_ampl:  amplitude of next saccade after playing sound
# order:      sound order in experiment
# word:       word number in the sentence
# Trialt:     duration of the trial (in ms); does not include questions, only reading time
# next_sacc:  amplitude of the next saccade in letters (after playing sound)



# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2", "mgcv", "itsadug") # list of used packages:

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
print(mQuest2)

# Convert to percetages:
mQuest2[,2:3]<- mQuest2[,2:3]*100


contrasts(q$cond)


if(!file.exists("Models/GLM1.Rda")){
  # doesn't converge with cond for items
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

load("data/dat.Rda")

DesFix<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
              measure=c("N1"), na.rm=TRUE)
mFix<- cast(DesFix, sound_type+del ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
print(mFix)
write.csv2(mFix[c(1,3,2,4), ], file= "Descriptives/FFD.csv")


colnames(mFix)<- c("Sound", "Delay", "Mean", "SD")
mFix$SE<- mFix$SD/sqrt(length(unique(dat$sub)))
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


# sound timing:
t0<- subset(dat, del== 0)
mean(t0$SOD)

t120<- subset(dat, del== 120 & delFix> -50)
mean(t120$SOD)

###### LMM analysis:

# Note, for historical reasons (i.e., ease of programming) I'm still using "DEV" to denote the different (deviant) sound.
# However, in this experiment a novel sound was used, so "DEV"= Novel.

# check contrast coding
dat$sound_type<- as.factor(dat$sound_type)
dat$sound_type<- factor(dat$sound_type, levels= c("STD", "DEV"))
contrasts(dat$sound_type)#<- c(1, 0)


dat$del<- as.factor(dat$del)
contrasts(dat$del)

if(!file.exists("Models/LM1.Rda")){
  # doesn't converge with delay slope for items
  summary(LM1<-lmer(log(N1) ~ sound_type*del + (sound_type+del|sub)+ (sound_type|item),
            data= dat, REML= TRUE))
  save(LM1, file= "Models/LM1.Rda")
  
}else{
  load("Models/LM1.Rda")
}

SM1<- round(coef(summary(LM1)),3)

rownames(SM1)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SM1, file = "Models/SM1_FixDur.csv")


# Calculate effect sizes:

# main effect of sound:
mEffS<- cast(DesFix, sound_type ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
subs<- cast(DesFix, sound_type+sub ~ variable
                    ,function(x) c(M=signif(mean(x),3)
                                   , SD= sd(x) ))
std<- subset(subs, sound_type== "STD")
dev<- subset(subs, sound_type== "DEV")
corr_main<- cor(std$N1_M, dev$N1_M) # correlation between means

source("https://raw.githubusercontent.com/martin-vasilev/reading_sounds/master/Functions/effect_sizes.R")

SoundMainES<- Cohens_d(M_C = mEffS$N1_M[mEffS$sound_type=="STD"], M_E = mEffS$N1_M[mEffS$sound_type=="DEV"],
                       S_C = mEffS$N1_SD[mEffS$sound_type=="STD"], S_E = mEffS$N1_SD[mEffS$sound_type=="DEV"],
                       N = length(unique(dat$sub)), r = corr_main, design = "within")

# 120 ms delay only:
Sound120<- Cohens_d(M_C = mFix$Mean[mFix$Sound== "Standard" & mFix$Delay==120], 
                    M_E = mFix$Mean[mFix$Sound== "Novel" & mFix$Delay==120], 
                    S_C = mFix$SD[mFix$Sound== "Standard" & mFix$Delay==120], 
                    S_E = mFix$SD[mFix$Sound== "Novel" & mFix$Delay==120], 
                    N = length(unique(dat$sub)), r = corr_main, design = "within")

# 0 ms delay only:
Sound0<- Cohens_d(M_C = mFix$Mean[mFix$Sound== "Standard" & mFix$Delay==0], 
                    M_E = mFix$Mean[mFix$Sound== "Novel" & mFix$Delay==0], 
                    S_C = mFix$SD[mFix$Sound== "Standard" & mFix$Delay==0], 
                    S_E = mFix$SD[mFix$Sound== "Novel" & mFix$Delay==0], 
                    N = length(unique(dat$sub)), r = corr_main, design = "within")

###########################
#      SACCADE DATA:      #
###########################

# remove 2 outliers:
dat<- subset(dat, sacc_vel<1000)

# Descriptives:
DesSacc<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
              measure=c('sacc_dur', 'sacc_peak', 'sacc_vel', 'next_sacc'), na.rm=TRUE)
mSacc<- cast(DesSacc, sound_type+del ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
write.csv2(mSacc[c(1,3,2,4), ], file= "Descriptives/Sacc.csv")

####
# LMM analyses:

contrasts(dat$sound_type)
contrasts(dat$del)

# saccade duration:

if(!file.exists("Models/LM2.Rda")){
  summary(LM2<- lmer(log(sacc_dur) ~ sound_type*del + (sound_type+del|sub)+ (sound_type+del|item),
             data= dat))
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

SLM2<- round(coef(summary(LM2)), 3)
rownames(SLM2)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM2, file = "Models/SLM2_saccDur.csv")

####
# saccade peak velocity:
if(!file.exists("Models/LM3.Rda")){
  summary(LM3<- lmer(sacc_peak ~ sound_type*del + (sound_type+del|sub)+ (sound_type+del|item),
             data= dat))
  save(LM3, file= "Models/LM3.Rda")
}else{
  load("Models/LM3.Rda")
}

SLM3<- round(coef(summary(LM3)), 3)
rownames(SLM3)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM3, file = "Models/SLM3_PeakVel.csv")

####
# average saccade velocity:
if(!file.exists("Models/LM4.Rda")){
  summary(LM4<- lmer(sacc_vel ~ sound_type*del + (sound_type+del|sub)+ (sound_type+del|item),
             data= dat))
  save(LM4, file= "Models/LM4.Rda")
}else{
  load("Models/LM4.Rda")
}

SLM4<- round(coef(summary(LM4)), 3)
rownames(SLM4)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM4, file = "Models/SLM4_AvgVel.csv")

####
# saccade amplitude:
if(!file.exists("Models/LM5.Rda")){
  summary(LM5<- lmer(next_sacc ~ sound_type*del + (sound_type+del|sub)+ (sound_type+del|item),
             data= dat))
  save(LM5, file= "Models/LM5.Rda")
}else{
  load("Models/LM5.Rda")
}

SLM5<- round(coef(summary(LM5)), 3)
rownames(SLM5)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM5, file = "Models/SLM5_SaccAmpl.csv")


###########################
#      GAMM ANALYSIS:     #
###########################

summary(gam1 <- bam(N1 ~  s(order, bs = "cr", k = 10) +sound_type, data= sound_check))
plot(gam1)


gam1 <- bam(N1 ~ sound_type+
#              s(order, bs="cr") +
              s(sub, bs="re") +
              s(sub, sound_type, bs="re") +
              s(order, by= sound_type),
            data=sound_check, method="fREML", discrete=TRUE, k=10)

summary(gam1)

# Calculate empircal means /wrt order:
DesFix3<- melt(sound_check, id=c('sub', 'item', 'cond', 'sound_type', 'del', 'order'), 
               measure=c("N1"), na.rm=TRUE)
mFix2<- cast(DesFix3, sound_type+order ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
s1<- subset(mFix2, sound_type== "STD")
s2<- subset(mFix2, sound_type== "DEV")

mDiff<- data.frame(m= s2$N1_M- s1$N1_M, order= 1:60)


library(lattice)
xyplot(x = m~ order,data = mDiff, xlab= "Trial order", ylab= "Mean difference in ms (Novel - STD)",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(0, lty=2)})



# make Plot:

pdf(file= "Plots/GAMM_plot.pdf", width = 7, height = 6)
plot_diff(gam1, view = "order", rm.ranef = F, comp = list(sound_type = c("DEV", 
          "STD")), ylim= c(-20, 50), col = pallete1[2], main= "Novel - Standard difference",
          ylab= "Estimated mean difference (FFD)", xlab= "Trial order", print.summary = T, family= "serif",
          cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)
points(x = mDiff$order, y = mDiff$m, pch= 16, col= pallete1[5])

dev.off()

# plot_diff(gam1, view = "order", rm.ranef = T, comp = list(sound_type = c("DEV", 
#            "STD")), ylim= c(-20, 50), col = "darkblue", main= "Novel - Standard difference",
#           ylab= "Estimated mean difference (FFD)", xlab= "Trial order", print.summary = T)
# points(x = mDiff$order, y = mDiff$m, pch= 23, col= "darkorange")
