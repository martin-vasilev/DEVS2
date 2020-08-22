
# Martin R. Vasilev, 2019


rm(list= ls())


#######################################################
#            Main data column explanation:            #
#######################################################

# sub:            subject number
# item:           item (sentence) number from the corpus file
# cond:           condition number (2= standard, 3= novel)
# seq:            trial sequence, or the order in which trials were presented in the experiment
# trialStart:     start of trial time stamp in raw asc. file
# trialEnd:       end of trial time stamp in raw asc. file
# sound:          target word number on which the sound is played
# sound_type:     sound condition (STD= Standard; DEV= Novel sound); NB: DEV is used for historical reasons only (convenience)
# regionS:        start of target word region (empty space before target word), in pixels (x pos)
# regionE:        end of target word region (end of target word), in pixels (x pos)
# tBnd:           timestamp in the raw data showing when the invisible boundary was crossed
# tSFIX:          timestamp corresponding to the start of the next fixation after crossing the boundary
# tPlaySound:     timestamp when the command to play the sound was executed
# ISI:            inter-stimulus interval between playing the current and the previous sound (in ms)
# nextFlag:       what's the next flag in the data file after the sound is played (ESACC= end of saccade; EFIX= end of fixation)
# delBnd:         delay (in ms) between crossing the invisible boundary and playing the sound
# delFix:         delay (in ms) in playing the sound relative to the start of the next fixation
# del:            Experimental delay condition (from experiment design)
# SOD:            stimulus onset delay of playing the sound relative to start of fixation
# prevFix:        x position of previous fixation before crossing the boundary
# nextFix:        x position of next fixation after crossing the boundary
# N1:             duration of next fixation after crossing boundary (i.e., this is when the sound is played)
# N2:             duration of the second fixation after boundary (i.e., fixation after the one when the sound is played)
# sacc_dur:       duration of next saccade after sound is played (in ms)
# sacc Sflag:     timestamp of the start of saccade after sound is played
# sacc_peak:      peak saccade velocity for next saccade after sound is played (in deg/s)
# sacc_vel:       average saccade velocity for next saccade after sound is played (in deg/s)
# sacc_ampl:      amplitude of next saccade after playing sound
# order:          sound order in experiment
# word:           word number in the sentenceW
# Trialt:         duration of the trial (in ms); does not include questions, only reading time
# next_sacc:      amplitude of the next saccade in letters (after playing sound)
# skip:           was the target skipped or not (1= yes; 0= no)
# refix_prob      first-pass refixation probability on the target word
# next_sacc_type  contains information about whether the next saccade was intra-word or inter-word


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

# load/ install required packages:
packages= c("reshape", "lme4", 'car', "ggplot2", "ggpubr", "grid", "emmeans", 'BayesFactor') # list of used packages:

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

table(dat$del, dat$sound_type)
chisq.test(table(dat$del, dat$sound_type))



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

library(ggplot2)

Plot <-ggplot(mFix, aes(x= Delay, y= Mean, group= Sound, fill=Sound, colour= Sound, shape= Sound,
                        ymin= Mean- SE, ymax= Mean+SE)) +
  theme_classic(18) +geom_point(size=4.5)+ geom_line(size=2)+ 
  #scale_colour_brewer(palette="Accent")+ 
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  scale_shape_manual(values=c(16, 17))+
  coord_cartesian(clip = 'off')+
  xlab("Sound onset delay (ms)")+ ylim(220, 280)+
  scale_shape_manual(values=c(16, 17))+
  scale_x_discrete(expand = c(0.1,0.1))+
  ylab("First fixation duration (ms)")+ geom_ribbon(alpha=0.1, colour=NA)+
  #annotate("text", x = -2, y = 290, label = "a)             ")+
  theme(legend.position= c(0.285,0.87), legend.key.width=unit(1.5,"cm"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")); Plot

ggsave(Plot, filename = "Plots/FFD_mainFig.pdf", width = 6, height = 6)


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
  # doesn't converge with any other slopes
  summary(LM1<-lmer(log(N1) ~ sound_type*del + (del|sub)+ (1|item),
            data= dat, REML= TRUE))
  save(LM1, file= "Models/LM1.Rda")
  
}else{
  load("Models/LM1.Rda")
}

SM1<- round(coef(summary(LM1)),3)

rownames(SM1)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SM1, file = "Models/SM1_FixDur.csv")

### simple effects analysis:

EM<- emmeans(LM1, pairwise ~ sound_type*del, pbkrtest.limit = 5825)


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

# Descriptives:
DesSacc<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del', 'next_sacc_type'), 
              measure=c('sacc_dur', 'sacc_peak', 'sacc_vel', 'sacc_ampl'), na.rm=TRUE)
mSacc<- cast(DesSacc, sound_type+del ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))
write.csv2(mSacc[c(1,3,2,4), ], file= "Descriptives/Sacc.csv")


##################
# saccade plot:
##################

df<- cast(DesSacc, sound_type+del+sub ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

df$del<- as.factor(df$del)
levels(df$del)<- c('0 ms delay', '120 ms delay')

colnames(df)<- c("Sound", "del", "sub", "sacc_dur_M", "sacc_dur_SD", "sacc_peak_M", "sacc_peak_SD",
                 "sacc_vel_M", "sacc_vel_SD", "sacc_ampl_M", "sacc_ampl_SD")

df$Sound<- as.factor(df$Sound)
levels(df$Sound)<- c('Novel', 'Standard')

# Peak velocity vs amplitude:
SP<- ggplot(df, aes(x=sacc_ampl_M, y=sacc_peak_M, color=Sound, shape= Sound))+
  geom_point(size=2.5, alpha= 0.65) + theme_classic(14)+
  xlab("Saccade amplitude (deg)")+ 
  ylab("Peak saccade velocity (deg/s)")+
  ggtitle('a)')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size= 1)+
  scale_shape_manual(values=c(16, 17))+ 
  scale_color_manual(values=pallete1[1:2])+
  theme(legend.position="bottom",
  strip.background = element_rect(colour="white", fill="white"),
  strip.text = element_text(size = 14, face= 'bold'),
  legend.key.size = unit(2, "lines"))+
  facet_grid(. ~ del);SP

ggsave(filename = 'Plots/main_seq_peak.pdf', plot = SP, width = 8, height = 5)


# Saccade duration vs amplitude:
SP2<- ggplot(df, aes(x=sacc_ampl_M, y=sacc_dur_M, color=Sound, shape= Sound))+
  geom_point(size=2.5, alpha= 0.65) + theme_classic(14)+
  xlab("Saccade amplitude (deg)")+ 
  ylab("Saccade duration (ms)")+
  ggtitle('b)')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size= 1)+
  scale_shape_manual(values=c(16, 17))+ 
  scale_color_manual(values=pallete1[1:2])+
  theme(legend.position="bottom",
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size = 14, face= 'bold'),
        legend.key.size = unit(2, "lines"))+
  facet_grid(. ~ del);SP2

ggsave(filename = 'Plots/main_seq_dur.pdf', plot = SP2, width = 8, height = 5)


# merge descriptive plots: 
library(ggpubr)

figure1 <- ggarrange(SP, SP2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(filename = 'Plots/main_seq.pdf', plot = figure1, width = 8, height = 9)

####
# LMM analyses:

contrasts(dat$sound_type)
contrasts(dat$del)

# saccade duration:

if(!file.exists("Models/LM2.Rda")){
  summary(LM2<- lmer(log(sacc_dur) ~ sound_type*del + (sound_type|sub)+ (1|item),
             data= dat))
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

SLM2<- round(coef(summary(LM2)), 3)
rownames(SLM2)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM2, file = "Models/SLM2_saccDur.csv")

####
# peak saccade velocity:
if(!file.exists("Models /LM3.Rda")){
  summary(LM3<- lmer(sacc_peak ~ sound_type*del + (del|sub)+ (1|item),
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
  summary(LM4<- lmer(sacc_vel ~ sound_type*del + (del|sub)+ (1|item),
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
  summary(LM5<- lmer(log(sacc_ampl) ~ sound_type*del + (sound_type|sub)+ (1|item),
             data= dat))
  save(LM5, file= "Models/LM5.Rda")
}else{
  load("Models/LM5.Rda")
}

SLM5<- round(coef(summary(LM5)), 3)
rownames(SLM5)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM5, file = "Models/SLM5_SaccAmpl.csv")



###########################
#     Survival curves:    #
###########################

# 0 ms delay:

DPA0 <- read.delim("DPA/DPA_0ms.txt")
DPA0$condition<- as.factor(DPA0$condition)
levels(DPA0$condition)<- c("Standard", "Novel")
colnames(DPA0)<- c("sub", "FFD", "Sound")

a<- ecdf(DPA0$FFD[DPA0$Sound=="Standard"])
d<- sort(unique(DPA0$FFD))
dp<- NULL

b<- ecdf(DPA0$FFD[DPA0$Sound=="Novel"])
d2<- sort(unique(DPA0$FFD))
dp2<- NULL

for(i in 1:length(d)){
  dp[i]<- (1- a(d[i]))*100 
}

for(i in 1:length(d2)){
  dp2[i]<- (1- b(d2[i]))*100
}

df1<- data.frame(d, dp)
df2<- data.frame(d2, dp2)

colnames(df2)<- c("d", "dp")
df1$Sound<- "Standard"
df2$Sound<- "Novel"

df<- rbind(df1, df2)
df$Sound<- as.factor(df$Sound)
df$Sound<- factor(df$Sound, levels= c('Novel', "Standard"))
levels(df$Sound)

P1<- ggplot(data=df, aes(x= d, y=dp, group= Sound, linetype= Sound, color= Sound))+  
  coord_cartesian(xlim = c(80, 800))+
  theme_classic(14)+ xlab("First fixation duration (ms)") + ylab("Survival (%)")+
  ggtitle("a) 0 ms delay")+theme(plot.title = element_text(hjust = 0.5))+ geom_vline(xintercept = 152, color= "#8f8f8f")+ 
  geom_vline(xintercept = 130, linetype= "dashed", color= "#8f8f8f") + geom_vline(xintercept = 185, linetype= "dashed", 
                                                                               color= "#8f8f8f") + 
#  geom_rect(fill= pallete1[5], color="black", alpha=0.005, show.legend = F, inherit.aes = T)+
  geom_line()+ annotate("text", x = 500, y = 85, label = "Divergence point= 152 ms [95% CI: 130, 185]")+
  scale_x_continuous(breaks= seq(100,1000,100))+ #xlim(100, 1000)+
  #scale_colour_brewer(palette="Dark2")  + 
  scale_color_manual(values= pallete1[1:2])#+
  #theme(panel.grid.major = element_line(colour= "#f4f5f7", size=0.4),
  #      panel.grid.minor = element_line(colour= "#f4f5f7", size=0.2)); P1

ggsave(filename = "Plots/Survival0ms.pdf", plot = P1, width = 7, height = 5)

#####
# 120 ms

DPA120 <- read.delim("DPA/DPA_120ms.txt"); DPA120<- subset(DPA120, duration<800)
DPA120$condition<- as.factor(DPA120$condition)
levels(DPA120$condition)<- c("Standard", "Novel")
colnames(DPA120)<- c("sub", "FFD", "Sound")

a<- ecdf(DPA120$FFD[DPA120$Sound=="Standard"])
d<- sort(unique(DPA120$FFD))
dp<- NULL

b<- ecdf(DPA120$FFD[DPA120$Sound=="Novel"])
d2<- sort(unique(DPA120$FFD))
dp2<- NULL

for(i in 1:length(d)){
  dp[i]<- (1- a(d[i]))*100 
}

for(i in 1:length(d2)){
  dp2[i]<- (1- b(d2[i]))*100
}

df1<- data.frame(d, dp)
df2<- data.frame(d2, dp2)

colnames(df2)<- c("d", "dp")
df1$Sound<- "Standard"
df2$Sound<- "Novel"

df<- rbind(df1, df2)
df$Sound<- as.factor(df$Sound)
df$Sound<- factor(df$Sound, levels= c('Novel', "Standard"))
levels(df$Sound)

P2<- ggplot(data=df, aes(x= d, y=dp, group= Sound, linetype= Sound, color= Sound)) +
  coord_cartesian(xlim = c(80, 800))+
  theme_classic(14)+ xlab("First fixation duration (ms)") + ylab("Survival (%)")+
  ggtitle("b) 120 ms delay")+theme(plot.title = element_text(hjust = 0.5))+ geom_vline(xintercept = 178, color= "#8f8f8f")+ 
  geom_vline(xintercept = 149, linetype= "dashed", color= "#8f8f8f") + geom_vline(xintercept = 198, linetype= "dashed", 
  color= "#8f8f8f") + geom_line()+ 
  annotate("text", x = 500, y = 85, label = "Divergence point= 178 ms [95% CI: 149, 198]")+
  scale_x_continuous(breaks= seq(100,1000,100))+
  #scale_colour_brewer(palette="Dark2") + 
  scale_color_manual(values= pallete1[1:2])#+
#  theme(panel.grid.major = element_line(colour= "#f4f5f7", size=0.4),
#        panel.grid.minor = element_line(colour= "#f4f5f7", size=0.2)); P2


ggsave(filename = "Plots/Survival120ms.pdf", plot = P2, width = 7, height = 5)


# merge plots:

figure <- ggarrange(P1, P2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", align = "h")
ggsave("Plots/Survival_Merged.pdf", figure, width= 7, height=7, units= "in")



########################################################################################################################################
#                                                      Bayes Factor Analyses                                                           #
########################################################################################################################################
#

dat$N1_log<- log(dat$N1)
dat$sub<- as.factor(dat$sub)
dat$item<- as.factor(dat$item)


# sound effect:
summary(FD<- lmBF(N1_log~  sub+item, data= dat, whichRandom = c('sub', 'item'), rscaleFixed = sqrt(2)/2))
summary(FD2<- lmBF(N1_log~ sound_type + sub+item, data= dat, whichRandom = c('sub', 'item'), rscaleFixed = sqrt(2)/2))

FD2/FD

# delay effect:
summary(FD3<- lmBF(N1_log~ del + sub+item, data= dat, whichRandom = c('sub', 'item'), rscaleFixed = sqrt(2)/2))
FD3/FD

# Sound x Delay interaction:
summary(FD4<- lmBF(N1_log~ sound_type+ del + sub+item, data= dat, whichRandom = c('sub', 'item'), rscaleFixed = sqrt(2)/2))
summary(FD5<- lmBF(N1_log~ sound_type*del + sub+item, data= dat, whichRandom = c('sub', 'item'), rscaleFixed = sqrt(2)/2))
FD5/FD4

########################################################################################################################################
#                                        First-pass re-fixation probability                                                            #
########################################################################################################################################

# Descriptives:
DesRefix<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
               measure=c('refix_prob'), na.rm=TRUE)
mRF<- cast(DesRefix, sound_type+del ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

# check contrast coding
contrasts(dat$sound_type)
contrasts(dat$del)

if(!file.exists("Models/GM1.Rda")){
  # doesn't converge with any other slopes
  
  summary(GM1<-glmer(refix_prob ~ sound_type*del + (1|sub)+ (del|item),
                     data= dat, family= binomial))
  
  save(GM1, file= "Models/GM1.Rda")
  
}else{
  load("Models/GM1.Rda")
}

SGM1<- round(coef(summary(GM1)),3)

rownames(SGM1)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SGM1, file = "Models/SGM1_Refix_prob.csv")



########################################################################################################################################
#                                                 Intra-word vs. Inter-word saccades                                                   #
########################################################################################################################################

# Separation by intra-word and inter-word saccades (as requested by Reviewer 2)
DesSacc2<- melt(subset(dat, !is.na(next_sacc_type)), id=c('sub', 'item', 'cond', 'sound_type', 'del', 'next_sacc_type'), 
                measure=c('sacc_dur', 'sacc_peak', 'sacc_vel', 'sacc_ampl'), na.rm=TRUE)
mSacc2<- cast(DesSacc2, sound_type+del+next_sacc_type ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

write.csv2(mSacc2, file= "Descriptives/Sacc2.csv")

table(dat$next_sacc_type)


# check contrast coding
contrasts(dat$sound_type)
contrasts(dat$del)

dat$next_sacc_type<- as.factor(dat$next_sacc_type)
contrasts(dat$next_sacc_type)<- c(1, 0)
contrasts(dat$next_sacc_type)


###################
# saccade duration:
###################

if(!file.exists("Models/SaccType/LM2_st.Rda")){
  summary(LM2_st<- lmer(log(sacc_dur) ~ sound_type*del*next_sacc_type + (1|sub)+ (1|item),
                        data= dat))
  save(LM2_st, file= "Models/SaccType/LM2_st.Rda")
}else{
  load("Models/SaccType/LM2_st.Rda")
}

SLM2_st<- round(coef(summary(LM2_st)), 3)
rownames(SLM2_st)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Saccade type", "Sound x Delay",
                      "Sound x Saccade type", "Delay x Saccade type", "Sound x Delay x Saccade type")
write.csv2(SLM2_st, file = "Models/SaccType/SLM2_saccDur_st.csv")


####################
# saccade amplitude:
####################

if(!file.exists("Models/SaccType/LM5_st.Rda")){
  summary(LM5_st<- lmer(log(sacc_ampl) ~ sound_type*del*next_sacc_type + (sound_type|sub)+ (del|item),
                        data= dat))
  save(LM5_st, file= "Models/SaccType/LM5_st.Rda")
}else{
  load("Models/SaccType/LM5_st.Rda")
}

SLM5_st<- round(coef(summary(LM5_st)), 3)
rownames(SLM5_st)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Saccade type", "Sound x Delay",
                      "Sound x Saccade type", "Delay x Saccade type", "Sound x Delay x Saccade type")
write.csv2(SLM5_st, file = "Models/SaccType/SLM5_SaccAmpl_st.csv")


########################
# peak saccade velocity:
########################

if(!file.exists("Models/SaccType/LM3_st.Rda")){
  summary(LM3_st<- lmer(sacc_peak ~ sound_type*del*next_sacc_type + (del|sub)+ (sound_type|item),
                        data= dat))
  save(LM3_st, file= "Models/SaccType/LM3_st.Rda")
}else{
  load("Models/SaccType/LM3_st.Rda")
}

SLM3_st<- round(coef(summary(LM3_st)), 3)
rownames(SLM3_st)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Saccade type", "Sound x Delay",
                      "Sound x Saccade type", "Delay x Saccade type", "Sound x Delay x Saccade type")
write.csv2(SLM3_st, file = "Models/SaccType/SLM3_PeakVel_st.csv")


###########################
# average saccade velocity:
###########################

if(!file.exists("Models/SaccType/LM4_st.Rda")){
  summary(LM4_st<- lmer(sacc_vel ~ sound_type*del*next_sacc_type + (del|sub)+ (1|item),
                        data= dat))
  save(LM4_st, file= "Models/SaccType/LM4_st.Rda")
}else{
  load("Models/SaccType/LM4_st.Rda")
}

SLM4_st<- round(coef(summary(LM4_st)), 3)
rownames(SLM4_st)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Saccade type", "Sound x Delay",
                      "Sound x Saccade type", "Delay x Saccade type", "Sound x Delay x Saccade type")
write.csv2(SLM4_st, file = "Models/SaccType/SLM4_AvgVel_st.csv")


########################################################################################################################################
#                                         Sensitivity anlysis excluding target skips                                                   #
########################################################################################################################################

dat2<- subset(dat, skip==0)

nrow(dat2)

# check contrast coding
contrasts(dat2$sound_type)
contrasts(dat2$del)


####################
# Fixation duration:
####################

if(!file.exists("Models/NoSkips/LM1_ns.Rda")){
  # doesn't converge with any other slopes
  summary(LM1_ns<-lmer(log(N1) ~ sound_type*del + (del|sub)+ (1|item),
                    data= dat2, REML= TRUE))
  save(LM1_ns, file= "Models/NoSkips/LM1_ns.Rda")
  
}else{
  load("Models/NoSkips/LM1_ns.Rda")
}

SM1_ns<- round(coef(summary(LM1_ns)),3)

rownames(SM1_ns)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SM1_ns, file = "Models/NoSkips/SM1_FixDur.csv")


###################
# saccade duration:
###################
if(!file.exists("Models/NoSkips/LM2_ns.Rda")){
  summary(LM2_ns<- lmer(log(sacc_dur) ~ sound_type*del + (sound_type|sub)+ (1|item),
                     data= dat2))
  save(LM2_ns, file= "Models/NoSkips/LM2_ns.Rda")
}else{
  load("Models/NoSkips/LM2_ns.Rda")
}

SLM2_ns<- round(coef(summary(LM2_ns)), 3)
rownames(SLM2_ns)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM2_ns, file = "Models/NoSkips/SLM2_saccDur.csv")



####################
# saccade amplitude:
####################

if(!file.exists("Models/NoSkips/LM5_ns.Rda")){
  summary(LM5_ns<- lmer(log(sacc_ampl) ~ sound_type*del + (sound_type|sub)+ (1|item),
                     data= dat2))
  save(LM5_ns, file= "Models/NoSkips/LM5_ns.Rda")
}else{
  load("Models/NoSkips/LM5_ns.Rda")
}

SLM5_ns<- round(coef(summary(LM5_ns)), 3)
rownames(SLM5_ns)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM5_ns, file = "Models/NoSkips/SLM5_SaccAmpl.csv")


########################
# peak saccade velocity:
########################

if(!file.exists("Models/NoSkips/LM3_ns.Rda")){
  summary(LM3_ns<- lmer(sacc_peak ~ sound_type*del + (sound_type|sub)+ (1|item),
                     data= dat2))
  save(LM3_ns, file= "Models/NoSkips/LM3.Rda")
}else{
  load("Models/NoSkips/LM3_ns.Rda")
}

SLM3_ns<- round(coef(summary(LM3_ns)), 3)
rownames(SLM3_ns)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM3_ns, file = "Models/NoSkips/SLM3_PeakVel.csv")


###########################
# average saccade velocity:
###########################

if(!file.exists("Models/NoSkips/LM4_ns.Rda")){
  summary(LM4_ns<- lmer(sacc_vel ~ sound_type*del + (del|sub)+ (1|item),
                     data= dat2))
  save(LM4_ns, file= "Models/NoSkips/LM4_ns.Rda")
}else{
  load("Models/NoSkips/LM4_ns.Rda")
}

SLM4_ns<- round(coef(summary(LM4_ns)), 3)
rownames(SLM4_ns)<- c("Intercept", "Sound (Novel vs STD)", "Delay (120 vs 0ms)", "Sound x Delay")
write.csv2(SLM4_ns, file = "Models/NoSkips/SLM4_AvgVel.csv")
