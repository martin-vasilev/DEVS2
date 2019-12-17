
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
packages= c("reshape", "lme4", 'car', "ggplot2", "mgcv", "itsadug", "ggpubr", "grid") # list of used packages:

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

Plot <-ggplot(mFix, aes(x= Delay, y= Mean, group= Sound, fill=Sound, colour= Sound, shape= Sound,
                        ymin= Mean- SE, ymax= Mean+SE)) +
  theme_classic(18) +geom_point(size=4.5)+ geom_line(size=2)+ 
  #scale_colour_brewer(palette="Accent")+ 
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
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

### simple effects analysis:

library(emmeans)
#EM<- emmeans(LM1, pairwise ~ sound_type*del, pbkrtest.limit = 5832)


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
DesSacc<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del'), 
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
# saccade peak velocity:
if(!file.exists("Models/LM3.Rda")){
  summary(LM3<- lmer(sacc_peak ~ sound_type*del + (1|sub)+ (1|item),
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
  summary(LM4<- lmer(sacc_vel ~ sound_type*del + (1|sub)+ (1|item),
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
  summary(LM5<- lmer(sacc_ampl ~ sound_type*del + (del|sub)+ (1|item),
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
  ggtitle("0 ms delay")+theme(plot.title = element_text(hjust = 0.5))+ geom_vline(xintercept = 152, color= "#8f8f8f")+ 
  geom_vline(xintercept = 130, linetype= "dashed", color= "#8f8f8f") + geom_vline(xintercept = 185, linetype= "dashed", 
                                                                               color= "#8f8f8f") + 
#  geom_rect(fill= pallete1[5], color="black", alpha=0.005, show.legend = F, inherit.aes = T)+
  geom_line()+ annotate("text", x = 500, y = 85, label = "Divergence point= 152 ms [95% CI: 130, 152]")+
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
  ggtitle("120 ms delay")+theme(plot.title = element_text(hjust = 0.5))+ geom_vline(xintercept = 178, color= "#8f8f8f")+ 
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



###########################
#      GAMM ANALYSIS:     #
###########################

gam1 <- bam(N1 ~ sound_type+
              s(sub, bs="re", k=10) +
              s(sub, sound_type, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, sound_type, bs="re") +
              s(order, bs= "cr", k=10)+
              s(order, by= sound_type, k=10, bs= "cr")+
              s(order, sub, bs= "fs", m=1, k=4),
            data=dat)

summary(gam1)

# remove the modulation of trial order by sound_type
gam2 <- bam(N1 ~ sound_type+
              s(sub, bs="re", k=10) +
              s(sub, sound_type, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, sound_type, bs="re") +
              s(order, bs= "cr", k=10)+
#              s(order, by= sound_type, k=10, bs= "cr")+
              s(order, sub, bs= "fs", m=1, k=4),
            data=dat)

summary(gam2)

compareML(gam1, gam2, suggest.report = T)

# Calculate empircal means /wrt order:
DesFix3<- melt(dat, id=c('sub', 'item', 'cond', 'sound_type', 'del', 'order'), 
               measure=c("N1"), na.rm=TRUE)
mFix2<- cast(DesFix3, sound_type+order ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
s1<- subset(mFix2, sound_type== "STD")
s2<- subset(mFix2, sound_type== "DEV")

mDiff<- data.frame(m= s2$N1_M- s1$N1_M, order= 1:60)


# library(lattice)
# xyplot(x = m~ order,data = mDiff, xlab= "Trial order", ylab= "Mean difference in ms (Novel - STD)",
#        panel = function(x, y) {
#          panel.xyplot(x, y)
#          panel.abline(0, lty=2)})


plot_smooth(gam1, view="order", plot_all="sound_type", rug=F)
acf_plot(resid(gam1), split_by=list(dat$order))



# make Plot:

#vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
#                           just=c("left","top"), 
#                           y=0.5, x=0.5)

#par(mfrow=c(2,1))

pdf(file= "Plots/GAMM_plot.pdf", width = 7, height = 6)
plot_diff(gam1, view = "order", rm.ranef = F, comp = list(sound_type = c("DEV", 
          "STD")), ylim= c(-20, 50), col = pallete1[2], main= "Novel - Standard difference",
          ylab= "Mean difference in FFD (in ms)", xlab= "Trial order in experiment", print.summary = T, family= "serif",
          cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)
points(x = mDiff$order, y = mDiff$m, pch= 18, col= pallete1[5])
dev.off()


#print(vp=vp.BottomRight, Plot)



# plot_diff(gam1, view = "order", rm.ranef = T, comp = list(sound_type = c("DEV", 
#            "STD")), ylim= c(-20, 50), col = "darkblue", main= "Novel - Standard difference",
#           ylab= "Estimated mean difference (FFD)", xlab= "Trial order", print.summary = T)
# points(x = mDiff$order, y = mDiff$m, pch= 23, col= "darkorange")
