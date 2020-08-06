
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
packages= c("reshape", "brms", "ggpubr", "grid", "emmeans", "parallel") # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}


###########################
# FIXATION DURATION DATA: #
###########################

load("data/dat.Rda")


###### LMM analysis:

# Note, for historical reasons (i.e., ease of programming) I'm still using "DEV" to denote the different (deviant) sound.
# However, in this experiment a novel sound was used, so "DEV"= Novel.

# check contrast coding
dat$sound_type<- as.factor(dat$sound_type)
dat$sound_type<- factor(dat$sound_type, levels= c("STD", "DEV"))
contrasts(dat$sound_type)#<- c(1, 0)


dat$del<- as.factor(dat$del)
contrasts(dat$del)


#### Model parameters:
NwarmUp<- 500
Niter<- 2000
Nchains<- 4

if(!file.exists("Models/Bayesian/LM1.Rda")){
  LM1<- brm(formula = log(N1) ~ sound_type*del + (del|sub)+ (1|item), data = dat, warmup = NwarmUp, iter = Niter, chains = Nchains,
            sample_prior = TRUE, cores = detectCores(),
            prior =  c(set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM1, file= "Models/Bayesian/LM1.Rda")
}else{
  load('Models/Bayesian/LM1.Rda')
}


summary(LM1)
prior_summary(LM1)


## Bayes factors:

# Note: the Bayes Factor is BH_01, so values >1 indicate evidence for the null, and values <1 indicate 
# evidence in support of the alternative

# sound effect:
BF_sound = hypothesis(LM1, hypothesis = 'sound_typeDEV = 0')  # H0: No sound effect
BF_sound$hypothesis

# delay effect:
BF_del = hypothesis(LM1, hypothesis = 'del120 = 0')  # H0: No delay effect
BF_del$hypothesis

# interaction effect:
BF_int = hypothesis(LM1, hypothesis = 'sound_typeDEV:del120 = 0')  # H0: No sound x delay interaction
BF_int$hypothesis


###########################
#      SACCADE DATA:      #
###########################


####
# LMM analyses:

contrasts(dat$sound_type)
contrasts(dat$del)

# saccade duration:
if(!file.exists("Models/Bayesian/LM2.Rda")){
  LM2<- brm(formula = log(sacc_dur) ~ sound_type*del + (sound_type|sub)+ (1|item), data = dat, warmup = NwarmUp,
            iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),
            prior =  c(set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM2, file= "Models/Bayesian/LM2.Rda")
}else{
  load("Models/Bayesian/LM2.Rda")
}

summary(LM2)
prior_summary(LM2)


## Bayes factors:

# Note: the Bayes Factor is BH_01, so values >1 indicate evidence for the null, and values <1 indicate 
# evidence in support of the alternative

# sound effect:
BF_sound2 = hypothesis(LM2, hypothesis = 'sound_typeDEV = 0')  # H0: No sound effect
BF_sound2$hypothesis

# delay effect:
BF_del2 = hypothesis(LM2, hypothesis = 'del120 = 0')  # H0: No delay effect
BF_del2$hypothesis

# interaction effect:
BF_int2 = hypothesis(LM2, hypothesis = 'sound_typeDEV:del120 = 0')  # H0: No sound x delay interaction
BF_int2$hypothesis


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


