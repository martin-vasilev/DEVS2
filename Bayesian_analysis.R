
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
packages= c("reshape", "brms", "ggpubr", "grid", "emmeans", "parallel", "boot") # list of used packages:

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

# slope priors:

log(260)- log(240) # rounded to 0.1
(log(260)- log(240))*2 # 2x SD

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
NwarmUp<- 1000
Niter<- 6000
Nchains<- 10

if(!file.exists("Models/Bayesian/LM1.Rda")){
  LM1<- brm(formula = log(N1) ~ sound_type*del + (del|sub)+ (1|item), data = dat, warmup = NwarmUp, iter = Niter, chains = Nchains,
            sample_prior = TRUE, cores = detectCores(), seed= 1234, control = list(adapt_delta = 0.9),
            prior =  c(set_prior('normal(0, 0.1)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 0.1)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 0.1)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM1, file= "Models/Bayesian/LM1.Rda")
}else{
  load('Models/Bayesian/LM1.Rda')
}


print(LM1, digits = 3)
prior_summary(LM1)
VarCorr(LM1)


## Bayes factors:

# Note: the Bayes Factor is BH_10, so values >1 indicate evidence for the alternative, and values <1 indicate 
# evidence in support of the null. Brms reports them the other way around, but I reverse them here because I 
# Think BF_10 reporting is somewhat more common

# sound effect:
BF_sound = hypothesis(LM1, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound$hypothesis$Evid.Ratio

# delay effect:
BF_del = hypothesis(LM1, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del$hypothesis$Evid.Ratio

# interaction effect:
BF_int = hypothesis(LM1, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int$hypothesis$Evid.Ratio


###########################
#      SACCADE DATA:      #
###########################


# prior
log(30)- log(25) # rounded to 0.2
(log(30)- log(25))*2 # 2x SD

# rounded to 20

####
# LMM analyses:

contrasts(dat$sound_type)
contrasts(dat$del)

# saccade duration:
if(!file.exists("Models/Bayesian/LM2.Rda")){
  LM2<- brm(formula = log(sacc_dur) ~ sound_type*del + (sound_type|sub)+ (1|item), data = dat, warmup = NwarmUp,
            iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
            prior =  c(set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM2, file= "Models/Bayesian/LM2.Rda")
}else{
  load("Models/Bayesian/LM2.Rda")
}

print(LM2, digits = 3)
prior_summary(LM2)
VarCorr(LM2)


## Bayes factors:


# sound effect:
BF_sound2 = hypothesis(LM2, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound2$hypothesis$Evid.Ratio

# delay effect:
BF_del2 = hypothesis(LM2, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del2$hypothesis$Evid.Ratio

# interaction effect:
BF_int2 = hypothesis(LM2, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int2$hypothesis$Evid.Ratio



####


# prior
# not transformed, typical values in humans are 200-300 deg/s


# peak saccade velocity:

if(!file.exists("Models/Bayesian/LM3.Rda")){
  LM3<- brm(formula = sacc_peak ~ sound_type*del + (del|sub)+ (1|item), data = dat, warmup = NwarmUp,
            iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
            prior =  c(set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 30)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 125)', class = 'Intercept')))
  
  save(LM3, file= "Models/Bayesian/LM3.Rda")
}else{
  load("Models/Bayesian/LM3.Rda")
}

print(LM3, digits = 3)
prior_summary(LM3)
VarCorr(LM3)

## Bayes factors:

# sound effect:
BF_sound3 = hypothesis(LM3, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound3$hypothesis$Evid.Ratio

# delay effect:
BF_del3 = hypothesis(LM3, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del3$hypothesis$Evid.Ratio

# interaction effect:
BF_int3 = hypothesis(LM3, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int3$hypothesis$Evid.Ratio



####
# average saccade velocity:
  
  if(!file.exists("Models/Bayesian/LM4.Rda")){
    LM4<- brm(formula = sacc_vel ~ sound_type*del + (del|sub)+ (1|item), data = dat, warmup = NwarmUp,
              iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
              prior =  c(set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV'),
                         set_prior('normal(0, 30)', class = 'b', coef= 'del120'),
                         set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120'),
                         set_prior('normal(0, 125)', class = 'Intercept')))
    
    save(LM4, file= "Models/Bayesian/LM4.Rda")
  }else{
    load("Models/Bayesian/LM4.Rda")
  }
  
  print(LM4, digits= 3)
  prior_summary(LM4)
  VarCorr(LM4)
  
  
  ## Bayes factors:

  # sound effect:
  BF_sound4 = hypothesis(LM4, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
  1/BF_sound4$hypothesis$Evid.Ratio
  
  # delay effect:
  BF_del4 = hypothesis(LM4, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
  1/BF_del4$hypothesis$Evid.Ratio
  
  # interaction effect:
  BF_int4 = hypothesis(LM4, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
  1/BF_int4$hypothesis$Evid.Ratio
  


####
# saccade amplitude:
  
  # prior: 
  log(2.2) - log (1.9) # rounded to 0.15
  (log(2.2) - log (1.9))*2 # 2x SD
  
  
  if(!file.exists("Models/Bayesian/LM5.Rda")){
    LM5<- brm(formula = log(sacc_ampl) ~ sound_type*del + (sound_type|sub)+ (1|item), data = dat, warmup = NwarmUp,
              iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(), seed= 1234, control = list(adapt_delta = 0.9),
              prior =  c(set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV'),
                         set_prior('normal(0, 0.15)', class = 'b', coef= 'del120'),
                         set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV:del120'),
                         set_prior('normal(0, 5)', class = 'Intercept')))
    
    save(LM5, file= "Models/Bayesian/LM5.Rda")
  }else{
    load("Models/Bayesian/LM5.Rda")
  }
  
  print(LM5, digits=3)
  prior_summary(LM5)
  VarCorr(LM5)
  
  
  ## Bayes factors:
  
  # sound effect:
  BF_sound5 = hypothesis(LM5, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
  1/BF_sound5$hypothesis$Evid.Ratio
  
  # delay effect:
  BF_del5 = hypothesis(LM5, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
  1/BF_del5$hypothesis$Evid.Ratio
  
  # interaction effect:
  BF_int5 = hypothesis(LM5, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
  1/BF_int5$hypothesis$Evid.Ratio

  
  #####
  
  # prior
  logit(0.2) - logit(0.10) # rounded down to 0.75
  
  # First-pass re-fixation probability
  if(!file.exists("Models/Bayesian/GM1.Rda")){
    GM1<- brm(formula = refix_prob ~ sound_type*del + (1|sub)+ (del|item), data = dat, family= bernoulli, warmup = NwarmUp,
              iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(), seed= 1234, control = list(adapt_delta = 0.9),
              prior =  c(set_prior('normal(0, 0.75)', class = 'b', coef= 'sound_typeDEV'),
                         set_prior('normal(0, 0.75)', class = 'b', coef= 'del120'),
                         set_prior('normal(0, 0.75)', class = 'b', coef= 'sound_typeDEV:del120'),
                         set_prior('normal(0, 3)', class = 'Intercept')))

    
    save(GM1, file= "Models/Bayesian/GM1.Rda")
  }else{
    load("Models/Bayesian/GM1.Rda")
  }
  
  print(GM1, digits=3)
  prior_summary(GM1)
  VarCorr(GM1)
    
  
  ## Bayes factors:
  
  # sound effect:
  BF_sound6 = hypothesis(GM1, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
  1/BF_sound6$hypothesis$Evid.Ratio
  
  # delay effect:
  BF_del6 = hypothesis(GM1, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
  1/BF_del6$hypothesis$Evid.Ratio
  
  # interaction effect:
  BF_int6 = hypothesis(GM1, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
  1/BF_int6$hypothesis$Evid.Ratio
    
  
  
  
  
########################################################################################################################
#                         Modulation by type of saccade (intra-line vs inter-line)
########################################################################################################################
  

# check contrast coding
contrasts(dat$sound_type)
contrasts(dat$del)
  
dat$next_sacc_type<- as.factor(dat$next_sacc_type)
contrasts(dat$next_sacc_type)<- c(1, 0)
contrasts(dat$next_sacc_type)



#### Saccade duration:  
if(!file.exists("Models/Bayesian/SaccType/LM2s.Rda")){
  LM2s<- brm(formula = log(sacc_dur) ~ sound_type*del*next_sacc_type + (sound_type|sub)+ (1|item), data = dat, warmup = NwarmUp,
            iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
            prior =  c(set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV'),
                       set_prior('normal(0, 0.4)', class = 'b', coef= 'next_sacc_type1'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'del120'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:del120'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'del120:next_sacc_type1'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:del120:next_sacc_type1'),
                       set_prior('normal(0, 0.2)', class = 'b', coef= 'sound_typeDEV:next_sacc_type1'),
                       set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM2s, file= "Models/Bayesian/SaccType/LM2s.Rda")
}else{
  load("Models/Bayesian/SaccType/LM2s.Rda")
}

print(LM2s, digits = 3)
prior_summary(LM2s)
VarCorr(LM2s)


## Bayes factors:


# sound effect:
BF_sound2s = hypothesis(LM2s, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound2s$hypothesis$Evid.Ratio

# delay effect:
BF_del2s = hypothesis(LM2s, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del2s$hypothesis$Evid.Ratio

# sacc_type effect:
BF_sacc2s = hypothesis(LM2s, hypothesis = 'next_sacc_type1 = 0', seed= 1234)  # H0: No delay effect
1/BF_sacc2s$hypothesis$Evid.Ratio

# Sound x Delay interaction effect:
BF_int2s1 = hypothesis(LM2s, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int2s1$hypothesis$Evid.Ratio

  
# Sound x SaccType interaction effect:
BF_int2s2 = hypothesis(LM2s, hypothesis = 'sound_typeDEV:next_sacc_type1 = 0', seed= 1234)  # No Sound x SaccType interaction
1/BF_int2s2$hypothesis$Evid.Ratio
  
# Delay x SaccType interaction effect:
BF_int2s3 = hypothesis(LM2s, hypothesis = 'del120:next_sacc_type1 = 0', seed= 1234)  # No Delay x SaccType interaction
1/BF_int2s3$hypothesis$Evid.Ratio

# Sound x Delay x SaccType interaction effect:
BF_int2s4 = hypothesis(LM2s, hypothesis = 'sound_typeDEV:del120:next_sacc_type1 = 0', seed= 1234)  # No Sound x Delay x SaccType interaction
1/BF_int2s4$hypothesis$Evid.Ratio





#### Saccade amplitude:  
if(!file.exists("Models/Bayesian/SaccType/LM5s.Rda")){
  LM5s<- brm(formula = log(sacc_ampl) ~ sound_type*del*next_sacc_type + (sound_type|sub)+ (1|item), data = dat, warmup = NwarmUp,
             iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
             prior =  c(set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV'),
                        set_prior('normal(0, 0.75)', class = 'b', coef= 'next_sacc_type1'),
                        set_prior('normal(0, 0.15)', class = 'b', coef= 'del120'),
                        set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV:del120'),
                        set_prior('normal(0, 0.15)', class = 'b', coef= 'del120:next_sacc_type1'),
                        set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV:del120:next_sacc_type1'),
                        set_prior('normal(0, 0.15)', class = 'b', coef= 'sound_typeDEV:next_sacc_type1'),
                        set_prior('normal(0, 5)', class = 'Intercept')))
  
  save(LM5s, file= "Models/Bayesian/SaccType/LM5s.Rda")
}else{
  load("Models/Bayesian/SaccType/LM5s.Rda")
}

print(LM5s, digits = 3)
prior_summary(LM5s)
VarCorr(LM5s)


## Bayes factors:


# sound effect:
BF_sound5s = hypothesis(LM5s, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound5s$hypothesis$Evid.Ratio

# delay effect:
BF_del5s = hypothesis(LM5s, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del5s$hypothesis$Evid.Ratio

# sacc_type effect:
BF_sacc5s = hypothesis(LM5s, hypothesis = 'next_sacc_type1 = 0', seed= 1234)  # H0: No delay effect
1/BF_sacc5s$hypothesis$Evid.Ratio

# Sound x Delay interaction effect:
BF_int5s1 = hypothesis(LM5s, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int5s1$hypothesis$Evid.Ratio


# Sound x SaccType interaction effect:
BF_int5s2 = hypothesis(LM5s, hypothesis = 'sound_typeDEV:next_sacc_type1 = 0', seed= 1234)  # No Sound x SaccType interaction
1/BF_int5s2$hypothesis$Evid.Ratio

# Delay x SaccType interaction effect:
BF_int5s3 = hypothesis(LM5s, hypothesis = 'del120:next_sacc_type1 = 0', seed= 1234)  # No Delay x SaccType interaction
1/BF_int5s3$hypothesis$Evid.Ratio

# Sound x Delay x SaccType interaction effect:
BF_int5s4 = hypothesis(LM5s, hypothesis = 'sound_typeDEV:del120:next_sacc_type1 = 0', seed= 1234)  # No Sound x Delay x SaccType interaction
1/BF_int5s4$hypothesis$Evid.Ratio




#### Peak saccade velocity:  
if(!file.exists("Models/Bayesian/SaccType/LM3s.Rda")){
  LM3s<- brm(formula = sacc_peak ~ sound_type*del*next_sacc_type + (del|sub)+ (1|item), data = dat, warmup = NwarmUp,
             iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
             prior =  c(set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV'),
                        set_prior('normal(0, 100)', class = 'b', coef= 'next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'del120'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'del120:next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120:next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:next_sacc_type1'),
                        set_prior('normal(0, 125)', class = 'Intercept')))
  
  save(LM3s, file= "Models/Bayesian/SaccType/LM3s.Rda")
}else{
  load("Models/Bayesian/SaccType/LM3s.Rda")
}

print(LM3s, digits = 3)
prior_summary(LM3s)
VarCorr(LM3s)


## Bayes factors:


# sound effect:
BF_sound3s = hypothesis(LM3s, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound3s$hypothesis$Evid.Ratio

# delay effect:
BF_del3s = hypothesis(LM3s, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del3s$hypothesis$Evid.Ratio

# sacc_type effect:
BF_sacc3s = hypothesis(LM3s, hypothesis = 'next_sacc_type1 = 0', seed= 1234)  # H0: No delay effect
1/BF_sacc3s$hypothesis$Evid.Ratio

# Sound x Delay interaction effect:
BF_int3s1 = hypothesis(LM3s, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int3s1$hypothesis$Evid.Ratio


# Sound x SaccType interaction effect:
BF_int3s2 = hypothesis(LM3s, hypothesis = 'sound_typeDEV:next_sacc_type1 = 0', seed= 1234)  # No Sound x SaccType interaction
1/BF_int3s2$hypothesis$Evid.Ratio

# Delay x SaccType interaction effect:
BF_int3s3 = hypothesis(LM3s, hypothesis = 'del120:next_sacc_type1 = 0', seed= 1234)  # No Delay x SaccType interaction
1/BF_int3s3$hypothesis$Evid.Ratio

# Sound x Delay x SaccType interaction effect:
BF_int3s4 = hypothesis(LM3s, hypothesis = 'sound_typeDEV:del120:next_sacc_type1 = 0', seed= 1234)  # No Sound x Delay x SaccType interaction
1/BF_int3s4$hypothesis$Evid.Ratio






#### Average saccade velocity:  
if(!file.exists("Models/Bayesian/SaccType/LM4s.Rda")){
  LM4s<- brm(formula = sacc_vel ~ sound_type*del*next_sacc_type + (del|sub)+ (1|item), data = dat, warmup = NwarmUp,
             iter = Niter, chains = Nchains, sample_prior = TRUE, cores = detectCores(),  seed= 1234, control = list(adapt_delta = 0.9),
             prior =  c(set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV'),
                        set_prior('normal(0, 100)', class = 'b', coef= 'next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'del120'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'del120:next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:del120:next_sacc_type1'),
                        set_prior('normal(0, 30)', class = 'b', coef= 'sound_typeDEV:next_sacc_type1'),
                        set_prior('normal(0, 125)', class = 'Intercept')))
  
  save(LM4s, file= "Models/Bayesian/SaccType/LM4s.Rda")
}else{
  load("Models/Bayesian/SaccType/LM4s.Rda")
}

print(LM4s, digits = 3)
prior_summary(LM4s)
VarCorr(LM4s)


## Bayes factors:


# sound effect:
BF_sound4s = hypothesis(LM4s, hypothesis = 'sound_typeDEV = 0', seed= 1234)  # H0: No sound effect
1/BF_sound4s$hypothesis$Evid.Ratio

# delay effect:
BF_del4s = hypothesis(LM4s, hypothesis = 'del120 = 0', seed= 1234)  # H0: No delay effect
1/BF_del4s$hypothesis$Evid.Ratio

# sacc_type effect:
BF_sacc4s = hypothesis(LM4s, hypothesis = 'next_sacc_type1 = 0', seed= 1234)  # H0: No delay effect
1/BF_sacc4s$hypothesis$Evid.Ratio

# Sound x Delay interaction effect:
BF_int4s1 = hypothesis(LM4s, hypothesis = 'sound_typeDEV:del120 = 0', seed= 1234)  # H0: No sound x delay interaction
1/BF_int4s1$hypothesis$Evid.Ratio


# Sound x SaccType interaction effect:
BF_int4s2 = hypothesis(LM4s, hypothesis = 'sound_typeDEV:next_sacc_type1 = 0', seed= 1234)  # No Sound x SaccType interaction
1/BF_int4s2$hypothesis$Evid.Ratio

# Delay x SaccType interaction effect:
BF_int4s3 = hypothesis(LM4s, hypothesis = 'del120:next_sacc_type1 = 0', seed= 1234)  # No Delay x SaccType interaction
1/BF_int4s3$hypothesis$Evid.Ratio

# Sound x Delay x SaccType interaction effect:
BF_int4s4 = hypothesis(LM4s, hypothesis = 'sound_typeDEV:del120:next_sacc_type1 = 0', seed= 1234)  # No Sound x Delay x SaccType interaction
1/BF_int4s4$hypothesis$Evid.Ratio
