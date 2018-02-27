library(readr)
q <- read_delim("preproc/raw_data/q.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
q<- subset(q, item<121)

library(reshape)

DesComp<- melt(q, id=c('subject', 'item', 'questcond'), 
             measure=c("accuracy"), na.rm=TRUE)
mQ<- cast(DesComp, subject ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mQ2<- cast(DesComp, questcond ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

save(q, file= "data/q.Rda")

round(mean(q$accuracy),2)
round(sd(q$accuracy),2)
mQ2<- round(mQ2, 2)

q$sentcond<- as.factor(q$sentcond)
q$sentcond<- factor(q$sentcond, levels= c("2", "3", "1"))
#levels(q$sentcond)
contrasts(q$sentcond)

summary(glmer(accuracy ~ sentcond + (sentcond|subject)+ (1|item),  family= binomial, data= q))

###############

P <- read_delim("C:/Users/mvasilev/Documents/DEVS/preproc/P.txt", 
                     " ", escape_double = FALSE, trim_ws = TRUE)
mean(P$age, na.rm=T)
sd(P$age, na.rm=T)
range(P$age, na.rm=T)

(1-table(P$gender)[2]/nrow(P))*100


