
# Martin R. Vasilev, 2017

library(readr)

design<- NULL

for(i in 1:48){
  t <- read_delim(paste("design/P", toString(i), ".txt", sep=''), 
                       " ", escape_double = FALSE, trim_ws = TRUE)
  
  t$sub<- i
  design<- rbind(design, t)
}

design<- subset(design, item<121) # remove practice items

table(design$item, design$sound)
table(design$item, design$pos)
table(design$sound, design$pos)
table(design$item, design$delay)
table(design$sound, design$delay, design$pos)

b<- data.frame(table(design$item, design$pos, design$sound))

a<- data.frame(table(design$item, design$sound, design$delay))

c<- data.frame(table(design$item, design$sound, design$pos, design$delay))

range(c$Freq)