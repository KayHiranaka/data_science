library(xlsx)
library(dplyr)
library(stringr)
library(readODS)

# calculate lab grade
lab <- read.csv('BMCC_AST110B_SP2016.csv',1)
labw <- lab[,-(3:8)]
labw[is.na(labw)] <- 0
labw[1,15] <- 10
labw[1,16] <- 0
labw[3,13] <- 10
labw[3,16] <- 0
labw[10,15] <- 10
labw[10,16] <- 0
labw[14:15,3] <- 10
labw[14:15,16] <- 0
labw[c(17,20,23),14] <- 10
labw[c(17,20,23),16] <- 0
labw <- labw[,-16]

labww <- labw[,c(-1,-2)]
labww <- t(apply(labww,1,sort)) #sort remaining rows (only labs)
labww <- labww[,-2] #drop 2 lowest labs
totl <- rowSums(labww)*10/12
labw <- mutate(labw, totlab = totl)

#calculate lecture grade
lec <- read.csv('BMCC_AST108L_SP2016.csv',1)

lecw <- lec[,-(3:8)]
lecw[is.na(lecw)] <- 0

lecw[,3] <- as.double(lecw[,3])
lecquiz <- lecw[,c(3:7,9:10)]
lecexam <- lecw[, c(8,11:12)]

# calculate hw grade
lecqw <- t(apply(lecquiz,1,sort)) #sort remaining rows (only labs)
lecqw <- lecqw[,-1] #drop lowest lab
totq <- rowSums(lecqw)*10/6
lecqw <- mutate(lecquiz, totquiz = totq)

#calculate exam grade
tote <- rowSums(lecexam)
lecew <- mutate(lecexam, totexam = tote)

# calculate final grade
#grade <- totq*0.3 + totl*0.2 + tote*0.25
grade <- totq*0.4 + tote*0.3
finalgrade108 <- lecw[,1:2] %>%
  mutate(grade = grade)


