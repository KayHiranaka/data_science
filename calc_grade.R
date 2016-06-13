library(xlsx)
library(dplyr)
library(stringr)
library(readODS)

#calculate lecture grade
lec <- read.xlsx('project1_data/lecture.xlsx',1)

lecw <- data.frame(ID = 1:32, MT1 = lec$MT1, MT2 = lec$MT2, final = lec$Final) #working data frame

mtfrac1 <- 0.2*lecw[lecw$MT1 <= lecw$MT2, 'MT1'] + 0.35*lecw[lecw$MT1 <= lecw$MT2, 'MT2'] #midterm grade if midterm1<=midterm2
mtfrac2 <- 0.2*lecw[lecw$MT2 < lecw$MT1, 'MT2'] + 0.35*lecw[lecw$MT2 < lecw$MT1, 'MT1'] # midterm grade if midterm1>midterm2

id <- c(lecw[lecw$MT1 <= lecw$MT2,'ID'],lecw[lecw$MT1 > lecw$MT2,'ID'])
mt1 <- c(MT1 = lecw[lecw$MT1 <= lecw$MT2,'MT1'],MT1 = lecw[lecw$MT1 > lecw$MT2,'MT1']) #lowest midterm
mt2 <- c(MT2 = lecw[lecw$MT1 <= lecw$MT2,'MT2'],MT2 = lecw[lecw$MT1 > lecw$MT2,'MT2']) #highest midterm
final <- c(lecw[lecw$MT1 <= lecw$MT2,'final'],lecw[lecw$MT1 > lecw$MT2,'final'])

lecww <- data.frame(ID=id,MT1=mt1,MT2=mt2,final=final)
lecww <- mutate(lecww, MTfrac = c(mtfrac1,mtfrac2))
MTfrac = c(mtfrac1,mtfrac2)
lecg <- MTfrac + final*0.45 #grade for lecture
lecww <- mutate(lecww, lecgrade = MTfrac + final*0.45)
lecww <- lecww[order(lecww$ID),] # sort by ID

#calculate lab12 grade
lab12 <- read.xlsx('project1_data/Fall2015Lab120_Sec 1L12.xlsx',1,startRow = 8,endRow = 20)
lab12[is.na(lab12)] <- 0 # fill in NULL
lab12$X.FINAL.LAB.GRADE.AND.COMMENT <- NULL #drop column

quizfrac <- 0.1*5*(lab12$QUIZ.2+lab12$QUIZ.1) #calculate quiz grade
#lab12 <- mutate(lab12, quizfrac=quizfrac)

lab12w <- lab12 %>%
  mutate(Student.s.Name = str_replace(Student.s.Name, 's','') )
lab12w$QUIZ.1 <- NULL #drop column
lab12w$QUIZ.2 <- NULL #drop column
lab12w$Student.s.Name <- NULL #drop more column

lab12w <- t(apply(lab12w,1,sort)) #sort remaining rows (only labs)
lab12w <- lab12w[,-(1:2)] #drop two lowest labs

labfrac <- rowSums(lab12w)/10*0.8 #calculate lab grade
#labgrade <- data.frame(ID=21:32,quizfrac=quizfrac,labfrac=labfrac,labgrade=quizfrac+labfrac)

#calculate lab9 grade
lab09 <- read.ods('project1_data/Fall2015Lab120section 1L09.ods',1)
lab09 <- lab09[-c(1:7),] #refine table
lab09 <- lab09[-c(22:41),] #patience
lab09 <- lab09[,-16] #getting there
lab09[1,14] <- 'Quiz1'
lab09[1,15] <- 'Quiz2'
names(lab09) <- lab09[1,]
lab09 <- lab09[-1,]


quizfr <- 0.1*5*(as.numeric(lab09w[,14])+as.numeric(lab09w[,15])) #calculate quiz grade
lab9 <- data.frame(ID=1:20,quizfrac=quizfr)

lab09w <- lab09w[,-c(14:15)] #drop quiz columns
lab09w <- lab09w[,-1] #and ID

lab09ww <- lab09[,1:13] 
lab09ww <- lab09ww[,-1]
lab09ww[lab09ww==100] <- 999 #strings are not behaving (they think 100 is small). I'm giving '100's a special treatment.
lab09ww <- t(apply(lab09ww,1,sort)) #now sort rows
lab09ww[lab09ww==999] <- 100 #put back 100
lab09ww <- lab09ww[,-(1:2)] #drop two lowest lab grades
lab09ww[lab09ww==''] <- 0 #replace blank elements with 0

lab09www <- as.data.frame(lab09ww, stringsAsFactors=FALSE) #here convert strings into numeric
for (column in 1:10) {
  lab09www[, column] <- as.numeric(lab09www[, column])
}
labfr <- rowSums(lab09www)/10*0.8 #calculate lab grades
lab09www <- mutate(lab09www,labfrac=labfr)

quizf <- c(quizfr,quizfrac) #combine quiz grades from two sections
labf <- c(labfr,labfrac) #combine lab grades
labg <- quizf+labf #add them up
grade <- lecg*0.85 + labg*0.15 #finally calculate final grades!
final_grade <- data.frame(ID = 1:32, lecgrade=lecg,quizfrace=quizf, 
                          labfrac=labf, labgrade=quizf+labf, finalgrade=grade)

  