library(xlsx)

lec <- read.xlsx('project1_data/lecture.xlsx',1)

lecw <- data.frame(ID = 1:32, MT1 = lec$MT1, MT2 = lec$MT2, final = lec$Final)

mtfrac1 <- 0.2*lecw[lecw$MT1 <= lecw$MT2, 'MT1'] + 0.35*lecw[lecw$MT1 <= lecw$MT2, 'MT2'] 
mtfrac2 <- 0.2*lecw[lecw$MT2 < lecw$MT1, 'MT2'] + 0.35*lecw[lecw$MT2 < lecw$MT1, 'MT1']

id <- c(lecw[lecw$MT1 <= lecw$MT2,'ID'],lecw[lecw$MT1 > lecw$MT2,'ID'])
mt1 <- c(MT1 = lecw[lecw$MT1 <= lecw$MT2,'MT1'],MT1 = lecw[lecw$MT1 > lecw$MT2,'MT1'])
mt2 <- c(MT2 = lecw[lecw$MT1 <= lecw$MT2,'MT2'],MT2 = lecw[lecw$MT1 > lecw$MT2,'MT2'])
final <- c(lecw[lecw$MT1 <= lecw$MT2,'final'],lecw[lecw$MT1 > lecw$MT2,'final'])

lecww <- data.frame(ID=id,MT1=mt1,MT2=mt2,final=final)
lecww <- mutate(lecww, MTfrac = c(mtfrac1,mtfrac2))

lecww <- mutate(lecww, lecgrade = MTfrac + final*0.45)
lecww <- lecww[order(lecww$ID),]

