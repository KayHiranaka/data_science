library(shiny)
library(readODS)
# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
lec <- read.xlsx('../../project1/project1_data/lecture.xlsx',1)
lecw <- data.frame(ID = 1:32, MT1 = lec$MT1, MT2 = lec$MT2, final = lec$Final)
lab12 <- read.xlsx('../../project1/project1_data/Fall2015Lab120_Sec 1L12.xlsx',1,startRow = 8,endRow = 20)
lab12[is.na(lab12)] <- 0 # fill in NULL
lab12$X.FINAL.LAB.GRADE.AND.COMMENT <- NULL #drop column
lab09 <- read.ods('../../project1/project1_data/Fall2015Lab120section 1L09.ods',1)
lab09 <- lab09[-c(1:7),] #refine table
lab09 <- lab09[-c(22:41),] #patience
lab09 <- lab09[,-16] #getting there
lab09[1,14] <- 'Quiz1'
lab09[1,15] <- 'Quiz2'
names(lab09) <- lab09[1,]
lab09 <- lab09[-1,]
lab09[lab09==''] <- 0 
tab <- data.frame(ID = 1:32, MT1 = lec$MT1, MT2 = lec$MT2, final = lec$Final, 
                  lab1 = c(lab09[,2],lab12[,2]),lab2 = c(lab09[,3],lab12[,3]),
                  lab3 = c(lab09[,4],lab12[,4]),lab4 = c(lab09[,5],lab12[,5]),
                  lab5 = c(lab09[,6],lab12[,6]),lab6 = c(lab09[,7],lab12[,7]),
                  lab7 = c(lab09[,8],lab12[,8]),lab8 = c(lab09[,9],lab12[,9]),
                  lab9 = c(lab09[,10],lab12[,10]),lab10 = c(lab09[,11],lab12[,11]),
                  lab11 = c(lab09[,12],lab12[,12]),lab12 = c(lab09[,13],lab12[,13]),
                  quiz1=c(lab09$Quiz1,lab12$QUIZ.1),quiz2=c(lab09$Quiz2,lab12$QUIZ.2))

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(2,
             selectInput("ID",
                         "ID:",
                         c("All",
                           unique(as.character(lecw$ID))))
      ),
      column(2,
             selectInput("MT",
                         "Midterm:",
                         c("All",
                           '1','2'))
      ),

      column(2,
             selectInput("final",
                         "Final Exam:",
                         c("All",
                           unique(as.character(lecw$final))))
      ),
      column(2,
             selectInput("lab",
                         "Lab:",
                         c("All",
                           1:12))
      ),
      column(2,
             selectInput("quiz",
                         "Quiz:",
                         c("All",
                           1:2))
      )
      
    ),
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("table")
    )
  )
)
