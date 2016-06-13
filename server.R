library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(xlsx)
# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
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

    if (input$ID != "All") {
      data <- tab[tab$ID == input$ID,]
    }
    if (input$MT != "All" & input$MT == '1') {
 #     lecw$MT2 <- NULL
      data <- data.frame(ID=1:32,MT1=tab$MT1)
    }
    if (input$MT != "All" & input$MT == '2') {
   #   lecw$MT1 <- NULL
      data <- data.frame(ID=1:32,MT2=tab$MT2)
    }
    if (input$final != "All") {
      data <- tab[tab$final == input$final,]
    }
    if (input$lab != 'All' & input$lab == 1){
      data <- data.frame(ID=1:32,lab1=tab$lab1)
    }
    if (input$lab != 'All' & input$lab == 2){
      data <- data.frame(ID=1:32,lab2=tab$lab2)
    }
    if (input$lab != 'All' & input$lab == 3){
      data <- data.frame(ID=1:32,lab3=tab$lab3)
    }
    if (input$lab != 'All' & input$lab == 4){
      data <- data.frame(ID=1:32,lab4=tab$lab4)
    }
    if (input$lab != 'All' & input$lab == 5){
      data <- data.frame(ID=1:32,lab5=tab$lab5)
    }
    if (input$lab != 'All' & input$lab == 6){
      data <- data.frame(ID=1:32,lab6=tab$lab6)
    }
    if (input$lab != 'All' & input$lab == 7){
      data <- data.frame(ID=1:32,lab7=tab$lab7)
    }
    if (input$lab != 'All' & input$lab == 8){
      data <- data.frame(ID=1:32,lab8=tab$lab8)
    }
    if (input$lab != 'All' & input$lab == 9){
      data <- data.frame(ID=1:32,lab9=tab$lab9)
    }
    if (input$lab != 'All' & input$lab == 10){
      data <- data.frame(ID=1:32,lab10=tab$lab10)
    }
    if (input$lab != 'All' & input$lab == 11){
      data <- data.frame(ID=1:32,lab11=tab$lab11)
    }
    if (input$lab != 'All' & input$lab == 12){
      data <- data.frame(ID=1:32,lab12=tab$lab12)
    }
    if (input$quiz != 'All' & input$quiz == 1){
      data <- data.frame(ID=1:32,quiz1=tab$quiz1)
    }
    if (input$quiz != 'All' & input$quiz == 2){
      data <- data.frame(ID=1:32,quiz2=tab$quiz2)
    }
    tab
  }))
  
})

