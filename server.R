#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Student: Daniel Blei       ID: xxxxxxx
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
data <- read.csv("datafile.csv",header=T,stringsAsFactors = F,  na.strings=c(NA,"NA"," NA",""))
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(waffle)

data <- na.omit(data)

data$Name <- as.factor(data$Name)
data$Price.Range[data$Price.Range == "$"] <- "Inexpensive"
data$Price.Range[data$Price.Range == "$$ - $$$"] <- "Moderate"
data$Price.Range[data$Price.Range == "$$$$"] <- "Expensive"
data$City <- as.factor(data$City)
data$Rating <- as.numeric(data$Rating)  
data$Number.of.Reviews <- as.numeric(data$Number.of.Reviews)
data$Price.Range <- as.factor(data$Price.Range)
data$Cuisine.Style <- as.factor(data$Cuisine.Style)
data$Ranking <- as.numeric(data$Ranking)

R_Numbers <- data %>% unique() %>%
  group_by(City) %>%
  summarize(avgR = mean(Rating, na.rm = T), avgNR = mean(Number.of.Reviews, na.rm = T))


tab2 <- data[,c(-1,-2,-5)]
tab3 <- data[,c(-1,-2,-4,-5,-6,-8)]

R_Numbers$City = with(R_Numbers, factor(City, levels = rev(levels(R_Numbers$City))))

shinyServer(function(input, output) {
  Sys.sleep(1)
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  
  show("app-content")

  output$summary <- renderPrint({
   
    summary(data[,input$Variables],maxsum = 10)
  })
  output$snpselect <- renderUI({
    selectInput("VB1", "Select a type of analysis:", 
                choices= list("Restaurants Rating Average per City" = "avgR",
                              "Restaurants Review Average per City" = "avgNR"))
  })
  output$DashImage <- renderPlot({
    if (is.null(input$VB1)) {
      return()
    }
   print(ggplot( R_Numbers, aes_string(x="City",y=input$VB1,fill=R_Numbers$City)) + geom_bar(stat = "identity",colour="black",width=0.7,position = 'dodge') +
      coord_flip() + guides(fill=FALSE) + xlab("City")+ ylab("") +
      theme(axis.text.x = element_text(colour="grey20",size=12),
            axis.text.y = element_text(colour="grey20",size=12),
            plot.title = element_text(hjust = 0.45,size=15,face="bold"))
   )
  })
  output$Cusine<- renderUI({
    selectInput("Cusine", "Cusine style summary from the city:", 
                choices= unique(tab2$City))
  })
   output$BoxPlot<- renderPrint({
     
     dataC <- reactive({
       
     data33 <-subset(tab2$Cuisine.Style, tab2$City == input$Cusine)
     })
     summary(dataC(),maxsum = 10)
   })
   
   output$price<- renderUI({
     selectInput("price", "Select a city to be analysed:", 
                 choices= unique(tab2$City))
   })
   
   dataR <- reactive({
     dataRR = subset(tab3, tab2$City == input$price)

     return(dataRR)
   })
   
   partsR <- reactive({
     
   a = sum(as.numeric(subset(dataR()$Price.Range,dataR()$Price.Range=="Moderate")))  
  b = sum(as.numeric(subset(dataR()$Price.Range,dataR()$Price.Range=="Inexpensive"))) 
  c = sum(as.numeric(subset(dataR()$Price.Range,dataR()$Price.Range=="Expensive")))
   parts <- c("Moderate" = a,"Inexpensive" =b,"Expensive" = c)
   return(parts)
   })
   
   output$Percentage <- renderPrint({
     xxx <- input$price
     xxxx <- sum(tab3$City == input$price)
     xx <-c("Price Percentage of the",xxxx, "restaurants in",xxx,":\n\n")
     
     cat(xx)
     print(round(partsR()/sum(partsR())*100,digits=1))
   })
   
   
   
   output$BoxPlot2<- renderPlot({
     if (is.null(partsR)) {
       return()
     }

     
       waffle(partsR()/15, rows=20, size=1,title = "Visual representation of the price range and the numbers of restaurants:",
              keep = TRUE,legend_pos = "right") +
         theme(plot.title = element_text(hjust = 0,size = 12, face = "bold", colour = "darkred"),
               legend.text = element_text(size = 12))

     

   })
 
})

