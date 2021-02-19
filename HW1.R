library(readr)
library(shiny)
library(ggplot2)
library(data.table)
data<- read_csv("C:/Users/Yangq/OneDrive/Desktop/download/covid_19_tests.csv")
data$collection_date<-as.Date(data$collection_date)
data$update_date<-as.Date(data$update_date)
setDT(data)
data[,month:=month(collection_date)]
data <- na.omit(data)



conversion<-function(x){
  for (i in 1:length(x)){
  if(x[i]=="Negative" | x[i]=="N" | x[i]=="F"){
    x[i]=0
  }
  else if(x[i]=="Positive" | x[i]=="Y" |x[i]=="M"){
    x[i]=1
  }
  }
  return (x)
}
data<-as.data.frame(data)
data[,c(3,5,6,7,9)]<-sapply(data[,c(3,5,6,7,9)],conversion)
data[,c(3,5,6,7,9)]<-sapply(data[,c(3,5,6,7,9)],as.numeric)
setDT(data)
data_agg<-data[,.(mean(test_result),mean(hospital_flag),mean(icu_flag),mean(vent_flag),mean(sex)),by=.(collection_date)]






ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("month"), 
                  selected = "month"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("month"), 
                  selected = "month,")
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

server <-function(input,output){
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = data, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  
}

  shinyApp(ui = ui, server = server)
