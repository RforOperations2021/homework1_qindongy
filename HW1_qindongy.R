library(readr)
library(shiny)
library(ggplot2)
library(data.table)
library(DT)

# Data Import and cleaning
data <-read_csv("covid_19_tests.csv")
data$collection_date <- as.Date(data$collection_date)
data$update_date <- as.Date(data$update_date)
setDT(data)
data[, month := month(collection_date)]
data <- na.omit(data)


# Convert categorical feature to Numeric feature
conversion <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == "Negative" | x[i] == "N" | x[i] == "F") {
      x[i] = 0
    }
    else if (x[i] == "Positive" | x[i] == "Y" | x[i] == "M") {
      x[i] = 1
    }
  }
  return (x)
}
# Aggregate the Data to Monthly Level and pick 5 most important features for visualization
data <- as.data.frame(data)
data[, c(3, 5, 6, 7, 9)] <- sapply(data[, c(3, 5, 6, 7, 9)], conversion)
data[, c(3, 5, 6, 7, 9)] <- sapply(data[, c(3, 5, 6, 7, 9)], as.numeric)
data <- na.omit(data)
setDT(data)
data_agg <-
  data[, .(mean(test_result),
           mean(hospital_flag),
           mean(icu_flag),
           mean(vent_flag),
           mean(sex)), by = .(month)]
names(data_agg)[2:6] <-
  c("test_result", "hospital_flag", "icu_flag", "vent_flag", 'sex')

# Create quarterly label
quarter = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
data_agg[, Quarter := quarter]




ui <- fluidPage(# Sidebar layout with a input and output definitions --------------
                sidebarLayout(
                  # Inputs: Select variables to plot ------------------------------
                  sidebarPanel(
                    # Select variable for y-axis ----------------------------------
                    selectInput(
                      inputId = "x",
                      label = "X-axis:",
                      choices = c("month"),
                      selected = "month"
                    ),
                    
                    # Select variable for x-axis ----------------------------------
                    selectInput(
                      inputId = "y",
                      label = "Y-axis:",
                      choices = c("test_result", "hospital_flag", "icu_flag", "vent_flag", "sex"),
                      selected = "test_result,"
                    ),
                    # Select variable for box-plot----------------------------------
                    radioButtons(
                      inputId = "variable",
                      label = "Variables to show for the box plot:",
                      choices = c("test_result", "hospital_flag", "icu_flag", "vent_flag", "sex"),
                      selected = "test_result"
                    ),
                  ),
                  
                  
                  # Output: Show mainPanel --------------------------------------
                  mainPanel(
                    h3("Pittsburgh Covid Testing Data Analysis"),
                    downloadButton('downloadbutton', 'Download The Covid Data Here!'),
                    plotOutput(outputId = "line"),
                    plotOutput("box"),
                    h2("Summary of the variable"),
                    verbatimTextOutput("sum"),
                    DTOutput('tbl'),
                    
                  )
                ))

server <- function(input, output) {
  # Create lineplot object--
  output$line <- renderPlot({
    ggplot(data = data_agg, aes_string(x = input$x, y = input$y)) +
      geom_line()
  })
  # Create boxplot object--
  output$box <- renderPlot({
    boxplot(
      get(input$variable) ~ Quarter,
      col = "white",
      border = "black",
      data = data_agg,
      main = "boxplot of covid data"
    )
  })
  # Create summary statistic object--
  output$sum <- renderPrint({
    summary(data_agg)
  })
  
  
  # Create a Data Table--
  output$tbl = renderDT(data_agg, options = list(lengthChange = FALSE))
  
  # Create a Download Botton
  output$downloadbutton <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(data_agg, con)
    }
  )
  
}

# Deploy the app
shinyApp(ui = ui, server = server)
