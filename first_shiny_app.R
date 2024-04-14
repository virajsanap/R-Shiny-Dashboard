####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(RCurl)
library(randomForest)
data("airquality")
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"))

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "My R Shiny first dashboard",
      tabPanel("Basic Text I/O",
               sidebarPanel(
                 tags$h3("Input:"),
                 textInput("txt1", "First Name:", ""),
                 textInput("txt2", "Last Name:", ""),
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Header 1"),
                            
                            h4("Full Name"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Histogram", 
               sidebarPanel(
                 sliderInput(inputId = "bins",
                             label = "number of bins",
                             min = 0,
                             max = 100,
                             value = 30
                             )
                 ),
               mainPanel(
                 "This panel is mainPanel",
                 plotOutput(outputId = "histo_plot")
                 ),
               ),
      tabPanel(
        "Display Table",
        sidebarPanel(
          "Sidepanel"
        ),
        mainPanel(
          DTOutput('tbl')
        )
        ),
      tabPanel(
        "ML-Iris Prediction",
        sidebarPanel(
          HTML("<h3>Input parameters</h4>"),
          sliderInput("Sepal.Length", label = "Sepal Length", value = 5.0,
                      min = min(TrainSet$Sepal.Length),
                      max = max(TrainSet$Sepal.Length)
          ),
          sliderInput("Sepal.Width", label = "Sepal Width", value = 3.6,
                      min = min(TrainSet$Sepal.Width),
                      max = max(TrainSet$Sepal.Width)),
          sliderInput("Petal.Length", label = "Petal Length", value = 1.4,
                      min = min(TrainSet$Petal.Length),
                      max = max(TrainSet$Petal.Length)),
          sliderInput("Petal.Width", label = "Petal Width", value = 0.2,
                      min = min(TrainSet$Petal.Width),
                      max = max(TrainSet$Petal.Width)),
          
          actionButton("submitbutton", "Submit", class = "btn btn-primary")
        ),
        mainPanel(
          tags$label(h3('Status/Output')), # Status/Output Text Box
          verbatimTextOutput('contents'),
          tableOutput('tabledata') # Prediction results table
        )
      )
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
    
    output$histo_plot <- renderPlot({
      x    <- airquality$Ozone
      x    <- na.omit(x)
      bins <- seq(min(x),max(x),length.out =  input$bins + 1)
      
      hist(x, breaks = bins, col="#393797",border = "black",
           xlab = "Ozone Level",
           main ="Histogram of Temp")
    })
    
    output$tbl <- renderDT(
      airquality
    )
    
    # Input Data
    datasetInput <- reactive({  
      
      df <- data.frame(
        Name = c("Sepal Length",
                 "Sepal Width",
                 "Petal Length",
                 "Petal Width"),
        Value = as.character(c(input$Sepal.Length,
                               input$Sepal.Width,
                               input$Petal.Length,
                               input$Petal.Width)),
        stringsAsFactors = FALSE)
      
      Species <- 0
      df <- rbind(df, Species)
      input <- transpose(df)
      write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
      
      Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
      print(Output)
      
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
      if (input$submitbutton>0) { 
        isolate("Calculation complete.") 
      } else {
        return("Server is ready for calculation.")
      }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
      if (input$submitbutton>0) { 
        isolate(datasetInput()) 
      } 
    })
      
      
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
