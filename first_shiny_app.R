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
data("airquality")


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
      tabPanel("Navbar 3", "This panel is intentionally left blank")
  
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
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
