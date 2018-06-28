#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#author: Wenjuan Sang
############################################################################################################
#cat(file=stderr(), "Starting app", "\n")
library(shiny)

#setwd('/srv/shiny-server/uitssur/')
source('DataClean_likert.R')


# Define UI for application that draws a plot
ui <- fluidPage(
  # Application title
  titlePanel("UITS Survey"),
  
  # Sidebar with selectors 
  sidebarLayout(
    sidebarPanel(
      radioButtons("qset", 
                   "Question type:",
                   c("Cyber Security"="agree", "General Assessment"="help", "Technology Services"="satisfy"),
                   selected="agree"),
      selectInput("dispara",
                  "Display Parameter",
                  choices = c("Mean Score"="mnscore",
                              "Satisfaction Rate (%)"="satrate",
                              "Highly Satisfied Rate (%)"="hisatrate",
                              "Usage Rate (%)"="userate",
                              "Number of Respondents"="nresp"), selected="mnscore"),
      checkboxGroupInput("campus", 
                         "Campus:",
                         c("IUB"="IUB", "IUK"="IUK", "IUPUC"="IUPUC", "IUPUI"="IUPUI", "IUS"="IUS"),
                         selected = c("IUB"="IUB", "IUK"="IUK", "IUPUC"="IUPUC", "IUPUI"="IUPUI", "IUS"="IUS")),
      checkboxGroupInput("status", 
                         "Status:",
                         c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                           "Undergraduate"="Undergraduate", "Student*"="Student"),
                         selected = c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                         "Undergraduate"="Undergraduate", "Student*"="Student")),
      p("*Please select Student for IUK, IUPUC, and IUS cases."),
      width = 2
    ),
    
    
    # Show a plot
    mainPanel(
      tabsetPanel(
        tabPanel("Yearly Data", plotOutput("distPlot1")),
        tabPanel("Multi-Year Data",NULL)
      ),
      width=10
    )
  )
)



#server
server <- function(input, output) {
  #qset
  scale <- reactive({
    get(input$qset)
  })
  output$dat <- renderPrint({
    scale()
  })
  observe({output$distPlot1 <- renderPlot({
    testfun(scale(), input$qset, c(input$campus), c(input$status), input$dispara)}, 
    width=1500,
    if(input$qset=="satisfy"){height=2500}
    else{height=600})
  })
}

#run
shinyApp(ui = ui, server = server)
