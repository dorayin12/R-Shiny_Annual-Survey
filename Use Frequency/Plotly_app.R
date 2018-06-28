#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#author: Wenjuan Sang & Justin Wilde
############################################################################################################
library(shiny)
library(plotly)


#setwd('/Volumes/Dorame/shinyApp/Plotly')
source('DataClean_likert_plotly.R')


ui <- fluidPage(
  # Application title
  titlePanel("UITS Survey"),
  
  # Sidebar with selectors 
  sidebarLayout(
    sidebarPanel(
      
      #1st page
      conditionalPanel(condition="input.tabselected==1",
                       radioButtons("qset", 
                                    "Question type:",
                                    c("Cyber Security"="agree", "General Assessment"="help", "Technology Services"="satisfy"),
                                    selected="agree"),
                       selectInput("dispara1st",
                                   "Display Parameter",
                                   choices = c("Mean Score"="mnscore",
                                               "Positive Rate (%)"="satrate",
                                               "Highly Positive Rate (%)"="hisatrate",
                                               "Usage Rate (%)"="userate",
                                               "Number of Respondents"="nresp"), selected="mnscore"),
                       checkboxGroupInput("campus1st", 
                                          "Campus:",
                                          c("IUB"="IUB", "IUE"="IUE", "IUN"="IUN", "IUPUI"="IUPUI", "IUSB"="IUSB"),
                                          selected = c("IUB"="IUB", "IUE"="IUE", "IUN"="IUN", "IUPUI"="IUPUI", "IUSB"="IUSB")),
                       checkboxGroupInput("status1st", 
                                          "Status:",
                                          c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                                            "Undergraduate"="Undergraduate", "Student*"="Student"),
                                          selected = c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                                                       "Undergraduate"="Undergraduate", "Student*"="Student")),
                       p("*Please select Student for IUE, IUN, and IUSB cases.")),
      
      #2nd page 
      conditionalPanel(condition="input.tabselected==2",
                       selectizeInput("qselect", "Type in keywords and click on the target question. You can select multiple questions.", 
                                      choices=questionList, selected = "SV001", multiple = TRUE),
                       p("*A question can be deleted by clicking on it and pressing backspace."),
                       selectInput("dispara2nd",
                                   "Display Parameter",
                                   choices = c("Mean Score"="mnscore",
                                               "Satisfaction Rate (%)"="satrate",
                                               "Highly Satisfied Rate (%)"="hisatrate",
                                               "Usage Rate (%)"="userate",
                                               "Number of Respondents"="nresp"), selected="mnscore"),
                       checkboxGroupInput("campus2nd", 
                                          "Campus:",
                                          c("IUB"="IUB", "IUE"="IUE", "IUN"="IUN", "IUPUI"="IUPUI", "IUSB"="IUSB"),
                                          selected = c("IUB"="IUB", "IUE"="IUE", "IUN"="IUN", "IUPUI"="IUPUI", "IUSB"="IUSB")),
                       checkboxGroupInput("status2nd", 
                                          "Status:",
                                          c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                                            "Undergraduate"="Undergraduate", "Student*"="Student"),
                                          selected = c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                                                       "Undergraduate"="Undergraduate", "Student*"="Student")),
                       p("*Please select Student for IUE, IUN, and IUSB cases.")),
      
      #3rd page
      conditionalPanel(condition="input.tabselected==3",
                       checkboxGroupInput("years", 
                                          "Years:*",
                                          yearList,
                                          selected=yearList),
                       p('*IUK, IUPUC, and IUS cases are only sampled on the odd years. IUE, IUN, and IUSB cases are only sampled on the even years.'),
                       selectizeInput("itemSelect", "Type in keywords and click on the target question. You can select multiple questions.",
                                      choices=questionList, selected="SV001", multiple=F, options=list(maxItems=1)),
                       p("*A question can be deleted by clicking on it and pressing backspace."),
                       checkboxGroupInput("campus", 
                                          "Campus:",
                                          c("IUB"="IUB", "IUK"="IUK", "IUPUC"="IUPUC", "IUPUI"="IUPUI", "IUS"="IUS"),
                                          selected= c("IUB"="IUB", "IUK"="IUK", "IUPUC"="IUPUC", "IUPUI"="IUPUI", "IUS"="IUS")),
                       checkboxGroupInput("subpop", 
                                          "Status:**",
                                          c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", 
                                            "Undergraduate"="Undergraduate", "Student**"="Student"),
                                          selected=c("Faculty"="Faculty", "Staff"="Staff", "Graduate"="Graduate", "Undergraduate"="Undergraduate")),
                       p("*Please select Student for IUK, IUPUC, and IUS cases.")),
      width = 2
    ),
    
    # Show plots
    mainPanel(
      tabsetPanel(
        tabPanel("Yearly Data", value="1", helpText("This is to visualize the UITS Survey of year 2018."),
                 plotlyOutput("distPlot1")),
        tabPanel("Question Comparison", value="2", helpText("This is to visualize selected questions in the UITS Survey of year 2018.
                                                             Please select questions using the left sidebar."),
                 plotlyOutput("distPlot2")),
        tabPanel("Multiyear Comparison", value="3", helpText("This is to visualize the UITS Survey in recent years."),
                 fluidRow(
                   plotOutput("plot", height=350, click="plotHover")
                 ),
                 fluidRow(
                   selectizeInput("testYears", "Please choose two years you would like to test",
                                  choices=yearList,
                                  multiple=T, options=list(maxItems=2)),
                   p('*IUK, IUPUC, and IUS cases are only sampled on the odd years. IUE, IUN, and IUSB cases are only sampled on the even years.'),
                   p('*Please select only even or odd years, when viewing these campuses\' results'),
                   radioButtons("alphaL",
                                "Choose an alpha level:",
                                c(0.10, 0.05, 0.01),
                                selected=0.05),
                   actionButton(inputId="go", label="Perform t-test")
                 ),
                 fluidRow(
                   p('')
                 ),
                 fluidRow(
                   tags$h4("Results"),
                   htmlOutput("text"),
                   tags$style('#p{line-height: 1.75;
                              }')
      )),
        id = "tabselected"
      ),
      width=10
    )
  )
)

server <- function(input, output, session) {
  #selected question set
  scale <- reactive({
    get(input$qset)
  })
  output$dat <- renderPrint({
    scale()
  })
  #1st tab
  observe({output$distPlot1 <- renderPlotly({
    yearlyPlotfun(scale(), input$qset, c(input$campus1st), c(input$status1st), input$dispara1st)})
  })
  #2nd tab
  output$distPlot2 <- renderPlotly({
    comparePlotfun(c(input$qselect), c(input$campus2nd), c(input$status2nd), input$dispara2nd)})
  #3rd tab
  
  # First plot
  observeEvent(c(input$years, input$itemSelect, input$campus, input$subpop), {
    output$plot<-renderPlot({
      if(is.null(input$years)==T | is.null(input$itemSelect)==T | is.null(input$campus)==T | is.null(input$subpop)==T){
        par(mar = c(0,0,0,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("No Data"), cex = 1.6, col = "black")
      } else {
        multiYear(input$years, input$itemSelect, c(input$campus), c(input$subpop))
      }
    })
  })
  # Plot on hover
  observeEvent(input$plotHover,{
    x=input$plotHover$x
    y=input$plotHover$y
    nearPoint<-nearPoints(plotDF, input$plotHover, threshold=10, maxpoints=1)
    output$plot<-renderPlot({
      if(nrow(nearPoint)==1){
        multiYear(input$years, input$itemSelect, c(input$campus), c(input$subpop)) +
          #geom_hline(yintercept=(nearPoint$mean-nearPoint$ci), color="blue", alpha=0.5) +
          #geom_hline(yintercept=(nearPoint$mean+nearPoint$ci), color="blue", alpha=0.5) +
          #geom_ribbon(ymin=(nearPoint$mean-nearPoint$ci), ymax=(nearPoint$mean+nearPoint$ci),
          #            fill="lightblue", alpha=0.5) +
          geom_label(x=x+(nearPoint$ci)+0.01, y=y,
                     label=paste0(paste("Sample: ", nearPoint$nSub), "\n", #nearPoint$Campus, "\n", nearPoint$type2String, "\n",
                                  paste("Mean: ", round(nearPoint$mean, digits=2)), "\n",
                                  paste("SE: ", round(nearPoint$se, digits=2))),
                     fill="darkgreen", fontface="bold", color="white",
                     label.padding=unit(0.5, "lines"), hjust="left", label.size=0.5)#, lable.r=0.35)
      } else {
        multiYear(input$years, input$itemSelect, c(input$campus), c(input$subpop))
      }
    })
  })
  # Text
  data1<-eventReactive(input$go, {
    input$testYears
  })
  data2<-eventReactive(input$go, {
    input$itemSelect
  })
  data3<-eventReactive(input$go, {
    input$campus
  })
  data4<-eventReactive(input$go, {
    input$subpop
  })
  data5<-eventReactive(input$go, {
    input$alphaL
  })
  output$text<-renderUI({
    HTML(tTEST(c(data1()), data2(), c(data3()), c(data4()), as.numeric(data5())))
  })
}

shinyApp(ui, server)