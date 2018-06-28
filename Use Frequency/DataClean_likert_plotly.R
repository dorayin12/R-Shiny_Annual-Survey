#This uses the itemsLikert_Shiny data to plot stacked percentage bars
#Package: plotly
#
#
#author: Wenjuan Sang & Justin Wilde
############################################################################################################

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)
library(data.table)
library(plotly)
library(ggpubr)

setwd("D:/shinyApp/Plotly")
###############################################################################
load("D:/shinyApp/Plotly/itemDesc_Shiny2018.Rda")
itemDesc_Shiny <- itemDesc_Shiny2018        
##################  Change two lines above for most recent year ###############


#1) preparation: group variables based on question types
satisfy <- itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% c("SV001",	"SV002",	"SV003",	"SV004",	"SV005",	"SV006",	"SV007",	"SV008",	"SV009",	"SV010",
                                                          "SV011",	"SV012",	"SV013",	"SV014",	"SV015",	"SV016",	"SV017",	"SV018",	"SV019",	"SV020",
                                                          "SV021",	"SV022",	"SV023",	"SV024",	"SV025",	"SV026",	"SV027",	"SV028",	"SV029",	"SV030",
                                                          "SV031",	"SV032",	"SV033",	"SV034",	"SV035",	"SV036",	"SV037",	"SV038",	"SV039",	"SV040",
                                                          "SV041",	"SV042",	"SV043",	"SV044",	"SV045",	"SV046",	"SV047",	"SV048",	"SV049",	"SV050",
                                                          "SV051",	"SV052",	"SV053",	"SV054",	"SV055",	"SV056",	"SV057",	"SV058",	"SV059",	"SV060",
                                                          "SV061",	"SV062",	"SV063",	"SV064",	"SV065",	"SV066",	"SV067",	"SV068",	"SV069",	"SV070",
                                                          "SV071",	"SV072",	"SV073",	"SV074",	"SV075",	"SV076",	"SV077",	"SV078",	"SV079",	"SV080",
                                                          "SV081",	"SV082",	"SV083",	"SV084",	"SV085",	"SV086",	"SV087",	"SV088",	"SV089",	"SV090",
                                                          "SV091",	"SV092"),]
agree <- itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% c("AV001",	"AV002",	"AV003",	"AV004",	"AV005"),]
help  <- itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% c("HV001",	"HV002",	"HV003"),]
questionList <-c("Overall satisfaction with UITS services"                                    ="SV001",
                 "Importance of UITS services"                                                ="SV002",
                 "Computer expertise"                                                         ="SV003",
                 "Umail"                                                                      ="SV005",
                 "Microsoft Exchange/Outlook/OWA"                                             ="SV006",
                 "Telephone Consulting"                                                       ="SV011",
                 "Walk-in Consulting"                                                         ="SV012",
                 "Chat  Consulting"                                                           ="SV013",
                 "Email  Consulting"                                                          ="SV014",
                 "Knowledge  Base"                                                            ="SV015",
                 "Personal  Consulting"                                                       ="SV017",
                 "IT Training self-study services"                                            ="SV018",
                 "IUanyWare"                                                                  ="SV019",
                 "IT Training instructor-led training"                                        ="SV020",
                 "IUware"                                                                     ="SV021",
                 "Cloud Storage at IU"                                                        ="SV022",
                 "Overall satisfaction with UITS support services"                            ="SV024",
                 "Electronic  News"                                                           ="SV025",
                 "Social  Media"                                                              ="SV026",
                 "UITS  Websites"                                                             ="SV027",
                 "Overall satisfaction with UITS communications"                              ="SV029",
                 "Hardware"                                                                   ="SV030",
                 "Software"                                                                   ="SV031",
                 "Black & white and color printing services"                                  ="SV032",
                 "Plotters"                                                                   ="SV033",
                 "Infostations"                                                               ="SV036",
                 "Overall satisfaction with UITS instructional and student computing services"="SV038",
                 "Quality of classroom technology services"                                   ="SV039",
                 "Video conferencing"                                                         ="SV040",
                 "Class Recording"                                                            ="SV042",
                 "Kaltura Mediaspace"                                                         ="SV044",
                 "Overall satisfaction with physical learning space design in the Student Technology
                 Centers"                                                                     ="SV045",
                 "Overall satisfaction with the design of classroom learning spaces"          ="SV046",
                 "Supercomputers/HPC"                                                         ="SV047",
                 "High speed disk storage"                                                    ="SV048",
                 "Scholarly Data Storage"                                                     ="SV049",
                 "Advanced Visualization Laboratory"                                          ="SV050",
                 "Research Analytics"                                                         ="SV051",
                 "Support for Life Sciences"                                                  ="SV053",
                 "Overall satisfaction with UITS research technology services"                ="SV054",
                 "General SIS functionality"                                                  ="SV055",
                 "Academic Advising Services"                                                 ="SV057",
                 "Student Self Service"                                                       ="SV058",
                 "Canvas"                                                                     ="SV064",
                 "General Functionality and Employee Self Service"                            ="SV065",
                 "Kuali Financial Information System (KFS)"                                   ="SV067",
                 "One.iu.edu"                                                                 ="SV073",
                 "Overall satisfaction with academic and business systems"                    ="SV075",
                 "The decommissioning process of Oncourse"                                    ="SV076",
                 "The decommissioning process of OneStart"                                    ="SV077",
                 "Overall satisfaction with the information about security threats provided by UITS"
                                                                                              ="SV078",
                 "Departmental publishing on IU Web servers"                                  ="SV082",
                 "Individual publishing on IU Web servers"                                    ="SV083",
                 "Web Content Management"                                                     ="SV084",
                 "Wireless Network"                                                           ="SV086",
                 "Wired Network"                                                              ="SV087",
                 "Overall satisfaction with the network services"                             ="SV088",
                 "Telephone System"                                                           ="SV089",
                 "Lync/Skype for Business"                                                    ="SV090",
                 "IU Fax"                                                                     ="SV091")


#2) throw into function
#############################################################################################################
#
#1st page: yearly data
#
#############################################################################################################

yearlyPlotfun <- function(qset, qname, campusList, typeList, dispara){
  #2) subset dataset
  #campus
  campusList=campusList
  #type of population
  typeList=typeList
  #subset 
  resourcedata <- qset

  tabdata<-resourcedata[resourcedata$Campus %in% campusList & 
                        resourcedata$type2String %in% typeList,]

  tabdata <- as.data.frame(tabdata)
  #3) matrix header and title
  #levels
  agglevels <- c("Strongly disagree", "Disagree", "Neutral", "Agree",  "Strongly agree")
  satlevels <- c("Not at all Satisfied", "Not satisfied", "Neural", "Satisfied", "Very Satisfied")
  helplevels <- c("Not at all Helpful", "Not helpful", "Neutral", "Helpful", "Very Helpful")
  #titles
  aggtitle  <- "To what extent do you agree with the following statements? (unit: percentage)"
  sattitle  <- "To what extent are you satisfied with the following services? (unit: percentage)"
  helptitle <- "To what extent do you think following services are helpful? (unit: percentage)"
  if(qname == "agree"){
    mylevels <- agglevels
    mytitle  <- aggtitle
    myheight <- 500
  }
  else if(qname == "satisfy"){
    mylevels <- satlevels
    mytitle  <- sattitle
    myheight <- 4000
  }
  else{
    mylevels <- helplevels
    mytitle  <- helptitle
    myheight <- 350
  }

  #create plot dataset
  varList<-c("Master.ID","Not at all Satisfied","2","3","4","Very Satisfied","Nsub","Question", "Not Applicable", "mean", "Service")
  likertPlot<-tabdata[varList]
  colnames(likertPlot)[2:6] <- mylevels
  
  #4) calculation
  #split by questions
  unique_row <- unique(likertPlot$Question) #get unique questions
  questionCat <- unique(likertPlot[,c("Question", "Service", "Master.ID")])
  number_quest <- length(unique_row)
  splitList <- split(likertPlot, likertPlot$Master.ID) #split dataset by question
  oneqst <- c()
  totalv <- c()
  totalv2 <- c()

  #calculate percentage & mean
  for (i in 1:number_quest){
    split_data <- splitList[[i]]
    totalv  <-  split_data[,2] + split_data[,3] + split_data[,4] + split_data[,5] + split_data[,6] #1-5
    totalv2 <-  split_data[,2] + split_data[,3] + split_data[,4] + split_data[,5] + split_data[,6] + split_data[,9] #1-6  
    tempcol1 <- sum(split_data[,2]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 1
    tempcol2 <- sum(split_data[,3]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 2
    tempcol3 <- sum(split_data[,4]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 3
    tempcol4 <- sum(split_data[,5]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 4
    tempcol5 <- sum(split_data[,6]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 5
    tempcol6 <- sum(split_data[,10]*split_data$Nsub)/sum(split_data$Nsub)       # mean
    tempcol7 <- tempcol3 + tempcol4 + tempcol5                                       # satisfaction rate
    tempcol8 <- tempcol4 + tempcol5                                                  # high satisfaction rate
    tempcol9 <- sum((1-split_data[,9]/totalv2)*split_data$Nsub)/sum(split_data$Nsub)  #usage rate
    tempcol10<- sum(split_data[,2],split_data[,3],split_data[,4],split_data[,5],split_data[,6],split_data[,9])     # number of respondents
    oneqst <- c(oneqst, round(100*tempcol1,2), round(100*tempcol2,2), 
                round(100*tempcol3,2), round(100*tempcol4,2), round(100*tempcol5,2), 
                round(tempcol6,2), 100*round(tempcol7,4), 100*round(tempcol8,4), 100*round(tempcol9,4), round(tempcol10, 0))
    #numbers <- oneqst[!is.na(oneqst)]
  }
  my_matrix <- matrix(oneqst, ncol=10, nrow=length(unique_row), byrow=TRUE)
  my_df <- as.data.frame(my_matrix)
  #colnames(my_df) <- c(mylevels, "Meanscore", "SatisfactionRate", "HighlySatisfiedRate", "Usage", "Nresp")
  my_df <- cbind(unique_row, my_df)
  my_df <- my_df[complete.cases(my_df),]
  my_df$ID <- seq.int(nrow(my_df))
  my_df$Master.ID <- questionCat$Master.ID
  my_df$Service <- questionCat$Service

  #5) plot
  
  #create a new db
  tab<-data.frame(my_df)
  names(tab)[1] <- "y"
  names(tab)[2] <- "x1"
  names(tab)[3] <- "x2"
  names(tab)[4] <- "x3"
  names(tab)[5] <- "x4"
  names(tab)[6] <- "x5"   
  names(tab)[7] <- "x6"   # mean
  names(tab)[8] <- "x7"   # satisfaction rate
  names(tab)[9] <- "x8"   # high satisfaction rate
  names(tab)[10] <- "x9"  # usage rate
  names(tab)[11] <- "x10" # number of respondents
  if(qname == "satisfy"){
    tab$y<-str_wrap(tab$y, width = 30)
  }
  else{
    tab$y<-str_wrap(tab$y, width = 50)
  } 
  tab$Service <- str_wrap(tab$Service, width = 20)
  

  #plotting
  if (length(unique(tab$Service))==1){
    #select a parameter to show
    #mean is selected
    if (dispara=="mnscore"){ 
      tracex <- tab$x6
      namex <- "Mean"}
    #satisfaction rate is selected
    else if (dispara=="satrate"){  
      tracex <- tab$x7
      namex <- "Positive rate"}
    #highly satisfied rate is selected
    else if (dispara=="hisatrate"){
      tracex <- tab$x8
      namex <- "Highly positive"}
    #user response rate is selected
    else if (dispara=="userate"){ 
      tracex <- tab$x9
      namex <- "User Response Rate"}
    #number of response is selected
    else {
      tracex <- tab$x10
      namex <- "Number of Response"}
    #hover text for parameters
    my_text=paste(namex) 
    #hover text for likert scales
    # x1
    tab$text_x1 <- paste(mylevels[1], round(tab$x1,2), "%")
    # x2
    tab$text_x2 <- paste(mylevels[2], round(tab$x2,2), "%")
    # x3
    tab$text_x3 <- paste(mylevels[3], round(tab$x3,2), "%")
    # x4
    tab$text_x4 <- paste(mylevels[4], round(tab$x4,2), "%")
    # x5
    tab$text_x5 <- paste(mylevels[5], round(tab$x5,2), "%")
  p <- plot_ly(tab) %>%
    add_trace(x = ~-x2, y = ~y, text=tab$text_x2, hoverinfo="text", marker = list(color = "rgb(244,165,130)"), type = "bar", orientation = "h") %>%
    add_trace(x = ~-x1, y = ~y, text=tab$text_x1, hoverinfo="text", marker = list(color = "rgb(202,0,32)"), type = "bar", orientation = "h") %>%
    add_trace(x = ~x3, y = ~y, text=tab$text_x3, hoverinfo="text", marker = list(color = "rgb(223,223,223)"), type = "bar", orientation = "h") %>%
    add_trace(x = ~x4, y = ~y, text=tab$text_x4, hoverinfo="text", marker = list(color = "rgb(146,197,222)"), type = "bar", orientation = "h") %>%
    add_trace(x = ~x5, y = ~y, text=tab$text_x5, hoverinfo="text", marker = list(color = "rgb(5,113,176)"), type = "bar", orientation = "h") %>%  
    add_trace(x = 120, y = ~y, mode = "markers", text=my_text, hoverinfo="text",
              marker = list(symbol = 31,color = "rgb(98, 193, 252)", size=c(40)), type = "scatter") %>% 
    layout(title=mytitle,
           xaxis = list(title = "",
                        showticklabels = TRUE,
                        zeroline = FALSE,
                        showline = TRUE,
                        dtick = 10,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showticklabels = FALSE,
                        zeroline = FALSE),
           barmode = "relative",
           paper_bgcolor = "rgb(248, 248, 255)", plot_bgcolor = "rgb(248, 248, 255)",
           height=myheight,
           width=1300,
           margin = list(l = 150, r = 50, t = 100, b = 50),
           showlegend=F) %>%
    # # labeling the y-axis
    add_annotations(xref = "paper", yref = "y", x = 0.14, y = tab$y,
                    xanchor = "right",
                    text = tab$y,
                    font = list(family = "Arial", size = 15,
                                color = "rgb(67, 67, 67)"),
                    showarrow = FALSE, align = "right") %>%
    # labeling markers
    add_annotations(xref = "x", yref = "y", x = 120, y = tab$y,
                    xanchor = "center",
                    text = as.character(tracex),
                    font = list(family = "Arial", size = 15,
                                color = "rgb(0, 0, 0)"),
                    showarrow = FALSE)
  }
  else {
    tab[duplicated(tab$Service),][,c("Service")] <- " "
    tab <- tab[order(-xtfrm(tab$ID)),]
    #select a parameter to show
    #mean is selected
    if (dispara=="mnscore"){ 
      tracex <- tab$x6
      namex <- "Mean"}
    #satisfaction rate is selected
    else if (dispara=="satrate"){  
      tracex <- tab$x7
      namex <- "Positive rate"}
    #highly satisfied rate is selected
    else if (dispara=="hisatrate"){
      tracex <- tab$x8
      namex <- "Highly positive"}
    #user response rate is selected
    else if (dispara=="userate"){ 
      tracex <- tab$x9
      namex <- "User Response Rate"}
    #number of response is selected
    else {
      tracex <- tab$x10
      namex <- "Number of Response"}
    #hover text for parameters
    my_text=paste(namex) 
    p <- plot_ly(tab) %>%
      add_trace(x = ~-x2, y = ~y, text=paste(mylevels[2], round(tab$x2,2), "%"), hoverinfo="text",  marker = list(color = "rgb(244,165,130)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~-x1, y = ~y, text=paste(mylevels[1], round(tab$x1,2), "%"), hoverinfo="text",  marker = list(color = "rgb(202,0,32)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x3, y = ~y,  text=paste(mylevels[3], round(tab$x3,2), "%"), hoverinfo="text",  marker = list(color = "rgb(223,223,223)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x4, y = ~y,  text=paste(mylevels[4], round(tab$x4,2), "%"), hoverinfo="text",  marker = list(color = "rgb(146,197,222)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x5, y = ~y,  text=paste(mylevels[5], round(tab$x5,2), "%"), hoverinfo="text",  marker = list(color = "rgb(5,113,176)"),   type = "bar", orientation = "h") %>%  
      add_trace(x = 120, y = ~y, mode = "markers", text=my_text, hoverinfo="text",
                marker = list(symbol = 31,color = "rgb(98, 193, 252)", size=c(40)), type = "scatter") %>% 
      layout(title=mytitle,
             xaxis = list(title = "",
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          showline = TRUE,
                          dtick = 10,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showticklabels = FALSE,
                          zeroline = FALSE,
                          categoryorder = "array", 
                          categoryarray = ~Service),
             barmode = "relative",
             paper_bgcolor = "rgb(248, 248, 255)", plot_bgcolor = "rgb(248, 248, 255)",
             autosize = F,
             width = 1300,
             height = myheight,
             margin = list(l = 150, r = 50, t = 50, b = 25),
             showlegend=F) %>%
      # # labeling the y-axis
      add_annotations(xref = "paper", yref = "y", x = 0.02, y = tab$y,
                      xanchor = "left",
                      text = tab$y,
                      font = list(family = "Arial", size = 10,
                                  color = "rgb(67, 67, 67)"),
                      showarrow = FALSE, align = "left") %>%
      # # labeling markers
      add_annotations(xref = "x", yref = "y", x = 120, y = tab$y,
                      xanchor = "center",
                      text = as.character(tracex),
                      font = list(family = "Arial", size = 15,
                                  color = "rgb(0, 0, 0)"),
                      showarrow = FALSE)%>%
      #labeling the y-axis (category)
      add_annotations(xref = "paper", yref = "y", x = -0.1, y = tab$y,
                      xanchor = "left",
                      text = tab$Service,
                      font = list(family = "Arial", size = 15,
                                  color = "rgb(119, 142, 178)"),
                      showarrow = FALSE, align = "left") 
  }
  return(p)
}

#test
# df7 <- yearlyPlotfun(satisfy, "satisfy", c("IUB", "IUN"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")
# df8 <- yearlyPlotfun(satisfy, "satisfy", c("IUE", "IUPUI"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")
# df9 <- yearlyPlotfun(satisfy, "satisfy", c("IUB", "IUN", "IUE", "IUPUI", "IUSB"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")
# df10 <- yearlyPlotfun(satisfy, "satisfy", c("IUN", "IUE", "IUPUI", "IUSB"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")
# p1 <- yearlyPlotfun(help, "help", c("IUB", "IUN", "IUE", "IUPUI", "IUSB"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")
# p2 <- yearlyPlotfun(help, "help", c("IUN", "IUE", "IUPUI", "IUSB"), c("Faculty", "Graduate", "Undergraduate", "Staff", "Student"), "mnscore")

#############################################################################################################
#
#2nd page: question comparison
#
#############################################################################################################

comparePlotfun <- function(selectedList, campusList, typeList, dispara){
  #2) subset dataset
  #campus
  campusList=campusList
  #type of population
  typeList=typeList
  #question selected 
  selectedList = selectedList
  tabdata<-itemDesc_Shiny[itemDesc_Shiny$Campus %in% campusList & 
                            itemDesc_Shiny$type2String %in% typeList &
                            itemDesc_Shiny$Master.ID %in% selectedList,]
  tabdata <- as.data.frame(tabdata)
  #3) matrix header and title
  #create plot dataset
  varList<-c("Master.ID","Not at all Satisfied","2","3","4","Very Satisfied","Nsub","Question", "Not Applicable", "mean", "Service")
  likertPlot<-tabdata[varList]

  
  #4) calculation
  #split by questions
  unique_row <- unique(likertPlot$Question) #get unique questions
  questionCat <- unique(likertPlot[,c("Question", "Service", "Master.ID")])
  number_quest <- length(unique_row)
  splitList <- split(likertPlot, likertPlot$Master.ID) #split dataset by question
  oneqst <- c()
  totalv <- c()
  totalv2 <- c()
  #calculate percentage & mean
  for (i in 1:number_quest){
    split_data <- splitList[[i]]
    totalv  <-  split_data[,2] + split_data[,3] + split_data[,4] + split_data[,5] + split_data[,6] #1-5
    totalv2 <-  split_data[,2] + split_data[,3] + split_data[,4] + split_data[,5] + split_data[,6] + split_data[,9] #1-6  
    tempcol1 <- sum(split_data[,2]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 1
    tempcol2 <- sum(split_data[,3]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 2
    tempcol3 <- sum(split_data[,4]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 3
    tempcol4 <- sum(split_data[,5]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 4
    tempcol5 <- sum(split_data[,6]/totalv*split_data$Nsub)/sum(split_data$Nsub) #scale 5
    tempcol6 <- sum(split_data[,10]*split_data$Nsub)/sum(split_data$Nsub)       # mean
    tempcol7 <- tempcol3 + tempcol4 + tempcol5                                       # satisfaction rate
    tempcol8 <- tempcol4 + tempcol5                                                  # high satisfaction rate
    tempcol9 <- sum((1-split_data[,9]/totalv2)*split_data$Nsub)/sum(split_data$Nsub)  #usage rate
    tempcol10<- sum(split_data[,2],split_data[,3],split_data[,4],split_data[,5],split_data[,6],split_data[,9])     # number of respondents
    oneqst <- c(oneqst, round(100*tempcol1,2), round(100*tempcol2,2), 
                round(100*tempcol3,2), round(100*tempcol4,2), round(100*tempcol5,2), 
                round(tempcol6,2), 100*round(tempcol7,4), 100*round(tempcol8,4), 100*round(tempcol9,4), round(tempcol10, 0))
    #numbers <- oneqst[!is.na(oneqst)]
  }
  my_matrix <- matrix(oneqst, ncol=10, nrow=length(unique_row), byrow=TRUE)
  my_df <- as.data.frame(my_matrix)
  #colnames(my_df) <- c(mylevels, "Meanscore", "SatisfactionRate", "HighlySatisfiedRate", "Usage", "Nresp")
  my_df <- cbind(unique_row, my_df)
  my_df <- my_df[complete.cases(my_df),]
  my_df$ID <- seq.int(nrow(my_df))
  my_df$Master.ID <- questionCat$Master.ID
  my_df$Service <- questionCat$Service

  #5) plot
  
  #create a new db
  tab<-data.frame(my_df)
  names(tab)[1] <- "y"
  names(tab)[2] <- "x1"
  names(tab)[3] <- "x2"
  names(tab)[4] <- "x3"
  names(tab)[5] <- "x4"
  names(tab)[6] <- "x5"   
  names(tab)[7] <- "x6"   # mean
  names(tab)[8] <- "x7"   # satisfaction rate
  names(tab)[9] <- "x8"   # high satisfaction rate
  names(tab)[10] <- "x9"  # usage rate
  names(tab)[11] <- "x10" # number of respondents
  tab$y<-str_wrap(tab$y, width = 40)

  
  #select a parameter to show
  #mean is selected
  if (dispara=="mnscore"){ 
    tracex <- tab$x6
    namex <- "Mean"}
  #satisfaction rate is selected
  else if (dispara=="satrate"){  
    tracex <- tab$x7
    namex <- "Satisfaction"}
  #highly satisfied rate is selected
  else if (dispara=="hisatrate"){
    tracex <- tab$x8
    namex <- "Highly Satisfied"}
  #user response rate is selected
  else if (dispara=="userate"){ 
    tracex <- tab$x9
    namex <- "User Response Rate"}
  #number of response is selected
  else {
    tracex <- tab$x10
    namex <- "Number of Response"}
  my_text=paste(namex)
  mylevels <- c("Not at all Satisfied", "Not satisfied", "Neural", "Satisfied", "Very Satisfied")
  #plotting
  p <-plot_ly(tab[order(-xtfrm(tab$ID)),]) %>%
      add_trace(x = ~-x2, y = ~y, name=mylevels[2],  marker = list(color = "rgb(244,165,130)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~-x1, y = ~y, name=mylevels[1], marker = list(color = "rgb(202,0,32)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x3, y = ~y, name=mylevels[3], marker = list(color = "rgb(223,223,223)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x4, y = ~y, name=mylevels[4], marker = list(color = "rgb(146,197,222)"), type = "bar", orientation = "h") %>%
      add_trace(x = ~x5, y = ~y, name=mylevels[5], marker = list(color = "rgb(5,113,176)"), type = "bar", orientation = "h") %>%  
      add_trace(x = 120, y = ~y, mode = "markers", text=my_text, hoverinfo="text",
                marker = list(symbol = 31,color = "rgb(98, 193, 252)", size=c(40)), type = "scatter") %>% 
      layout(title= "Evalution of Selected Services (unit: percentage)",
             xaxis = list(title = "",
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          domain = c(0.15, 1)),
             yaxis = list(title = "",
                          showticklabels = FALSE,
                          zeroline = FALSE),
             barmode = "relative",
             paper_bgcolor = "rgb(248, 248, 255)", plot_bgcolor = "rgb(248, 248, 255)",
             showlegend = F,
             autosize = F,
             height = 100*length(tab$y)+100,
             width = 1300,
             margin = list(l = 150, r = 50, t = 100, b = 25),
             showlegend=F) %>%
      # # labeling the y-axis
      add_annotations(xref = "paper", yref = "y", x = 0.14, y = tab$y,
                      xanchor = "right",
                      text = tab$y,
                      font = list(family = "Arial", size = 15,
                                  color = "rgb(67, 67, 67)"),
                      showarrow = FALSE, align = "right") %>%
      # labeling markers
      add_annotations(xref = "x", yref = "y", x = 120, y = tab$y,
                      xanchor = "center",
                      text = as.character(tracex),
                      font = list(family = "Arial", size = 15,
                                  color = "rgb(0, 0, 0)"),
                      showarrow = FALSE)
 
  return(p)
}


#test

# df11 <- comparePlotfun(c("SV001", "SV002", "SV076",	"SV077"), c("IUB", "IUPUI", "IUE", "IUN", "IUSB"), c("Faculty", "Graduate"), "mnscore")
# df12 <- comparePlotfun(c("SV001", "SV002", "SV076",	"SV077"), c("IUE", "IUN", "IUSB"), c("Faculty", "Graduate"), "mnscore")
#############################################################################################################
#
#3rd page: multi-year comparison
#
#############################################################################################################

# Provide year list for multiyear comparison: add more as datasets are developed
## Currently there are only datasets (with prefix 'itemDesc_Shiny' for 2015-2017) !!!!!!!!!!!!!!!!!!!!!!
yearList=c("2015"="2015", "2016"="2016", "2017"="2017", "2018"="2018")

# PLOT
# Load dataset from selected years
multiYear<-function(yearList2, item, campusList, subpopList){
  # Get datasets
  rootName1<-"itemDesc_Shiny"
  rootName2<-".Rda"
  ## Loop to load datasets
  for(i in 1:length(yearList2)){
    load(paste0(rootName1,yearList2[i],rootName2))
  }
  ## Loop to add 'Year' variable to datasets
  for(i in 1:length(yearList2)){
    eval(parse(text=paste0("itemDesc_Shiny",yearList2[i],"$Year<-",yearList2[i])))
  }
  ## Load datasets to list for merging
  dataList<-list()
  for(i in 1:length(yearList2)){
    dataList[[i]]<-eval(parse(text=paste0("itemDesc_Shiny",yearList2[i])))
  }
  ## Merge datasets
  itemDesc_Shiny<-do.call("rbind", dataList)
  itemDesc_Shiny<-itemDesc_Shiny[,c(26,1:25)]
  
  # Create dataset (for later use with "<<-")
  multiCompare<<-itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% item,]
  multiCompare<<-multiCompare[multiCompare$Campus        %in% campusList,]
  multiCompare<<-multiCompare[multiCompare$type2String   %in% subpopList,]
  # Get the year averages
  splitList<<-split(multiCompare, multiCompare$Year)
  plotData<<-c()
  for(i in 1:length(yearList2)){
    splitData<<-splitList[[i]]
    meanStar<<-sum(splitData$mean*splitData$Nsub)/sum(splitData$Nsub)
    stErStar<<-sqrt(sum(((splitData$Nsub/sum(splitData$Nsub))^2)*
                          (splitData$variance/splitData$nSub_ans)*
                          ((splitData$Nsub-splitData$nCamp_ans)/splitData$Nsub)))
    coInStar<<-1.98*stErStar
    nSubStar<<-sum(splitData$nSub_ans)
    plotData<<-c(plotData, round(meanStar,2), round(stErStar, 2), round(coInStar, 2), round(nSubStar, 0))
  }
  plotMatrix<<-matrix(plotData, ncol=4, nrow=length(yearList2), byrow=T)
  plotDF<<-as.data.frame(plotMatrix)
  varList<<-multiCompare[,c("Year","Master.ID","Service","Question")]
  varList<<-unique(varList[,c("Year","Master.ID","Service","Question")])
  plotDF<<-cbind(varList, plotDF)
  colnames(plotDF)[5:8]<<-c("mean","se","ci","nSub")
  # Create Plot
  multiPlot<-ggplot(plotDF, aes(y=Year, x=mean, group=1)) +
    geom_errorbarh(aes(xmin=mean-ci, xmax=mean+ci), size=5, color="cornflowerblue", height=0, alpha=0.6) +
    geom_point(aes(size=nSub), color="chocolate4", alpha=1) +
    scale_size_continuous(range=c(5,10)) + 
    ylim((min(plotDF$Year)-1), (max(plotDF$Year)+1)) +
    xlim((min(plotDF$mean)-max(plotDF$ci)-0.05), (max(plotDF$mean)+max(plotDF$ci)+0.05)) +
    #xlim(1, 5) +
    theme(legend.position="none") +
    labs(y="Year", x="Mean satisfaction response", title=plotDF[1,4],
         subtitle="Mean satisfaction by year with 95% confidence interval: Click on a year's mean to view the sample size, mean value, and standard error")
  return(multiPlot)
}

# Test the function
#years<-c("2015","2016","2017")
#itemSelect<-"SV001"
#campus<-c("IUB","IUPUI")
#subpop<-c("Undergraduate","Graduate")
#multiYear(years,itemSelect,campus,subpop)
###################################################################################################

# t-TEST
# Load dataset from selected years
tTEST<-function(yearList2, item, campusList, subpopList, alphaLev){
  # Get datasets
  rootName1<-"itemDesc_Shiny"
  rootName2<-".Rda"
  ## Loop to load datasets
  for(i in 1:length(yearList2)){
    load(paste0(rootName1,yearList2[i],rootName2))
  }
  ## Loop to add 'Year' variable to datasets
  for(i in 1:length(yearList2)){
    eval(parse(text=paste0("itemDesc_Shiny",yearList2[i],"$Year<-",yearList2[i])))
  }
  ## Load datasets to list for merging
  dataList<-list()
  
  for(i in 1:length(yearList2)){
    dataList[[i]]<-eval(parse(text=paste0("itemDesc_Shiny",yearList2[i])))
  }
  
  ## Merge datasets
  itemDesc_Shiny_tt<-do.call("rbind", dataList)
  itemDesc_Shiny_tt<-itemDesc_Shiny_tt[,c(26,1:25)]
  
  # Create dataset (for later use with "<<-")
  multiCompare<<-itemDesc_Shiny_tt[itemDesc_Shiny_tt$Master.ID %in% item,]
  multiCompare<<-multiCompare[multiCompare$Campus        %in% campusList,]
  multiCompare<<-multiCompare[multiCompare$type2String   %in% subpopList,]
  ### Get the year averages
  splitList<<-split(multiCompare, multiCompare$Year)
  ttestData<<-c()
  for(i in 1:length(yearList2)){
    splitData<<-splitList[[i]]
    meanStar<<-sum(splitData$mean*splitData$Nsub)/sum(splitData$Nsub)
    stErStar<<-sqrt(sum(((splitData$Nsub/sum(splitData$Nsub))^2)*
                          (splitData$variance/splitData$nSub_ans)*
                          ((splitData$Nsub-splitData$nCamp_ans)/splitData$Nsub)))
    coInStar<<-1.98*stErStar
    nSubStar<<-sum(splitData$nSub_ans)
    stDvStar<<-sqrt((stErStar*sqrt(nSubStar))^2) #https://www.statsdirect.com/help/basic_descriptive_statistics/standard_deviation.htm
    ttestData<<-c(ttestData, round(meanStar,2), round(stErStar, 2), round(coInStar, 2), round(nSubStar, 0), round(stDvStar, 2))
  }
  ttestMatrix<<-matrix(ttestData, ncol=5, nrow=length(yearList2), byrow=T)
  ttestDF<<-as.data.frame(ttestMatrix)
  varList<<-multiCompare[,c("Year","Master.ID","Service","Question")]
  varList<<-unique(varList[,c("Year","Master.ID","Service","Question")])
  ttestDF<<-cbind(varList, ttestDF)
  colnames(ttestDF)[5:9]<-c("mean","se","ci","nSub","sd")
  ttestDF <- as.data.frame(ttestDF)
  # Do t-test
  ## Get relevant values
  mu1<<-ttestDF[1,5]
  mu2<<-ttestDF[2,5]
  sd1<<-ttestDF[1,9]
  sd2<<-ttestDF[2,9]
  n1 <<-ttestDF[1,8]
  n2 <<-ttestDF[2,8]
  
  ## Calculate t statistics
  tStat<-(mu1-mu2)/sqrt((sd1^2/n1)+(sd2^2/n2))
  dfree<-(((sd1^2/n1)+(sd2^2/n2))^2/((sd1^4/(n1^2*(n1-1)))+(sd2^4/(n2^2*(n2-1)))))
  pValu<-2*pt(-abs(tStat), df=dfree)
  
  ## Calculate CI
  alpha<-alphaLev
  tCrit<-qt(c((alpha/2), 1-(alpha/2)), df=dfree)
  ciLow<-(mu1-mu2)+(tCrit[1]*sqrt((sd1^2/n1)+(sd2^2/n2)))
  ciHig<-(mu1-mu2)+(tCrit[2]*sqrt((sd1^2/n1)+(sd2^2/n2)))
  cohen<-(mu1-mu2)/sqrt((((n1-1)*sd1^2)+(n2-1)*sd2^2)/(n1+n2-2))
  ## Calculate power
  Cpowe<-qt(1-(alpha/2), n1+n2-2)
  ciPow<-100*(1-alpha)
  sigPw<-sqrt((((n1-1)*sd1^2)+(n2-1)*sd2^2)/(n1+n2-2))
  sePow<-sigPw*sqrt((1/n1)+(1/n2))
  delta<-mu1-mu2
  power<-1-pt(Cpowe, n1+n2-2, ncp=delta/sePow)+pt(-Cpowe, n1+n2-2, ncp=delta/sePow)
  ## Print results
  if(pValu < 0.001){
    result1<<-paste0("t(",round(dfree,2),") = ",round(tStat,2),", p < 0.001")
  } else {
    result1<<-paste0("t(",round(dfree,2),") = ",round(tStat,2),", p = ",round(pValu,3))
  }
  result2<<-paste0(round(ciPow,0),"% CI [",round(ciLow,2),", ",round(ciHig,2),"]")
  result3<<-paste0("Cohen's d = ",round(cohen,2))
  result4<<-paste0("Power = ",round(power,2))
  result <<-c(result1, result2, result3, result4)
  return(paste0(result[1:4], sep="<br/>"))
}

# Test the function
# years<-c("2015","2017")
# itemSelect<-"SV001"
# campus<-c("IUB","IUPUI")
# subpop<-c("Undergraduate","Graduate")
# alphaL<-0.05
# df14 <- tTEST(years,itemSelect,campus,subpop,alphaL)

# END
