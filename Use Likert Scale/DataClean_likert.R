#This uses the itemDesc_Shiny data to plot stacked percentage bars
#
#
#author: Wenjuan Sang
############################################################################################################

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)
library(data.table)

load("D:/shinyApp/DATA/itemDesc_Shiny.Rda")


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
                                                          "SV091",	"SV092")]
agree <- itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% c("AV001",	"AV002",	"AV003",	"AV004",	"AV005")]
help  <- itemDesc_Shiny[itemDesc_Shiny$Master.ID %in% c("HV001",	"HV002",	"HV003")]

#throw into function
testfun <- function(qset, qname, campusList, typeList, dispara){
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
  aggtitle  <- "To what extent do you agree with the following statements?"
  sattitle  <- "To what extent are you satisfied with the following services?"
  helptitle <- "To what extent do you think following services are helpful?"
  if(qname == "agree"){
    mylevels <- agglevels
    mytitle  <- aggtitle
  }
  else if(qname == "satisfy"){
    mylevels <- satlevels
    mytitle  <- sattitle
  }
  else{
    mylevels <- helplevels
    mytitle  <- helptitle
  }
  #create plot dataset
  varList<-c("Master.ID","Not at all Satisfied","2","3","4","Very Satisfied","Nsub","Question", "Not Applicable", "mean")
  likertPlot<-tabdata[varList]
  setnames(likertPlot, old=c("Not at all Satisfied","2","3","4","Very Satisfied"), new=mylevels)
  
  #4) calculation
  #split by questions
  unique_row <- unique(likertPlot$Question) #get unique questions
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
    tempcol6 <- sum(split_data[,10]*split_data$Nsub)/sum(split_data$Nsub)       #mean
    tempcol7 <- tempcol3 + tempcol4 + tempcol5                                       #satisfaction rate
    tempcol8 <- tempcol4 + tempcol5                                                  # high satisfaction rate
    tempcol9 <- sum((1-split_data[,9]/totalv2)*split_data$Nsub)/sum(split_data$Nsub)  #usage rate
    tempcol10<- sum(split_data[,2],split_data[,3],split_data[,4],split_data[,5],split_data[,6],split_data[,9])     # number of respondents
    oneqst <- c(oneqst, tempcol1, tempcol2, tempcol3, tempcol4, tempcol5, 
                round(tempcol6,2), 100*round(tempcol7,4), 100*round(tempcol8,4), 100*round(tempcol9,4), round(tempcol10, 0))
    #numbers <- oneqst[!is.na(oneqst)]
  }
  my_matrix <- matrix(oneqst, ncol=10, nrow=length(unique_row), byrow=TRUE)
  my_df <- as.data.frame(my_matrix)
  colnames(my_df) <- c(mylevels, "Meanscore", "SatisfactionRate", "HighlySatisfiedRate", "Usage", "Nresp")
  my_df <- cbind(unique_row, my_df)
  my_df <- my_df[complete.cases(my_df),]
  
  #5) plot
  #use only percentages
  tab<-data.frame(my_df[c(1:6)])
  
  #split the middle level into 2 levels
  numlevels<-length(tab[1,])-1
  numcenter<-ceiling(numlevels/2)+1
  tab$midvalues<-tab[,numcenter]/2
  tab2<-cbind(tab[,1],tab[,2:ceiling(numlevels/2)],
              tab$midvalues,tab$midvalues,tab[,numcenter:numlevels+1]) #split "almost the same" into "midlow" and "midhigh"
  colnames(tab2)<-c("outcome",mylevels[1:floor(numlevels/2)],"midlow",
                    "midhigh",mylevels[numcenter:numlevels])
  numlevels<-length(mylevels)+1
  
  #colors
  point1<-2
  point2<-((numlevels)/2)+1
  point3<-point2+1
  point4<-numlevels+1
  mymin<- -50 #(ceiling(max(rowSums(tab2[,point1:point2]))*4)/4)*-100
  mymax<-  100 #(ceiling(max(rowSums(tab2[,point3:point4]))*4)/4)*100
  numlevels<-length(tab[1,])-1
  temp.rows<-length(tab2[,1])
  pal<-brewer.pal((numlevels-1),"RdBu")
  pal[ceiling(numlevels/2)]<-"#DFDFDF"
  legend.pal<-pal
  pal<-c(pal[1:(ceiling(numlevels/2)-1)], pal[ceiling(numlevels/2)], 
         pal[ceiling(numlevels/2)], pal[(ceiling(numlevels/2)+1):(numlevels-1)]) #list of 6 colors (middle part grey twice)
  
  #wide to long
  tab3<-melt(tab2,id="outcome") 
  tab3$col<-rep(pal,each=temp.rows)
  tab3$value<-tab3$value*100
  tab3$outcome<-str_wrap(tab3$outcome, width = 40)
  highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
  lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
  lows <- lows[rev(rownames(lows)),]
  lows$col <- factor(lows$col, levels = c("#CA0020","#F4A582", "#DFDFDF"))
  wrap_row <- str_wrap(my_df$unique_row, width = 40)
  #plot
  #mean score is selected
  if (dispara=="mnscore"){ 
    selector <- my_df$Meanscore}
  #Satisfaction Rate is selected
  else if (dispara=="satrate"){  
    selector <- my_df$SatisfactionRate}
  #highly satisfied rate is selected
  else if (dispara=="hisatrate"){
    selector <- my_df$HighlySatisfiedRate}
  #user response rate is selected
  else if (dispara=="userate"){ 
    selector <- my_df$Usage}
  #number of response is selected
  else {
    selector <- my_df$Nresp}
  tabplot <- ggplot() + 
    geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
    geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
    theme_fivethirtyeight() + 
    coord_flip() +
    labs(title=mytitle, y="",x="") +
    theme(plot.title = element_text(size=15, hjust=0.5)) +
    theme(axis.text.y = element_text(size=13, hjust=0)) +
    theme(legend.position = "top") +
    scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax),position = "bottom") +
    geom_point(data=my_df, aes(x=wrap_row, y =100), size=20, shape=18, colour="dodgerblue1") +
    geom_text(data=my_df, aes(x=wrap_row, y= 100, label=selector), size=5, colour="white")
  return(tabplot)
}

#test
#df2<- testfun(help, "help", c("IUB","IUPUI"), c("Undergraduate","Graduate","Student", "Faculty"), "mnscore")