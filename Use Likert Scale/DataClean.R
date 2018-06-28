#This uses the raw UITS data to plot stacked percentage bars
#
#
#author: Wenjuan Sang
############################################################################################################
library(foreign)
library(likert)
library(plyr)
library(ggplot2)
library(Rmisc)
library(xlsx)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(ggthemes)
library(stringr)

#00.  Import data from xlsx files

iubData   <- read.spss("D:/shinyApp/DATA/2017/UITS17_IUB_Final.sav",   1, header=TRUE, to.data.frame=TRUE, use.value.labels=FALSE)
iupuiData <- read.spss("D:/shinyApp/DATA/2017/UITS17_IUPUI_Final.sav", 1, header=TRUE, to.data.frame=TRUE, use.value.labels=FALSE)
iukData   <- read.spss("D:/shinyApp/DATA/2017/UITS17_IUK_Final.sav",   1, header=TRUE, to.data.frame=TRUE, use.value.labels=FALSE)
iupucData <- read.spss("D:/shinyApp/DATA/2017/UITS17_IUPUC_Final.sav", 1, header=TRUE, to.data.frame=TRUE, use.value.labels=FALSE)
iusData   <- read.spss("D:/shinyApp/DATA/2017/UITS17_IUS_Final.sav",   1, header=TRUE, to.data.frame=TRUE, use.value.labels=FALSE)

#campus
iubData$campus   <- 10
iupuiData$campus <- 20
iukData$campus   <- 30
iupucData$campus <- 40
iusData$campus   <- 50

all17 <- rbind(iubData, iupuiData, iukData, iupucData, iusData)
all17.use <- all17
all17.use$group <- all17.use$type + all17.use$campus

#01. split variable list

demvar  <- c("type", "campus", "group")
impvar  <- c("q2", "q3")
satvar  <- c("q1", 
             "Q6_a",	"Q6_b", 
             "q7_a",	"q7_b",	"q7_c",	"q7_d",	"q7_e",	"q7_f",	"q7_g",	"q7_h",	"q7_i",	"q7_j",	"q7_k",	"q8",
             "q9_a",	"q9_b",	"q9_c",	"q10",
             "q11_a",	"q11_b",	"q11_c",	"q11_d",	"q11_e",	"q12",	"q13",	"q14_a",	"q14_b",	"q14_c",	"q15",	"q16",
             "q17_a",	"q17_b",	"q17_c",	"q17_d",	"q17_e",	"q17_f",	"q18",
             "q19_a",	"q19_b",	"q19_c",	"q19_e",	'q19_f',	'q19_g',	"q19_h",	"q20",	'q21',	"q22",
             "q23",
             "q26_a",	"q26_b",	"q26_c",
             "q27_a",	"q27_b",	"q28", "q29_a",	"q29_b",	"q29_c")
aggvar  <- c("q25_a",	"q25_b",	"q25_c",	"q25_d",	"q25_e")
helpvar <- c( "q30",	"q31",	"q32")



#02.  split dataset
impdata  <- all17.use[impvar] #importance & expertise
satdata  <- all17.use[satvar] #satifaction questions
aggdata  <- all17.use[aggvar] #aggreement questions
helpdata <- all17.use[helpvar] #helpful questions



#03.  Label factors
all17.use$campus <- factor(all17.use$campus,
                           levels = c(10, 20, 30, 40, 50),
                           labels = c("IUB", "IUPUI", "IUK", "IUPUC", "IUS"))
all17.use$type   <- factor(all17.use$type,
                           levels = c(1, 2, 3, 4),
                           labels = c("Faculty", "Staff", "Undergraduate", "Graduate"))
all17.use$group  <- factor(all17.use$group,
                           levels = c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 54),
                           labels = c("IUB_F", "IUB_S", "IUB_U", "IUB_G",
                                      "IUPUI_F", "IUPUI_S", "IUPUI_U", "IUPUI_G",
                                      "IUK_F", "IUK_S", "IUK_U", "IUK_G",
                                      "IUPUC_F", "IUPUC_S", "IUPUC_U", "IUPUC_G",
                                      "IUS_F", "IUS_S", "IUS_U", "IUS_G"))
impdata$q2 <- factor(impdata$q2,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("not at all important", "not Immportant", "Neutral", "Important", "Very Important"))
impdata$q3 <- factor(impdata$q3,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Novice", "Advanced Beginner", "Competent", "Proficient", "Expert"))
sat.df<- data.frame(sapply(satdata, factor,
                      levels = c( 1, 2, 3, 4, 5),
                      labels = c( "Not at all Satisfied", "Not satisfied", "Neural",
                                  "Satisfied", "Very Satisfied"),
                      simplify = FALSE))
agg.df<- data.frame(sapply(aggdata, factor,
                      levels = c( 1, 2, 3, 4, 5),
                      labels = c( "Strongly disagree", "Somewhat disagree", "Neither agree or disagree",
                                   "Somewhat agree", "Strongly agree"),
                      simplify = FALSE))
help.df<- data.frame(sapply(helpdata, factor,
                      levels = c( 1, 2, 3, 4, 5),
                      labels = c( "Not at all Helpful", "Not helpful", "Neutral",
                                  "Helpful", "Very Helpful"),
                      simplify = FALSE))



imp.use <- plyr::rename(impdata, c(q2="Importance of UITS services", 
                             q3="Computer expertise"))
agg.use <- plyr::rename(agg.df, c(q25_a="Phishing is a serious problem",
                            q25_b="The Duo registration process was simple and straightforward",
                            q25_c="I know how to report a phishing attempt",
                            q25_d="I would like UITS to proactively inform me of known phishing attempts that affect the IU community",
                            q25_e="I would find useful a web page listing known phishing attempts against which I could check suspicious messages."))
help.use <- plyr::rename(help.df, c(q30="Helpfulness of the information technology in teaching",
                              q31="Helpfulness of the information technology in research",
                              q32="Helpfulness of the information technology in learning"))
sat.use <- plyr::rename(sat.df, c(q1="Overall satisfaction with UITS services",
                            Q6_a="Umail/Imail",
                                   Q6_b="Microsoft Exchange/Outlook/OWA",
                                   q7_a="Telephone Consulting",
                                   q7_b="Walk-in Consulting",
                                   q7_c="Chat  Consulting",
                                   q7_d="Email  Consulting",
                                   q7_e="Knowledge  Base",
                                   q7_f="Personal  Consulting",
                                   q7_g="IT Training self-study services",
                                   q7_h="IUanyWare",
                                   q7_i="IT Training instructor-led training",
                                   q7_j="IUware",
                                   q7_k="Cloud Storage at IU",
                                   q8="Overall satisfaction with UITS support services",
                                   q9_a="Electronic  News",
                                   q9_b="Social  Media",
                                   q9_c="UITS Websites",
                                   q10="Overall satisfaction with UITS communications",
                                   q11_a="Hardware",
                                   q11_b="Software",
                                   q11_c="Black & white and color printing services",
                                   q11_d="Plotters",
                                   q11_e="Infostations",
                                   q12="Overall satisfaction with UITS computing services",
                                   q13="Classroom Technology Services",
                                   q14_a="Videoconferencing",
                                   q14_b="Class Recording",
                                   q14_c="Kaltura Mediaspace",
                                   q15="Overall satisfaction with physical learning space design in the Student Technology Centers",
                                   q16="Overall satisfaction with the design of classroom learning spaces",
                                   q17_a="Supercomputers/HPC",
                                   q17_b="High speed disk storage",
                                   q17_c="Scholarly Data Storage",
                                   q17_d="Advanced Visualization Laboratory",
                                   q17_e="Research Analytics",
                                   q17_f="Support for Life Sciences",
                                   q18="Overall satisfaction with UITS research technology services",
                                   q19_a="General SIS functionality",
                                   q19_b="Academic Advising Services",
                                   q19_c="Student Self Service",
                                   q19_e="Canvas",
                                   q19_f="General Functionality and Employee Self Service",
                                   q19_g="Kuali Financial Information System (KFS)",
                                   q19_h="One.iu.edu",
                                   q20="Overall satisfaction with academic and business systems",
                                   q21="The decommissioning process of Oncourse",
                                   q22="The decommissioning process of OneStart",
                                   q23="Overall satisfaction with the information about security threats provided by UITS",
                                   q26_a="Departmental publishing on IU Web servers",
                                   q26_b="Individual publishing on IU Web servers",
                                   q26_c="Web Content Management",
                                   q27_a="Wireless Network",
                                   q27_b="Wired Network",
                                   q28="Overall satisfaction with the network services",
                                   q29_a="Telephone System",
                                   q29_b="Lync/Skype for Business",
                                   q29_c="IU Fax"))


#04.  Calculate likert table
aggScaled.likert <-likert(agg.use, grouping = factor(all17.use$campus:all17.use$type))
satScaled.likert <-likert(sat.use, grouping = factor(all17.use$campus:all17.use$type))
helpScaled.likert <-likert(help.use, grouping = factor(all17.use$campus:all17.use$type))
agree   <- aggScaled.likert$results
satisfy <- satScaled.likert$results
help    <- helpScaled.likert$results
agree  <- data.frame(agree[-c(1)], colsplit(agree$Group, ":", names = c("Campus", "Type")))
satisfy  <- data.frame(satisfy[-c(1)], colsplit(satisfy$Group, ":", names = c("Campus", "Type")))
help  <- data.frame(help[-c(1)], colsplit(help$Group, ":", names = c("Campus", "Type")))

# write.table(aggScaled.likert$results,file="D:/shinyApp/DATA/aggscaled.csv",  sep=",", row.names=FALSE)
# write.table(satScaled.likert$results,file="D:/shinyApp/DATA/satscaled.csv",  sep=",", row.names=FALSE)
# write.table(helpScaled.likert$results,file="D:/shinyApp/DATA/helpscaled.csv",  sep=",", row.names=FALSE)

#05.  Table of subpopulation
# weittab <- unique(itemsLikert_Shiny[c("Campus", "type", "Nsub")])
# weittab$Type <- weittab$type
# weittab$Type   <- factor(weittab$Type,
#                            levels = c(1, 2, 3, 4),
#                            labels = c("Faculty", "Staff", "Undergraduate", "Graduate"))
# colnames(weittab)[colnames(weittab)=="type"] <- "type.n"
# write.table(weittab,file="D:/shinyApp/DATA/Subpop_2017.csv",  sep=",", row.names=FALSE)
subpop_2017 <- read.csv("D:/shinyApp/DATA/Subpop_2017.csv")




#06.  Preparing table & plotting
testfun <- function(qset, qname, campus, type){
  rawdata <- qset
  #campus
  if (deparse(substitute(campus))=="allc"){
    campus=c("IUB", "IUK", "IUPUC", "IUPUI", "IUS")
  }
  else{
    campus=campus
  }
  #type of pop
  if (deparse(substitute(type))=="alls"){
    type=c("Faculty", "Staff", "Graduate", "Undergraduate")
  }
  else{
    type=type
  }  
  tabdata <- rawdata[rawdata$Campus %in% campus & rawdata$Type %in% type, ] #full table
  subpop  <- subpop_2017[subpop_2017$Campus %in% campus & subpop_2017$Type %in% type, ] #weights
  temp_merged <- merge(tabdata, subpop[c(-2)], by=c("Campus","Type"), all=T)
  temp_merged$Nsub[temp_merged[,4]==0 & temp_merged[,5]==0 & temp_merged[,6]==0 & temp_merged[,7]==0 & temp_merged[,8]==0] <- 0
  temp_sorted <- temp_merged[order(temp_merged$Item),]
  
  #levels
  agglevels <- c("Strongly disagree", "Disagree", "Neutral", "Agree",  "Strongly agree")
  satlevels <- c("Not at all Satisfied", "Not satisfied", "Neural", "Satisfied", "Very Satisfied")
  helplevels <- c("Not at all Helpful", "Not helpful", "Neutral", "Helpful", "Very Helpful")
  #titles
  aggtitle  <- "To what extent do you agree with the following statements?"
  sattitle  <- "To what extent are you satisfied with the following services?"
  helptitle <- "To what extent do you think following services are helpful?"
  
  #matrix header and title
  #qname.string <- deparse(substitute(qname))
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
  
  #########calculation#########
  unique_row <- unique(temp_sorted$Item) #get unique questions
  number_quest <- length(unique_row)
  splitList <- split(temp_sorted, temp_sorted$Item) #split dataset by question
  oneqst <- c()
  for (i in 1:number_quest){
    split_data <- splitList[[i]]
    tempcol1 <- sum(split_data[,4]*split_data[,9])/(sum(split_data[,9])*100)
    tempcol2 <- sum(split_data[,5]*split_data[,9])/(sum(split_data[,9])*100)
    tempcol3 <- sum(split_data[,6]*split_data[,9])/(sum(split_data[,9])*100)
    tempcol4 <- sum(split_data[,7]*split_data[,9])/(sum(split_data[,9])*100)
    tempcol5 <- sum(split_data[,8]*split_data[,9])/(sum(split_data[,9])*100)
    oneqst <- c(oneqst, tempcol1, tempcol2, tempcol3, tempcol4, tempcol5)
    #numbers <- oneqst[!is.na(oneqst)]
  }
  my_matrix <- matrix(oneqst, ncol=5, nrow=length(unique_row), byrow=TRUE)
  colnames(my_matrix) <- mylevels
  my_df <- as.data.frame(my_matrix)
  my_df <- cbind(unique_row, my_df)
  my_df <- my_df[complete.cases(my_df),]
  ##########plotting#########
  tab<-data.frame(my_df)

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
  tab3$outcome<-tab3$outcome #str_wrap(tab3$outcome, width = 40)
  highs<-na.omit(tab3[(length(tab3[,1])/2)+1:length(tab3[,1]),])
  lows<-na.omit(tab3[1:(length(tab3[,1])/2),])
  lows <- lows[rev(rownames(lows)),]
  lows$col <- factor(lows$col, levels = c("#CA0020","#F4A582", "#DFDFDF"))
  
  #plot
  tabplot <- ggplot() + 
    geom_bar(data=highs, aes(x = outcome, y=value, fill=col), position="stack", stat="identity") +
    geom_bar(data=lows, aes(x = outcome, y=-value, fill=col), position="stack", stat="identity") +
    geom_hline(yintercept = 0, color =c("white")) +
    scale_fill_identity("Percent", labels = mylevels, breaks=legend.pal, guide="legend") + 
    theme_fivethirtyeight() + 
    coord_flip() +
    labs(title=mytitle, y="",x="") +
    theme(plot.title = element_text(size=14, hjust=0.5)) +
    theme(axis.text.y = element_text(hjust=0)) +
    theme(legend.position = "top") +
    scale_y_continuous(breaks=seq(mymin,mymax,25), limits=c(mymin,mymax),position = "bottom")+
    scale_x_discrete(limits= unique(highs$outcome))
  return(tabplot)
}

#test
#testfun(help, help, c("IUB", "IUK"), c("Faculty", "Staff"))



