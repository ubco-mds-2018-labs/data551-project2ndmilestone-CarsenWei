######################################
#Preparing the dataset

UNSDG <- read.csv("C:\\Users\\Jiachen\\Downloads\\SDG Database UN\\simplifiedGOAL1-17.csv")
indType <- read.csv("C:\\Users\\Jiachen\\OneDrive\\MDS Labs TEMP\\SDG\\generated data\\indicatorTypes.csv")
#UNSDG left joins indType using PK {Indicator,SeriesCode}
UNSDG <- merge(x=UNSDG, y=indType, by=c("Indicator","SeriesCode"), all.x=TRUE)

library(dplyr)
UNSDG <- distinct(UNSDG) #remove duplicate rows
subset(UNSDG,IndicatorType=='NA')  #ensure every row has a non-null IndicatorType value
colnames(UNSDG)
# [1] "Indicator"         "SeriesCode"        "Goal"              "Target"           
# [5] "SeriesDescription" "GeoAreaCode"       "GeoAreaName"       "TimePeriod"       
# [9] "Value"             "Units"             "IndicatorType" 
unique(UNSDG$IndicatorType) #lb hb b1 ex ch
# lb: the lower the better 
# hb: the higher the better 
# b1: binary data with 1 being desirable
# b0: binary data with 0 being desirable
# ch: categorical value indicated by a number, the higher the better
# ex: exclude the indicator for our analysis


#subset china without rows where IndicatorType='ex' and TimePeriod!=2018 (incomplete data in 2018)
china <- subset(UNSDG,GeoAreaName=='China' & IndicatorType!='ex' & TimePeriod!=2018)
#check year range
sort(unique(china$TimePeriod)) #2000 to 2017
yrange <- length(unique(china$TimePeriod)) #18

#unstack the dataframe: use TimePeriod values as new columns
library(reshape)
china<-reshape(china, idvar=colnames(china[,c(-8,-9)]), #exclude "TimePeriod" and "Value" 
               timevar ='TimePeriod', direction = "wide")
china<-china[sort(colnames(china))] #reorder the columns
names(china)[10:27] <-seq(2000,2017) #rename the coluns
colnames(china)
# [1] "GeoAreaCode"       "GeoAreaName"       "Goal"              "Indicator"        
# [5] "IndicatorType"     "SeriesCode"        "SeriesDescription" "Target"           
# [9] "Units"             "2000"              "2001"              "2002"             
# [13] "2003"              "2004"              "2005"              "2006"             
# [17] "2007"              "2008"              "2009"              "2010"             
# [21] "2011"              "2012"              "2013"              "2014"             
# [25] "2015"              "2016"              "2017"   
table(duplicated(china[,c(4,6)]))  #ensure there is no duplicated {Indicator,SeriesCode} values

######################################
#Predicting values for 2025 and 2030

library(caret)
   #getModelInfo(model='lm') provides information on the models available in train()
   #or visit http://topepo.github.io/caret/available-models.html

#For each {Indicator,SeriesCode}, we have values~years.
#Write a function called bestPred() to fill the columnes (col1,col2) with values given by the best model

#The following section will be finished before final capstone
bestPred<-function(df,col1,col2){
  tmp<-t(df[,c(4,6,10:27)])  #subset columns and transpose 
  
  # For each {Indicator,SeriesCode}
  for(i in 1:ncol(tmp)){
    #lm
    yval<-as.numeric(tmp[3:20,i])
    xval<-seq(2000,2017)
    l_m <- lm(yval~xval)
    
    #caret models
    # bt <- train(method='bstTree',  #boosted tree
    #             x=,y= )
    
    
    #compare metrics and select the best model
    
    
    #return predicted values to df
    # predict(l_m, data=)
  }
  
  
}

#add 2 empty columns
china$'2025'<-'NA'
china$'2030'<-'NA'

#Apply the bestPred() function to each row
apply(china, MARGIN = 1, FUN=bestPred(), col1='2025', col2='2030')

#--------------------------------
#For Milestone II
#I used 2016 values as 2025 predicted values 
#and 2017 values as 2030 predicted values
tmp<-china
names(tmp)[26] <- '2025'
names(tmp)[27] <- '2030'
#export to csv file and visualize in Tableau
write.csv(tmp,file="C:\\Users\\Jiachen\\OneDrive\\MDS Labs TEMP\\SDG\\poster\\heatmap.csv")

#In the next stage, when the predicted values for 2025 and 2030 have been generated for all countries,
#I will normalize the predicted values to performance scores (0-1)based on IndicatorType values (Gower’s distance: lb hb b1 ch)
#and produce interactive maps

#For the poster, I used simulated aggregated data for China (score.csv)
for(i in 1:nrow(tmp)){ #normalize China (tmp) to 0-1
  for(j in 10:27){
    if(!is.null(tmp[i,j])){
      #if IndicatorType=='hb'
        #tmp[i,j]<- #normalize
      #if IndicatorType=='lb'
        #tmp[i,j]<- #normalize
      #if b1 or ch 
        #tmp[i,j]<- #normalize
        }
  }
}

tmp<-read.csv("C:\\Users\\Jiachen\\OneDrive\\MDS Labs TEMP\\SDG\\poster\\score.csv")
names(tmp)[11:28] <- c(2001:2015,2025,2030)
tmp <- tmp[c(4,5,6,7,9,11:28)] #remove other columns
#aggregate {Indicator,SeriesCode} to {Indicator}
tmp1<- aggregate(x=tmp, by=list(tmp$Indicator), FUN=mean)
#Aggregate to {Target}
tmp2<- aggregate(x=tmp, by=list(tmp$Target), FUN=mean)
#Aggregate to {Goal}
tmp3<- aggregate(x=tmp, by=list(tmp$Goal), FUN=mean)
#export to csv file and visualize in Tableau
write.csv(tmp2,file="C:\\Users\\Jiachen\\OneDrive\\MDS Labs TEMP\\SDG\\poster\\barchart.csv")

#--------------------------------
#The following section will be finished before final capstone

######################################
#Aggregate {Indicator,SeriesCode} to {Indicator}



######################################
#Aggregate to {Target}



######################################
#Aggregate to {Goal}


