#This algorithm was developed by Ian P. Dwyer and Samuel J. Gurr
#in January 2017 for the analysis of data produced by oxygen sensors
#around Long Island, New York. Criteria below define the duration
#and character of hypoxic events and offer a series of options for
#analysis and reporting of these events.

# Use this script for your field data? **Please cite to credit intellectual contributors and origin use**: 
# Gurr, S. J., Dwyer, I. P., Goleski, J., Lima, F. P., Seabra, R., Gobler, C. J., & Volkenborn, N. (2021).
# Acclimatization in the bay scallop Argopecten irradians along a eutrophication gradient: 
# Insights from heartbeat rate measurements during a simulated hypoxic event. Marine and 
# Freshwater Behaviour and Physiology, 54(1), 23-49.

library(lubridate)
library(dplyr)
library(readr)

#Updated 5/15/2025 by K. Lenderman
 #- Added all parameters for extreme events
 #- need to update code for temp as high temps may include low ones
 #- Look into chl-a code for a high and low extreme
 #- code needs to be updated at three points within each parameter to have targeted site



####################################################################
###################### Low salinity events #########################
####################################################################



Thresholds<-c(.99,4.99,9.99,14.99,19.99); #Choose any number of thresholds for hypoxia (Salinity<=Threshold)
ThresholdLabels<-c("Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of hypoxia to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured hypoxic points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and Salinity in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'FENC' #'ASHC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, Salinity) %>%  # call the three column of interest
                        # dplyr::rename(Salinity  = `Salinity (PSU)`)) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.






#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
Salinity<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventSalinity<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgSalinity<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of Salinitys.
  AvgSalinitys<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time.
      if(Salinity[i]<=Thresholds[j]){
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between low salinity points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point.
        if(Salinity[i]<=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgSalinity<-mean(Salinity[runStart:runEnd]);
          avgSalinityround<-round(avgSalinity,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Salinity:",
                                                   avgSalinityround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update Salinity for incomplete events.
          #AvgSalinitys[length(AvgSalinitys)+1]<-avgSalinity; #stores the avg Salinity for each event
          duration[length(duration)+1]<-NA;
          eventSalinity[length(eventSalinity)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgSalinity<-mean(Salinity[runStart:runEnd]);
          avgSalinityround<-round(avgSalinity,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Salinity:",
                  avgSalinityround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgSalinitys[length(AvgSalinitys)+1]<-avgSalinity; #stores the avg Salinity for each event
          duration[length(duration)+1]<-NA;
          eventSalinity[length(eventSalinity)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Salinity:",
                                                        avgSalinityround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgSalinitys[length(AvgSalinitys)+1]<-avgSalinity; #stores the avg Salinity for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventSalinity[length(eventSalinity)+1]<-avgSalinity;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgSalinity[j]<-mean(AvgSalinitys); #updates the average average Salinity for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventSalinity=eventSalinity);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgSalinity=avgAvgSalinity);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of file needs to change base off site used for both csv.
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","FENC_SalinityEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","FENC_SalinitySummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);





####################################################################
###################### pH extreme events ###########################
####################################################################


#User-defined variables:

Thresholds<-c(6.89, 6.99, 7.09, 7.19, 7.29, 7.39); #Using these points as pH below 7.1 = severe (pH<=Threshold)
ThresholdLabels<-c("Extremely Severe", "Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of low pH to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and pH in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'ASHC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
                       dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
  dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
  dplyr::select(TIME, TIME_NUM_FORMAT, pH) %>%  # call the three column of interest
  # dplyr::rename(pH  = `pH (PSU)`)) %>% 
  na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
pH<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventpH<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgpH<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of pHs.
  AvgpHs<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();

#The loop that iterates through each row of the data and finds runs:
for (i in 1:length(timeNum)){
  #if we're not currently in a run...
  if(runEnd==0){
    #if we've found a starting point, set the start and end of the run to that time.
    if(pH[i]<=Thresholds[j]){
      runStart<-i;
      runEnd<-i;
    };
  }#LINE TERMINATOR WILL MESS THIS UP
  #if we're already in a run...
  else{
    #if we haven't exceeded the maximum allowable gap between low pH points...
    if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
      #and we've just found another applicable point, reset the end point to this new point.
      if(pH[i]<=Thresholds[j]){
        runEnd<-i;
      };
      #If we hit the end of the file during a run that meets our criteria.
      if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
        #DO DATA HERE FOR ANALYSIS
        NumberOfRuns<-NumberOfRuns+1;
        avgpH<-mean(pH[runStart:runEnd]);
        avgpHround<-round(avgpH,digits=2);
        dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg pH:",
        avgpHround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
        #for the output reports
        startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
        endTime[length(endTime)+1]<-NA;#stores the end time
        startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
        endNum[length(endNum)+1]<-NA;#stores the end num
        #Actually we don't want to update pH for incomplete events.
        #AvgpHs[length(AvgpHs)+1]<-avgpH; #stores the avg pH for each event
        duration[length(duration)+1]<-NA;
        eventpH[length(eventpH)+1]<-NA;
        type[length(type)+1]<-ThresholdLabels[j];
        #Actually we don't want to update durations for averaging because this doesn't count
        #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
        
        #set these back to 0 for the next file, or else it will go badly if I try to automate.
        runStart<-0;
        runEnd<-0;
      };
    }#LINE TERMINANTOR WILL MESS THIS UP
    #if we've exceeded the maximum allowable gap and not found an applicable point...
    else{
      #if the run we've found is at least the minimum length we've defined... 
     if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
       #DO DATA HERE FOR ANALYSIS
       NumberOfRuns<-NumberOfRuns+1;
       avgpH<-mean(pH[runStart:runEnd]);
       avgpHround<-round(avgpH,digits=2);
       #Contingency for if run starts at beginning of file
       if(runStart==1){dataReport[length(dataReport)+1]<-(
         paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg pH:",
         avpHround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
       #for the output reports
       startTime[length(startTime)+1]<-NA;#stores the start time
       endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
       startNum[length(startNum)+1]<-NA;#stores the start num
       endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
       #we don't want to store this in this case.
       #AvgpHs[length(AvgpHs)+1]<-avgpH; #stores the avg pH for each event
       duration[length(duration)+1]<-NA;
       eventpH[length(eventpH)+1]<-NA;
       type[length(type)+1]<-ThresholdLabels[j];
       #we don't want to store this in this case.
       #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
       }
       #Every other case
       else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg pH:",
       avgpHround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
       #for the output reports
       startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
       endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
       startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
       endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
       AvgpHs[length(AvgpHs)+1]<-avgpH; #stores the avg pH for each event
       duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
       eventpH[length(eventpH)+1]<-avgpH;
       type[length(type)+1]<-ThresholdLabels[j];
       durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
       }
       
     };
      #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
      #knows we're not currently in a run anymore.
      runStart<-0;
      runEnd<-0;
    };
  };
};
runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.

avgAvgpH[j]<-mean(AvgpHs); #updates the average average pH for the second report file.
avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
type=type,
durationMinutes=duration,
startTime=startTime,
endTime=endTime,
startNum=startNum,
endNum=endNum,
eventpH=eventpH);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
type=ThresholdLabels,
threshold=Thresholds,
numberFound=runCounts,
avgDurMinutes=avgDur,
avgAvgpH=avgAvgpH);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of file needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","ASHC_pHEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","ASHC_pHSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);




#######################################################################################
################################## Hypoxia events #####################################
#######################################################################################




#User-defined variables:

Thresholds<-c(.99,1.99,2.99,3.49,4.79); #Choose any number of thresholds for hypoxia (DO<=Threshold)
ThresholdLabels<-c("Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of low DO to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and DO in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'ASHC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, RDO.Concentration) %>%  # call the three column of interest
                         dplyr::rename(DO  = RDO.Concentration) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
DO<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventDO<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgDO<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of DOs.
  AvgDOs<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time.
      if(DO[i]<=Thresholds[j]){
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between hypoxia points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point.
        if(DO[i]<=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgDO<-mean(DO[runStart:runEnd]);
          avgDOround<-round(avgDO,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg DO:",
                                                   avgDOround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update DO for incomplete events.
          #AvgDOs[length(AvgDOs)+1]<-avgDO; #stores the avg DO for each event
          duration[length(duration)+1]<-NA;
          eventDO[length(eventDO)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgDO<-mean(DO[runStart:runEnd]);
          avgDOround<-round(avgDO,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg DO:",
                  avDOround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgDOs[length(AvgDOs)+1]<-avgDO; #stores the avg DO for each event
          duration[length(duration)+1]<-NA;
          eventDO[length(eventDO)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg DO:",
                                                        avgDOround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgDOs[length(AvgDOs)+1]<-avgDO; #stores the avg DO for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventDO[length(eventDO)+1]<-avgDO;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgDO[j]<-mean(AvgDOs); #updates the average average DO for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventDO=eventDO);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgDO=avgAvgDO);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of files needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","ASHC_DOEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","ASHC_DOSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);




#######################################################################################
############################# Low temperature events ##################################
#######################################################################################


#User-defined variables:

Thresholds<-c(.99,1.99,2.99, 3.99, 4.99); #Choose any number of thresholds for low temp (Low temp<=Threshold)
ThresholdLabels<-c("Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of low temp to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and Low temperature in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'FENC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_FENC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_FENC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, Temperature) %>%  # call the three column of interest
                        dplyr::rename(LTemp  = Temperature) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
LTemp<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventLTemp<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgLTemp<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of LTemps.
  AvgLTemps<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time.
      if(LTemp[i]<=Thresholds[j]){
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between hypoxia points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point.
        if(LTemp[i]<=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #LTemp DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgLTemp<-mean(LTemp[runStart:runEnd]);
          avgLTempround<-round(avgLTemp,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low Temperature:",
                                                   avgLTempround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update LTemp for incomplete events.
          #AvgLTemps[length(AvgLTemps)+1]<-avgLTemp; #stores the avg LTemp for each event
          duration[length(duration)+1]<-NA;
          eventLTemp[length(eventLTemp)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgLTemp<-mean(LTemp[runStart:runEnd]);
          avgLTempround<-round(avgLTemp,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low Temperature:",
                  avLTempround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgLTemps[length(AvgLTemps)+1]<-avgLTemp; #stores the avg LTemp for each event
          duration[length(duration)+1]<-NA;
          eventLTemp[length(eventLTemp)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low Temperature:",
                                                        avgLTempround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgLTemps[length(AvgLTemps)+1]<-avgLTemp; #stores the avg LTemp for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventLTemp[length(eventLTemp)+1]<-avgLTemp;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgLTemp[j]<-mean(AvgLTemps); #updates the average average LTemp for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventLTemp=eventLTemp);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgLTemp=avgAvgLTemp);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of files needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","FENC_LTempEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","FENC_LTempSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);





#######################################################################################
################################## High temperature events ############################
#######################################################################################


#Notes: Need to check thresholds as the most sever might be including the optimal numbers

#User-defined variables:

Thresholds<-c(28.99, 29.99, 30.99); #Choose any number of thresholds for High temp (High temp<=Threshold)
ThresholdLabels<-c("Sub-Optimal","Moderate", "Severe" ); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresholds, and must be in the same order. Format labels as "Label"

MinEvent<-29.99; #defined in minutes, the minimum duration of low temp to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-30; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and Low temperature in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'ASHC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, Temperature) %>%  # call the three column of interest
                        dplyr::rename(HTemp  = Temperature) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
HTemp<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventHTemp<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgHTemp<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of HTemps.
  AvgHTemps<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time. Changed greater than sign
      if(HTemp[i]>=Thresholds[j]){ 
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between hypoxia points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point. changed >
        if(HTemp[i]>=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #HTemp DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgHTemp<-mean(HTemp[runStart:runEnd]);
          avgHTempround<-round(avgHTemp,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High Temperature:",
                                                   avgHTempround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update HTemp for incomplete events.
          #AvgHTemps[length(AvgHTemps)+1]<-avgHTemp; #stores the avg HTemp for each event
          duration[length(duration)+1]<-NA;
          eventHTemp[length(eventHTemp)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgHTemp<-mean(HTemp[runStart:runEnd]);
          avgHTempround<-round(avgHTemp,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High Temperature:",
                  avgHTempround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgHTemps[length(AvgHTemps)+1]<-avgHTemp; #stores the avg high temp for each event
          duration[length(duration)+1]<-NA;
          eventHTemp[length(eventHTemp)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High Temperature:",
                                                        avgHTempround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgHTemps[length(AvgHTemps)+1]<-avgHTemp; #stores the avg HTemp for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventHTemp[length(eventHTemp)+1]<-avgHTemp;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgHTemp[j]<-mean(AvgHTemps); #updates the average average HTemp for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventHTemp=eventHTemp);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgHTemp=avgAvgHTemp);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of files needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","ASHC_HTempEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","ASHC_HTempSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);





#######################################################################################
############################## Low Chlorophyll-a events ###############################
#######################################################################################


#User-defined variables:

Thresholds<-c(.99,1.99,2.99, 3.99, 4.99); #Choose any number of thresholds for low chla (Chl-a<=Threshold)
ThresholdLabels<-c("Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of low temp to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and Low temperature in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'FENC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, Chlorophyll.a.Fluorescence) %>%  # call the three column of interest
                        dplyr::rename(LChla  = Chlorophyll.a.Fluorescence) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
LChla<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventLChla<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgLChla<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of Chla.
  AvgLChla<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time.
      if(LChla[i]<=Thresholds[j]){
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between hypoxia points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point.
        if(LChla[i]<=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #LChla DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgLChla<-mean(LChla[runStart:runEnd]);
          avgLChlaround<-round(avgLChla,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low CHL-A:",
                                                   avgLChlaround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update Chla for incomplete events.
          #AvgLChla[length(AvgLChla)+1]<-avgLChla; #stores the avg Chla for each event
          duration[length(duration)+1]<-NA;
          eventLChla[length(eventLChla)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgLChla<-mean(LChla[runStart:runEnd]);
          avgLChlaround<-round(avgLChla,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low CHL-A:",
                  avgLChlaround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgLChla[length(AvgLChla)+1]<-avgLChla; #stores the avg low Chla for each event
          duration[length(duration)+1]<-NA;
          eventLChla[length(eventLChla)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg Low CHL-A:",
                                                        avgLChlaround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgLChla[length(AvgLChla)+1]<-avgLChla; #stores the avg Chla for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventLChla[length(eventLChla)+1]<-avgLChla;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgLChla[j]<-mean(AvgLChla); #updates the average average Chla for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventLChla=eventLChla);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgLChla=avgAvgLChla);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of files needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","FENC_LChlaEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","FENC_LChlaSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);





#######################################################################################
############################## High Chlorophyll-a events ###############################
#######################################################################################


#User-defined variables:

Thresholds<-c(10.00,15.00,20.00, 25.00, 30.00); #Choose any number of thresholds for high chla (Chl-a<=Threshold)
ThresholdLabels<-c("Severe","Moderately Severe", "Moderate","Marginal", "Sub-Optimal"); #Choose labels for these thresholds
#Length of the labels list must be the same as the list of thresolds, and must be in the same order. Format labels as "Label"

MinEvent<-59.9; #defined in minutes, the minimum duration of low temp to be considered an event 
#(slightly shorter than interval because exactly 1 hr events were being discarded for some reason.)
Interval<-60; #defined in minutes, the maximum distance (<=) between two measured low salinity points that are part of same event
timeDigits<-5; #specifies the number of decimal places to which the time data are accurate.
#For reference, 5 decimal places is a resolution of slightly smaller than seconds.

#Define some desired outputs here as booleans. Don't worry about it for now.
#Might exclude this entirely. Not sure.

# CHANGES MADE September 26 2023 to cater script for LISS Sonde data - by Samuel Gurr
# most changes pertain to the inconsistencies in date formatting between sensors 

#Imports the data.
#Note that the following functions expect the columns to be 
#TIME, TIME_NUM_FORMAT, and Low temperature in that order.
getwd()
#setwd("C:/Users/sam.gurr/Documents/Github_repositories/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data")
filename     <- as.character("GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Juvenile_sonde_master.csv")
target_site  <- 'FENC' # call the site you want
raw_df       <- as.data.frame(read.csv(filename, sep = ',')) %>%  
  dplyr::filter(Site %in% target_site) 
# raw_df   <- as.data.frame(raw)
# raw_df[2:(ncol(raw_df))] <- lapply(raw_df[2:(ncol(raw_df))],as.numeric)
# names(raw_df) <- gsub(" \\([0-9]+\\)", "", columns) # ommit the numeric information from all column names 
# 
# if(gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% c('082023_ASHC_Sonde.csv', 
#                                                              '082023_LAUR_Sonde.csv', 
#                                                              '092023_ASHC_Sonde.csv',
#                                                              '102023_ASHC_Sonde.csv',
#                                                              '102023_FENC_Sonde.csv')) { # data files that have date formated as mdy_hm
#   raw_df[,1] <- mdy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hm(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '072023_ASHC_Sonde.csv') {
#     raw_df[,1] <- mdy_hms(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else if (gsub(".*/","", (gsub("\\","/",filename, fixed=T))) %in% '0924_GOLD_Sonde.csv') {
#     raw_df[,1] <- dmy_hm(raw_df[,1]) # with_tz(force_tz(mdy_hms(raw_df[,1]),tz='America/New_York'),'UTC')
#   } else (raw_df[,1] <- ymd_hms(raw_df[,1])  #with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC') # all other data files that are formatted as ymd_hm
# )
# with_tz(force_tz(ymd_hms(raw_df[,1]),tz='America/New_York'),'UTC')

data <- as.data.frame(raw_df[!is.na(raw_df$Date.Time),] %>% 
                        dplyr::mutate(TIME = as_datetime(Date.Time), # convert time and rename 
                                      # NOTE: lubraidate as numeric converts to number of seconds since 1/1/1970 - convert this to number of years
                                      TIME_NUM_FORMAT = (as.numeric(TIME) / 86400 / 365) ) %>% # get a numeric versoin of time
                        dplyr::select(TIME, TIME_NUM_FORMAT, Chlorophyll.a.Fluorescence) %>%  # call the three column of interest
                        dplyr::rename(HChla  = Chlorophyll.a.Fluorescence) %>% 
                        na.omit())
# View(as.data.frame(raw_df[!is.na(raw_df$`Date Time`),]))
# View(data)
# View(raw_df)

print(data) # look at it, should be three columns.




#USER SHOULD NOT TOUCH ANYTHING BELOW THIS LINE FOR THIS PARAMETER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#Sorts the data by time in ascending order before anything else happens.
data<-data[order(data[[2]]),];

#Separates out the elements into their own variables for easy use.
timeString<-as.character(data[[1]]);
timeNum<-data[[2]];
HChla<-(data[[3]]);

#Creates a character vector to store what will eventually be the report.
dataReport<-character();

#Creates variables(s) that will eventually be output to (a) file(s) (hopefully if I get this right).
type<-character();
startTime<-character();
endTime<-character();
duration<-numeric();
eventHChla<-numeric();
startNum<-numeric();
endNum<-numeric();

avgAvgHChla<-numeric();
avgDur<-numeric();
# every hour is 0.000115


# 0.0001141553*24*365 -- 0.0001141553 == 1 hour to years
#Defines the minimum event and maximum distance of measurements in terms of time code (new unit=days).
minEventNum<-round(MinEvent/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data
intervalNum<-round(Interval/24/60/365,digits=timeDigits);#Rounded to the same level of precision as the time data

#Here's where the analysis begins in earnest:
#Defines and initializes variables to hold the starting and ending row numbers of each run detected
runStart<-0;#must start at 0 in order to work.
runEnd<-0;#must start at 0 in order to work.

#Defines a vector for keeping track of the number of runs of each type.
runCounts<-numeric();

#Loop through for each threshold.
for(j in 1:length(Thresholds)){
  #Defines and initializes a variable for counting runs.
  NumberOfRuns<-0;
  #Defines and vector for keeping track of Chla.
  AvgHChla<-numeric();
  #defines a vector to keep track of durations
  durs<-numeric();
  
  #The loop that iterates through each row of the data and finds runs:
  for (i in 1:length(timeNum)){
    #if we're not currently in a run...
    if(runEnd==0){
      #if we've found a starting point, set the start and end of the run to that time.
      if(HChla[i]>=Thresholds[j]){
        runStart<-i;
        runEnd<-i;
      };
    }#LINE TERMINATOR WILL MESS THIS UP
    #if we're already in a run...
    else{
      #if we haven't exceeded the maximum allowable gap between hypoxia points...
      if(round(timeNum[i]-timeNum[runEnd],digits=timeDigits)<=intervalNum){
        #and we've just found another applicable point, reset the end point to this new point.
        if(HChla[i]>=Thresholds[j]){
          runEnd<-i;
        };
        #If we hit the end of the file during a run that meets our criteria.
        if(i==length(timeNum)&&(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum)){
          #HChla DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgHChla<-mean(HChla[runStart:runEnd]);
          avgHChlaround<-round(avgHChla,digits=2);
          dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High CHL-A:",
                                                   avgHChlaround," Start/End:",timeString[runStart],"-",timeString[runEnd],"(Cut Off By End Of File)"));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-NA;#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-NA;#stores the end num
          #Actually we don't want to update Chla for incomplete events.
          #AvgHChla[length(AvgHChla)+1]<-avgHChla; #stores the avg Chla for each event
          duration[length(duration)+1]<-NA;
          eventHChla[length(eventHChla)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #Actually we don't want to update durations for averaging because this doesn't count
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          
          #set these back to 0 for the next file, or else it will go badly if I try to automate.
          runStart<-0;
          runEnd<-0;
        };
      }#LINE TERMINANTOR WILL MESS THIS UP
      #if we've exceeded the maximum allowable gap and not found an applicable point...
      else{
        #if the run we've found is at least the minimum length we've defined... 
        if(round(timeNum[runEnd]-timeNum[runStart],digits=timeDigits)>=minEventNum){
          #DO DATA HERE FOR ANALYSIS
          NumberOfRuns<-NumberOfRuns+1;
          avgHChla<-mean(HChla[runStart:runEnd]);
          avgHChlaround<-round(avgHChla,digits=2);
          #Contingency for if run starts at beginning of file
          if(runStart==1){dataReport[length(dataReport)+1]<-(
            paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High CHL-A:",
                  avgHChlaround," Start/End:",timeString[runStart],"(Cut off by beginning of file)","-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-NA;#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-NA;#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          #we don't want to store this in this case.
          #AvgHChla[length(AvgHChla)+1]<-avgHChla; #stores the avg high Chla for each event
          duration[length(duration)+1]<-NA;
          eventHChla[length(eventHChla)+1]<-NA;
          type[length(type)+1]<-ThresholdLabels[j];
          #we don't want to store this in this case.
          #durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60; #stores event durations
          }
          #Every other case
          else{dataReport[length(dataReport)+1]<-(paste("Found",ThresholdLabels[j],"Event #",NumberOfRuns," Avg High CHL-A:",
                                                        avgHChlaround," Start/End:",timeString[runStart],"-",timeString[runEnd]));
          #for the output reports
          startTime[length(startTime)+1]<-timeString[runStart];#stores the start time
          endTime[length(endTime)+1]<-timeString[runEnd];#stores the end time
          startNum[length(startNum)+1]<-round(timeNum[runStart],digits=timeDigits);#stores the start num
          endNum[length(endNum)+1]<-round(timeNum[runEnd],digits=timeDigits);#stores the end num
          AvgHChla[length(AvgHChla)+1]<-avgHChla; #stores the avg Chla for each event
          duration[length(duration)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365;
          eventHChla[length(eventHChla)+1]<-avgHChla;
          type[length(type)+1]<-ThresholdLabels[j];
          durs[length(durs)+1]<-(timeNum[runEnd]-timeNum[runStart])*24*60*365; #stores event durations
          }
          
        };
        #regardless of whether we ran analysis or not, set these varaibles to 0 so the loop
        #knows we're not currently in a run anymore.
        runStart<-0;
        runEnd<-0;
      };
    };
  };
  runCounts[j]<-NumberOfRuns; #stores the total events for this threshold detected.
  dataReport[length(dataReport)+1]<-"";#adds a blank line to the data file to separate blocks.
  
  avgAvgHChla[j]<-mean(AvgHChla); #updates the average average Chla for the second report file.
  avgDur[j]=mean(durs);#updates the average average duration for the second report file.
};



#adds the summary lines to the data report.
dataReport[length(dataReport)+1]<-"Events Detected:";
for(k in 1:length(Thresholds)){
  dataReport[length(dataReport)+1]<-paste(ThresholdLabels[k],"Events:",runCounts[k]);
};

#compiles the reports for saving
report1<-data.frame(
  type=type,
  durationMinutes=duration,
  startTime=startTime,
  endTime=endTime,
  startNum=startNum,
  endNum=endNum,
  eventHChla=eventHChla);

report1$type<-as.character(report1$type);
report1$startTime<-as.character(report1$startTime);
report1$endTime<-as.character(report1$endTime);

report2<-data.frame(
  type=ThresholdLabels,
  threshold=Thresholds,
  numberFound=runCounts,
  avgDurMinutes=avgDur,
  avgAvgHChla=avgAvgHChla);

report2$type<-as.character(report2$type);

#saves out the report files.
#Title of files needs to change base off site used for both csv
#The structure the file title needs to stay the same "SITE_ParameterEvents.csv
output1<-gsub("Juvenile_sonde_master.csv","FENC_HChlaEvents_juv.csv",filename);
write.csv(report1, file=output1,row.names=FALSE);

output2<-gsub("Juvenile_sonde_master.csv","FENC_HChlaSummary_juv.csv",filename);
write.csv(report2, file=output2,row.names=FALSE);

#prints the report.
print(dataReport);