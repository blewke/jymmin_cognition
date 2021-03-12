#' load the data and make it pretty
#' 
#' load the data set and add some useful columns that can be comuted from the rest
#' very idosyncratic for this specific data file.
#' 
#' 
#' @param datafilename The name of the file where the data is saved as csv (include .csv in the filename).
#' @return A data frame with the loaded data.


data_preprocessing = function (datafilename, dateformat = 'dmy'){
  require(lubridate)
  all_data = read.csv(datafilename,
                      stringsAsFactors = FALSE)
  
  colnames(all_data) = replace(colnames(all_data),
                               match('Accuracy...', colnames(all_data)),
                               'Accuracy')
  colnames(all_data) = replace(colnames(all_data),
                               match('Engagement...', colnames(all_data)),
                               'Engagement')
  colnames(all_data) = replace(colnames(all_data),
                               match('Duration.Seconds.', colnames(all_data)),
                               'ResponseTime')
  all_data$TimePerClock = all_data$ResponseTime / all_data$ClocksInSet
  
  
  all_data$TimePoint = NA
  if (dateformat == 'mdy'){
    all_data$TimePoint = as.numeric(mdy_hms(paste(all_data$Date,all_data$Time)))
  }else{
    
  all_data$TimePoint = as.numeric(dmy_hms(paste(all_data$Date,all_data$Time)))
  }
  
  all_data$nCorrect = NA
  all_data$nCorrect = as.integer(all_data$Accuracy*all_data$ClocksInSet*0.01)
  
  all_data$LevelType = NA
  all_data$LevelType = all_data$Level%%5
  
  
  
  return(all_data)

}


#' Assign some participants to random groups
#' 
#' Assigns random (but fixed) groups and determines study period based on time point.
#' Enters condition/control for every row based on group and period.
#' 
#' @param dataframe The data frame to which a random assignment is to be added.
#' @return A data frame with the added columns. Only contains 24 participants.

random_assignment = function(dataframe){
require (lubridate)

all_data = dataframe


#random bit fixed list of group membership
#A = c(4055, 4059, 4063, 4064, 4068, 4070, 4072, 4073, 4074, 4077, 4082, 4083)
#B = c(4052, 4056, 4058, 4062, 4065, 4066, 4067, 4069, 4071, 4076, 4078, 4080)

A = 1:12
B = 13:24

all_data$Group = NA
all_data$Group[all_data$SubjectCode %in% A] = 'A'
all_data$Group[all_data$SubjectCode %in% B] = 'B'


# fixed dates
test_start_day = as.numeric(dmy_hms(paste('6.07.2018', '0:00:00')))
end_first_period = as.numeric(dmy_hms(paste('18.07.2018', '23:59:59')))
begin_second_period = as.numeric(dmy_hms(paste('20.07.2018', '0:00:00')))
end_day = as.numeric(dmy_hms(paste('01.08.2018', '23:59:59')))

all_data$Period = NA
all_data$Period[all_data$TimePoint  >= test_start_day & all_data$TimePoint <= end_first_period]= 1
all_data$Period[all_data$TimePoint  >= begin_second_period & all_data$TimePoint <= end_day]= 2

#all_data$Period[all_data$TimePoint  >= 1532044800 & all_data$tTimePoint  <= 1533935260]= 2

all_data$Jymmin = NA
all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 1] = 'Yes'
all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 2] = 'Yes'
all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 2] = 'No'
all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 1] = 'No'



#d = all_data[!is.na(all_data$Group), ]
#d = d['Exercise'== d$Stage, ]

#d$Period[is.na(d$Period)]=0
#d$Assignment[is.na(d$Assignment)]='.'

#return(d)


return(all_data)

}




real_assignment = function(dataframe){
  require (lubridate)
  
  all_data = dataframe
  
  
  #random bit fixed list of group membership
  #A = c(4055, 4059, 4063, 4064, 4068, 4070, 4072, 4073, 4074, 4077, 4082, 4083)
  #B = c(4052, 4056, 4058, 4062, 4065, 4066, 4067, 4069, 4071, 4076, 4078, 4080)
  
  A = c('A','B','C','D')
  B = c('E','F','G','H')
  
  all_data$Group = NA
  all_data$Group[substring(all_data$SubjectCode,1,1) %in% A] = 'A'
  all_data$Group[substring(all_data$SubjectCode,1,1) %in% B] = 'B'
  
  
  # fixed dates
  test_start_day = as.numeric(dmy_hms(paste('6.07.2018', '0:00:00')))
  end_first_period = as.numeric(dmy_hms(paste('18.07.2018', '23:59:59')))
  begin_second_period = as.numeric(dmy_hms(paste('20.07.2018', '0:00:00')))
  end_day = as.numeric(dmy_hms(paste('01.08.2018', '23:59:59')))
  
  all_data$Period = NA
  all_data$Period[all_data$TimePoint  >= test_start_day & all_data$TimePoint <= end_first_period]= 1
  all_data$Period[all_data$TimePoint  >= begin_second_period & all_data$TimePoint <= end_day]= 2
  
  #all_data$Period[all_data$TimePoint  >= 1532044800 & all_data$tTimePoint  <= 1533935260]= 2
  
  all_data$Jymmin = NA
  all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 1] = 'Yes'
  all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 2] = 'Yes'
  all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 2] = 'No'
  all_data$Jymmin[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 1] = 'No'
  
  
  
  #d = all_data[!is.na(all_data$Group), ]
  #d = d['Exercise'== d$Stage, ]
  
  #d$Period[is.na(d$Period)]=0
  #d$Assignment[is.na(d$Assignment)]='.'
  
  #return(d)
  
  
  return(all_data)
  
}











remove_empty_rows = function (df){
  
  return(df[df$Type =='',])
}


add_nTrialLevel = function(df){
  
  df$nTrialLevel = NA
  
  df = df[order(df$TimePoint),]
  
  
  for(s in unique(df$SubjectCode)){
    for (l in unique(df$Level)){
      
      rowindices = which(df$SubjectCode==s & df$Level == l & !is.na(df$ClocksInSet))
      #sldat = sldat[order(sldat$transformed_time24),]
      for  (ir in 1:length(rowindices))
      {
        if (ir == 1)  {
          df$nTrialLevel[rowindices[ir]] = 0
        }else{
          df$nTrialLevel[rowindices[ir]] = df$nTrialLevel[rowindices[ir-1]] + df$ClocksInSet[rowindices[ir-1]]
        }
      }
    }
  }
  
  return(df) 
  
}

#' scale the number of trials by the mean number of trials needed in each level
#' 
#' add a new column nTrialScaled to the dataframe.
#' columns Level, SubjectCode and nTrialLevel need to exist
#' 
#' 
#' @param df  a data frame with the right columns.
#' @return same dataframe data frame with the additional columnn nTrialScaled.


add_nTrialScaled = function(df){
  
  #for every subject and every level, find out how many trails have occurred
  trials = aggregate(df[,c('ClocksInSet')], by = list(SubjectCode = df$SubjectCode, Level = df$Level), FUN = sum)
  mean_nTrials_perS_perL = mean(trials$x)
  df$nTrialScaled = NA
  df$nTrialScaled = df$nTrialLevel/mean_nTrials_perS_perL
  return(df)
}
  



