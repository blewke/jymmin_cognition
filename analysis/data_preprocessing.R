#' load the data and make it pretty
#' 
#' load the data set and add some useful columns that can be comuted from the rest
#' very idosyncratic for this specific data file.
#' 
#' 
#' @param datafilename The name of the file where the data is saved as csv (include .csv in the filename).
#' @return A data frame with the loaded data.


data_preprocessing = function (datafilename){
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
                               'DurationInSeconds')
  all_data$TimePerClock = all_data$DurationInSeconds / all_data$ClocksInSet
  
  
  all_data$TimePoint = NA
  all_data$TimePoint = as.numeric(dmy_hms(paste(all_data$Date,all_data$Time)))
  
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
A = c(4055, 4059, 4063, 4064, 4068, 4070, 4072, 4073, 4074, 4077, 4082, 4083)
B = c(4052, 4056, 4058, 4062, 4065, 4066, 4067, 4069, 4071, 4076, 4078, 4080)

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
all_data$Period[all_data$TimePoint  >= begin_second_period & all_data$tTimePoint <= end_day]= 2

all_data$Assignment = NA
all_data$Assignment[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 1] = 'Yes'
all_data$Assignment[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 2] = 'Yes'
all_data$Assignment[!is.na(all_data$Group) & all_data$Group=='A'& all_data$Period == 2] = 'No'
all_data$Assignment[!is.na(all_data$Group) & all_data$Group=='B'& all_data$Period == 1] = 'No'



d = all_data[!is.na(all_data$Group), ]
#d = d['Exercise'== d$Stage, ]

d$Period[is.na(d$Period)]=0
d$Assignment[is.na(d$Assignment)]='.'

return(d)

}

