toHours <- function(unixEpoch){
  return(unixEpoch/3600)
}

toDays <- function(unixEpoch){
  return(unixEpoch/86400)
}

#####################################################################################
#
# Creates a prefix of an activity list A,B,C,... that contains the first k activities
# The activity list is a string of activities separated by comma's
# E.g. activityListPrefix("A,B,C",2)="A,B"
# If k > the length of the activity list, the full activity list is returned
# Precondition: 
# - the activity list should not be the empty string
# - k >= 1
#
#####################################################################################

activityListPrefix <- function(activityList, k){
  activityVector = strsplit(activityList,",")[[1]]
  prefixLength = min(k,length(activityVector))
  return(paste(activityVector[1:prefixLength],collapse=","))
}

#####################################################################################
#
# Given a list of completion times,
# returns the completion time of the k-th activity.
# If k > length, returns the completion time of the last activity.
# e.g. timePassedUntil("10,20,30", 2) = 20
#      timePassedUntil("10,20,30", 7) = 30
#
#####################################################################################

timePassedUntil <- function(completiontimes, k){
  completiontimesVector = as.numeric(strsplit(completiontimes,",")[[1]])
  completiontimesVector = sort(completiontimesVector)
  kth_or_last = min(k,length(completiontimesVector))
  return(completiontimesVector[kth_or_last])
}

#####################################################################################
#
# Replaces factor levels that do not exceed the minimum occurrence threshold with
# the factor label 'toofew'.
# Takes a list of values and a number. Each value that does not occur at least minimum
# times is replaced with 'toofew'.
#
#####################################################################################

minimizeOccurrence <- function(values, minimum){
  valuecount = as.data.frame(table(values))
  levels_with_toofew = valuecount[valuecount$Freq < minimum, 'values']
  result = values
  result[result %in% levels_with_toofew] = "toolow"
  return(result)
}

#####################################################################################
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
#      - the log has a column called 'Activity' that contains activities that occur
#
# To the set of cases, adds a column for each activity. For each pair row r, column
# that corresponds to activity a, add a number that represents how often a appears 
# r$prefix
#
# returns that set of cases
# 
#####################################################################################

addActivityOccurrences <- function(activitylog, cases){
  activities <- unique(activitylog$Activity)
  for (activity in activities){
    cases[[activity]] = as.numeric(sapply(cases$prefix, function(prefix) {activity %in% strsplit(prefix,",")[[1]]}))
  }
  return(cases)
}

