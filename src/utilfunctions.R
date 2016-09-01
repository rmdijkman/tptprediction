toHours <- function(unixEpoch){
  return(unixEpoch/3600)
}

toDays <- function(unixEpoch){
  return(unixEpoch/86400)
}

# Creates a prefix of an activity list A,B,C,... that contains the first k activities
# The activity list is a string of activities separated by comma's
# E.g. activityListPrefix("A,B,C",2)="A,B"
# If k > the length of the activity list, the full activity list is returned
# Precondition: 
# - the activity list should not be the empty string
# - k >= 1
#
activityListPrefix <- function(activityList, k){
  activityVector = strsplit(activityList,",")[[1]]
  prefixLength = min(k,length(activityVector))
  return(paste(activityVector[1:prefixLength],collapse=","))
}