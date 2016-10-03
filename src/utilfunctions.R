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
# If k = 0 the list is the empty list
# Precondition: 
# - the activity list should not be the empty string
#
#####################################################################################

activityListPrefix <- function(activityList, k){
  if (k == 0){
    return("")
  }
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
# To the set of cases, adds a column for each activity. For each pair row r, column
# that corresponds to activity a, add a number that represents how often a appears 
# r$prefix
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
#      - the log has a column called 'Activity' that contains activities that occur
#
# returns the set of cases with a column added for each activity that contains
#   for each case with a prefix, the number of times the activity occurred in the prefix
# 
#####################################################################################

addActivityOccurrences <- function(activitylog, cases){
  activities <- unique(activitylog$Activity)
  for (activity in activities){
    cases[[activity]] = as.numeric(sapply(cases$prefix, function(prefix) {activity %in% strsplit(prefix,",")[[1]]}))
  }
  return(cases)
}

#####################################################################################
#
# returns the set of cases where the column 'proctime' is added that contains the 
#   remaining processing time after the prefix has occurred
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
# 
#####################################################################################

addRemainingTime <- function(cases){
  computeRemaining <- function(prefix, starttime, endtime, completiontimes){
    ctvector = strsplit(completiontimes,",")[[1]]
    step = length(strsplit(prefix,",")[[1]])
    if (step == 0){
      return(endtime-starttime)
    }else{
      return(endtime - as.numeric(ctvector[[step]]))
    }
  }
  cases$proctime = as.numeric(mapply(computeRemaining,cases$prefix, cases$starttime, cases$endtime, cases$completiontimes))
  return(cases)
}

#####################################################################################
#
# returns the set of cases where the column 'passedtime' is added that contains the 
#   the that has passed up to the moment that the prefix was done
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
# 
#####################################################################################

addPassedTime <- function(cases){
  computePassed <- function(prefix, starttime, completiontimes){
    ctvector = strsplit(completiontimes,",")[[1]]
    step = length(strsplit(prefix,",")[[1]])
    if (step == 0){
      return(0)
    }else{
      return(as.numeric(ctvector[[step]])-starttime)
    }
  }
  cases$passedtime = as.numeric(mapply(computePassed,cases$prefix, cases$starttime, cases$completiontimes))
  return(cases)
}

#####################################################################################
#
# returns the set of cases where the column 'prefix' is added, in which the activity
#   prefix of length k is put for each case
# 
#####################################################################################

addPrefix <- function(cases, k){
  cases$prefix = sapply(cases$activities, function(x) activityListPrefix(x,k))
  return(cases)
}

#####################################################################################
#
# returns the set of cases where the column 'remainingactivities' is added
#   that contains the number of activities that remain to be executed for the case
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
#      - the cases set has a column calles 'nractivities' that contains the number
#        of activities that is performed for the case in total
# 
#####################################################################################

addRemainingActivities <- function(cases){
  cases$remainingactivities = mapply(function(prefix,nractivities){nractivities - length(strsplit(prefix,",")[[1]])},cases$prefix,cases$nractivities)
  return(cases)
}


#####################################################################################
#
# returns the set of cases where the column 'prefix' is added, in which activity
#   prefixes ofall lengths are added. The set of cases is enlarged (the number of
#   rows is increased) accordingly.
#
# Pre: - the cases set has a column calles 'nractivities' that contains the number
#        of activities that is performed for the case in total
# 
#####################################################################################

addAllPrefixes <- function(cases){
  newcases = data.frame(matrix(NA, nrow = 0, ncol = length(colnames(cases))+1))
  colnames(newcases) = append(colnames(cases),"prefix")
  for (j in seq(1,nrow(cases))){
    for (i in seq(0,cases[j,]$nractivities)){
      c = cases[j,]
      c$prefix = activityListPrefix(c$activities, i)
      newcases[nrow(newcases) + 1, ] = c
    }
  }
  return(newcases)
}

#####################################################################################
#
# returns the set of cases with a column 'proctimes' added that contains the duration
#   of each activity that occurs in the column 'activities'
# 
#####################################################################################

addProcTimes <- function(cases){
  computeProcTimes <- function(starttime, completiontimes){
    ctvector = as.numeric(strsplit(completiontimes,",")[[1]])
    ptvector = c()
    previous = starttime
    for (ct in ctvector){
      ptvector = append(ptvector,ct-previous)
      previous = ct
    }
    return(paste(ptvector,collapse=","))
  }
  cases$proctimes = mapply(computeProcTimes, cases$starttime, cases$completiontimes)
  return(cases)
}

#####################################################################################
#
# To the set of cases, adds a column for each activity. For each pair row r, column
# that corresponds to activity a, add a number that represents the average processing 
# time for those activities that appear in r$prefix
#
# Pre: - the cases set has a column called 'prefix' that contains lists of activities
#        separated by , (e.g. "A,B,C")
#      - the log has a column called 'Activity' that contains activities that occur
#      - the log has a column called 'proctimes' that contains the duration of activities
#
# returns the set of cases with a column added for each activity that contains
#   for each case with a prefix, the average processing time of that activity
# 
#####################################################################################

addActivityProcTimes <- \TODO