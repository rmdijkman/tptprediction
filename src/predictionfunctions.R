################################################################################
#
# Functions for learning and predicting the processing time of an event log
# The following functions are defined:
# f.addclass.eh
# f.learnclass.dectree
# f.learnproctime.mean
# f.predictclass.dectree
# f.predictproctime
#
################################################################################


################################################################################
#
# attaching the actual class of a case
# classes are Easy/Hard based on below/above median absolute error
# return set with additional column 'class'
#
################################################################################

f.addclass.eh <- function(cases.subset){
  mean.proctime <- mean(cases.subset$proctime)
  median.error <- median(abs(cases.subset$proctime - mean.proctime))
  f.class <- function(proctimevalue){
    if (abs(proctimevalue-mean.proctime) < median.error){
      return("Easy")
    }else{
      return("Hard")
    }
  }
  cases.subset$class = mapply(f.class,cases.subset$proctime)
  return(cases.subset)
}

################################################################################
#
# learning the class of a case based on other variables
# learned model is a decision tree
# the model that is learned is defined in the 'relation'
# return model
# precondition: contains a column 'class', which can be generated by 'f.addclass'
#
################################################################################

f.learnclass.dectree <- function(cases.subset, relation){
  fit <- rpart(relation, method="class", data=cases.subset)
  pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  return(pfit)
}

################################################################################
#
# learning the proctime of a case per class based on other variables
# the processing time is learned for each class 
# as the mean processing time of the cases in that class
# return vector of models
# precondition: contains a column 'class', which can be generated by 'f.addclass'
#
################################################################################

f.learnproctime.mean <- function(cases.subset){
  classes = unique(cases.subset$class)
  predictionfunctions = vector(mode = "list", length = length(classes))
  names(predictionfunctions) = classes
  for (class in classes){
    meanvalue = mean(cases.subset[cases.subset$class==class,]$proctime)
    predictionfunctions[[class]] = function(row){return(meanvalue)}
  }
  return(predictionfunctions)
}

################################################################################
#
# predicting the class of a case based on other variables
# the class is predicted based on a decision tree model 
# that must be passed as the 'model' argument
# return set with additional column 'predictedclass'
#
################################################################################

f.predictclass.dectree <- function(cases.subset, model){
  cases.subset$predictedclass = predict(model,cases.subset,type="class")
  return(cases.subset)
}

################################################################################
#
# predicting the throughput time of a class 
# return set with additional column 'predictedproctime'
# precondition: contains a column 'predictedclass', which can be generated by 'f.predictclass'
#
################################################################################

f.predictproctime <- function(cases.subset, modelvector){
  classes = unique(cases.subset$predictedclass)
  cases.subset$predictedproctime = 0
  for (class in classes){
    cases.subset[cases.subset$predictedclass == class,]$predictedproctime = modelvector[[class]](cases.subset[cases.subset$predictedclass == class,])
  }
  return(cases.subset)
}