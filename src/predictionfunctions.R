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
# not classifying the cases by simply labeling them all with 'noclass'
# return set with additional column 'class'
#
################################################################################

f.addclass.noclass <- function(cases.subset){
  cases.subset$class = "noclass"
  return(cases.subset)
}
f.learnclass.noclass <- function(cases.subset, relation){
  return(0)
}
f.predictclass.noclass <- function(cases.subset, model){
  cases.subset$predictedclass = "noclass"
  return(cases.subset)
}

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
# attaching the actual class of a case
# this is proprietary for case 1
# classes are based on an array of averages (that can be determined by clustering)
# a case belongs to the class with the closest average proctime
# return set with additional column 'class'
#
################################################################################

f.addclass.clusters1 <- function(cases.subset){
  means = c(5884891,59129320)
  f.class <- function(proctimevalue){
    closest = means[1]
    for (i in means){
      if (abs(i-proctimevalue) < abs(closest-proctimevalue)){
        closest = i
      }
    }
    return(paste0("c",closest))
  }
  cases.subset$class = mapply(f.class,cases.subset$proctime)
  return(cases.subset)
}

################################################################################
#
# attaching the actual class of a case
# this is proprietary for case 1
# classes are based on an array of averages (that can be determined by clustering)
# a case belongs to the class with the closest average proctime
# but a distinction is made between the 50% closest to the average (the easy category)
# and the 50% furthest from the average (the hard category)
# return set with additional column 'class'
#
################################################################################

f.addclass.clusterseh1 <- function(cases.subset){
  means = c(5884891,59129320)
  f.class <- function(proctimevalue){
    closest = means[1]
    for (i in means){
      if (abs(i-proctimevalue) < abs(closest-proctimevalue)){
        closest = i
      }
    }
    return(closest)
  }
  cases.subset$class = mapply(f.class,cases.subset$proctime)
  for (i in means){
    median.error <- median(abs(cases.subset[cases.subset$class==i,]$proctime - i))
    f.class.eh <- function(proctimevalue){
      if (abs(proctimevalue-i) < median.error){
        return(paste0("E",i))
      }else{
        return(paste0("H",i))
      }
    }
    cases.subset[cases.subset$class==i,]$class = mapply(f.class.eh,cases.subset[cases.subset$class==i,]$proctime)
  }
  return(cases.subset)
}

################################################################################
#
# attaching the actual class of a case
# this is proprietary for case 1
# classes are assigned based on visual inspection of the processing time histogram:
# - ZeroEasy: 0 processing time
# - ZeroHard: 0 < processing time <= 30.000.000
# - NormalEasy: the 50% easy to predict cases with processing time > 30.000.000
# - NormalHard: the 50% hard to predict cases with processing time > 30.000.000
#
################################################################################

f.addclass.prop1  <- function(cases.subset){
  mean.proctime <- mean(cases.1[cases.1$proctime > 30000000,]$proctime)
  median.error <- median(abs(cases.1[cases.1$proctime > 30000000,]$proctime - mean.proctime))
  f.class <- function(proctimevalue){
    if (proctimevalue == 0){
      return("ZeroEasy")
    }else if ((proctimevalue > 0)&&(proctimevalue <= 30000000)){
      return("ZeroHard")
    }else if ((proctimevalue > 30000000) && abs(proctimevalue-mean.proctime) < median.error){
      return("NormalEasy")
    }else{
      return("NormalHard")
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

f.learnproctime.mean <- function(cases.subset, relation){
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
# learning the proctime of a case per class based on other variables
# the processing time is learned for each class based on regression
# the regession formula is given as 'relation'
# return vector of models
# precondition: contains a column 'class', which can be generated by 'f.addclass'
#
################################################################################

f.learnproctime.regression <- function(cases.subset, relation){
  classes = unique(cases.subset$class)
  predictionfunctions = vector(mode = "list", length = length(classes))
  names(predictionfunctions) = classes
  for (class in classes){
    f = lm(relation, data=cases.subset)
    predictionfunctions[[class]] = function(row){return(predict(f,row))}
  }
  return(predictionfunctions)
}

################################################################################
#
# learning the proctime of a case per class based on other variables
# the processing time is learned for each class based on a regression tree
# the regession formula is given as 'relation'
# return vector of models
# precondition: contains a column 'class', which can be generated by 'f.addclass'
#
################################################################################

f.learnproctime.regressiontree <- function(cases.subset, relation){
  classes = unique(cases.subset$class)
  predictionfunctions = vector(mode = "list", length = length(classes))
  names(predictionfunctions) = classes
  for (class in classes){
    fit <- rpart(relation, method="anova", data=cases.subset)
    pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
    predictionfunctions[[class]] = function(row){return(predict(pfit,row,type="vector"))}
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
