################################################################################
#
# Procedure for learning and predicting the throughput time of an event log
#   includes 10-fold cross validation
#   prints the results to standard output
#
################################################################################

compute.proctime <- function(cases, class.relation, proctime.relation, f.addclass, f.learnclass, f.learnproctime, f.predictclass, f.predictproctime){
  k = 10
  
  cases = f.addclass(cases)
  classes = unique(cases$class)
  
  folds <- createFolds(cases$proctime, k)
  
  acc.avg = 0
  mae.avg.95 = 0
  mae.avg.80 = 0
  mae.avg = 0
  rmse.avg = 0
  smape.avg = 0
  smape.avg.byclass = vector(mode = "numeric", length = length(classes))
  names(smape.avg.byclass) = classes
  
  for (i in 1:k){
    cases.train <- cases[-folds[[i]],]
    cases.test <- cases[folds[[i]],]
    
    model.class = f.learnclass(cases.train, class.relation)
    modelvector.proctime = f.learnproctime(cases.train, proctime.relation)
    
    cases.test = f.predictclass(cases.test, model.class)
    cases.test = f.predictproctime(cases.test, modelvector.proctime)
    
    threshold.95 = sort(cases.test$proctime - cases.test$predictedproctime)[0.95*as.numeric(count(cases.test))]
    threshold.80 = sort(cases.test$proctime - cases.test$predictedproctime)[0.80*as.numeric(count(cases.test))]
    
    acc.avg = acc.avg + (sum(cases.test$class == cases.test$predictedclass)/length(cases.test$class))
    
    mae.avg.95 = mae.avg.95 + mean(abs(cases.test$proctime - threshold.95))
    mae.avg.80 = mae.avg.80 + mean(abs(cases.test$proctime - threshold.80))
    
    mae.avg = mae.avg + mae(cases.test$proctime, cases.test$predictedproctime)
    rmse.avg = rmse.avg + rmse(cases.test$proctime, cases.test$predictedproctime)
    smape.avg = smape.avg + smape(cases.test$proctime, cases.test$predictedproctime)
    for (class in classes){
      smape.avg.byclass[[class]] = smape.avg.byclass[[class]] + smape(cases.test[cases.test$class==class,]$proctime, cases.test[cases.test$class==class,]$predictedproctime)
    }
  }
  
  acc.avg = acc.avg/k
  mae.avg = mae.avg/k
  mae.avg.95 = mae.avg.95/k
  mae.avg.80 = mae.avg.80/k
  rmse.avg = rmse.avg/k
  smape.avg = smape.avg/k
  for (class in classes){
    smape.avg.byclass[[class]] = smape.avg.byclass[[class]]/k
  }
  
  cat("Number of cases: ", as.numeric(count(cases)), "\n")
  cat("Average processing time: ", mean(cases$proctime), "\n")
  cat("Accuracy of class prediction: ", acc.avg, "\n")
  cat("MAE of processing time prediction (s): ", mae.avg, "\n")
  cat("MAE with 95% on time prediction (s): ", mae.avg.95, "\n")
  cat("MAE with 80% on time prediction (s): ", mae.avg.80, "\n")
  cat("RMSE of processing time prediction (s): ", rmse.avg, "\n")
  cat("SMAPE of processing time prediction (%): ", smape.avg, "\n")
  cat("SMAPE of processing time prediction per class (%): ", smape.avg.byclass, "(", names(smape.avg.byclass), ")\n")
}