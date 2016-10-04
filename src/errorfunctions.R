smape <- function(observationseries, predictedseries){
  return(100*mean(abs(predictedseries-observationseries)/(abs(observationseries)+abs(predictedseries))))
}