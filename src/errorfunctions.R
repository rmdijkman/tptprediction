smape <- function(observationseries, predictedseries){
  return(mean(200*abs(observationseries-predictedseries)/(observationseries+predictedseries)))
}