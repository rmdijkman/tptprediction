library(sqldf)
library(Metrics)
library(rpart)

source("src/timefunctions.R")
source("src/errorfunctions.R")

#The file was created by exporting csv from Disco and then zipping the result
data <- data.frame(read.csv(unz("data/1.zip","1.csv"), header = TRUE, sep = ";", quote = "\"", dec = "."))
data$Complete.Timestamp <- as.POSIXct(strptime(data$Complete.Timestamp, "%Y/%m/%d %H:%M:%S", tz = "GMT"))

cases <- sqldf('SELECT `Case.ID` AS caseid,
                    GROUP_CONCAT(Activity) AS activities,
                    GROUP_CONCAT(Resource) AS resources,
                    Variant as variant,
                    GROUP_CONCAT(amount) AS amount,
                    GROUP_CONCAT(article) AS article,
                    GROUP_CONCAT(points) AS points,
                    GROUP_CONCAT(vehicleClass,"") AS vehicleclass,
                    GROUP_CONCAT(totalPaymentAmount) AS total,
                    MIN(`Complete.Timestamp`) AS mintime, 
                    MAX(`Complete.Timestamp`) AS maxtime, 
                    MAX(`Complete.Timestamp`) - MIN(`Complete.Timestamp`) AS difftime FROM data GROUP BY `Case.ID`')

#Simple prediction, just using the mean TPT
meantime <- mean(cases$difftime)

cases$prediction <- meantime
cases$error <- abs(cases$difftime - cases$prediction)
prediction.simple.mae <- mae(cases$difftime, cases$prediction)
prediction.simple.rmse <- rmse(cases$difftime, cases$prediction)
prediction.simple.smape <- smape(cases$difftime, cases$prediction)

#visualize the error
hist(toDays(cases$error),xlim=c(-0,1000),breaks=500)

#split into easy/hard to predict (i.e. small/big error)
#simple split: 50/50
medianerror <- median(cases$error)
cases$iseasy = (cases$error < medianerror)

#split mean TPT easy/mean TPT hard
meantime.easy = mean(cases[cases$iseasy==TRUE,]$difftime)
meantime.hard = mean(cases[cases$iseasy==FALSE,]$difftime)

#predict iseasy
fit <- rpart(iseasy ~ amount + article + points + vehicleclass, method="class", data=cases)
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

#predict mean TPT easy/mean TPT hard