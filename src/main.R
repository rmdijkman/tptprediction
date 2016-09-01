library(sqldf)
library(Metrics)
library(rpart)
library(caret)
library(dplyr)
library(lubridate)

source("src/timefunctions.R")
source("src/errorfunctions.R")
source("src/predictionfunctions.R")
source("src/predictionprocedure.R")

#Event log, created by exporting csv from Disco and then zipping the result
data.1 <- data.frame(read.csv(unz("data/1.zip","1.csv"), header = TRUE, sep = ";", quote = "\"", dec = "."))
data.1$Complete.Timestamp <- as.POSIXct(strptime(data.1$Complete.Timestamp, "%Y/%m/%d %H:%M:%S", tz = "GMT"))

#Aggregate the cases from the event log
cases.1 <- sqldf('SELECT 
                    `Case.ID` AS caseid,
                    GROUP_CONCAT(Activity) AS activities,
                    GROUP_CONCAT(Resource) AS resources,
                    Variant as variant,
                    GROUP_CONCAT(amount) AS amount,
                    GROUP_CONCAT(article) AS article,
                    GROUP_CONCAT(points) AS points,
                    GROUP_CONCAT(vehicleClass,"") AS vehicleclass,
                    GROUP_CONCAT(totalPaymentAmount) AS total,
                    MIN(`Complete.Timestamp`) AS starttime, 
                    MAX(`Complete.Timestamp`) AS endtime, 
                    MAX(`Complete.Timestamp`) - MIN(`Complete.Timestamp`) AS proctime 
                FROM `data.1` GROUP BY `Case.ID`')
cases.1$amount = as.numeric(cases.1$amount)
cases.1$points = as.numeric(cases.1$points)
cases.1$month = as.numeric(month(as.POSIXlt(cases.1$starttime, origin="1970-01-01", tz = "GMT")))
cases.1$day = as.numeric(day(as.POSIXlt(cases.1$starttime, origin="1970-01-01", tz = "GMT")))
#The relation that must be learned to predict the class of a case (Easy, Hard, ...)
relation.1 = class ~ amount + article + points + vehicleclass + month + day

compute.proctime(cases.1, relation.1, f.addclass.eh, f.learnclass.dectree, f.learnproctime.mean, f.predictclass.dectree, f.predictproctime)