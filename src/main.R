library(sqldf)
library(Metrics)
library(rpart)
library(caret)
library(dplyr)
library(lubridate)
library(fpc)

source("src/utilfunctions.R")
source("src/errorfunctions.R")
source("src/predictionfunctions.R")
source("src/predictionprocedure.R")

################################################################################
#
# Dataset 1
#
################################################################################

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
cases.1$amount[is.na(cases.1$amount)] = 0
cases.1$amount = as.numeric(cases.1$amount)
cases.1$points = as.numeric(cases.1$points)
cases.1$month = as.numeric(month(as.POSIXlt(cases.1$starttime, origin="1970-01-01", tz = "GMT")))
cases.1$day = as.numeric(day(as.POSIXlt(cases.1$starttime, origin="1970-01-01", tz = "GMT")))
#The relation that must be learned to predict the class of a case (Easy, Hard, ...)
class.relation.1 = class ~ amount + article + points + vehicleclass + month + day
proctime.relation.1 = proctime ~ amount + points + month + day

################################################################################
#
# Dataset 2
#
################################################################################

#Event log, created by exporting csv from Disco and then zipping the result
data.2 <- data.frame(read.csv(unz("data/2.zip","2.csv"), header = TRUE, sep = ";", quote = "\"", dec = "."))
data.2$Complete.Timestamp <- as.POSIXct(strptime(data.2$Complete.Timestamp, "%Y/%m/%d %H:%M:%S", tz = "GMT"))

#Aggregate the cases from the event log
cases.2 <- sqldf('SELECT 
                 `Case.ID` AS caseid,
                 GROUP_CONCAT(Activity) AS activities,
                 GROUP_CONCAT(Resource) AS resources,
                 Variant as variant,
                 MIN(`X.case..AMOUNT_REQ`) AS amount,
                 MIN(`Complete.Timestamp`) AS starttime, 
                 MAX(`Complete.Timestamp`) AS endtime, 
                 MAX(`Complete.Timestamp`) - MIN(`Complete.Timestamp`) AS proctime 
                 FROM `data.2` GROUP BY `Case.ID`')
cases.2$amount[is.na(cases.2$amount)] = 0
cases.2$amount = as.numeric(cases.2$amount)
cases.2$month = as.numeric(month(as.POSIXlt(cases.2$starttime, origin="1970-01-01", tz = "GMT")))
cases.2$day = as.numeric(day(as.POSIXlt(cases.2$starttime, origin="1970-01-01", tz = "GMT")))
cases.2$prefix = sapply(cases.2$activities, function(x) activityListPrefix(x,3))
#The relation that must be learned to predict the class of a case (Easy, Hard, ...)
class.relation.2 = class ~ amount + month + day + prefix
proctime.relation.2 = proctime ~ amount + month + day + prefix

################################################################################
#
# Dataset 3
#
################################################################################

#Event log, created by exporting csv from Disco and then zipping the result
data.3 <- data.frame(read.csv(unz("data/3.zip","3.csv"), header = TRUE, sep = ";", quote = "\"", dec = "."))
data.3$Complete.Timestamp <- as.POSIXct(strptime(data.3$Complete.Timestamp, "%Y/%m/%d %H:%M:%S", tz = "GMT"))

#Aggregate the cases from the event log
cases.3 <- sqldf('SELECT 
                 `Case.ID` AS caseid,
                 GROUP_CONCAT(Activity) AS activities,
                 GROUP_CONCAT(Resource) AS resources,
                 Variant as variant,
                 `impact`,
                 `org.group` AS orggroup,
                 `org.role` AS orgrole,
                 `organization.country` AS orgcountry,
                 `product` AS orgproduct,
                 MIN(`Complete.Timestamp`) AS starttime, 
                 MAX(`Complete.Timestamp`) AS endtime, 
                 MAX(`Complete.Timestamp`) - MIN(`Complete.Timestamp`) AS proctime 
                 FROM `data.3` GROUP BY `Case.ID`')
cases.3$month = as.numeric(month(as.POSIXlt(cases.3$starttime, origin="1970-01-01", tz = "GMT")))
cases.3$day = as.numeric(day(as.POSIXlt(cases.3$starttime, origin="1970-01-01", tz = "GMT")))
#The relation that must be learned to predict the class of a case (Easy, Hard, ...)
cases.3$prefix = sapply(cases.3$activities, function(x) activityListPrefix(x,1))
class.relation.3 = class ~ impact + orgrole + month + day + prefix
proctime.relation.3 = proctime ~ month + day + prefix

################################################################################
#
# Dataset 4
#
################################################################################

#Event log, created by exporting csv from Disco and then zipping the result
data.4 <- data.frame(read.csv(unz("data/4.zip","4.csv"), header = TRUE, sep = ";", quote = "\"", dec = "."))
data.4$Complete.Timestamp <- as.POSIXct(strptime(data.4$Complete.Timestamp, "%Y/%m/%d %H:%M:%S", tz = "GMT"))

#Aggregate the cases from the event log
cases.4 <- sqldf('SELECT 
                 `Case.ID` AS caseid,
                 GROUP_CONCAT(Activity) AS activities,
                 GROUP_CONCAT(Resource) AS resources,
                 Variant as variant,
                 MIN(`X.case..SUMleges`) as leges,
                 `X.case..parts` AS casetype,
                 `X.case..Responsible_actor` AS responsible,
                 MIN(`Complete.Timestamp`) AS starttime, 
                 MAX(`Complete.Timestamp`) AS endtime, 
                 MAX(`Complete.Timestamp`) - MIN(`Complete.Timestamp`) AS proctime 
                 FROM `data.4` GROUP BY `Case.ID`')
cases.4$leges[is.na(cases.4$leges)] = 0
cases.4$leges = as.numeric(cases.4$leges)
cases.4$month = as.numeric(month(as.POSIXlt(cases.4$starttime, origin="1970-01-01", tz = "GMT")))
cases.4$day = as.numeric(day(as.POSIXlt(cases.4$starttime, origin="1970-01-01", tz = "GMT")))
cases.4$prefix = sapply(cases.4$activities, function(x) activityListPrefix(x,1))
#The relation that must be learned to predict the class of a case (Easy, Hard, ...)
class.relation.4 = class ~ leges + responsible + month + day + prefix
proctime.relation.4 = proctime ~ leges + month + day + prefix

################################################################################
#
# Do the analysis
#
################################################################################

################################################################################
#
# Note: There currently is a line in predictionprocedure.R that only keeps
#       the elements of the test set that also have a prefix that also occurs in
#       the training set. That may be a problem.
#
################################################################################

################################################################################
#
# TODO: 
#   - Use a step number and time passed to predict processing time
#   - Also report the number of elements in each class
#
################################################################################

compute.proctime(cases.1, class.relation.1, proctime.relation.1, 
                 f.addclass.clusters,
                 f.learnclass.dectree, 
                 f.learnproctime.regressiontree, 
                 f.predictclass.dectree, 
                 f.predictproctime)

################################################################################
#
# Histograms of Processing Times
#
################################################################################
#
#hist(cases.1$proctime/(60*60*24),breaks=200,xlab="Processing Time (days)",main="Histogram of Processing Time Case 1",xlim=c(0,1500))
#hist(cases.2$proctime/(60*60*24),breaks=200,xlab="Processing Time (days)",main="Histogram of Processing Time Case 2",xlim=c(0,40))
#hist(cases.3$proctime/(60*60*24),breaks=200,xlab="Processing Time (days)",main="Histogram of Processing Time Case 3",xlim=c(0,150))
#hist(cases.4$proctime/(60*60*24),breaks=200,xlab="Processing Time (days)",main="Histogram of Processing Time Case 4",xlim=c(0,400))
