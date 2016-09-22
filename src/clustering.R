test = cases.1$proctime

testsample = sample(test, 1000)

k = pamk(testsample)$nc

# K-Means Cluster Analysis
fit <- kmeans(test, k)

# get cluster means 
clusters = aggregate(test,by=list(fit$cluster),FUN=mean)

# append cluster assignment
test <- data.frame(test, fit$cluster)
