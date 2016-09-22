test = cases.1$proctime

testsample = sample(test, 1000)

k = pamk(testsample)$nc

# The following statement can be used to determing the most frequently occurring k]
# ks = c()
# ks = append(ks, 2)
# ks = append(ks, 10)
# ...
# k = names(sort(table(ks),decreasing=TRUE))[1]

# K-Means Cluster Analysis
fit <- kmeans(test, k)

# get cluster means 
clusters = aggregate(test,by=list(fit$cluster),FUN=mean)

# append cluster assignment
test <- data.frame(test, fit$cluster)
