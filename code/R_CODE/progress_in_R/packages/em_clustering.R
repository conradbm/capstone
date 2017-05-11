# Example for EM Clustering
library(EMCluster, quietly=TRUE)

ret <- init.EM(df[,4:14], nclass = 30)
ret.new <- assign.class(df, ret, return.all = FALSE)
str(ret.new)

  