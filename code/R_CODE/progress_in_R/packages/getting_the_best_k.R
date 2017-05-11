#getting_the_best_k.R
# Code I used to determine the best K

library(ggplot2)
library(caret)
setwd("/Users/bmc/Desktop/CSCI-49500/THE_QUEST___Progress_Folder/progress_in_R")
source("/Users/bmc/Desktop/CSCI-49500/THE_QUEST___Progress_Folder/progress_in_R/packages/functions.R")

df <- read.csv("data/capstone_data.csv", header=TRUE)
df<- df[,1:ncol(df)-1]
df <- df[which((df$state %in% c(1:51))),]
df[,5:ncol(df)] <- scale(df[,5:ncol(df)], center=TRUE, scale=TRUE)
df$city <- as.character(df$city)
df$state <- as.character(df$state)
df[which(df$city == "TAVISTOCK"),]
df[which(df$city == "TAVISTOCK BOROUGH"),"city"] <- "TAVISTOCK"


df_best <- read.csv("data/50_best.txt", header=FALSE, col.names = c("city","state"))
df_best$city <- toupper(as.character(df_best$city))
df_best$state <- gsub('\\s+', '', df_best$state)
head(df_best)

df_worst <- read.csv("data/50_worst.txt", header=FALSE, col.names = c("city","state"))
df_worst$city <- toupper(as.character(df_worst$city))
df_worst$state <- gsub('\\s+', '', df_worst$state)
head(df_worst)

# Cluster
set.seed(1234)
cluster <- kmeans(df[,5:14], centers=5, nstart=30)
df$cluster <- cluster$cluster
df_first50 <- createCityOverTimeDataFrame(df_best, df)
df_second50 <- createCityOverTimeDataFrame(df_worst, df)
main_df <- rbind(df_first50, df_second50)
main_df <- main_df[c(1:100),]
head(main_df)
write.csv(main_df, "k5.csv")

# Cluster
set.seed(1234)
cluster <- kmeans(df[,5:14], centers=10, nstart=30)
df$cluster <- cluster$cluster

df_first50 <- createCityOverTimeDataFrame(df_best, df)
df_second50 <- createCityOverTimeDataFrame(df_worst, df)
main_df <- rbind(df_first50, df_second50)
main_df <- main_df[c(1:100),]
write.csv(main_df, "k10.csv")

# Cluster
set.seed(1234)
cluster <- kmeans(df[,5:14], centers=15, nstart=30)
df$cluster <- cluster$cluster
df_first50 <- createCityOverTimeDataFrame(df_best, df)
df_second50 <- createCityOverTimeDataFrame(df_worst, df)
main_df <- rbind(df_first50, df_second50)
main_df <- main_df[c(1:100),]
write.csv(main_df, "k15.csv")

# Cluster
set.seed(1234)
cluster <- kmeans(df[,5:14], centers=20, nstart=30)
df$cluster <- cluster$cluster
df_first50 <- createCityOverTimeDataFrame(df_best, df)
df_second50 <- createCityOverTimeDataFrame(df_worst, df)
main_df <- rbind(df_first50, df_second50)
main_df <- main_df[c(1:100),]
write.csv(main_df, "k20.csv")

# Cluster
set.seed(1234)
cluster <- kmeans(df[,5:14], centers=25, nstart=30)
df$cluster <- cluster$cluster
df_first50 <- createCityOverTimeDataFrame(df_best, df)
df_second50 <- createCityOverTimeDataFrame(df_worst, df)
main_df <- rbind(df_first50, df_second50)
main_df <- main_df[c(1:100),]
write.csv(main_df, "k25.csv")
