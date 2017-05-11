
library(dplyr)
library(ggplot2)
setwd("/Users/bmc/Desktop/CSCI-49000/BlogPost/progress_in_R/")


df <- read.csv("capstone_data.csv", header=TRUE)
df<- df[,1:ncol(df)-1]
df <- df[which((df$state %in% c(1:51))),]
head(df)


df_best <- read.csv("50_best.txt", header=FALSE, col.names = c("city","state"))
df_best$city <- toupper(as.character(df_best$city))
df_best$state <- gsub('\\s+', '', df_best$state)
head(df_best)


df_worst <- read.csv("50_worst.txt", header=FALSE, col.names = c("city","state"))
df_worst$city <- toupper(as.character(df_worst$city))
df_worst$state <- gsub('\\s+', '', df_worst$state)
head(df_worst)


set.seed(1234)
cluster <- kmeans(df[,5:14], centers=15, nstart=30)
df$cluster <- cluster$cluster
head(df$cluster)
df$agg <- apply(df[,5:14], 1, sum)



df_sorted_asc <- df[with(df, order(population)), ]
df_sorted_asc$city <- as.character(df_sorted_asc$city)
df_sorted_asc$year <- as.numeric(df_sorted_asc$year)
df_sorted_asc$state <- as.character(df_sorted_asc$state)

x <- unique(as.data.frame(t(apply(df_sorted_asc[,c("city","state")], 1, sort))));
x$city <- as.character(x$city)
x$state <- as.character(x$state)




df1 <- df %>%
  select(city, state, year, population, agg, cluster) %>%
  #this will create a new column with the city and states combined
  mutate(city_state = paste(as.character(city), as.character(state), sep = "_"))

df1 <- df1[,c(ncol(df1), 3,4,5,6)]
df2 <- df1 %>% select(city_state, year, population, agg, cluster) %>%
  mutate(city_state_year = paste(as.character(city_state), as.character(year), sep="_"))
df3 <- df3[,c(1,3,4,5)]
head(df3)
library(tidyr)
df4 <- df3 %>% spread(city_state_year, agg)
head(df4)
