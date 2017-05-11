#functions.R
# An exaustive repo for all of my functions I have built for my capstone analysis

# Include Constants
source("/Users/bmc/Desktop/CSCI-49500/THE_QUEST___Progress_Folder/progress_in_R/constants.R")

###################################################################################################
# Function: createCityOverTimeDataFrame                                                           
# Parameters: (dataframe(2 cols), data.frame                                                      
# Return: Each city as a row observation, over time it's cluster assignment                       
###################################################################################################
createCityOverTimeDataFrame <- function(fiftyCityStates, df_all_hits){
  df.list <- list()
  df.list <- apply(fiftyCityStates, 1, function(uniqueCityState){
    k <- df_all_hits[which(df_all_hits$city == uniqueCityState[1]),]
    hits <- k[which(k$state == uniqueCityState[2]),]
    return(hits)
  })
  
  # for each city DF merge them on year
  container <- data.frame(year = c(1:35))
  for(i in 1:length(df.list)){
    tmp1 <- df.list[[i]][,c("state","year","cluster")]
    names(tmp1) <- c("year", paste("cluster",i))
    container <- merge(container,tmp1, all=TRUE)
  }
  
  if(length(unique(fiftyCityStates$city)) != 50){
    # Then handle the unique each unique case with an additional number added to its end
    fiftyOverTime<-data.frame(t(container[,2:ncol(container)]))
    rownames(fiftyOverTime) <- make.names(fiftyCityStates$city, unique=TRUE)
    names(fiftyOverTime) <- c(1:35)
    head(fiftyOverTime)
  }
  else{
    fiftyOverTime<-data.frame(t(container[,2:ncol(container)]), row.names=fiftyCityStates$city)
    names(fiftyOverTime) <- c(1:35)
    head(fiftyOverTime)
  }
  
  return(fiftyOverTime)
}

###################################################################################################
# Function: getListOfCityDataFrames                                                               
# Parameters:  data.frame of unique city states, and all data in another df                       
# Return: A list of each unique city-state as a dataframe                                         
# Example: getListOfCityDataFrames(df_good, df)[[1]] will return the first unique data frame      
###################################################################################################
getListOfCityDataFrames <- function(all_city_states, df){
  df.list <- list()
  df.list <- apply(all_city_states, 1, function(uniqueCityState){
    k <- df[which(df$city == uniqueCityState[1]),]
    hits <- k[which(k$state == uniqueCityState[2]),]
    return(hits)
  })
  return(df.list)
}

###################################################################################################
###################################################################################################
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

###################################################################################################
###################################################################################################
get_halfs <- function(ex){
  firsthalf <- ex[1:(length(ex)/2)]
  secondhalf <- ex[(length(firsthalf)+1):length(ex)]
  return(list(firsthalf, secondhalf))
}

###################################################################################################
###################################################################################################
getQuarters <- function(ex1){
  halfs <- get_halfs(ex1)
  quarter_1_2 <- get_halfs(halfs[[1]])
  quarter_3_4 <- get_halfs(halfs[[2]])
  quarters <- c(mode(quarter_1_2[[1]]),mode(quarter_1_2[[2]]),mode(quarter_3_4[[1]]),mode(quarter_3_4[[2]]))
  return(quarters)
}

###################################################################################################
###################################################################################################
isGood <- function(someNumber){
    if((someNumber == LIST_UNIQUE_BEST_CITIES[1]) || 
       (someNumber == LIST_UNIQUE_BEST_CITIES[2])){
         return(TRUE)
    } else{
         return(FALSE) 
    }
}

###################################################################################################
###################################################################################################
isBad <- function(someNumber){ 
    if((someNumber == LIST_UNIQUE_WORST_CITIES[1]) || (someNumber == LIST_UNIQUE_WORST_CITIES[2])){
        return(TRUE) 
      } 
    else{
        return(FALSE) 
    }
}

###################################################################################################
###################################################################################################
getBadGrowth <- function(df_increasing_cities, df){
  #Grammar for Bad Growth
  # S  -> S4 | S3 | S2
  # S4 -> (G|B) (G|B) (B) (B)
  # S3 -> (G|B) B B
  # S2 -> (G|B) B
  # B  -> (1|2)
  # B  -> (6|8)
  
  df.bad.growth = data.frame(city=c(), state=c())
  for(i in 1:nrow(df_increasing_cities)){
    
    ex1 <- df[which(df$city == df_increasing_cities[i,1] & df$state == df_increasing_cities[i,2]),c("cluster")]
    quarters1 <- getQuarters(ex1)
    
    quarters1 <- quarters1[!is.na(quarters1)]
    
    # By not having restrictions on quarters1[1] or quarters[2], we can get 153 results instead of just 63
    if(length(quarters1) == 4)
    {
      if(isGood(quarters1[1])){
        if(isGood(quarters1[2])){
          if(isBad(quarters1[3])){
            if(isBad(quarters1[4])){
              df.bad.growth <- rbind(df.bad.growth,
                                     df[which(df$city == df_increasing_cities[i,1] & df$state == df_increasing_cities[i,2]),c("city","state")])
            }
          }
        }
      }
    }
    
    if(length(quarters1) == 3)
    {
      if(isGood(quarters1[1])){
        if(isBad(quarters1[2])){
          if(isBad(quarters1[3])){
            df.bad.growth <- rbind(df.bad.growth,
                                   df[which(df$city == df_increasing_cities[i,1] & df$state == df_increasing_cities[i,2]),c("city","state")])
          }
        }
      }
    }
    
    if(length(quarters1) == 2)
    {
      if(isGood(quarters1[1])){
        if(isBad(quarters1[2])){
          df.bad.growth <- rbind(df.bad.growth,
                                 df[which(df$city == df_increasing_cities[i,1] & df$state == df_increasing_cities[i,2]),c("city","state")])
        }
      }
    }
  } #end for
  
  return(unique(df.bad.growth))
}

###################################################################################################
# Function: getPopulationChangersIncreasing
# Objective: 
# - Pass a huge list of dataframes containing each city and all its features over time
# - Collect all of the unique city-states that find themself increasing from beginning to end
###################################################################################################
getPopulationChangersIncreasing <- function(df.list.crime.changers){
  
  df.all.changers <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.crime.changers)){
    
    ls1 <- unique(df.list.crime.changers[[i]][,"population"])
    hits <- df.list.crime.changers[[i]][,"population"]
    
    # Capture growing cities
    if(hits[1] <= hits[length(hits)]){
      df.all.changers <- rbind(df.all.changers, unique(df.list.crime.changers[[i]][,c("city","state")]))
    }
    
    
  }
  
  # Update constants
  DF_UNIQUE_ASC_POP_CITY_STATE <- df.all.changers
  
  return(df.all.changers)
}

###################################################################################################
###################################################################################################
getPopulationChangersDecreasing <- function(df.list.crime.changers){
  
  df.all.changers <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.crime.changers)){
    
    ls1 <- unique(df.list.crime.changers[[i]][,"population"])
    hits <- df.list.crime.changers[[i]][,"population"]
    
    
    # Capture shrinking cities
    if(hits[1] >= hits[length(hits)]){
      df.all.changers <- rbind(df.all.changers, unique(df.list.crime.changers[[i]][,c("city","state")]))
    }
    
  }
  
  
  #update constants
  DF_UNIQUE_ASC_POP_CITY_STATE <- df.all.changers
  
  return(df.all.changers)
}

###################################################################################################
# Function: getCrimeChangers                                                                      
# Return: a dataframe of unique city-states who had the crime cluster changes                    
###################################################################################################
getCrimeChangers <- function(uc_best, uc_worst, df.list){

  df.crime.changers <- data.frame(city=c(), state=c())
  for(i in 1:length(df.list)){
    i1 <- intersect(uc_best,unique(df.list[[i]][,"cluster"]))
    i2 <- intersect(uc_worst,unique(df.list[[i]][,"cluster"]))
    
    if( length(i1) >= 1 & length(i2) >= 1){
      df.crime.changers <- rbind(df.crime.changers, unique(df.list[[i]][,c("city","state")]))
    }
  }
  
  #update constants
  DF_UNIQUE_CRIME_CHANGERS_CITY_STATE <- df.crime.changers
  
  return(df.crime.changers)
} 

###################################################################################################
# Function: get_best_unique_clusters                                                              
# Return: All of the unique clusters to the a set of cities inside of the dataframe given         
###################################################################################################
get_best_unique_clusters <- function(df_best, df){
  
  df.list.best <- apply(df_best, 1, function(row){df[which(df$city == row[1] & df$state == row[2]),]})
  
  uc <- list()
  for(i in 1:length(df.list.best)){
    uc <- c(uc, unique(df.list.best[[i]][,"cluster"]))
  }
  uc <- unique(uc)
  
  #update constants
  DF_UNIQUE_BEST_CLUSTERS <- uc
  
  return(uc)
}

###################################################################################################
# Function: get_worst_unique_clusters                                                             
# Return: All of the unique clusters to the a set of cities inside of the dataframe given         
###################################################################################################
get_worst_unique_clusters <- function(df_worst, df){
  df.list.worst <- apply(df_worst, 1, function(row){df[which(df$city == row[1] & df$state == row[2]),]})
  
  uc <- list()
  for(i in 1:length(df.list.worst)){
    uc <- c(uc, unique(df.list.worst[[i]][,"cluster"]))
  }
  
  uc <- unique(uc)
  
  #update constants
  DF_UNIQUE_WORST_CLUSTERS <- uc
  
  return(uc)
}

