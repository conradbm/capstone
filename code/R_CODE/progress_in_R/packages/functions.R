#functions.R
# An exaustive repo for all of my functions I have built for my capstone analysis

# Include Libraries
library(plotly)
library(tidyr)

# Include Constants
source("/Users/bmc/Desktop/CSCI-49500/THE_QUEST___Progress_Folder/progress_in_R/packages/constants.R")

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

##########################################################
# Get population over time from list of data frames
##########################################################
getPopulationOverTimeDataFrame <- function(unique_2014above50K, df.list.2014above50k){
  # PRO-TIP:
  # Concluding thoughts ... If you need to fill in missing data, load up a matrix the right size, it initializes with all NA's.
  # Then just find the indicies you NEED for each object, then fill in accordingly, just skip the missing indices and they will already be filled with NA's.
  
  mat <- matrix(nrow = length(df.list.2014above50k), ncol=35)
  for(i in 1:length(df.list.2014above50k)){
    ls1= c(35:1)
    
    gay <- df.list.2014above50k[[i]][order(-df.list.2014above50k[[i]][,"year"]),c("year","population")] 
    #gay <- aggregate(population~year,data=gay,FUN=sum)
    #gay <- gay[order(-gay[,"year"]),]
    missingYears <- setdiff(ls1, gay)
    
    #cat("Working on city_",i,"\n")
    for( j in intersect(ls1, gay[,"year"]))
    {
      
      mat[i,j] <- gay[which(gay$year == j),"population"]
      
    }
    cat("\Processed city_",i,"\t")
    #cat(mat[i,],"\n")
  }
  
  mat <- data.frame(mat)
  mat <- mat[,c(35:1)]
  
  
  thegang <- unique_2014above50K %>%
    select(city, state) %>%
    #this will create a new column with the city and states combined
    mutate(city_state = paste(as.character(city), as.character(state), sep = "_"))
  
  row.names(mat) <- thegang[,"city_state"]
  names(mat) <- c(35:1)
  
  return(mat)
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
    #cat("Processed ", uniqueCityState[1], ",", uniqueCityState[2], "\t");
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
    if((someNumber == LIST_UNIQUE_BEST_CLUSTERS[1]) || 
       (someNumber == LIST_UNIQUE_BEST_CLUSTERS[2])){
         return(TRUE)
    } else{
         return(FALSE) 
    }
}

###################################################################################################
###################################################################################################
isBad <- function(someNumber){ 
    if((someNumber == LIST_UNIQUE_WORST_CLUSTERS[1]) ||
       (someNumber == LIST_UNIQUE_WORST_CLUSTERS[2])){
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
  # S4 -> (G) (G) (B) (B)
  # S3 -> (G) B B
  # S2 -> (G) B
  # B  -> (1|2)
  # G  -> (6|8)
  
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
###################################################################################################
getGoodGrowth <- function(df_increasing_cities, df){
  #Grammar for Good Growth
  # S  -> S4 | S3 | S2
  # S4 -> (B) (B) (G) (G)
  # S3 -> (B) (G) (G)
  # S2 -> (B) (G)
  # B  -> (1|2)
  # G  -> (6|8)
  
  df.good.growth = data.frame(city=c(), state=c())
  
  for(i in 1:nrow(df_increasing_cities)){
    
    ex2 <- df[which(df$city == df_increasing_cities[i,1] & df$state == df_increasing_cities[i,2]),c("cluster")]
    quarters2 <- getQuarters(ex2)
    
    quarters2 <- quarters2[!is.na(quarters2)]
    
    # By not having restrictions on quarters1[1] or quarters[2], we can get 153 results instead of just 63
    if(length(quarters2) == 4)
    {
      if(isBad(quarters2[1])){
        if(isGood(quarters2[2])){
          if(isGood(quarters2[3])){
            if(isGood(quarters2[4])){
              df.good.growth <- rbind(df.good.growth,
                                      df[which(df$city == df_increasing_cities[i,1] & 
                                                 df$state == df_increasing_cities[i,2]), c("city","state")])
            }
          }
        }
      }
    }
    
    if(length(quarters2) == 3)
    {
      if(isBad(quarters2[1])){
        if(isGood(quarters2[2])){
          if(isGood(quarters2[3])){
            df.good.growth <- rbind(df.good.growth,
                                    df[which(df$city == df_increasing_cities[i,1] & 
                                               df$state == df_increasing_cities[i,2]),c("city","state")])
          }
        }
      }
    }
    
    if(length(quarters2) == 2)
    {
      if(isBad(quarters2[1])){
        if(isGood(quarters2[2])){
          df.good.growth <- rbind(df.good.growth,
                                  df[which(df$city == df_increasing_cities[i,1] &
                                             df$state == df_increasing_cities[i,2]),c("city","state")])
        }
      }
    }
  } #end for
  
  return(unique(df.good.growth))
}






###################################################################################################
# Function: getPopulationChangersIncreasing
# Objective: 
# - Pass a huge list of dataframes containing each city and all its features over time
# - Collect all of the unique city-states that find themself increasing from beginning to end
###################################################################################################
getPopulationChangersIncreasing <- function(df.list.crime.changers, thresh=NULL){
  
  df.all.changers <- data.frame(city=c(), state=c())
  
  
  for(i in 1:length(df.list.crime.changers)){
    
    ls1 <- unique(df.list.crime.changers[[i]][,"population"])
    hits <- df.list.crime.changers[[i]][,"population"]
    
    # Capture growing cities
    if(hits[1] <= hits[length(hits)]){
      if(!missing(thresh))
      {
        if(max(hits) >= thresh){
          cat("Skipping iteration ", i, " because of max population threshold given.\n")
          next
        }
      }

      df.all.changers <- rbind(df.all.changers, unique(df.list.crime.changers[[i]][,c("city","state")]))
    }
  }
  
  # Update constants
  DF_UNIQUE_ASC_POP_CITY_STATE <- df.all.changers
  
  return(df.all.changers)
}

###################################################################################################
###################################################################################################
getPopulationChangersDecreasing <- function(df.list.crime.changers, thresh=NULL){
  
  df.all.changers <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.crime.changers)){
    
    ls1 <- unique(df.list.crime.changers[[i]][,"population"])
    hits <- df.list.crime.changers[[i]][,"population"]
    
    
    # Capture shrinking cities
    if(hits[1] >= hits[length(hits)]){
      if(!missing(thresh)){
        if(max(hits) >= thresh){
          cat("Skipping iteration ", i, " because of max population threshold given.\n")
          next
        }
      }
      df.all.changers <- rbind(df.all.changers, unique(df.list.crime.changers[[i]][,c("city","state")]))
    }
    
  }
  
  
  #update constants
  DF_UNIQUE_DESC_POP_CITY_STATE <- df.all.changers
  
  return(df.all.changers)
}


###################################################################################################
# Note: This function should determine if a city's population is within:
# - epsilon = 1 or 2 * sd(population)
# - target the first population found and the last, determine if they are epsilon apart
# - if so, we will call it stagnant
###################################################################################################
getPopulationStagnant <- function(df.list.crime.changers){
  # Life is a journey, and this function is a work in progress
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
  
  return(uc)
}


###################################################################################################
      
###################################################################################################
plot_all_unique_cities <- function(uniqueCityList, df)
{
  plt <- ggplot()
  for(i in 1:nrow(uniqueCityList))
  {
    cat("Working: ", i)
    plt <- plt + geom_line(data=df[which(df$city == uniqueCityList[i,1] &
                                        df$state == uniqueCityList[i,2]),c("year","population","cluster","city","state")],
                           aes(x=year, y=population, color=factor(cluster)))

  }
  plt <- plt + scale_colour_manual(name="Crime Clusters", values = c("red", "black", "green", "blue"))
  #p + scale_colour_manual( values = c("8" = "red","4" = "blue","6" = "green"))
  plt <- plt + ggtitle("California",
                       subtitle="Population Increasing, Crime Patterns Bettering")
  # Make the plot interactive so that I can see the city state on hover
  plt <- plt + theme(plot.title = element_text(face="bold",size="18"),
                                     plot.subtitle = element_text(size="10"),
                                     axis.text = element_text(family = "mono",color="black"),
                                     axis.title.x = element_text(face="bold"),
                                     axis.title.y = element_text(face="bold"),
                                     panel.grid.major = element_line(colour="#CDCDCD"),
                                     panel.grid.minor = element_line(colour="#F0F0F0"),
                                     panel.background = element_rect(fill = "#F0F0F0"),
                                     plot.background = element_rect(fill = "#F0F0F0"),
                                     legend.background = element_rect(fill = "#F0F0F0"))
  plt <- plt + guides(fill=guide_legend(title="Crime Clusters"))
  abbrev_x <- c("1979","'84","'89","'94","'99","'04","'09","'14")
  plt<- plt + scale_x_continuous(breaks = c(seq(0,35,by=5)), labels=abbrev_x)
  plt <- plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plt)
}





###########################################################################################
# Code used to generate (Group 1) -- Drastic population increasers
###########################################################################################
###########################################################################################

find2014Above50K <- function(df.list){
  
  container <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list)){
    hit <- df.list[[i]][which(df.list[[i]][,"year"] == 35 & 
                                df.list[[i]][,"population"] >= 50000), c("year","population")]
    
    if(nrow(hit) > 0){
      cat("Processed: ", i, "\t")
      container <- rbind(container, unique(df.list[[i]][,c("city","state")]))
    }else{ }
  }
  
  return(container)
  
}

findMinYearBelow10K <- function(df.list.above){
  container <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.above)){
    
    hit<- df.list.above[[i]][which(df.list.above[[i]][,"year"] == min(df.list.above[[i]][,"year"]) & 
                                df.list.above[[i]][,"population"] <= 10000),]
    
    
    if(nrow(hit) > 0){
      cat("Storing ", i, " ")
      container <- rbind(container, unique(df.list.above[[i]][,c("city","state")]))
    }else{ }
  }
  
  return(container)
}

###########################################################################################
# Code used to generate (Group 2) -- Stagnant cities
###########################################################################################
###########################################################################################
find1979to2014Stagnants <- function(df.list){
  container <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list)){
    
    # The goal here is to find the populations whos start index "1" is about the same as their end index "n"
    # This is done by checking if the final is within a + or - standard deviation of the first.
    n = nrow(df.list[[i]])
    dev = sd(df.list[[i]][,"population"])
    the_max <- df.list[[i]][1,"population"] + dev
    the_min <- df.list[[i]][1, "population"] - dev
    eh1 <- df.list[[i]][n,"population"] <= the_max
    eh2 <- df.list[[i]][n, "population"] >= the_min
    
    if(!is.na(eh1) & !is.na(eh2)){
      if(eh1 & eh2){
        container <- rbind(container, unique(df.list[[i]][,c("city","state")]))
      }
    }
    #cat(i,"\t",eh1,"\t",eh2,"\n")
    
  }
  
  return(container)
}

###########################################################################################
# Code used to generate (Group 3) -- Drastic population decreasers
###########################################################################################
###########################################################################################

find1979Above50K <- function(df.list.above){
  container <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.above)){
    hit <- df.list.above[[i]][which(df.list.above[[i]][,"year"] == 1 & 
                                      df.list.above[[i]][,"population"] >= 50000), c("year","population")]
    
    if(nrow(hit) > 0){
      container <- rbind(container, unique(df.list.above[[i]][,c("city","state")]))
    }else{ }
  }
  
  return(container)
}

find2014Below10K <- function(df.list.above){
  container <- data.frame(city=c(), state=c())
  
  for(i in 1:length(df.list.above)){
    hit <- df.list.above[[i]][which(df.list.above[[i]][,"year"] == 35 & 
                                      df.list.above[[i]][,"population"] <= 10000), c("year","population")]
    
    if(nrow(hit) > 0){
      container <- rbind(container, unique(df.list.above[[i]][,c("city","state")]))
    }else{ }
  }
  
  return(container)
}

# only 3 bins
# if > 1  == 0
# if < -1 == 1
# else    == 2

getBin <- function(someNumber){
  
  # Population increase
  if(someNumber > 0){
    return(0)
  }
  # Population Decrease
  else if(someNumber < 0 ){
    return(1)
  }
  # Population Stagnant
  else{
    return(2)
  }
}

##########################################
# get average year population growth
#########################################
getAYPI <- function(df.list.b){
  
  df_city_aypi <- data.frame(city=c(),state=c(),aypi=c(),population=c(), bin=c())
  
  for(i in 1:length(df.list.b)){ # each city_i
    
    # Target our city of interest
    #cat("Working on :", df.list[[i]][,"city"][1], " ", df.list[[i]][,"state"], "\n")
    
    
    df_year_pop <- df.list.b[[i]][order(-df.list.b[[i]][,"year"]),c("year","population")]
    
    # drop duplicate rows
    #df_year_pop <- df_year_pop[!duplicated(df_year_pop[,c("city","state","year")]),]
    val=0
    n = (nrow(df_year_pop)-1)
    
    for(j in 1:n){ #execute equation
      tmp = (df_year_pop[j,"population"] - df_year_pop[j+1,"population"] ) / df_year_pop[j+1,"population"]
      val = val + tmp
    }
    val = (val / n)
    
    
    # Store our results
    name <- df.list.b[[i]][order(-df.list.b[[i]][,"population"]),c("city")]
    state <- df.list.b[[i]][order(-df.list.b[[i]][,"population"]), c("state")]
    pop2014 <- df.list.b[[i]][order(-df.list.b[[i]][,"year"]),c("population")][1]
    binCategory <- getBin(val)
    
    df_city_aypi[i,"city"] <- name[1]
    df_city_aypi[i, "state"]<-state[1]
    df_city_aypi[i, "aypi"] <- val
    df_city_aypi[i, "population"] <- pop2014
    df_city_aypi[i, "bin"] <- binCategory
    
    # Confirm results
    cat("Storing: ", df_city_aypi[i,"city"], ":", df_city_aypi[i,"aypi"], "\n")
  }
  return(df_city_aypi)
}

##########################################
# get average year population growth
#########################################
getAYPI_return_ls_of_df <- function(df.list.b){
  
  df_city_aypi <- data.frame(city=c(),state=c(),aypi=c(),population=c(), bin=c())
  
  for(i in 1:length(df.list.b)){ # each city_i
    
    # Target our city of interest
    #cat("Working on :", df.list[[i]][,"city"][1], " ", df.list[[i]][,"state"], "\n")
    
    
    df_year_pop <- df.list.b[[i]][order(-df.list.b[[i]][,"year"]),c("year","population")]
    
    # drop duplicate rows
    #df_year_pop <- df_year_pop[!duplicated(df_year_pop[,c("city","state","year")]),]
    val=0
    n = (nrow(df_year_pop)-1)
    
    for(j in 1:n){ #execute equation
      tmp = (df_year_pop[j,"population"] - df_year_pop[j+1,"population"] ) / df_year_pop[j+1,"population"]
      val = val + tmp
    }
    val = (val / n)
    
    
    # Store our results
    name <- df.list.b[[i]][order(-df.list.b[[i]][,"population"]),c("city")]
    state <- df.list.b[[i]][order(-df.list.b[[i]][,"population"]), c("state")]
    pop2014 <- df.list.b[[i]][order(-df.list.b[[i]][,"year"]),c("population")][1]
    binCategory <- getBin(val)
    
    df_city_aypi[i,"city"] <- name[1]
    df_city_aypi[i, "state"]<-state
    df_city_aypi[i, "aypi"] <- val
    df_city_aypi[i, "population"] <- pop2014
    df_city_aypi[i, "bin"] <- binCategory
    
    if(df_city_aypi[i, "aypi"] < 0){
      df.list.b[[i]]$below=TRUE
    }
    else{
      df.list.b[[i]]$below=FALSE
    }
    
    # Confirm results
    cat("Storing: ", df_city_aypi[i,"city"], ":", df_city_aypi[i,"aypi"], "\n")
  }
  return(df.list.b)
}