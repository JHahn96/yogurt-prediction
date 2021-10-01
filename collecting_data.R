# load relevant libraries


library(gtrendsR)
library(hablar)
library(tidyverse)

## For debugging only
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loading
library("readxl")



load.data.frame<-function(keyword.vec){
  
  # read csvs
  filenames <- list.files("data_hs21", pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  
  # union all dfs
  df.cat <- do.call(rbind, ldf)
  
  df.cat <- convert.to.correct.data.types(df.cat)
  
  # get all trends from keyword vector
  for (keyword in keyword.vec) {
    df.cat<-get.google.trend(keyword, df.cat)
  }
  
  # add yogurt prices
  df.cat<-merge(x = df.cat, y = read.yogurt.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
  
  # add milk prices
  df.cat<-merge(x = df.cat, y = read.milk.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
  
  
  return (df.cat)
}


load.data.frames<-function(keyword.vec){
  filenames <- list.files("data_hs21", pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  ldf <- lapply(ldf, convert.to.correct.data.types)
  as.data.frame(ldf[1])
  
  # get google trend for all keywords
  for (keyword in keyword.vec) {
    for (i in 1:length(ldf)){

      ldf[[i]] <-get.google.trend(keyword, as.data.frame(ldf[i]))
      
      # add yogurt prices
      ldf[[i]] <-merge(x = ldf[i], y = read.yogurt.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
      
      # add milk prices
      ldf[[i]] <-merge(x = ldf[i], y = read.milk.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
    }
  }
  #ldf<-get.google.trend(keyword, as.data.frame(ldf[1]))
  return (ldf)
}


convert.to.correct.data.types<-function(df){
  dates <- df %>%
    select(grep(names(.), pattern = "yrwk")) %>%
    names
  
  
  patterns <- c("^category_", "^promo_")
  
  factors <- df %>%
    select(grep(names(.), pattern=paste(patterns, collapse="|"))) %>%
    names
  
  factors <- c(factors, "year", "month", "week")
  
  df <- df %>%
    convert(fct(factors)) %>%
    mutate_at(vars(all_of(dates)), funs(as.Date(., format = "%Y-%m-%d"))) %>%
    arrange(yrwk_start)
  df["date.month"]<-as.Date(paste(paste(df$year, df$month, sep="-"),"01", sep="-"),format="%Y-%m-%d")
  
  return(df)
  
}

get.google.trend<-function(keyword, df){
  start <- paste(substr(min(df$yrwk_start),1,8), "01",sep="")
  end <- max(df$yrwk_end)
  #print(paste(start,end,sep=" "))
  A<-gtrends(keyword,geo="CH",time=paste(start,end,sep=" "))
  B<-A$interest_over_time
  
  B <- B %>%
    rename(!!gsub(" ", ".", keyword) := hits)%>%
    select(-keyword,-geo,-time, -gprop, -category)
    
  
  B$date<-as.Date(B$date, format="%Y-%m-%d")
  df<-merge(x = df, y = B, by.y = "date", by.x = "date.month" ,  all.x = TRUE)
  #print(df)
  return(df)
}


read.yogurt.data<-function(){
  # xlsx files
  yogurt <- read_excel("data_hs21/yogurt.xlsx")
  
  yogurt <- yogurt %>%
    mutate(month = substr(.$datum,1,2),
           year =  paste(20,substr(.$datum,4,5),sep="")) %>%
    mutate (date = as.Date(paste("01",paste(month, year, sep="."),sep="."), format="%d.%m.%Y"))%>%
    select(-month,-year,-datum)
  return(yogurt)
}

read.milk.data<-function(){
  # xls files
  milk <- read_excel("data_hs21/milchpreis.xlsx")
  
  milk <- milk %>%
    mutate(month = substr(.$datum,1,2),
           year =  paste(20,substr(.$datum,4,5),sep="")) %>%
    mutate (date = as.Date(paste("01",paste(month, year, sep="."),sep="."), format="%d.%m.%Y"))%>%
    select(-month,-year,-datum)
  return(milk)
}

###############################################################################
## DATA PREPARATION finished
###############################################################################
