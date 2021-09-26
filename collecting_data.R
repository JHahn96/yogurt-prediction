# load relevant libraries

library(hablar)
library(tidyverse)


load.data.frame<-function(){
  
  # read csvs
  filenames <- list.files("data_hs21", pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  
  # union all dfs
  df.cat <- do.call(rbind, ldf)
  
  df.cat <- convert.to.correct.data.types(df.cat)
  
  
  return (df.cat)
}

load.data.frames<-function(){
  filenames <- list.files("data_hs21", pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  ldf <- lapply(ldf, convert.to.correct.data.types)
  
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
  
  return(df)
  
}

###############################################################################
## DATA PREPARATION finished
###############################################################################
