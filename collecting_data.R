# load relevant libraries

library(data.table)
library(gtrendsR)
library(hablar)
library(tidyverse)
#install.packages("GPArotation")
library(psych)
library(lubridate)
library("readxl")

## For debugging only
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loading


load.data.frame<-function(keyword.vec, data.folder, pred.data){
  
  # read csvs
  filenames <- list.files(data.folder, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  
  
  if (pred.data){
    data.folder.pred <-paste(data.folder,"_pred", sep="") 
    dfs.pred<-load.prediction.data.frames(data.folder.pred)
    for(i in seq(1,length(ldf))){
      df<-convert.to.correct.data.types(ldf[[i]])
      for(k in seq(1,length(dfs.pred))){
        df.pred<-dfs.pred[[k]]
        if (df$article_name[1]==df.pred$article_name[1]){
          ldf[[i]]<-rbindlist(list(df, df.pred), fill = TRUE)
        }
      }
    }
  }
  
  # union all dfs
  df.cat <- do.call(rbind, ldf)
  
  if (!pred.data){
    df.cat <- convert.to.correct.data.types(df.cat)
  }
  
  # get all trends from keyword vector
  for (keyword in keyword.vec) {
    t<-get.google.trend(keyword, df.cat)
    df.cat<-merge(x = df.cat, 
                  y = t, 
                  by.y = c("week", "month",  "year"), 
                  by.x = c("week", "month",  "year") ,  
                  all.x = TRUE)
    df.cat
  }
  
  # add yogurt prices
  df.cat<-merge(x = df.cat, y = read.yogurt.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
  
  # add milk prices
  df.cat<-merge(x = df.cat, y = read.milk.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
  
  # add Mclassi factor and Rest factor
  df.cat<-calc_mclassi_rest(df.cat, df.cat)
  
  return (df.cat)
}
#read.csv(filenames[16])

load.prediction.data.frames<-function(data.folder){
  filenames <- list.files(data.folder, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  ldf <- lapply(ldf, convert.to.correct.data.types)
  return (ldf)
  }

load.data.frames<-function(keyword.vec, data.folder, pred.data){
  filenames <- list.files(data.folder, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  ldf <- lapply(ldf, convert.to.correct.data.types)
  
  # get google trend for all keywords
  df<- load.data.frame(keyword.vec, data.folder, pred.data)
  
  if (pred.data){
    data.folder.pred <-paste(data.folder,"_pred", sep="")
    dfs.pred<-load.prediction.data.frames(data.folder.pred)
    for(i in seq(1,length(ldf))){
      df.y<-ldf[[i]]
      for(k in seq(1,length(dfs.pred))){
        df.pred<-dfs.pred[[k]]
        if (df.y$article_name[1]==df.pred$article_name[1]){
          ldf[[i]]<-rbindlist(list(df.y, df.pred), fill = TRUE)
        }
      }
    }
  }
  
  
  for (i in 1:length(ldf)){
    i<-1
    ## get the gtrends data
    gtrends<-df %>%
      select(c("yrwk_start", "article_name"),gsub(" ", ".", keyword.vec))
    
    ldf[[i]] <-merge(x = ldf[i], 
                     y = gtrends, 
                     by.y = c("yrwk_start", "article_name"), 
                     by.x = c("yrwk_start", "article_name") ,  
                     all.x = TRUE)
    
    ## add yogurt prices
    ldf[[i]] <-merge(x = ldf[i], y = read.yogurt.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
    
    ## add milk prices
    ldf[[i]] <-merge(x = ldf[i], y = read.milk.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
    
    ## add Mclassi factor and Rest factor
    ldf[[i]]<-calc_mclassi_rest(ldf[i], df)
    ldf[[i]][is.na(ldf[[i]])] <- 0
  }
  
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

#keyword<-"Joghurt"
#df<-df.cat
#current_year <- 2021
get.google.trend<-function(keyword, df){
    start <- paste(substr(min(df$yrwk_start),1,8), "01",sep="")
    end <- max(df$yrwk_end)
    x<-get_daily_gtrend(
      keyword = c(keyword), 
      geo = "CH", 
      from = start, 
      to = end
      )
    t<-x %>%
      filter(date>= min(df$yrwk_start))%>%
      mutate(week = week(date), month = month(date), year = year(date))%>%
      group_by(week,month,year ) %>%
      summarise(mean = mean(est_hits))%>%
      rename(!!gsub(" ", ".", keyword) := mean)
    
    return(t)
}


read.yogurt.data<-function(){
  ## xlsx files
  yogurt <- read_excel("data_hs21/yogurt.xlsx")
  
  yogurt <- yogurt %>%
    mutate(month = substr(.$datum,1,2),
           year =  paste(20,substr(.$datum,4,5),sep="")) %>%
    mutate (date = as.Date(paste("01",paste(month, year, sep="."),sep="."), format="%d.%m.%Y"))%>%
    select(-month,-year,-datum)
  return(yogurt)
}

read.milk.data<-function(){
  ## xls files
  milk <- read_excel("data_hs21/milchpreis.xlsx")
  
  milk <- milk %>%
    mutate(month = substr(.$datum,1,2),
           year =  paste(20,substr(.$datum,4,5),sep="")) %>%
    mutate (date = as.Date(paste("01",paste(month, year, sep="."),sep="."), format="%d.%m.%Y"))%>%
    select(-month,-year,-datum)
  return(milk)
}

df_yogurt<-ldf[i]
df_all<-df
#df_yogurt<-df.cat
#df_all<-df.cat
calc_mclassi_rest<-function(df_yogurt, df_all){
  
  df_all$sales[is.na(df_all$sales)]<--1
  Namen<-names(table(df_all$article_name))
  ## split dataframe based on variabe
  
  a<-df_all$article_name==Namen[1]
  b<-df_all$article_name==Namen[2]
  c<-df_all$article_name==Namen[3]
  d<-df_all$article_name==Namen[4]
  e<-df_all$article_name==Namen[5]
  f<-df_all$article_name==Namen[6]
  g<-df_all$article_name==Namen[7]
  h<-df_all$article_name==Namen[8]
  i<-df_all$article_name==Namen[9]
  j<-df_all$article_name==Namen[10]
  k<-df_all$article_name==Namen[11]
  l<-df_all$article_name==Namen[12]
  m<-df_all$article_name==Namen[13]
  n<-df_all$article_name==Namen[14]
  o<-df_all$article_name==Namen[15]
  p<-df_all$article_name==Namen[16]
  q<-df_all$article_name==Namen[17]
  r<-df_all$article_name==Namen[18]
  s<-df_all$article_name==Namen[19]
  
  A<-df_all[a,]
  B<-df_all[b,]
  C<-df_all[c,]
  D<-df_all[d,]
  E<-df_all[e,]
  FF<-df_all[f,]
  G<-df_all[g,]
  H<-df_all[h,]
  I<-df_all[i,]
  J<-df_all[j,]
  K<-df_all[k,]
  L<-df_all[l,]
  M<-df_all[m,]
  N<-df_all[n,]
  O<-df_all[o,]
  P<-df_all[p,]
  Q<-df_all[q,]
  R<-df_all[r,]
  S<-df_all[s,]
  
  Tot<-cbind.data.frame(A$sales,B$sales,C$sales,D$sales,E$sales,FF$sales,G$sales,H$sales,I$sales,J$sales,K$sales,L$sales,M$sales,N$sales,O$sales,P$sales,Q$sales,R$sales,S$sales)
  colnames(Tot)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S")
  
  Tot<-Tot/1000
  
  Faktoren<-fa(Tot,nfactors=2)
  Werte<-Faktoren$scores
  MClassi<-Werte[,1]
  Rest<-Werte[,2]
  
  Tot<-cbind.data.frame(A$yrwk_start,MClassi)
  Tot<-cbind.data.frame(Tot,Rest)
  colnames(Tot)<-c("date", "MClassi", "Rest")
  
  df <- merge(x = df_yogurt, y = Tot, by.y = "date", by.x = "yrwk_start",  all.x = TRUE)
  return(df)
}



## http://alexdyachenko.com/all/how-to-get-daily-google-trends-data-for-any-period-with-r/
get_daily_gtrend <- function(keyword = c('Taylor Swift', 'Kim Kardashian'), geo = 'US', from = '2013-01-01', to = '2019-08-15') {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop("Specifying \'to\' date in the current month is not allowed")
    }
  }
  
  mult_m <- gtrends(keyword = keyword, geo = geo, time = paste(from, to))$interest_over_time %>%
    mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    group_by(month = floor_date(date, 'month'), keyword) %>%
    summarise(hits = sum(hits)) %>%
    ungroup() %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, keyword, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    print(paste('for', pm$s[i], pm$e[i], 'retrieved data (all keywords)'))
    raw_trends_m <- rbind(raw_trends_m,
                          curr$interest_over_time)
    Sys.sleep(.1)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, keyword, hits) %>%
    mutate(ym = format(date, '%Y-%m'),
           hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m) %>%
    mutate(est_hits = hits * mult) %>%
    select(date, keyword, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}



###############################################################################
## DATA PREPARATION finished
###############################################################################
