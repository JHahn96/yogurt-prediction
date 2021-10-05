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
  
  # add Mclassi factor and Rest factor
  df.cat<-calc_mclassi_rest(df.cat, df.cat)
  
  return (df.cat)
}


load.data.frames<-function(keyword.vec){
  filenames <- list.files("data_hs21", pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
  ldf <- lapply(ldf, convert.to.correct.data.types)
  as.data.frame(ldf[1])
  
  df<- load.data.frame(keyword.vec)
  
  # get google trend for all keywords
  for (keyword in keyword.vec) {
    for (i in 1:length(ldf)){

      ldf[[i]] <-get.google.trend(keyword, as.data.frame(ldf[i]))
      
      # add yogurt prices
      ldf[[i]] <-merge(x = ldf[i], y = read.yogurt.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
      
      # add milk prices
      ldf[[i]] <-merge(x = ldf[i], y = read.milk.data(), by.y = "date", by.x = "date.month" ,  all.x = TRUE)
      
      # add Mclassi factor and Rest factor
      ldf[[i]]<-calc_mclassi_rest(ldf[i], df)
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



calc_mclassi_rest<-function(df_yogurt, df_all){
  Namen<-names(table(df_all$article_name))
  #split dataframe based on variabe
  
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
  
  #install.packages("GPArotation")
  library(psych)
  
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

###############################################################################
## DATA PREPARATION finished
###############################################################################
