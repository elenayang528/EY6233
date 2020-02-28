install.packages("readxl")
library(readxl)
library(quantmod)
library(tidyr)
library(stringr)
library(rvest)
library(plyr)
library(dplyr)

#Download/Read the html: NCREIF--------------------------------------------------------------------------------
region<- c("N","E","W","S","M","H","A","R","I","O")
region_name <- c("National","East","West","South","Middle","Hotel","Apartment","Retail","Industrial","Office")

all_return=data.frame()
for(i in 1:length(region)){
  url<-paste0("https://epitest.ncreif.org/property-index-returns.aspx?region=",region[i],"#farmland")
  html<- read_html(url)  
  get_return<- html_nodes(html,"#farmland")
  return_table <- html_table(get_return)
  return<-data.frame(return_table) 
  return$region<-rep(region_name[i],nrow(return))
  
  all_return<-rbind(all_return, return)
  print(paste0("Finished Download Region: ", region[i]))
}
pct_to_number<- function(x){
  x_replace_pct<-sub("%", "", x)
  x_as_numeric<-as.numeric(x_replace_pct)/100
}
all_return$Quarter.1<- pct_to_number(all_return$Quarter.1)
all_return$Quarter.2<- pct_to_number(all_return$Quarter.2)
all_return$Quarter.3<- pct_to_number(all_return$Quarter.3)
all_return$Quarter.4<- pct_to_number(all_return$Quarter.4)

diff_return <- all_return %>% gather(Quarter, Return, Quarter.1:Quarter.4)
diff_return <-diff_return[order(diff_return$Year),]%>% spread(region, Return)
diff_return$Quarter <- str_extract(diff_return$Quarter, "\\d")
col_order <- c("Year", "Quarter", "National","East","West","South","Middle","Hotel","Apartment","Retail","Industrial","Office")
diff_return <- diff_return[, col_order] %>% unite("Year", Year:Quarter, sep = ":", remove = TRUE)

ncreif_return<- diff_return[grep("1989:1", diff_return$Year):grep("2016:2", diff_return$Year),]


#Using quantmod to get stock return------------------------------------------------------------------------------

#sp500
sp500<- data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = "1989-01-01",to= "2016-06-30"))
SP500<-quarterlyReturn(sp500)
names(SP500)[1]<- c("S&P500")

#Wilhire5000
wilshire<- data.frame(getSymbols("^W5000",auto.assign = FALSE, from = "1989-01-01",to= "2016-06-30"))
WILSHIRE<-quarterlyReturn(wilshire)
names(WILSHIRE)[1]<- c("WILSHIRE")
stock_return<-cbind(SP500,WILSHIRE)

#Nareit-------------------------------------------------------------------------------------------------------------

Nareit <- read_excel("C:/Users/JY Development Group/Desktop/Elena/nareit_return.xlsx")[3]
Nareit <- unlist(Nareit)
View(Nareit)


#All data--------------------------------------------------------------------------------------------------------
all<- cbind.data.frame(ncreif_return,stock_return,Nareit)
rownames(all) <- all$Year
all_return<- all[2:ncol(all)]

View(all_return)
summary(all_return)
