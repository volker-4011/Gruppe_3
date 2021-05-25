library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(forcats)
library(RCurl)
library(readxl)
library(ical)
library(calendar)
library(base)

years <- as.character(c(2013:2019))

#https://de.wikipedia.org/wiki/Liste_der_deutschen_Bundesländer_nach_Bevölkerung
LaenderUndEinwohner <- read_csv("https://raw.githubusercontent.com/g3r1t/EDSML-Projekt/main/LaenderUndEinwohner.csv")



Datum <- seq(as.Date("2013-07-01"),as.Date("2019-06-06"), by = "days")
Jahresferien <- data.frame(matrix(ncol = (length(LaenderUndEinwohner$Bundesland) + 1), nrow = length(Datum)))
colnames(Jahresferien) <- c("Datum", LaenderUndEinwohner$Bundesland)
Jahresferien$Datum <- Datum


options(timeout= 4000000)
umsatzdaten_ferien <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")
allYears <- unique(substr(umsatzdaten_ferien$Datum, 1, 4))

for(i in allYears){
  create_folder <- paste("icals/",i,"/", sep = "")
  dir.create(create_folder, recursive = TRUE)
}





for (year in years) {
  for (Bl in LaenderUndEinwohner$Bundesland) {
    filename <- paste("ferien_", Bl, "_", year, ".ics", sep = "")
    url <- paste("https://www.ferienwiki.de/exports/ferien/",year,"/de/", Bl, sep = "")
    filedest <- paste("icals/", year,"/", filename, sep = "")
    curl::curl_download(url, filedest)
  }
}

df <- data.frame()


for(i in allYears){
  this_folder <- paste("icals/",i,"/", sep = "")
  
  files <- list.files(path=this_folder, pattern="*.ics", full.names=TRUE, recursive=FALSE)
  for(j in files){
    
    print(files)
    
    
    x = readLines(j)
    df <- rbind(df,ic_dataframe(x))

  
  print("/n")
  print("/n")
  print("/n")
  print("/n")
  }
  
}






































###### Ab hier kannst du den Code ignorieren
###### Das hier ist das erste ical importiert muss, wenn es in die richtige form gebracht ist, noch in
###### den for-loop gehängt werden
data <- data.frame("Ferienstart" = ical_parse_df(filedest)$start, "Ferienende" = ical_parse_df(filedest)$end)
data$Ferienstart <- format(data$Ferienstart, format='%Y-%m-%d')
data$Ferienende <- format(data$Ferienende, format='%Y-%m-%d')
Ferien <- seq(as.Date(data$Ferienstart[1]), as.Date(data$Ferienende[1]), by = "days")


###### das war der beginn eines versuchs alle icals in ein df zu importieren
filename <- paste("ferien_", Bl, "_", year, ".ics", sep = "")
url <- paste("https://www.ferienwiki.de/exports/ferien/",year,"/de/", Bl, sep = "")
filedest <- paste("icals/", year,"/", filename, sep = "")
curl::curl_download(url, filedest)
Ferienliste <- vector()
for (i in c(1:length(ical_parse_df(filedest)$start))) {
  vector <- c(vector, seq(as.Date(data$Ferienstart[i]), as.Date(data$Ferienende[i]), by = "days"))
}

