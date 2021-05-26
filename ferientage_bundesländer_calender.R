#Laden aller benötigten Libaries
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
#Laden aller benötigten Libaries


#https://de.wikipedia.org/wiki/Liste_der_deutschen_Bundesländer_nach_Bevölkerung
LaenderUndEinwohner <- read_csv("https://raw.githubusercontent.com/g3r1t/EDSML-Projekt/main/LaenderUndEinwohner.csv")



Datum <- seq(as.Date("2013-07-01"),as.Date("2019-06-06"), by = "days")
Jahresferien <- data.frame(matrix(ncol = (length(LaenderUndEinwohner$Bundesland) + 1), nrow = length(Datum)))
colnames(Jahresferien) <- c("Datum", LaenderUndEinwohner$Bundesland)
Jahresferien$Datum <- Datum



#Alle benötigten Jahre als Vector aus dem Originaldatensatz
options(timeout= 4000000) #Bei langsamer Verbindung, Verbindungszeit erhöhen
umsatzdaten_ferien <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")
allYears <- unique(substr(umsatzdaten_ferien$Datum, 1, 4))
#Alle benötigten Jahre als Vector aus dem Originaldatensatz

#Anlegen der Verzeichnisse
#Prüfen, ob Verzeichnis bereits vorhanden oder nicht
  for(i in allYears){ #Wenn nicht, dann erstelle alle benötigte Verzeichnisse 
    create_folder <- paste("icals/",i,"/", sep = "") 
    if(!file.exists(substr(create_folder,1,nchar(create_folder)-1))){
    dir.create(create_folder, recursive = TRUE) #Erstellt für jedes Jahr ein Verzeichnis in "icals/"
    }
}
#Anlegen der Verzeichnisse





#Download aller benötigten .ics-Kalenderdateien in dem zuvor angelegten Verzeichnissen
for (year in allYears) {
  for (Bl in LaenderUndEinwohner$Bundesland) { #Für jedes Bundesland / Liste in LaenderUndEinwohner, Spalte Bundesländer als Vector
    filename <- paste("ferien_", Bl, "_", year, ".ics", sep = "")
    url <- paste("https://www.ferienwiki.de/exports/ferien/",year,"/de/", Bl, sep = "")
    filedest <- paste("icals/", year,"/", filename, sep = "")
    if(!file.exists(filedest)){
      curl::curl_download(url, filedest)
    }
  }
}
#Download aller benötigten .ics-Kalenderdateien in dem zuvor angelegten Verzeichnissen






df <- data.frame() #Anlegen eines neuen Dataframes, zum herausfiltern der Daten


#Lesen aller ics.-Kalenderdateien über die Jahre in das neue Dataframe df
for(i in allYears){
  this_folder <- paste("icals/",i,"/", sep = "") #Verzeichnis und Dateinamen
  files <- list.files(path=this_folder, pattern="*.ics", full.names=TRUE, recursive=FALSE) #Dateien jeweils als Vector
  #Kopieren der Daten in den Dataframe df
  for(j in files){
    x = readLines(j)
    df <- rbind(df,ic_dataframe(x))
  }
  #Kopieren der Daten in den Dataframe df
}
#Lesen aller ics.-Kalenderdateien über die Jahre in das neue Dataframe df



#Umbenennung der Spaltennamen
df <- rename(df,dtStart = "DTSTART;VALUE=DATE", dtEnde = "DTEND;VALUE=DATE", Bundesland = DESCRIPTION, FerienArt = SUMMARY)

#Hinzufügen einer Booleschen Erkennung
df$Ferien<-df$FerienArt #Hinzufügen einer Spalte für Booleschen Wert
df$Ferien <- str_replace_all(df$Ferien, "\\(?[a-zA-Z]+\\)?", "1") #Umwandlung in 1 / True
#Hinzufügen einer Booleschen Erkennung

#Anpassen der Strings ut-8
df$Bundesland <- str_replace_all(df$Bundesland, "Baden-WÃ¼rttemberg", "Baden-Wuerttemberg")
df$Bundesland <- str_replace_all(df$Bundesland, "ThÃ¼ringen", "Thueringen")
#Anpassen der Strings






#Anlegen eines neuen Dataframe zum Herausfiltern der wesentlichen Daten
dm <- data.frame()

#Sachsen und Sachsen-Anhalt müssen aufgrund der selben Erkennung von Strings umbenannt werden
df$Bundesland <- str_replace_all(df$Bundesland, "Sachsen-Anhalt", "gutschi")
df$Bundesland <- str_replace_all(df$Bundesland, "Sachsen", "glubschie")
#Sachsen und Sachsen-Anhalt müssen aufgrund der selben Erkennung von Strings umbenannt werden

#Laden aller Bundesländer in einen Vector für Schleifendurchlauf
bundesland <- c("Baden-Wuerttemberg","Bayern","Berlin","Brandenburg","Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "glubschie","gutschi", "Schleswig-Holstein", "Thueringen")

#Herausfiltern der Bundesländernamen aus dem Datensatz
for(i in bundesland){
  db <- filter(df, grepl(i, Bundesland))
  db$Bundesland <- i
  dm <- rbind(dm,db)
  remove(db)
}
#Herausfiltern der Bundesländernamen aus dem Datensatz

#Wiederherstellen von Sachsen und Sachsn-Anhalt
df$Bundesland <- str_replace_all(df$Bundesland, "gutschi", "Sachsen-Anhalt")
df$Bundesland <- str_replace_all(df$Bundesland, "glubschie", "Sachsen")
dm$Bundesland <- str_replace_all(dm$Bundesland, "gutschi", "Sachsen-Anhalt")
dm$Bundesland <- str_replace_all(dm$Bundesland, "glubschie", "Sachsen")
#Wiederherstellen von Sachsen und Sachsn-Anhalt


#Löschen der 1. und 2. nicht benötigten Spalten
dm <- subset (dm, select = -c(1,2))

#Erstellen eines neuen Dataframes zum Herausfiltern der Zwischen-Datumswerte
dc <- data.frame(matrix(ncol = 4, nrow = 0))

#Herausfiltern der Zwischen-Datumswerte
for(i in 1:nrow(dm)) {
  splitDate <- as.Date(as.Date(dm[i,1]):as.Date(dm[i,2]), origin="1970-01-01") #Gibt alle Datumsangaben zwischen Spalte1 und Spalte2 aus
  Bundesland <- dm[i,3]
  FerienArt <- dm[i,4]
  Ferien <- dm[i,5]
  dc <- rbind(dc,data.frame(splitDate,Bundesland,FerienArt,Ferien)) #Einbinden in das neue Dataframe
}
#Herausfiltern der Zwischen-Datumswerte


ferientage_bundeslaender <- data.frame(matrix(ncol = 4, nrow = 0))

ferientage_bundeslaender <- dc

remove(dc,df,dm,Jahresferien,umsatzdaten_ferien,allYears,Bl,bundesland,Bundesland,Datum,Ferien,FerienArt,filedest,filename,files,i,j,splitDate,this_folder,url,x,year,create_folder)
