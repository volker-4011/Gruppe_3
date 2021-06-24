#Import aller benötigten Labraries
source("prep_environment.R")
##################################

#Import aller zur Verfügung stehenden Daten########################################################################################
#Längere Verbindung, für das Laden / vermeidet Fehler
options(timeout= 4000000) 
#####################################################
#Verweise der Daten
umsatzdaten_source = "https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv"
wetter_source = "https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/wetter.csv"
kiwo_source = "https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/kiwo.csv"
wetter_dwd_source = "https://raw.githubusercontent.com/volker-4011/Gruppe_3/main/wetter_dwd.csv" #Nur Local

#Laden der Dateien##################
umsatzdaten <- read_csv(umsatzdaten_source)
wetter <- read_csv(wetter_source)
kiwo <- read_csv(kiwo_source)
wetter_dwd <- read_delim(wetter_dwd_source, delim = ";")
#Laden der Ferien und Feiertage aus den jeweiligen Dateien
source("https://raw.githubusercontent.com/volker-4011/Gruppe_3/main/ferientage.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/volker-4011/Gruppe_3/main/feiertage.R", encoding = "UTF-8")

#####################################################

#Bearbeiten von wetter_dwd
wetter_dwd$MESS_DATUM <- as.Date(wetter_dwd$MESS_DATUM, "%d.%m.%Y")
wetter_dwd <- dplyr::rename(wetter_dwd, Datum = MESS_DATUM)
wetter_dwd[wetter_dwd==-999.000] <- NA
# Hinzufügen Dataframe wetter_dwd. Vorher entfernen der nicht benötigten Spalten
wetter_dwd[ , c('Windspitze',
                'Windgeschwindigkeit',
                'STATIONS_ID',
                'QN_3',
                'QN_4',
                'Niederschlagsform',
                'Schneehoehe',
                'Bedeckungsgrad',
                'Dampfdruck',
                'Luftdruck',
                'Temperatur',
                'Max-Temperatur',
                'Min_Temperatur',
                'Min_Temperatur_Boden',
                'eor'
)] <- list(NULL)

#Konvertieren der hinzugefügten Spalten aus wetter_dwd von "Character" zu "numeric"
wetter_dwd$Sonnenscheindauer <- as.numeric(wetter_dwd$Sonnenscheindauer)
wetter_dwd$Relative_Feuchte <- as.numeric(wetter_dwd$Relative_Feuchte)
wetter_dwd$Niederschlagsmenge <- as.numeric(wetter_dwd$Niederschlagsmenge)




####################################
####Ausreißer löschen (Silvester und Heiligabend)
#Für Silvester und Heiligabend in Spalte "helper" eintragen
umsatzdaten$helper <- 0
for(i in 1:nrow(umsatzdaten)){
  if((format(umsatzdaten$Datum[i], "%d/%m") == "24/12") | (format(umsatzdaten$Datum[i], "%d/%m") == "31/12")){
    umsatzdaten$helper[i] <- 1
  } else{
    umsatzdaten$helper[i] <- 0
  }
}
#Silvester und Heiligabend löschen, anschließend Spalte "helper" löschen
umsatzdaten <- umsatzdaten[umsatzdaten$helper==0, ]
umsatzdaten$helper <- NULL

###Vorbereitung für die Vorhersage: Zeilen für alle 6 Warengruppen hinzufügen für den ersten Tag, für den keine Umsatzdaten vorhanden sind
newDay <- max(umsatzdaten$Datum)+1
for(i in 1:6){
  umsatzdaten <- add_row(umsatzdaten, Datum = newDay, Warengruppe = i)
}


#####################################################
# Import of overnight stay data
#The required data is saved in the form of *.xlsx sheets at https://www.statistik-nord.de/fileadmin/Dokumente/Statistische_Berichte/industrie__handel_und_dienstl/G_IV_1_m_S/G_IV_1-m1506_SH.xlsx 
#in this case for the month of June of the year 2015 indicated by "1506". In theory only this four digit code changes. For 78 of the 84 months
#that we are interested in, this statement is true. More about that later. The conclusion drawn from this means: First we need to create a vector
#containing all four digit "month-year-codes" for the desired months.

#Create vector of all month numbers in double digits
#months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#create vector of all years of "umsatzdaten"
#years <- as.character(13:19)


#create vector "JahrMonat" from vectors "months" and "years" containing all combinations of "years" and "months"
#for (e in years)
#  if (e == "13") {
#    JahrMonat <- paste(e, months, sep = "")
#  } else {
#    a <- paste(e, months, sep = "")
#    JahrMonat <- c(JahrMonat, a)
#  }

#This for-loop iterates through every element of the vector "JahrMonat" pasting it into the URL. In every iteration it therefore downloads the
#next *.xlsx sheet. This statement is true for most of the sheets. Unfortunately for 7 of the 84 sheets the person overseeing the upload has
#made some typos and thereby almost made me loose my sanity bcs I had to figure out the exact typos made for each download error. So for 77 
#of the 84 #cases only the lines 91-95 and 116-122 are needed. Lines 96-115 are only needed to catch the typo-sheets ¯\_(ツ)_/¯

#Uebernachtungen <- vector()
#for (e in JahrMonat) {
#  filename <- paste("G_IV_1-m",e,"_SH.xlsx", sep = "")
#  url <- paste(
#    "https://www.statistik-nord.de/fileadmin/Dokumente/Statistische_Berichte/industrie__handel_und_dienstl/G_IV_1_m_S/", filename, sep = "")
#  if (url.exists(url=url)) {
#    filedest <- paste("sheets/", filename, sep = "")
#    curl::curl_download(url, filedest)
#    xls <- read_excel(filedest, sheet = "T1_1")
#    Uebernachtungen <- c(Uebernachtungen, xls[4][xls[1] == "02 Kiel"])
#  } else {
#    filename <- paste("G_IV_1-m",e,"_SH-.xlsx", sep = "")
#    url <- paste(
#      "https://www.statistik-nord.de/fileadmin/Dokumente/Statistische_Berichte/industrie__handel_und_dienstl/G_IV_1_m_S/", filename, sep="")
#    if (!url.exists(url=url)) {
#      filename <- paste("G_IV_1_m_S_",e,".xlsx", sep = "")
#      url <- paste(
#        "https://www.statistik-nord.de/fileadmin/Dokumente/Statistische_Berichte/industrie__handel_und_dienstl/G_IV_1_m_S/", filename, sep = "")
#      if (!url.exists(url=url)) {
#        filename <- paste("G_IV_1_m",e,"_SH.xlsx", sep = "")
#        url <- paste(
#          "https://www.statistik-nord.de/fileadmin/Dokumente/Statistische_Berichte/industrie__handel_und_dienstl/G_IV_1_m_S/", filename, sep = "")
#      }
#    }
#    #declare file destination to be inside folder "sheets"
#    filedest <- paste("sheets/", filename, sep = "")
#    #download file from "url" into "filedest"
#    curl::curl_download(url, filedest)
#import only sheet "T1_1" from file "filename" into variable xls
#    xls <- read_excel(filedest, sheet = "T1_1")
#extract only overnight stays for kiel from xls and concatenate it with the former vector "Uebernachtungen"
#   Uebernachtungen <- c(Uebernachtungen, xls[4][xls[1] == "02 Kiel"])
#  }
#}
#create common dataframe for "Uebernachtungen" and "JahrMonat"
#Uebernachtungen <- data.frame("Monatscode"=JahrMonat, "Uebernachtungen"=Uebernachtungen)

####################################



###Zusammensetzen der Daten
fullData <- merge(umsatzdaten,wetter, by="Datum", all.x = TRUE)
fullData <- merge(fullData,kiwo, by="Datum", all.x = TRUE)
fullData <- merge(fullData,wetter_dwd, by="Datum", all.x = TRUE)
fullData <- merge(fullData,ferientage, by="Datum", all.x = TRUE)
fullData <- merge(fullData,feiertage, by="Datum", all.x = TRUE)
#################################################################

##### Lineare Interpolation zum Ersetzen von NAs
# Eine möglichkeit zur linearen Interpolation. Schreiben einer Funktion hat hier nicht funktioniert
# daycurrent_value = dayone_value + ((daytwo_value - dayone_value) * ((daycurrent_utc - dayone_utc)/(daytwo_utc - dayone_utc)))'

## Lineare Interpolation Bewölkung
dayone_utc = 0
daytwo_utc = 0
daycurrent_utc = 0

dayone = 0
daytwo = 0
daycurrent = 0

dayone_value = 0
daytwo_value = 0
daycurrent_value = 0  

for (i in as.numeric(row.names(fullData))){
  
  if(is.na(fullData$Bewoelkung[i])){
    
    dayone <- max(fullData$Datum[i])-1
    daytwo <- max(fullData$Datum[i])+1
    
    dayone_value = max(fullData$Bewoelkung[i-1])
    
    daytwo_value = NA
    j = 1
    while (is.na(daytwo_value)) {   
      daytwo_value = max(fullData$Bewoelkung[i+j])
      j = j + 1
    } 
    
    dayone_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])-1, format="%Y-%m-%d"))
    daytwo_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])+1, format="%Y-%m-%d"))
    daycurrent_utc = as.numeric(as.POSIXct(max(fullData$Datum[i]), format="%Y-%m-%d"))
    
    daycurrent_value = dayone_value + ((daytwo_value - dayone_value) * ((daycurrent_utc - dayone_utc)/(daytwo_utc - dayone_utc)))
    
    
    fullData$Bewoelkung[i] <- round(daycurrent_value)
    
  }
}

####
## Lineare Interpolation Temperatur
dayone_utc = 0
daytwo_utc = 0
daycurrent_utc = 0

dayone = 0
daytwo = 0
daycurrent = 0

dayone_value = 0
daytwo_value = 0
daycurrent_value = 0  

for (i in as.numeric(row.names(fullData))){
  
  if(is.na(fullData$Temperatur[i])){
    
    dayone <- max(fullData$Datum[i])-1
    daytwo <- max(fullData$Datum[i])+1
    
    dayone_value = max(fullData$Temperatur[i-1])
    
    daytwo_value = NA
    j = 1
    while (is.na(daytwo_value)) {   
      daytwo_value = max(fullData$Temperatur[i+j])
      j = j + 1
    } 
    
    dayone_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])-1, format="%Y-%m-%d"))
    daytwo_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])+1, format="%Y-%m-%d"))
    daycurrent_utc = as.numeric(as.POSIXct(max(fullData$Datum[i]), format="%Y-%m-%d"))
    
    daycurrent_value = dayone_value + ((daytwo_value - dayone_value) * ((daycurrent_utc - dayone_utc)/(daytwo_utc - dayone_utc)))
    
    
    fullData$Temperatur[i] <- daycurrent_value
    
    
  }
}

####
## Lineare Interpolation Windgeschwindigkeit
dayone_utc = 0
daytwo_utc = 0
daycurrent_utc = 0

dayone = 0
daytwo = 0
daycurrent = 0

dayone_value = 0
daytwo_value = 0
daycurrent_value = 0  

for (i in as.numeric(row.names(fullData))){
  
  if(is.na(fullData$Windgeschwindigkeit[i])){
    
    dayone <- max(fullData$Datum[i])-1
    daytwo <- max(fullData$Datum[i])+1
    
    dayone_value = max(fullData$Windgeschwindigkeit[i-1])
    
    daytwo_value = NA
    j = 1
    while (is.na(daytwo_value)) {   
      daytwo_value = max(fullData$Windgeschwindigkeit[i+j])
      j = j + 1
    } 
    
    dayone_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])-1, format="%Y-%m-%d"))
    daytwo_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])+1, format="%Y-%m-%d"))
    daycurrent_utc = as.numeric(as.POSIXct(max(fullData$Datum[i]), format="%Y-%m-%d"))
    
    daycurrent_value = dayone_value + ((daytwo_value - dayone_value) * ((daycurrent_utc - dayone_utc)/(daytwo_utc - dayone_utc)))
    
    
    fullData$Windgeschwindigkeit[i] <- daycurrent_value
    
  }
}

####
## Lineare Interpolation Niederschlagsmenge
dayone_utc = 0
daytwo_utc = 0
daycurrent_utc = 0

dayone = 0
daytwo = 0
daycurrent = 0

dayone_value = 0
daytwo_value = 0
daycurrent_value = 0  

for (i in as.numeric(row.names(fullData))){
  
  if(is.na(fullData$Niederschlagsmenge[i])){
    
    dayone <- max(fullData$Datum[i])-1
    daytwo <- max(fullData$Datum[i])+1
    
    dayone_value = max(fullData$Niederschlagsmenge[i-1])
    
    daytwo_value = NA
    j = 1
    while (is.na(daytwo_value)) {   
      daytwo_value = max(fullData$Niederschlagsmenge[i+j])
      j = j + 1
    } 
    
    dayone_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])-1, format="%Y-%m-%d"))
    daytwo_utc = as.numeric(as.POSIXct(max(fullData$Datum[i])+1, format="%Y-%m-%d"))
    daycurrent_utc = as.numeric(as.POSIXct(max(fullData$Datum[i]), format="%Y-%m-%d"))
    
    daycurrent_value = dayone_value + ((daytwo_value - dayone_value) * ((daycurrent_utc - dayone_utc)/(daytwo_utc - dayone_utc)))
    
    
    fullData$Niederschlagsmenge[i] <- daycurrent_value
    
  }
}

#Aufteilung in Klassen
##################Niederschlagsmenge Kategorisieren
#Kategorisierung nach eigenem ermessen
fullData$Niederschlagsmenge[is.na(fullData$Niederschlagsmenge)] <- as.numeric(0) #0 als Standardwert für nicht vorhandene Daten
for (i in as.numeric(row.names(fullData))){
  checkregen <- as.numeric(fullData$Niederschlagsmenge[i])
  
  if(checkregen <= as.numeric(8)){fullData$Niederschlagsmenge[i] <- 1}
  else if(checkregen > as.numeric(8) && checkregen <= as.numeric(16)){fullData$Niederschlagsmenge[i] <- 2}
  else if(checkregen > as.numeric(16) && checkregen <= as.numeric(24)){fullData$Niederschlagsmenge[i] <- 3}
  else if(checkregen > as.numeric(24) && checkregen <= as.numeric(32)){fullData$Niederschlagsmenge[i] <- 4}
  else if(checkregen > as.numeric(32) && checkregen <= as.numeric(40)){fullData$Niederschlagsmenge[i] <- 5}
} 
##################Niederschlagsmenge Kategorisieren


##################Windgeschwindigkeit Kategorisieren
#Kategorisierung nach https://www.wind-turbine-models.com/winds
fullData$Windgeschwindigkeit[is.na(fullData$Windgeschwindigkeit)] <- as.numeric(3) #3 als Standardwert für nicht vorhandene Daten

for (i in as.numeric(row.names(fullData))){
  checkWind <- as.numeric(fullData$Windgeschwindigkeit[i])
  
  if(checkWind <= as.numeric(5)){fullData$Windgeschwindigkeit[i] <- "Windstille"}
  else if(checkWind > as.numeric(5) && checkWind <= as.numeric(11)){fullData$Windgeschwindigkeit[i] <- "leichte_Briese"}
  else if(checkWind > as.numeric(11) && checkWind <= as.numeric(19)){fullData$Windgeschwindigkeit[i] <- "schwache_Briese"}
  else if(checkWind > as.numeric(19) && checkWind <= as.numeric(28)){fullData$Windgeschwindigkeit[i] <- "maessige_Briese"}
  else if(checkWind > as.numeric(28) && checkWind <= as.numeric(38)){fullData$Windgeschwindigkeit[i] <- "frische_Briese"}
  else if(checkWind > as.numeric(38) && checkWind <= as.numeric(49)){fullData$Windgeschwindigkeit[i] <- "starker_Wind"}
} 
##################Windgeschwindigkeit Kategorisieren


##################Temperatur Kategorisieren
#Kategorisierung nach Wikipedia
fullData$Temperatur <- round(fullData$Temperatur) #3 als Standardwert für nicht vorhandene Daten

for (i in as.numeric(row.names(fullData))){
  checkTemp <- as.numeric(fullData$Temperatur[i])
  
  if(checkTemp <= as.numeric(0)){fullData$Temperatur[i] <- 1} #"Eisig"
  else if(checkTemp > as.numeric(0) && checkTemp <= as.numeric(5)){fullData$Temperatur[i] <- 2} #"Kalter_Tag"
  else if(checkTemp > as.numeric(5) && checkTemp <= as.numeric(12)){fullData$Temperatur[i] <- 3} #"Vegetationstag"
  else if(checkTemp > as.numeric(12) && checkTemp <= as.numeric(20)){fullData$Temperatur[i] <- 4} #"Fruehling"
  else if(checkTemp > as.numeric(20) && checkTemp <= as.numeric(30)){fullData$Temperatur[i] <- 5} #Sommertag"
  else if(checkTemp > as.numeric(30)){fullData$Temperatur[i] <- 6} #"Heisser_Tag"
}
##################Temperatur Kategorisieren

####################################################################################################################
#merge over night stays with "fullData"
#create column of "Monatscode" for each date of "fullData" to full_join() by "Monatscode"
#for (e in as.numeric(row.names(fullData))) {
#  fullData$Monatscode[e] <- paste(
#    (year(fullData$Datum[e])-2000),
#    formatC(month(fullData$Datum[e]), width = 2, format = "d", flag = "0"), sep = "")
#}
#fullData <- full_join(fullData, Uebernachtungen, by = "Monatscode")
#########################

###Optimierung der Daten/ Hinzufügen neuer Variablen

#Windchillfaktor berechnen und hinzufügen (gefühlte Temperatur für unteren Temperaturbereich)
# fullData$Windchill <- with(fullData, 13.12+0.6215*Temperatur+(0.3965*Temperatur-11.37)*Windgeschwindigkeit^0.16)
####################################################################################################################

# Warengruppennummer in Warengruppenname uebersetzen
# 1=Brot, 2=Broetchen, 3=Crossaint, 4=Konditorei, 5=Kuchen, 6=Saisonbrot
Warengruppen <- c("Brot", "Broetchen", "Crossaint", "Konditorei", "Kuchen", "Saisonbrot")
for (e in as.numeric(row.names(fullData)))
  fullData$Warengruppe[e] <- Warengruppen[as.numeric(fullData$Warengruppe[e])]

#Extrahieren des Wochentags aus dem Datum und speichern in neuer Variablen
fullData$Wochentag <- weekdays(fullData$Datum)

#Extrahieren des Monats aus dem Datum und speichern als neue Variablen
fullData$Monat <- month(fullData$Datum)

#Extrahieren des Jahres aus dem Datum und speichern als neue Variablen
fullData$Jahr <- format(fullData$Datum, "%Y")

#Hinzufügen der neuen Variable Wochenende
fullData$Wochenende <- with(fullData, ifelse(Wochentag=="Sonntag" | Wochentag=="Samstag", 1, 0))

#Dezimalstellen wenig sinvoll, besser Runden
fullData$Umsatz <- round(fullData$Umsatz)

#Alle NA durch 0 ersetzen in Boolschen Variablen / Bei JA/NEIN Variablen problemlos
fullData$KielerWoche[is.na(fullData$KielerWoche)] <- 0 # Alle NA in KielerWoche durch 0 ersetzen
fullData$Ferien[is.na(fullData$Ferien)] <- 0 #Entweder Ferien oder "nicht = 0"
fullData$Feiertag[is.na(fullData$Feiertag)] <- 0 #Entweder Feiertag oder "nicht = 0"

#Variablen vom chr in num wandeln
fullData$Ferien <- as.numeric(fullData$Ferien)
fullData$Feiertag <- as.numeric(fullData$Feiertag)

###Einfügen der Variable Umsatz_naiv

#Überprüfe, ob Umsatz von vor 7 Tagen für die jeweilige Warengruppe vorhanden, wenn ja --> Umsatz vor 7 Tagen = Umsatzu_naiv, wenn nein --> aktueller Umsatz = Umsatz_naiv
for(i in 1:nrow(fullData)){
  d_naiv <- fullData$Datum[i]-7
  w6 <- any(fullData$Datum == d_naiv & fullData$Warengruppe == fullData$Warengruppe[i])
  if(w6 == TRUE) {
    fullData$Umsatz_naiv[i] <- fullData$Umsatz[fullData$Datum == d_naiv & fullData$Warengruppe == fullData$Warengruppe[i]]
  } else {
    fullData$Umsatz_naiv[i] <- fullData$Umsatz[i] 
  }
}


###Mittlerer Umsatz pro Monat und Warengruppe
mean_umsatz <- aggregate(fullData[1:(nrow(fullData)-6), 3], list(fullData$Jahr[1:(nrow(fullData)-6)], fullData$Monat[1:(nrow(fullData)-6)], fullData$Warengruppe[1:(nrow(fullData)-6)]), mean)
fullData$Umsatz_mean <- 0

#Für jede Zeile Variable füllen
for(i in 1:nrow(fullData)-6){
  fullData$Umsatz_mean[i] <- mean_umsatz$x[mean_umsatz$Group.1 == fullData$Jahr[i] & mean_umsatz$Group.2 == fullData$Monat[i] & mean_umsatz$Group.3 == fullData$Warengruppe[i]]
}

#########################

###Vorbereiten des Dataframe für die Vorhersage

#Dummy Encoden der Variablen für die Vorhersage
dummy_list <- c("Monat", "Wochentag", "Warengruppe" , "Bewoelkung", "Windgeschwindigkeit", "Niederschlagsmenge", "Jahr", "Temperatur")
fullData_dummy = dummy_cols(fullData, dummy_list)


#Dataframe für nächsten Tag erstellen (für die Vorhersage)  

newData <- rbind(fullData_dummy[fullData_dummy$Datum == newDay, ])

########################Für Python/Tenser NNE
#eine csv Datei erstellen, zum schnelleren Laden der Variablen
unlink("fullData.csv")
writecsvData <- fullData
writecsvData <- cbind(ID = 1:nrow(writecsvData), writecsvData)

write.csv(writecsvData,"fullData.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")
########################Für Python/Tenser NNE

#Löschen der Daten für den Tag, der vorhergesagt werden soll aus den Trainingsdaten
fullData <- subset(fullData, Datum != newDay)
fullData_dummy <- subset(fullData_dummy, Datum != newDay)

# alle NAs durch 0 ersetzen, damit die svm läuft
# NAs sind nur in Variablen vorhanden, die nicht zur Vorhersage genutzt werden. NAs in benötigten Variablen, wurden zuvor durch lineare Interpolation bereinigt.
fullData_dummy[is.na(fullData_dummy)] <- 0
newData[is.na(newData)] <- 0
####################################################################################################################################