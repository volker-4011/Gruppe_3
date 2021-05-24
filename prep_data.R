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
    wetter_dwd_source = "wetter_dwd.csv" #Nur Local
    
    #Laden der Dateien##################
    umsatzdaten <- read_csv(umsatzdaten_source)
    wetter <- read_csv(wetter_source)
    kiwo <- read_csv(kiwo_source)
    wetter_dwd <- read_delim(wetter_dwd_source, delim = ";")
    #ferientage <- read_csv("ferientage.csv")
    #feiertage <- read_csv("feiertage.csv")
    
    source("ferientage.R", encoding = "UTF-8")
    source("feiertage.R", encoding = "UTF-8")
    
    #Macht Fehler beim Laden der Daten/ Ursache nicht gefunden. Die einzelnen Dateien laden richtig
    #source("feiertage.R")
    #source("ferientage.R")
    #library(memisc)
    #source("feiertage.R")
    #include("ferientage.R",warn=FALSE)
    ####################################
    #Bearbeiten der Daten###############
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
    
    ####################################
    

    #Zusammensetzen der Daten
    fullData <- merge(umsatzdaten,wetter, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,kiwo, by="Datum", all.x = TRUE)
    #fullData <- merge(fullData,feiertage, by="Datum", all.x = TRUE)
    #fullData <- merge(fullData,ferientage, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,wetter_dwd, by="Datum", all.x = TRUE)
    #fullData <- merge(x=fullData, y=wetter_dwd, by.x="Datum", by.y="MESS_DATUM", x.all=FALSE, y.all=FALSE)
    fullData <- merge(fullData,ferientage, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,feiertage, by="Datum", all.x = TRUE)
    #########################
    
    
    #Vorschlag: Optimierung der Daten
    
    fullData$Umsatz <- round(fullData$Umsatz) #Dezimalstellen wenig sinvoll, besser Runden
    fullData$KielerWoche[is.na(fullData$KielerWoche)] <- 0 #Entweder Kieler-Woche oder "nicht = 0"
    fullData$Ferien[is.na(fullData$Ferien)] <- 0 #Entweder Ferien oder "nicht = 0"
    fullData$Feiertag[is.na(fullData$Feiertag)] <- 0 #Entweder Feiertag oder "nicht = 0"
    
    #Vorschlag: Optimierung der Daten 

  ####################################################################################################################################