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
    #ferientage <- read_csv("ferientage.csv")
    #feiertage <- read_csv("feiertage.csv")
    
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

    ###Vorbereitung für die Vorhersage: Zeilen für alle 6 Warengruppen hinzufügen für den ersten Tag, für den keine Umsatzdaten vorhanden sind
    d <- max(umsatzdaten$Datum)+1
    for(i in 1:6){
      umsatzdaten <- add_row(umsatzdaten, Datum = d, Warengruppe = i)
    }
    
    ####################################
    
    ###Zusammensetzen der Daten
    fullData <- merge(umsatzdaten,wetter, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,kiwo, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,wetter_dwd, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,ferientage, by="Datum", all.x = TRUE)
    fullData <- merge(fullData,feiertage, by="Datum", all.x = TRUE)
    #########################
    
    ###Optimierung der Daten/ Hinzufügen neuer Variablen
    
    #Windchillfaktor berechnen und hinzufügen (gefühlte Temperatur für unteren Temperaturbereich)
    fullData$Windchill <- with(fullData, 13.12+0.6215*Temperatur+(0.3965*Temperatur-11.37)*Windgeschwindigkeit^0.16)
    
    #Extrahieren des Wochentags aus dem Datum und speichern in neuer Variablen
    fullData$Wochentag <- weekdays(fullData$Datum)
    
    #Extrahieren des Monats aus dem Datum und speichern in neuer Variablen
    fullData$Monat <- month(fullData$Datum)
    
    #Hinzufügen neue Variable Wochenende
    fullData$Wochenende <- with(fullData, ifelse(Wochentag=="Sonntag" | Wochentag=="Samstag", 1, 0))
    
    #Dezimalstellen wenig sinvoll, besser Runden
    fullData$Umsatz <- round(fullData$Umsatz)
    
    #Alle NA durch 0 ersetzen in Boolschen Variablen
    fullData$KielerWoche[is.na(fullData$KielerWoche)] <- 0 # Alle NA in KielerWoche durch 0 ersetzen
    fullData$Ferien[is.na(fullData$Ferien)] <- 0 #Entweder Ferien oder "nicht = 0"
    fullData$Feiertag[is.na(fullData$Feiertag)] <- 0 #Entweder Feiertag oder "nicht = 0"
    
    #########################
    
    #Dataframe für nächsten Tag erstellen (für die Vorhersage)  
    
    #Abspeichern der Daten für den Tag, der vorhergesagt werden soll in einem neuen Dataframe
    newData <- fullData[fullData$Datum == d, ]
    
    #Löschen der Daten für den Tag, der vorhergesagt werden soll aus den Trainingsdaten
    fullData <- subset(fullData, Datum != d)

  ####################################################################################################################################