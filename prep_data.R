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
    fullData$Windchill <- with(fullData, 13.12+0.6215*Temperatur+(0.3965*Temperatur-11.37)*Windgeschwindigkeit^0.16)
    
    # Warengruppennummer in Warengruppenname uebersetzen
    # 1=Brot, 2=Broetchen, 3=Crossaint, 4=Konditorei, 5=Kuchen, 6=Saisonbrot
     Warengruppen <- c("Brot", "Broetchen", "Crossaint", "Konditorei", "Kuchen", "Saisonbrot")
     for (e in as.numeric(row.names(fullData)))
       fullData$Warengruppe[e] <- Warengruppen[as.numeric(fullData$Warengruppe[e])]
    
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
    
    #Variablen vom chr in num wandeln
    fullData$Ferien <- as.numeric(fullData$Ferien)
    fullData$Feiertag <- as.numeric(fullData$Feiertag)
    
    
    #Daten in ein CSV speichern
    

    #########################
    
    ###Vorbereiten des Dataframe für die Vorhersage
    
    #Dummy Encoden der Variablen für die Vorhersage
    
    dummy_list <- c("Monat", "Wochentag", "Warengruppe" , "Bewoelkung")
    fullData_dummy = dummy_cols(fullData, dummy_list)
    
    
    
    #Dataframe für nächsten Tag erstellen (für die Vorhersage)  
    
    newData <- rbind(fullData_dummy[fullData_dummy$Datum == d, ])
    
    #Löschen der Daten für den Tag, der vorhergesagt werden soll aus den Trainingsdaten
    fullData_dummy <- subset(fullData_dummy, Datum != d)
    
    # alle NAs durch 0 ersetzen, damit die svm läuft
    # ist nicht die feine Art, wir müssen uns nochmal genauer um die NAs kümmern.
    fullData_dummy[is.na(fullData_dummy)] <- 0
    newData[is.na(newData)] <- 0
    
  
########################Für Python/Tenser    
    writecsvData <- fullData
    writecsvData <- cbind(ID = 1:nrow(writecsvData), writecsvData)
    
    write.csv(writecsvData,"fullData.csv", append = FALSE, quote = TRUE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"),
              fileEncoding = "")

  ####################################################################################################################################