#Falls csv bereits vorhanden, laden
if(file.exists("data/ferientage_bundeslaender_webseite.csv")){
  ferientage_bundeslaender <- read_csv("data/ferientage_bundeslaender_webseite.csv")
}else{
  dir.create("data/", recursive = TRUE)
#Importieren der Libaries
source("prep_environment.R")
#Importieren der Libaries
#Verbindungszeit erhöhen
options(timeout= 4000000)
#Laden der Projektdatei
umsatzdaten_ferien <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")
#Funktion zum html rausschneiden
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
#Herausfiltern aller Jahre im Datensatz:
#Nur das Jahr mit substr(x, start, stop)
#substr(umsatzdaten$Datum, start, stop)
#z.B. 2013-07-01  
#     12345678910
#substr(umsatzdaten$Datum, 1, 4)
allYears <- unique(substr(umsatzdaten_ferien$Datum, 1, 4))
#allYears <- c("2013")
#print(allYears)
#Ausgabe: [1] "2013" "2014" "2015" "2016" "2017" "2018" "2019"
#Erzeugt leeres Dataframe
ferientage <- data.frame(matrix(ncol = 4, nrow = 0))
#Alle Bundesländer als Vector
bundesland <- c("Baden-Wuerttemberg","Bayern","Berlin","Brandenburg","Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen","Sachsen-Anhalt", "Schleswig-Holstein", "Thueringen")

for(b in bundesland){#Durchläuft jedes Bundesland
  for(l in allYears){#Durchläuft je Bundesland das Jahr
    #Laden des html der Seite / aus https://www.ferienkalender.com
    site = paste("https://www.ferienkalender.com/ferien_deutschland/",b,"/",l,"-ferien-",tolower(b),".htm", sep = "")
    #Daten auf das jeweilige html Kästchen/Tag Filtern
    test = read_html(site)
    #print(site)
    test <- test %>% 
      html_nodes(".kasten") %>%
      html_text() %>% 
      .[1]
    #Daten auf das jeweilige html Kästchen/Tag Filtern
    
    #Herausfiltern und Ersetzen überflüssiger Zeichen
    test = str_replace_all(test,"\n", "") 
    b2 = str_replace_all(b,"ue","ü")
    test = str_replace_all(test, paste("Ferienkalender ",b2," ",l,"Ferien ",l," im deutschen Bundesland ",b2,":", sep = ""), "")
    test = str_replace_all(test," / Frühjahrsferien", "") 
    test = str_replace_all(test, paste("Ferien ",l," für ",b2, sep = ""), "") 
    test = str_replace_all(test,"Winterferien-","")
    test = str_replace_all(test,"Pfingstferien-","")
    
    #Überprüfung, ob ein "/" enthält. Wenn ja muss, ein Tag/Zeitraum hinzugefügt werden
    #Abfrage muss für jeden Abschnitt erfolgen
    pattern <- "Pfingstferien\\s*(.*?)\\s*Sommerferien"
    result <- regmatches(test, regexec(pattern, test))
    result <- result[[1]][2]
    #print(result)
    if(grepl("/", result, fixed = TRUE)){
      test = str_replace_all(test,result,str_replace_all(result,"/","Pfingstferien"))
    }
    
    pattern <- "Herbstferien\\s*(.*?)\\s*Weihnachtsferien"
    result <- regmatches(test, regexec(pattern, test))
    result <- result[[1]][2]
    #print(result)
    if(grepl("/", result, fixed = TRUE)){
      test = str_replace_all(test,result,str_replace_all(result,"/","Herbstferien"))
    }
    
    pattern <- "Sommerferien\\s*(.*?)\\s*Herbstferien"
    result <- regmatches(test, regexec(pattern, test))
    result <- result[[1]][2]
    #print(result)
    if(grepl("/", result, fixed = TRUE)){
      test = str_replace_all(test,result,str_replace_all(result,"/","Sommerferien"))
    }
    
    pattern <- "Osterferien\\s*(.*?)\\s*Pfingstferien"
    result <- regmatches(test, regexec(pattern, test))
    result <- result[[1]][2]
    #print(result)
    if(grepl("/", result, fixed = TRUE)){
      test = str_replace_all(test,result,str_replace_all(result,"/","Osterferien"))
    }
    
    #######################################
    #Herausfiltern und Ersetzen überflüssiger Zeichen
    
    #Alle Zahlen mit Punkt als Vector schreiben / Enthält die Datumsangaben der Ferientage
    test = str_replace_all(test," - ", "0000")
    numbers <- str_extract_all(test,"\\(?[0-9,.]+\\)?")[[1]]
    numbers <- paste(numbers, l, sep="")
    numbers = str_replace_all(numbers,"0000", paste(l," Y ", sep = ""))
    #Wird später für die Weihnachtsferien verwendet / geht über das Jahr hinaus / Jahr + 1
    year_add <- as.numeric(as.numeric(l)+1)

    #Alle Buchstaben als Vector schreiben / Enthält die Ferien-Bezeichnungen
    characters <- str_extract_all(test,"\\(?[a-zA-Z]+\\)?")[[1]] 

    #######################################
    
    k = 1;
    m = 1;
    
    for (j in numbers){
      Datum <- j 
      #Weihnachtsferien gehen über das betrachtete Jahr hinaus - Ersetzen des Jahres der zweiten Reihe mit +1 Jahr
      if(characters[m]=="Weihnachtsferien"){
        Datum = str_sub(Datum,end = -5)
        Datum = paste(Datum, year_add, sep = "")
        #print(Datum)
        #break
      }
    
      #Datum wird zu sequenzierung vorbereitet
      Datum <- strsplit(Datum, split = " Y ") #Ein Y zur Trennung der Daten für besseres separieren
      Datum <-unlist(Datum, use.names=FALSE); #Liste umwandeln in ein Vektor
      Datum <- as.Date(Datum, "%d.%m.%Y") #Deutsches Datumsformat in das englische umwandeln
      #Jeweilige Bezeichnung wird in den Vektor geschrieben
      Ferien <- characters[k]
      #Gibt alle Tage/Datumsangaben zwischen den beiden Datumsangaben aus und schreiben es in den Vektor
      if(length(Datum) > 1){
        Datum <- as.Date(as.Date(Datum[1]):as.Date(Datum[2]), origin="1970-01-01")
      }
      Bundesland <- b
      FerienArt <- Ferien
      #Nachdem die Vektoren vervollständigt wurden, werden sie in das Dataframe gespeichert
      ferientage <- rbind(ferientage,data.frame(Datum,FerienArt,Ferien,Bundesland))
      
      k = k + 1;   
      m = m + 1;   
    }
  }
  #nicht mehr benötigte Variablen löschen
  remove(characters,Datum,j,k,m,Ferien,numbers,site,test,year_add)
}
#nicht mehr benötigte Variablen löschen
remove(allYears,characters,Datum,j,k,l,m,Ferien,numbers,site,test,year_add,umsatzdaten_ferien,b)
#Das fertige Dataframe kann hier in Bollesche Werte umgewandelt werden
ferientage$Ferien <- str_replace_all(ferientage$Ferien, "\\(?[a-zA-Z]+\\)?", "1")
#Daten, die über das die benötigte Zeit hinaus gehen können gelöscht werden
ferientage_bundeslaender <- filter(ferientage, Datum <= "2019-12-31")
#nicht mehr benötigte Variablen löschen
remove(ferientage)
#Falls nicht vorhanden, um erneutes laden zu vermeiden, als csv speichern
write.csv(ferientage_bundeslaender,"data/ferientage_bundeslaender_webseite.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")
}