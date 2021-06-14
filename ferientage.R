#Falls csv bereits vorhanden, laden. Wenn nicht dann Skript ausführen
if(file.exists("data/ferientage_SH.csv")){
  ferientage <- read_csv("data/ferientage_SH.csv")
}else{
  dir.create("data/", recursive = TRUE)
#Importieren der Libaries
source("prep_environment.R")
#Importieren der Libaries
#Ladezeit erhöhen bei schlechter Verbindung
options(timeout= 4000000)
#Laden der Umsatzdaten csv, um die benötigten Jahre zu ermitteln
umsatzdaten_ferien <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")
#Alternative Funktion zum herausfiltern unnötiger html Zeichen
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
#print(allYears)
#Ausgabe: [1] "2013" "2014" "2015" "2016" "2017" "2018" "2019"
#Leeres Dataframe erstellen für die Ferientage
ferientage <- data.frame(matrix(ncol = 2, nrow = 0))
#For-Schleife zum Füllen der Daten aus der Webseite für alle Jahre
for(l in allYears){
  #Ersetzen der URl für jedes Jahr
  site = paste("https://www.ferienkalender.com/ferien_deutschland/Schleswig-Holstein/",l,"-ferien-schleswig-holstein.htm", sep = "")
  #Funktion zum lesen des html.codes der Webseite
  test = read_html(site)
  #Selektieren des html-Kastens, die alle benötigten Daten enthält
  test <- test %>% 
    html_nodes(".kasten") %>%
    html_text() %>% 
    .[1]
  #Hier werden, schrittweise um Fehler zu vermeiden, der übrige Text auf das wesentliche geschnitten
  test = str_replace_all(test,"\n", "") 
  test = str_replace_all(test, paste("Ferienkalender Schleswig-Holstein ",l,"Ferien ",l," im deutschen Bundesland Schleswig-Holstein:Winterferien-", sep = ""), "")
  test = str_replace_all(test," / Frühjahrsferien", "") 
  test = str_replace_all(test, paste("Ferien ",l," für Schleswig-Holstein", sep = ""), "") 
  #######################################
  test = str_replace_all(test," - ", "0000") 
  numbers <- str_extract_all(test,"\\(?[0-9,.]+\\)?")[[1]]
  numbers <- paste(numbers, l, sep="")
  numbers = str_replace_all(numbers,"0000", paste(l," Y ", sep = ""))
  #Da Ferientage im Jahr 2019 in 2020 hinübergehen, wird hier 2020 für die nächste Schleife definiert (Letztes Jahr +1)
  year_add <- as.numeric(as.numeric(l)+1)
  #######################################
  #######################################
  #Trennung zwischen Bezeichnungen mit Buchstaben und Zahlen mit den Datumsangaben
  characters <- str_extract_all(test,"\\(?[a-zA-Z]+\\)?")[[1]] 
  #######################################
  #Schleife, um number(Datum) und character(Bezeichnungen) in ein Dataframe zu schreiben
  #Laufvariablen
  k = 1;
  m = 1;
  for (j in numbers){
    Datum <- j 
    #Bei 2020 überschneiden sich die Ferientage, es muss geprüft werden, ob es sich um ein Datum im Jahre 2020 handelt und dementsprechen angepasst werden
    if(m==5){
      Datum = str_sub(Datum,end = -5)
      Datum = paste(Datum, year_add, sep = "")
    }
    #Ein Y zur Trennung der Daten für besseres separieren
    Datum <- strsplit(Datum, split = " Y ") 
    #Liste umwandeln in ein Vektor
    Datum <-unlist(Datum, use.names=FALSE);
    #Deutsches Datumsformat in das englische umwandeln
    Datum <- as.Date(Datum, "%d.%m.%Y")  
    #Jeweilige Bezeichnung wird in den Vektor geschrieben
    Ferien <- characters[k]
    #Gibt alle Tage/Datumsangaben zwischen den beiden Datumsangaben aus und schreiben es in den Vektor
    if(length(Datum) > 1){
      Datum <- as.Date(as.Date(Datum[1]):as.Date(Datum[2]), origin="1970-01-01")
    }
    #Nachdem die Vektoren vervollständigt wurden, werden sie in das Dataframe gespeichert
    ferientage <- rbind(ferientage,data.frame(Datum,Ferien))
    #Laufvariablen
    k = k + 1;
    m = m + 1;  
  }
}
#nicht mehr benötigte Variablen löschen
remove(allYears,characters,Datum,j,k,l,m,Ferien,numbers,site,test,year_add,umsatzdaten_ferien)
#Das fertige Dataframe kann hier in Bollesche Werte umgewandelt werden
ferientage$Ferien <- str_replace_all(ferientage$Ferien, "\\(?[a-zA-Z]+\\)?", "1")
#Daten, die über das die benötigte Zeit hinaus gehen können gelöscht werden
ferientage <- filter(ferientage, Datum <= "2019-12-31")
#Falls nicht vorhanden, um erneutes laden zu vermeiden, als csv speichern
write.csv(ferientage,"data/ferientage_SH.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")
}