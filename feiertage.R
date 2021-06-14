#Falls csv bereits vorhanden, laden. Wenn nicht dann Skript ausführen
if(file.exists("data/feiertage_SH.csv")){
  feiertage <- read_csv("data/feiertage_SH.csv")
}else{
  dir.create("data/", recursive = TRUE)
#Importieren der Libaries
source("prep_environment.R")
#Importieren der Libaries
#Ladezeit erhöhen bei schlechter Verbindung
options(timeout= 4000000)
#Laden der Umsatzdaten csv
umsatzdaten_feiertage <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")
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
allYears <- unique(substr(umsatzdaten_feiertage$Datum, 1, 4))
#print(allYears)
#Ausgabe: [1] "2013" "2014" "2015" "2016" "2017" "2018" "2019"
#Leeres Dataframe erstellen für die Feiertage
feiertage <- data.frame(matrix(ncol = 2, nrow = 0))
#For-Schleife zum Füllen der Daten aus der Webseite für alle Jahre
for(i in allYears){
  #testJahr für jedes Jahr in allYears
  testJahr = i;
  #Ersetzen der URl für jedes Jahr
  site = paste("https://www.schulferien.org/Kalender_mit_Ferien/kalender_",testJahr,"_ferien_Schleswig_Holstein.html");
  #Ausschneiden der Leerzeichen
  site = str_replace_all(site," ", "")
  #Funktion zum lesen des html.codes der Webseite
  test = read_html(site)
  #Selektieren des html-Kastens, die alle Feiertage enthält
  test <- test %>% 
    html_nodes(".feiertag_liste") %>%
    html_text() %>% 
    .[1]
  #Hier werden, schrittweise um Fehler zu vermeiden, der übrige Text auf das wesentliche geschnitten
  test = str_replace_all(test,"\n", "")
  string1 = str_replace(paste("              ",testJahr)," ", "");
  string2 = str_replace(paste(".",testJahr)," ", "");
  test = str_replace_all(test,string1, string2)
  test = str_replace_all(test,"                                    ", "")
  test = str_replace_all(test,"   ", "")
  test = str_replace_all(test,"Jan", "01")
  test = str_replace_all(test,"Feb", "02")
  test = str_replace_all(test,"Mär", "03")
  test = str_replace_all(test,"Apr", "04")
  test = str_replace_all(test,"Mai", "05")
  test = str_replace_all(test,"Jun", "06")
  test = str_replace_all(test,"Jul", "07")
  test = str_replace_all(test,"Aug", "08")
  test = str_replace_all(test,"Sep", "09")
  test = str_replace_all(test,"Okt", "10")
  test = str_replace_all(test,"Nov", "11")
  test = str_replace_all(test,"Dez", "12")
  test = str_replace_all(test,"  ", ";")
  test = str_replace_all(test," ", "")  
  #Durch das Semikolon separaieren wir die benötigten Informationen
  rs <- (test)
  rs = strsplit(rs, split = ";")
  #Die Liste in Vector umwandeln, zur besseren Bearbeitung
  rs <-unlist(rs, use.names=FALSE);
  #Hinzufügen weiterer Vektoren, für das am ende erstellte fertige Dataframe
  Datum <- vector();
  Feiertag <- vector();
  Jahr <- vector();
  #Hier wird nun die Vektor mit den benötigten Informationen in die jeweiligen Vektoren gespeichert
  j = 1;
  for(i in rs){
    if((j %% 2) != 0){
      Datum <- append(Datum,i);
      Jahr <- append(Jahr,testJahr);
    }else{
      Feiertag <- append(Feiertag,i);
    }
    j = j + 1;
  }
  #Nachdem die Vektoren vervollständigt wurden, werden sie in das Dataframe gespeichert
  feiertage <- rbind(feiertage,data.frame(Datum,Feiertag))
}
#Deutsches Datumsformat in das englische umwandeln
feiertage$Datum <- as.Date(feiertage$Datum, "%d.%m.%Y")
#nicht mehr benötigte Variablen löschen
remove(allYears,Jahr,Datum,Feiertag,i,j,rs,site,string1,string2,test,testJahr, umsatzdaten_feiertage)
#Das fertige Dataframe kann hier in Bollesche Werte umgewandelt werden
feiertage$Feiertag <- str_replace_all(feiertage$Feiertag, "\\(?[a-zA-Z1-9.]+\\)?", "1")

#Falls nicht vorhanden, um erneutes laden zu vermeiden, als csv speichern
write.csv(feiertage,"data/feiertage_SH.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")
}