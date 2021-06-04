#Falls csv bereits vorhanden, laden
if(file.exists("data/feiertage_SH.csv")){
  feiertage <- read_csv("data/feiertage_SH.csv")
}else{
  
  dir.create("data/", recursive = TRUE)
  
# Import needed libraries
source("prep_environment.R")
# Import turnover data
options(timeout= 4000000)
umsatzdaten_feiertage <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")

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

feiertage <- data.frame(matrix(ncol = 2, nrow = 0))
for(i in allYears){
  
  testJahr = i;
  
  site = paste("https://www.schulferien.org/Kalender_mit_Ferien/kalender_",testJahr,"_ferien_Schleswig_Holstein.html");
  
  site = str_replace_all(site," ", "")
  
  #print(site);
  test = read_html(site)
  
  
  test <- test %>% 
    html_nodes(".feiertag_liste") %>%
    html_text() %>% 
    .[1]
  
  test = str_replace_all(test,"\n", "")
  
  string1 = str_replace(paste("              ",testJahr)," ", "");
  #print(string1)
  
  string2 = str_replace(paste(".",testJahr)," ", "");
  
  #print(string2)
  
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
  
  #print(test)
 
  test = str_replace_all(test," ", "")  

  rs <- (test)
  rs = strsplit(rs, split = ";")
  #print(rs)
  
  #print(typeof(rs))
  
  
  rs <-unlist(rs, use.names=FALSE);
  
  #print(rs)
  
  #print(test)
  
  
  Datum <- vector();
  Feiertag <- vector();
  Jahr <- vector();
  
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
  
  #Datum <- as.Date(Datum,format = "%d.%m.%y")
  
  feiertage <- rbind(feiertage,data.frame(Datum,Feiertag))
  
}

feiertage$Datum <- as.Date(feiertage$Datum, "%d.%m.%Y")

remove(allYears,Jahr,Datum,Feiertag,i,j,rs,site,string1,string2,test,testJahr, umsatzdaten_feiertage)

feiertage$Feiertag <- str_replace_all(feiertage$Feiertag, "\\(?[a-zA-Z1-9.]+\\)?", "1")

#Falls nicht vorhanden, um erneutes laden zu vermeiden, als csv speichern
write.csv(feiertage,"data/feiertage_SH.csv", append = FALSE, quote = TRUE, sep = ",",
          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")


}


