# Import needed libraries
source("prep_environment.R")
# Import turnover data
options(timeout= 4000000)
umsatzdaten_ferien <- read_csv("https://raw.githubusercontent.com/opencampus-sh/einfuehrung-in-data-science-und-ml/main/umsatzdaten_gekuerzt.csv")

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

ferientage <- data.frame(matrix(ncol = 2, nrow = 0))
for(l in allYears){
  
  site = paste("https://www.ferienkalender.com/ferien_deutschland/Schleswig-Holstein/",l,"-ferien-schleswig-holstein.htm", sep = "")
  test = read_html(site)
  
  test <- test %>% 
    html_nodes(".kasten") %>%
    html_text() %>% 
    .[1]
  
  test = str_replace_all(test,"\n", "") 
  test = str_replace_all(test, paste("Ferienkalender Schleswig-Holstein ",l,"Ferien ",l," im deutschen Bundesland Schleswig-Holstein:Winterferien-", sep = ""), "")
  test = str_replace_all(test," / Frühjahrsferien", "") 
  test = str_replace_all(test, paste("Ferien ",l," für Schleswig-Holstein", sep = ""), "") 

  #######################################
  test = str_replace_all(test," - ", "0000") 
  numbers <- str_extract_all(test,"\\(?[0-9,.]+\\)?")[[1]]
  numbers <- paste(numbers, l, sep="")
  numbers = str_replace_all(numbers,"0000", paste(l," Y ", sep = ""))

  year_add <- as.numeric(as.numeric(l)+1)
  
  #######################################

  #######################################
  characters <- str_extract_all(test,"\\(?[a-zA-Z]+\\)?")[[1]] 

  print(characters)
  
  length(numbers)
  #######################################
  
  k = 1;
  m = 1;
  
  for (j in numbers){
    Datum <- j 
    
    if(m==5){
      Datum = str_sub(Datum,end = -5)
      Datum = paste(Datum, year_add, sep = "")
    }

    Datum <- strsplit(Datum, split = " Y ") 

    Datum <-unlist(Datum, use.names=FALSE);
    print(Datum) 
    
    Datum <- as.Date(Datum, "%d.%m.%Y")  
    print(Datum) 
    
    Ferien <- characters[k]

 
    if(length(Datum) > 1){
      #Datum <- seq(as.Date(Datum[1]), as.Date(Datum[2]), by="days")
      Datum <- as.Date(as.Date(Datum[1]):as.Date(Datum[2]), origin="1970-01-01")
    }
 
    ferientage <- rbind(ferientage,data.frame(Datum,Ferien))
    
    k = k + 1;
    m = m + 1;  
  }
  
  
}

remove(allYears,characters,Datum,j,k,l,m,Ferien,numbers,site,test,year_add,umsatzdaten_ferien)

ferientage$Ferien <- str_replace_all(ferientage$Ferien, "\\(?[a-zA-Z]+\\)?", "1")

#ferientage <- dplyr::select(ferientage, -contains("2020"))
#dplyr::select(ferientage, contains('2020')) 

ferientage <- filter(ferientage, Datum <= "2019-12-31")

#write.csv(ferientage,"C:\\Users\\Peyman Farshidfar\\Documents\\ferientage.csv")
