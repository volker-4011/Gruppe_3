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
#allYears <- c("2013")
#print(allYears)
#Ausgabe: [1] "2013" "2014" "2015" "2016" "2017" "2018" "2019"

ferientage <- data.frame(matrix(ncol = 4, nrow = 0))

bundesland <- c("Baden-Wuerttemberg","Bayern","Berlin","Brandenburg","Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen","Sachsen-Anhalt", "Schleswig-Holstein", "Thueringen")
#bundesland <- c("Bremen")
for(b in bundesland){
  

for(l in allYears){
  
  #l = allYears
  #b = bundesland

  site = paste("https://www.ferienkalender.com/ferien_deutschland/",b,"/",l,"-ferien-",tolower(b),".htm", sep = "")
  test = read_html(site)
  print(site)
  test <- test %>% 
    html_nodes(".kasten") %>%
    html_text() %>% 
    .[1]
  
  test = str_replace_all(test,"\n", "") 
  print(test)
  b2 = str_replace_all(b,"ue","ü")
  print(b2)
  test = str_replace_all(test, paste("Ferienkalender ",b2," ",l,"Ferien ",l," im deutschen Bundesland ",b2,":", sep = ""), "")
  print(test)
  test = str_replace_all(test," / Frühjahrsferien", "") 
  print(test)
  test = str_replace_all(test, paste("Ferien ",l," für ",b2, sep = ""), "") 
  print(test)
  test = str_replace_all(test,"Winterferien-","")
  print(test)
  
  test = str_replace_all(test,"Pfingstferien-","")
  print(test)
  
  
  pattern <- "Pfingstferien\\s*(.*?)\\s*Sommerferien"
  result <- regmatches(test, regexec(pattern, test))
  result <- result[[1]][2]
  print(result)
  if(grepl("/", result, fixed = TRUE)){
    test = str_replace_all(test,result,str_replace_all(result,"/","Pfingstferien"))
    print(test)
  }
  
  pattern <- "Herbstferien\\s*(.*?)\\s*Weihnachtsferien"
  result <- regmatches(test, regexec(pattern, test))
  result <- result[[1]][2]
  print(result)
  if(grepl("/", result, fixed = TRUE)){
    test = str_replace_all(test,result,str_replace_all(result,"/","Herbstferien"))
    print(test)
  }
  
  pattern <- "Sommerferien\\s*(.*?)\\s*Herbstferien"
  result <- regmatches(test, regexec(pattern, test))
  result <- result[[1]][2]
  print(result)
  if(grepl("/", result, fixed = TRUE)){
    test = str_replace_all(test,result,str_replace_all(result,"/","Sommerferien"))
    print(test)
  }
  
  pattern <- "Osterferien\\s*(.*?)\\s*Pfingstferien"
  result <- regmatches(test, regexec(pattern, test))
  result <- result[[1]][2]
  print(result)
  if(grepl("/", result, fixed = TRUE)){
    test = str_replace_all(test,result,str_replace_all(result,"/","Osterferien"))
    print(test)
  }
  
  #######################################
  test = str_replace_all(test," - ", "0000")
  print(test)
  numbers <- str_extract_all(test,"\\(?[0-9,.]+\\)?")[[1]]
  print(numbers)
  numbers <- paste(numbers, l, sep="")
  print(numbers)
  numbers = str_replace_all(numbers,"0000", paste(l," Y ", sep = ""))
  print(numbers)
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
    
    if(characters[m]=="Weihnachtsferien"){
      Datum = str_sub(Datum,end = -5)
      Datum = paste(Datum, year_add, sep = "")
      #print(Datum)
      #break
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
    
    ferientage <- rbind(ferientage,data.frame(Datum,Ferien,Ferien,b))
    
    k = k + 1;
    m = m + 1;  
  }
  
  
}
  remove(characters,Datum,j,k,m,Ferien,numbers,site,test,year_add)
}
remove(allYears,characters,Datum,j,k,l,m,Ferien,numbers,site,test,year_add,umsatzdaten_ferien,b)

ferientage$Ferien <- str_replace_all(ferientage$Ferien, "\\(?[a-zA-Z]+\\)?", "1")

#ferientage <- dplyr::select(ferientage, -contains("2020"))
#dplyr::select(ferientage, contains('2020')) 

ferientage <- filter(ferientage, Datum <= "2019-12-31")


