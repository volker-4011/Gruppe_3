#Laden aller benötigten Libaries
pkgs <- c("readr","lubridate","ggplot2","plyr","dplyr","tidyverse","rvest","xml2","stringr","forcats","timeDate","e1071","Metrics","RCurl","readxl","reticulate","fastDummies")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
#Laden aller benötigten Libaries