###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c("readr", "fastDummies")

# Load each listed library and check if it is installed and install if necessary
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


###################################################
### Data Import ####

# Reading the data file
# Importing Function Packages
source("prep_environment.R")

# Import der Daten

#if(!file.exists("fullData.csv")){
#  source("prep_data.R")
#}
source("prep_data.R")
testData <- read_csv("fullData.csv")


#str(testData)


#########################

###Vorbereiten des Dataframe für die Vorhersage

#Dummy Encoden der Variablen für die Vorhersage

dummy_list <- c("Monat", "Wochentag", "Warengruppe" , "Bewoelkung", "Windgeschwindigkeit", "Temperatur", "Niederschlagsmenge")
fullData_dummy = dummy_cols(testData, dummy_list)


#names(fullData_dummy)

#deleteColumns <- data.frame(x=2, y=4:7, z=9:11, a=4)
#fullData_dummy <- subset (fullData_dummy, select = -c(15:16))
#fullData_dummy <- subset (fullData_dummy, select = -c(10:12))
#fullData_dummy <- subset (fullData_dummy, select = -c(5:8))
#fullData_dummy <- subset (fullData_dummy, select = -c(3))



colnames(fullData_dummy)



fullData_dummy[ , c('Wettercode',
                'Relative_Feuchte',
                'Sonnenscheindauer',
                'Temperatur',
                'Wochentag', 
                'Windgeschwindigkeit'
)] <- list(NULL)

###################################################
### Data Preparation ####

# Recoding of the variables into one-hot encoded (dummy) variables
#dummy_list <- c("view", "condition")
#house_pricing_dummy = dummy_cols(house_pricing, dummy_list)

# Definition of lists for each one-hot encoded variable (just to make the handling easier)
monat_dummies = c("Monat_1","Monat_2","Monat_3","Monat_4","Monat_5","Monat_6","Monat_7",
                 "Monat_8","Monat_9","Monat_10","Monat_11","Monat_12")
wochentag_dummies = c("Wochentag_Dienstag","Wochentag_Donnerstag","Wochentag_Freitag",
                      "Wochentag_Mittwoch","Wochentag_Montag","Wochentag_Samstag","Wochentag_Sonntag")
warengruppe_dummies = c("Warengruppe_Broetchen","Warengruppe_Brot","Warengruppe_Crossaint","Warengruppe_Konditorei"
                        ,"Warengruppe_Kuchen","Warengruppe_Saisonbrot")
bewoelkung_dummies = c("Bewoelkung_0","Bewoelkung_1","Bewoelkung_2","Bewoelkung_3","Bewoelkung_4","Bewoelkung_5"
                       ,"Bewoelkung_6","Bewoelkung_7","Bewoelkung_8","Bewoelkung_NA")

windgeschwindigkeit_dummies = c("Windgeschwindigkeit_frische_Briese","Windgeschwindigkeit_leichte_Briese","Windgeschwindigkeit_maessige_Briese", 
                                "Windgeschwindigkeit_schwache_Briese", "Windgeschwindigkeit_Windstille", "Windgeschwindigkeit_NA")

temperatur_dummies = c("Temperatur_Eisig","Temperatur_Fruehling","Temperatur_Heisser_Tag","Temperatur_Kalter_Tag","Temperatur_Sommertag",
                       "Temperatur_Vegetationstag","Temperatur_NA")

niederschlag_dummies = c("Niederschlagsmenge_1","Niederschlagsmenge_2","Niederschlagsmenge_3","Niederschlagsmenge_4","Niederschlagsmenge_5")


#Spalten in numerische Zahlen umwandeln
for(i in 1:ncol(fullData_dummy)) {       # for-loop over columns
  columnType <- typeof(fullData_dummy[ , i])
  #print(columnType)
  if(columnType == "integer" || columnType == "num"){
    fullData_dummy[ , i] <- as.double(fullData_dummy[ , i])
  }
}



# Look at the data
str(fullData_dummy)

colnames(fullData_dummy)





###################################################
### Selection of the Feature Variables and the Label Variable ####

# Selection of the features (the independent variables used to predict the dependent)
#features <- c('sqft_lot', 'waterfront', 'grade', 'bathrooms', view_dummies, condition_dummies)
#features <- c('Ferien', 'Feiertag','Wochenende', 'KielerWoche',bewoelkung_dummies,monat_dummies, 
#              wochentag_dummies, warengruppe_dummies,windgeschwindigkeit_dummies,temperatur_dummies,niederschlag_dummies)
features <- c('Ferien', 'Feiertag','Wochenende', 'KielerWoche',bewoelkung_dummies,monat_dummies, 
              wochentag_dummies, warengruppe_dummies)
# Selection of the label (the dependent variable)
labels <- 'Umsatz'


###################################################
### Selection of Training, Validation and Test Data ####

# Look at the data
#str(fullData_dummy)

# Setting the random counter to a fixed value, so the random initialization stays the same (the random split is always the same)
set.seed(1)

# Shuffling the dataset (to get random orders within each dataset as well)
new_row_order <- sample(nrow(fullData_dummy))
fullData_dummy <- fullData_dummy[new_row_order, ]


# alle NAs durch 0 ersetzen, damit die svm läuft
# ist nicht die feine Art, wir müssen uns nochmal genauer um die NAs kümmern.
fullData_dummy[is.na(fullData_dummy)] <- 0

newData <- rbind(fullData_dummy[fullData_dummy$Datum == "2019-06-07", ])

fullData_dummy <- rbind(fullData_dummy[fullData_dummy$Datum != "2019-06-07", ])
#Löschen der Daten für den Tag, der vorhergesagt werden soll aus den Trainingsdaten
fullData_dummy <- subset(fullData_dummy, Datum != "2019-06-07")







# Assign each row number in the full dataset randomly to one of the three groups of datasets
# The probability of being in one of the groups results then in crresponding group sizes
assignment <- sample(1:3, size = nrow(fullData_dummy), prob = c(.7, .2, .1), replace = TRUE)
#assignmentV <- sample(1:3, size = nrow(newData), prob = c(.7, .2, .1), replace = TRUE)

# Create training, validation and test data for the features and the labels
training_features <- fullData_dummy[assignment == 1, features]    # subset house_pricing to training indices only
training_labels <- fullData_dummy[assignment == 1, labels]    # subset house_pricing to training indices only

validation_features <- fullData_dummy[assignment == 2, features]  # subset house_pricing to validation indices only
validation_labels <- fullData_dummy[assignment == 2, labels]  # subset house_pricing to validation indices only

test_features <- fullData_dummy[assignment == 3, features]   # subset house_pricing to test indices only
test_labels <- fullData_dummy[assignment == 3, labels]   # subset house_pricing to test indices only





