###################################################
### Preparation of the Environment ####

# Clear environment
remove(list = ls())

# Needed libraries
source("prep_environment.R")


###################################################
### Data Import ####
source("prep_data.R")


###################################################
### Data Preparation ####

#For Testung just bluntly remove all rows with NAs
fullNoNAs <- fullData[complete.cases(fullData), ]

# Recoding of the variables into one-hot encoded (dummy) variables
dummy_list <- c("Warengruppe", "Monat", "Wochentag")
fullData_dummy = dummy_cols(fullNoNAs, dummy_list)

# Definition of lists for each one-hot encoded variable (just to make the handling easier)
#Remove first dummy encoded variable to avoid colinearities('Warengruppe_1','Monat_1', 'Wochentag_Montag')
Warengruppe_dummies = c('Warengruppe_2', 'Warengruppe_3', 'Warengruppe_4', 'Warengruppe_5', 'Warengruppe_6')
Monat_dummies = c('Monat_2', 'Monat_3', 'Monat_4','Monat_5','Monat_6','Monat_7','Monat_8','Monat_9','Monat_10','Monat_11','Monat_12')
Wochentag_dummies = c('Wochentag_Dienstag', 'Wochentag_Mittwoch', 'Wochentag_Donnerstag', 'Wochentag_Freitag', 'Wochentag_Samstag', 'Wochentag_Sonntag')


###################################################
### Selection of the Feature Variables and the Label Variable ####

# Selection of the features (the independent variables used to predict the dependent)
#features <- c('sqft_lot', 'waterfront', 'grade', 'bathrooms', view_dummies, condition_dummies)
features <- c('Windchill', 'KielerWoche', Warengruppe_dummies, Monat_dummies, Wochentag_dummies)
# Selection of the label (the dependent variable)
labels <- 'Umsatz'


###################################################
### Selection of Training, Validation and Test Data ####

# Look at the data
str(fullData_dummy)

# Setting the random counter to a fixed value, so the random initialization stays the same (the random split is always the same)
set.seed(1)

# Shuffling the dataset (to get random orders within each dataset as well)
new_row_order <- sample(nrow(fullData_dummy))
house_pricing_dummy <- fullData_dummy[new_row_order, ]

# Assign each row number in the full dataset randomly to one of the three groups of datasets
# The probability of being in one of the groups results then in crresponding group sizes
assignment <- sample(1:3, size = nrow(fullData_dummy), prob = c(.7, .2, .1), replace = TRUE)

# Create training, validation and test data for the features and the labels
training_features <- fullData_dummy[assignment == 1, features]    # subset house_pricing to training indices only
training_labels <- fullData_dummy[assignment == 1, labels]    # subset house_pricing to training indices only

validation_features <- fullData_dummy[assignment == 2, features]  # subset house_pricing to validation indices only
validation_labels <- fullData_dummy[assignment == 2, labels]  # subset house_pricing to validation indices only

test_features <- fullData_dummy[assignment == 3, features]   # subset house_pricing to test indices only
test_labels <- fullData_dummy[assignment == 3, labels]   # subset house_pricing to test indices only
