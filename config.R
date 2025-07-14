###############################################
################ Variable used ################
###############################################

# Change these based on the year and league stats

currentYear <- 2024 # last year of stats to collect. So set to '2022' before '2023' season
##### Stat multipliers based on different league settings #####
# passing
passingYardMultiplier <- .06
passingTDMultiplier <- 4
# rushing
rushingYardMultiplier <- (.1)
rushingTDMultiplier <- (6)
# receiving
receivingYardMultiplier <- (.1)
teReceivingYardMultiplier <- (.1) # most leagues this will match receivingYardMultiplier. Except in some dynasty leagues
receivingTDMultiplier <- (6)
pointsPerReceptionMultiplier <- (0)
# turnovers
interceptionMultiplier <- (-2)
fumbleMultiplier <- (-2)

# Set variables based on the year (starting year determines how far back to look)
nextYear <- currentYear + 1
startingYear <- currentYear - 10

# Set this to NULL or "" if you don't want a suffix
# customTitle <- null # or "" or NULL for no title
customTitle <- "dynasty_league" # or "" or NULL for no title

createExcelDocs <- TRUE # Set to FALSE to skip Excel file generation
