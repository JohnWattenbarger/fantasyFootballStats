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
receivingTDMultiplier <- (6)
pointsPerReceptionMultiplier <- (0)
# turnovers
interceptionMultiplier <- (-2)
fumbleMultiplier <- (-2)

# Set variables based on the year (starting year determines how far back to look)
nextYear <- currentYear + 1
startingYear <- currentYear - 10
