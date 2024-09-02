# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 starting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Info for future self
#
# How To Run:
# > Open tableCreator.R, select all (Ctrl + a), and run (Ctrl + enter)
# > Switch back to this file
# > Change the currentYear variable (to the season that just finished. So if prepping for the 2023 season, current season should be 2022)
# > Change the multiplier variables to match league settings
# > Select all and run (Ctrl + a, then Ctrl + enter)
#
# Uses:
# > after running all code, data folder (C://code/fantasyFootball/data) should have 2 files:
#    - Stats from the most recent year (ex: '2019 Stats')
#    - Stats used for data analysis (ex: '2019 Data Analysis Stats')
#        > This has a players stats from one season, and then how many fantasy points they got the next year
# > analyze the stats by opening linearRegression in Atom, changing the year, and running (don't select anything, then Ctrl + Shift + B)
#
# Possible future changes:
# > move the year to a config file (that is read by both this and the python programs)
# > create a parent program or executable that will run this, then the python
# > improve column names (some like percentage columns are weird)
# > remove some of the unnecessary columns that Pro Football Reference added
# > better handle NA values for some columns (usually makes sense to be 0)
# > remove bad rows (i.e. 3 pass attempts or NA)
# > consider storing old tables (would prevent some web scrapping, but probably not worth the effort)
#
# Fixing Issues:
# > Look at console for 1st error
# > Check that columns and setup is the same
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 current
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Everything should be working as of 9/5/2020
# Most likely future issues are Pro Football Reference is out of date or an R function has changed
# Pro Football Reference:
# > URL: https://www.pro-football-reference.com/years/2019/passing.htm
# > Check column names

library("rvest")
library(ggplot2)
library(stringr)
library(xlsx)


###############################################
################ Variable used ################
###############################################

# Change these based on the year and league stats

currentYear <- 2023 # last year of stats to collect. So set to '2022' before '2023' season
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

###############################################
################ Create Tables ################
###############################################

# create a table with 10 years of passing stats
passing <- multiYearTable("passing")
View(passing)

# create a table with 10 years of rushing stats
rushing <- multiYearTable("rushing")
View(rushing)

# create a table with 10 years of receiving stats
receiving <- multiYearTable("receiving")
View(receiving)

# create a table with 10 years of kicking stats
kicking <- multiYearTable("kicking")
View(kicking)

# Create Position Tables
yearText <- "Year"
playerText <- "Player"
positionText <- "Pos"
ageText <- "Age"
gameText <- "G"
gameStartedText <- "GS"
# consider not using team if a traded player appears twice in one year
teamText <- "Tm"

mergeCategories <- c(yearText, playerText, positionText, teamText, ageText, gameText, gameStartedText)
mergeCategoriesRb <- append(mergeCategories, "Fmb")

QB <- merge(x = passing, y = rushing, by.x = mergeCategories, by.y = mergeCategories, all.x = TRUE)
# Need to manually change the column name if different that the above. i.e. change QB$Pos to whatever the right side of `positionText <- "Pos"` is
QB <- QB[QB$Pos %in% "QB", ]
# set null values (QB with no rushing stats) to 0
QB[is.na(QB)] <- 0

RB <- merge(x = rushing, y = receiving, by.x = mergeCategoriesRb, by.y = mergeCategoriesRb, all.x = TRUE)
RB <- RB[RB$Pos %in% "RB", ]
# set null values (RB with no receiving stats) to 0
RB[is.na(RB)] <- 0

WR <- receiving[receiving$Pos %in% "WR", ]

TE <- receiving[receiving$Pos %in% "TE", ]

# add a fantasy point total column
# XXX: Need to manually edit the column names to match the current column names (from renameDuplicates)
QB$"Fantasy_Points" <- passingYardMultiplier * QB$`Pa_Yds` + passingTDMultiplier * QB$`Pa_TDs` + interceptionMultiplier * QB$`Int` + rushingYardMultiplier * QB$`Ru_Yds` + rushingTDMultiplier * QB$`Ru_TDs` + fumbleMultiplier * QB$`Fmb`

RB$"Fantasy_Points" <- rushingYardMultiplier * RB$`Ru_Yds` + rushingTDMultiplier * RB$`Ru_TDs` + fumbleMultiplier * RB$`Fmb` + receivingYardMultiplier * RB$`Re_Yds` + receivingTDMultiplier * RB$`Re_TDs` + pointsPerReceptionMultiplier * RB$`Rec`

WR$"Fantasy_Points" <- receivingYardMultiplier * WR$`Re_Yds` + receivingTDMultiplier * WR$`Re_TDs` + pointsPerReceptionMultiplier * WR$`Rec` + fumbleMultiplier * WR$`Fmb`

TE$"Fantasy_Points" <- receivingYardMultiplier * TE$`Re_Yds` + receivingTDMultiplier * TE$`Re_TDs` + pointsPerReceptionMultiplier * TE$`Rec` + fumbleMultiplier * TE$`Fmb`

# set NA to 0 (might be better to do this in cleanup() or toNumeric())
kicking[is.na(kicking)] <- 0

kicking$"Fantasy_Points" <- 1 * kicking$`XPM` - 1 * (kicking$`FGA` - kicking$`FGM`) +
  3 * (kicking$`FGM_0_19` + kicking$`FGM_20_29` + kicking$`FGM_30_39`) + 4 * (kicking$`FGM_40_49`) +
  5 * (kicking$`FGM_50`)

##########################################################################################
########################################### My Stuff #####################################
##########################################################################################

#### new code ####

# Create a new table that adds last year's fantasy points to the current year's data
# Note: position is the actual data table. Might be better to setup differently, but this works
createTableToExport <- function(position, currentYear, numberOfColumns) {
  position.currentYear <- oneYearTable(position, currentYear)
  position.lastYear <- oneYearTable(position, currentYear - 1)
  numberOfColumns <- ncol(position.currentYear)
  temp <- position.lastYear[c(2, numberOfColumns)]
  colnames(temp)[c(2)] <- c(paste(toCharacter(currentYear - 1), " Fantasy Points"))
  position.export <- merge(x = position.currentYear, y = temp, by.x = c("Player"), by.y = c("Player"), all.x = TRUE)
}

QB.export <- createTableToExport(QB, currentYear)
RB.export <- createTableToExport(RB, currentYear)
WR.export <- createTableToExport(WR, currentYear)
TE.export <- createTableToExport(TE, currentYear)

# create an excel file with just this year's stats
spreadsheetName <- paste("./data/", paste(currentYear, "Stats.xlsx"), sep = "")
dir.create(dirname(spreadsheetName))
write.xlsx(QB.export, file = spreadsheetName, sheetName = "QB", row.names = FALSE)
write.xlsx(RB.export, file = spreadsheetName, sheetName = "RB", append = TRUE, row.names = FALSE)
write.xlsx(WR.export, file = spreadsheetName, sheetName = "WR", append = TRUE, row.names = FALSE)
write.xlsx(TE.export, file = spreadsheetName, sheetName = "TE", append = TRUE, row.names = FALSE)


# Get the differences b/w starters and non-starters

# returns how many starters there are (for a 10 man standard league)
getNumberOfStarters <- function(position) {
  numberOfStarters <- 0

  numberOfStarters <- switch(position,
    "QB" = 10,
    "RB" = 20,
    "WR" = 20,
    "TE" = 10
  )

  return(numberOfStarters)
}

# get only the starting players at a position
getStartingPlayers <- function(table, position) {
  for (i in (1:10))
  {
    year <- startingYear + i
    temp <- table[table$Year %in% year, ]
    temp <- temp[with(temp, order(-temp$`Fantasy_Points`)), ]
    temp <- temp[1:getNumberOfStarters(position), ]
    if (i == 1) {
      startingPlayers <- temp
    } else {
      startingPlayers <- rbind(startingPlayers, temp)
    }
  }

  return(startingPlayers)
}

# create a table with the starting players at each position
QB.startingPlayers <- getStartingPlayers(QB, "QB")
RB.startingPlayers <- getStartingPlayers(RB, "RB")
WR.startingPlayers <- getStartingPlayers(WR, "WR")
TE.startingPlayers <- getStartingPlayers(TE, "TE")

# create a table with the difference between the 1st and last starter each year
getDifferenceTable <- function(table, position) {
  for (i in (1:10))
  {
    year <- startingYear + i
    temp <- oneYearTable(table, year)
    difference <- max(temp$Fantasy_Points) - min(temp$Fantasy_Points)

    if (i == 1) {
      differenceTable <- c(position, year, difference)
    } else {
      differenceTable <- rbind(differenceTable, c(position, year, difference))
    }
  }
  colnames(differenceTable) <- c("Position", "Year", "Difference")
  return(differenceTable)
}

# Create tables with the point difference between the best and worst starters at each position
QB.differenceTable <- getDifferenceTable(QB.startingPlayers, "QB")
RB.differenceTable <- getDifferenceTable(RB.startingPlayers, "RB")
WR.differenceTable <- getDifferenceTable(WR.startingPlayers, "WR")
TE.differenceTable <- getDifferenceTable(TE.startingPlayers, "TE")

differenceTable <- rbind(QB.differenceTable, RB.differenceTable, WR.differenceTable, TE.differenceTable)

# convert from a matrix to a data frame
differenceTable <- data.frame(differenceTable)

# Convert datatypes from factors to characters and numerics
differenceTable <- toCharacter(differenceTable)
differenceTable[, 2:3] <- toNumeric(differenceTable[, 2:3])

# plot starter difference change over time
ggplot(differenceTable, aes(Year, `Difference`, group = Position)) +
  geom_line(aes(col = differenceTable$Position), size = 1.5) +
  geom_point(aes(col = differenceTable$Position), size = 3) +
  scale_x_continuous("Year", labels = as.character(differenceTable$Year), breaks = differenceTable$Year) +
  guides(color = guide_legend(title = "Position")) +
  ggtitle("Difference between best and worst starting players over time") +
  theme(plot.title = element_text(size = 30, hjust = 0.5)) +
  scale_color_discrete(breaks = c("QB", "RB", "WR", "TE")) +
  scale_y_continuous(name = "Difference")



######################### Setup Tables with Next Years Stats ###########################

# Add next year's fantasy points to the end of each dataset
createTableToExport <- function(position, startingYear, currentYear) {
  for (tempYear in startingYear:(currentYear - 1)) {
    tempTable <- addNextYearsPoints(position, tempYear)
    if (tempYear == startingYear) {
      totalTable <- tempTable
    } else {
      totalTable <- rbind(totalTable, tempTable)
    }
  }
  return(totalTable)
}

addNextYearsPoints <- function(position, year) {
  position.year <- oneYearTable(position, year)
  position.nextYear <- oneYearTable(position, year + 1)
  temp <- position.nextYear[c(2, ncol(position))]
  colnames(temp)[c(2)] <- c(paste("Next Year", "Fantasy Points"))
  position.export <- merge(x = position.year, y = temp, by.x = c("Player"), by.y = c("Player"), all.x = TRUE)
  return(position.export)
}

# add the next year fantasy points
QB.withNextYear <- createTableToExport(QB, startingYear, currentYear)
RB.withNextYear <- createTableToExport(RB, startingYear, currentYear)
WR.withNextYear <- createTableToExport(WR, startingYear, currentYear)
TE.withNextYear <- createTableToExport(TE, startingYear, currentYear)

######################### CREATE DATA FOR LINEAR REGRESSION ###########################

removeNa <- function(table) {
  table <- table[!is.na(table$`Next Year`), ]
  return(table)
}

# remove NA rows (possibly should switch to 0 instead?)
QB.testData <- removeNa(QB.withNextYear)
RB.testData <- removeNa(RB.withNextYear)
WR.testData <- removeNa(WR.withNextYear)
TE.testData <- removeNa(TE.withNextYear)


# create an excel file with the last 10 years of data and next year fantasy stats
spreadsheetName <- paste("./data/", paste(toCharacter(currentYear), "Data Analysis Stats.xlsx"), sep = "")
dir.create(dirname(spreadsheetName))
write.xlsx(QB.testData, file = spreadsheetName, sheetName = "QB", row.names = FALSE)
write.xlsx(RB.testData, file = spreadsheetName, sheetName = "RB", append = TRUE, row.names = FALSE)
write.xlsx(WR.testData, file = spreadsheetName, sheetName = "WR", append = TRUE, row.names = FALSE)
write.xlsx(TE.testData, file = spreadsheetName, sheetName = "TE", append = TRUE, row.names = FALSE)

# All finished and ready to be analyzed!
