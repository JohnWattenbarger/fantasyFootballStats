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
# library(xlsx)
library(openxlsx)

source("config.R")
source("tableCreator.R")
source("functions.R")

print(currentYear)

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
# teamText <- "Tm"
teamText <- "Team"

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

TE$"Fantasy_Points" <- teReceivingYardMultiplier * TE$`Re_Yds` + receivingTDMultiplier * TE$`Re_TDs` + pointsPerReceptionMultiplier * TE$`Rec` + fumbleMultiplier * TE$`Fmb`

# set NA to 0 (might be better to do this in cleanup() or toNumeric())
kicking[is.na(kicking)] <- 0

kicking$"Fantasy_Points" <- 1 * kicking$`XPM` - 1 * (kicking$`FGA` - kicking$`FGM`) +
  3 * (kicking$`FGM_0_19` + kicking$`FGM_20_29` + kicking$`FGM_30_39`) + 4 * (kicking$`FGM_40_49`) +
  5 * (kicking$`FGM_50`)

##########################################################################################
########################################### My Stuff #####################################
##########################################################################################

#### new code ####

QB.export <- createTableToExport(QB, currentYear)
RB.export <- createTableToExport(RB, currentYear)
WR.export <- createTableToExport(WR, currentYear)
TE.export <- createTableToExport(TE, currentYear)

# create an excel file with just this year's stats
if (createExcelDocs) {
  tryCatch(
    {
      spreadsheetName <- paste("./data/", paste(currentYear, "Stats.xlsx"), sep = "")
      # Build optional suffix
      suffix <- if (!is.null(customTitle) && customTitle != "") paste0("_", customTitle) else ""
      # Final spreadsheet name with optional suffix
      spreadsheetName <- paste0("./data/", currentYear, " Stats", suffix, ".xlsx")

      # Create workbook
      wb <- createWorkbook()

      # Add sheets
      addWorksheet(wb, "QB")
      addWorksheet(wb, "RB")
      addWorksheet(wb, "WR")
      addWorksheet(wb, "TE")

      # Write data to sheets
      writeData(wb, "QB", QB.export)
      writeData(wb, "RB", RB.export)
      writeData(wb, "WR", WR.export)
      writeData(wb, "TE", TE.export)

      # Save workbook
      saveWorkbook(wb, spreadsheetName, overwrite = TRUE)
    },
    error = function(e) {
      message("Export failed: ", e)
    }
  )
}

# create a table with the starting players at each position
QB.startingPlayers <- getStartingPlayers(QB, "QB")
RB.startingPlayers <- getStartingPlayers(RB, "RB")
WR.startingPlayers <- getStartingPlayers(WR, "WR")
TE.startingPlayers <- getStartingPlayers(TE, "TE")


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

# add the next year fantasy points
QB.withNextYear <- createTablesToExport(QB, startingYear, currentYear)
RB.withNextYear <- createTablesToExport(RB, startingYear, currentYear)
WR.withNextYear <- createTablesToExport(WR, startingYear, currentYear)
TE.withNextYear <- createTablesToExport(TE, startingYear, currentYear)

######################### CREATE DATA FOR LINEAR REGRESSION ###########################

# remove NA rows (possibly should switch to 0 instead?)
QB.testData <- removeNa(QB.withNextYear)
RB.testData <- removeNa(RB.withNextYear)
WR.testData <- removeNa(WR.withNextYear)
TE.testData <- removeNa(TE.withNextYear)

# create an excel file with the last 10 years of data and next year fantasy stats
if (createExcelDocs) {
  tryCatch(
    {
      spreadsheetName <- paste("./data/", paste(toCharacter(currentYear), "Data Analysis Stats.xlsx"), sep = "")
      # Build optional suffix
      suffix <- if (!is.null(customTitle) && customTitle != "") paste0("_", customTitle) else ""
      # Final spreadsheet name with optional suffix
      spreadsheetName <- paste0("./data/", currentYear, " Data Analysis Stats", suffix, ".xlsx")

      # Create workbook
      wb <- createWorkbook()

      # Add sheets
      addWorksheet(wb, "QB")
      addWorksheet(wb, "RB")
      addWorksheet(wb, "WR")
      addWorksheet(wb, "TE")

      # Write data to sheets
      writeData(wb, "QB", QB.testData)
      writeData(wb, "RB", RB.testData)
      writeData(wb, "WR", WR.testData)
      writeData(wb, "TE", TE.testData)

      # Save workbook
      saveWorkbook(wb, spreadsheetName, overwrite = TRUE)
    },
    error = function(e) {
      message("Export failed: ", e)
    }
  )
}

# # Optionally run this to look at VoRP:
# source("valueOverReplacement.R")

# All finished and ready to be analyzed!
