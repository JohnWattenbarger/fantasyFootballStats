source("config.R")
source("tableCreator.R")

# Adds the previous year's points for players table. Used to look back at previous success.
createTableToExport <- function(position, currentYear, numberOfColumns) {
    position.currentYear <- oneYearTable(position, currentYear)
    position.lastYear <- oneYearTable(position, currentYear - 1)
    numberOfColumns <- ncol(position.currentYear)
    # temp <- position.lastYear[c(2, numberOfColumns)]
    temp <- position.lastYear[, c("Player", "Fantasy_Points")]
    colnames(temp)[2] <- paste(as.character(currentYear - 1), "Fantasy Points")
    position.export <- merge(x = position.currentYear, y = temp, by.x = "Player", by.y = "Player", all.x = TRUE)
    return(position.export) # <--- add this line
}


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

# Add next year's fantasy points to the end of each dataset
# This will do this for multiple years. For 1 year use createTableToExport (singular)
createTablesToExport <- function(position, startingYear, currentYear) {
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

removeNa <- function(table) {
    table <- table[!is.na(table$`Next Year`), ]
    return(table)
}
