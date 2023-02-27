library('rvest')
library(ggplot2)
library(stringr)
library(xlsx)

# Take an HTML_Table from ESPN (changed websites) player stats, get the data, 
#' @export
getHtmlTable <- function(type, url, header=TRUE)
{
  columns <- getNumberOfColumns(type)
  
  webpage <- read_html(url)
  values <- html_table(webpage, header)
  table <- data.frame(matrix(unlist(values), ncol=columns, byrow=F))
  
  # set column header
  if (type == "passing" || type == "receiving") {
    colnames(table) <- lapply(table[30, ], as.character)
    table <- unique(table)
  }
  else {
    colnames(table) <- lapply(table[1, ], as.character)
    table <- table[-1,]
  }
  
  return(table)
}

# Creates and combines tables from the last 10 years
#' @export
multiYearTable <- function(type)
{
  for(i in 1:10)
  {
    year <- startingYear+i
    tempTable <- makeTable(type, year)
    if (i == 1)
      tableCombined <- tempTable
    else
      tableCombined <- rbind(tableCombined, tempTable)
  }
  return(tableCombined)
}

# convert data type from factor to character
#' @export
toCharacter <- function(data)
{
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  
  return(data)
}

# convert data type from character to numeric
#' @export
toNumeric <- function(data)
{
  data <- sapply(data, as.numeric)
  return(data)
}

# gets the data to the correct format
#' @export
cleanUp <- function(table, type)
{
  if (type == "passing")
  {
    table <- toCharacter(table)
    table[,c(1, 4, 6:7, 8:ncol(table))] <- toNumeric(table[,c(1, 4, 6:7, 8:ncol(table))])
    
    # ensure every Position has a value
    table$Pos[table$Pos==""] <- "QB"
  }
  if (type == "rushing")
  {
    table <- toCharacter(table)
    table[,c(1, 4, 6:ncol(table))] <- toNumeric(table[,c(1, 4, 6:ncol(table))])
    
    # ensure every Position has a value
    table$Pos[table$Pos==""] <- "RB"
  }
  if (type == "receiving")
  {
    table <- toCharacter(table)
    table[,c(1, 4, 6:9, 10:ncol(table))] <- toNumeric(table[,c(1, 4, 6:9, 10:ncol(table))])
    
    # ensure every Position has a value
    table$Pos[table$Pos==""] <- "WR"
  }
  # Note: this will be difficult. Must split columns 8 - 12 into 2 columns each
  if (type == "kicking")
  {
    table <- toCharacter(table)
    numericRows <- c(1, 4, 6:20, 22:23, 25:27, 29:ncol(table))
    table[,numericRows] <- toNumeric(table[,numericRows])
    
    # ensure every Position has a value
    table$Pos[table$Pos==""] <- "K"
  }
  
  table$Pos <- toupper(table$Pos)
  table <- removeHeaderRows(table)
  table <- removeExtraCharacters(table)
  table <- renameDuplicates(table, type)
  table <- trimWhitespace(table)
  
  return(table)
}

#' @export
removeHeaderRows <- function(table) {
  table <- removeCharacter(table, "*")  table <- table[!table[,2] == "Player", ]
  
  return(table)
}

#' @export
removeExtraCharacters <- function(table) {
  table[,2] <- removeCharacter(table[,2], "\\*")
  table[,2] <- removeCharacter(table[,2], "\\+")
  
  return(table)
}


# remove a certain character from a table
removeCharacter <- function(table, character)
{
  if (character != "") {
    table <- gsub(character, "", table)
  }
  
  return(table)
}

trimWhitespace <- function(table) {
  table <- data.frame(lapply(table, function(x) if(class(x)=="character") str_trim(x) else(x)), stringsAsFactors=F)
  
  return(table)
}

# Create a table using ESPN.com or NFL.com
#' @export
makeTable <- function(type, year)
{
  table <- makeTableProFootballReference(type, year)
  table <- cleanUp(table, type)
  table <- addYear(table, year)
  
  return(table)
}

# makes the 1st column display the year
addYear <- function(table, year)
{
  table[,1] <- year
  colnames(table)[1] <- "Year"
  return(table)
}

#' @export
makeTableProFootballReference <- function(type, year) {
  year <- lapply(year, as.character)
  
  # https://www.pro-football-reference.com/years/2019/passing.htm
  urlPart1 <- "https://www.pro-football-reference.com/years/"
  urlPart2 <- ".htm"
  
  totalUrl <- paste(urlPart1, year, "/", type, urlPart2, sep="")
  
  table <- getHtmlTable(type, totalUrl)
  return(table)
}

#' @export
getNumberOfColumns <- function(type)
{
  if(type == "passing")
    return(31)
  if(type == "rushing")
    return(15)
  if(type == "receiving")
    return(19)
  if(type == "kicking")
    return(34)
}

# Rename columns that also describe different stats in other tables
#' @export
renameDuplicates <- function(table, type)
{
  if (type == "passing") {
    colnames(table)[c(10, 12:13, 17:22, 26)] <- c("Pa_Att", "Pa_Yds", "Pa_TDs", "Pa_1D", "Pa_Lng", "Pa_Yds_A", "AY_A", "Yd_Com", "Pa_Yds_G", "Sk_Yds")
  }
  if(type == "rushing") {
    colnames(table)[c(8:14)] <- c("Ru_Att", "Ru_Yds", "Ru_TDs", "Ru_1D", "Ru_Lng", "Ru_Yds_A", "Ru_Yds_G")
  }
  if (type == "receiving") {
    colnames(table)[c(11:18)] <- c("Re_Yds", "Yd_Rec", "Re_TDs", "Re_1D", "Re_Lng", "Y_Tgt", "Re_G", "Re_Yds_G")
  }
  if (type == "kicking") {
    colnames(table)[c(8:19)] <- c("FGA_0_19", "FGM_0_19", "FGA_20_29", "FGM_20_29", "FGA_30_39", "FGM_30_39", "FGA_40_49", "FGM_40_49", "FGA_50", "FGM_50", "FGA", "FGM")
  }
  
  return(table)
}

# get a position for a certain year
#' @export
oneYearTable <- function(table, year)
{
  oneYear <- table[table$Year %in% year,]
  return(oneYear)
}