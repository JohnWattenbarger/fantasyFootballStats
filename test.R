# This file exists simply as a playground to test and debug code

# Load your main R script / source files
source("config.R") # Replace with the actual filename
source("functions.R")
source("test.R")

# # Set base year for testing
startingYear <- 2013 # Used by multiYearTable()

# # --- TEST: Single year, single position ---
# # Example: Get 2021 rushing table
passing2024 <- makeTable("passing", 2024)
print(head(passing2024))

# # --- TEST: Multi-year table (cached) ---
# # This will pull from cache if available
allRushing <- multiYearTable("rushing")
print(nrow(allRushing))

# # --- TEST: Force overwrite for 2022 only ---
rushing2022 <- makeTable("rushing", 2022, overwrite = TRUE)

# # --- TEST: Clear cache for 2021 kicking data ---
clearCache("kicking", 2021)
# clearCache()

# # --- TEST: Load cached data directly (no download) ---
# cached2020 <- makeTable("receiving", 2020, useCache = TRUE)
# print(unique(cached2020$Year))
cached2020 <- makeTable("passing", 2020, useCache = TRUE)
print(unique(cached2020$Year))
View(cached2020)

# # --- TEST: Make sure year was added correctly ---
if (!"Year" %in% colnames(rushing2021)) {
    stop("Year column missing!")
}

# # --- TEST: Print all available cached files ---
print(list.files("cache"))

# test what we receive from PFR
type <- "receiving"
year <- 2024
table <- makeTableProFootballReference(type, year)
View(table)
print(head(table))

ptable <- table
View(ptable)

rtable <- table
View(rtable)

retable <- table
View(retable)

# test cleanup on that table
table <- retable
table <- cleanUp(table, type)
View(table)
table <- addYear(table, year)
