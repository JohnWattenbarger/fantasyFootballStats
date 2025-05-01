# VoRP_analysis.R

# Assumes you have already run your main.R script to get the QB, RB, WR, TE tables with Fantasy_Points column

library(dplyr)
library(openxlsx)

# source("main.R")
# source("config") --> Might need to manually change export config to FALSE after source() call

# Customize these values for your league setup
# TODO: add these values to config.R
starter_counts <- list(
    QB = 24, # e.g., 2 QBs x 12 teams
    RB = 36,
    WR = 48,
    TE = 12
)

# Helper function to get average points per position rank
getAvgPointsByRank <- function(df, pos, top_n = 100) {
    df_pos <- df[df$Pos == pos, ]
    df_pos <- df_pos[order(df_pos$Year, -df_pos$Fantasy_Points), ]

    df_pos$Rank <- ave(df_pos$Fantasy_Points, df_pos$Year, FUN = function(x) rank(-x, ties.method = "first"))
    df_ranked <- df_pos[df_pos$Rank <= top_n, ]

    avg_points <- df_ranked %>%
        group_by(Rank) %>%
        summarise(Avg_Fantasy_Points = mean(Fantasy_Points)) %>%
        mutate(Position = pos)

    return(avg_points)
}

# Get average points by rank for each position
top_n <- 100 # can adjust based on how deep you want to go
avg_QB <- getAvgPointsByRank(QB, "QB", top_n)
avg_RB <- getAvgPointsByRank(RB, "RB", top_n)
avg_WR <- getAvgPointsByRank(WR, "WR", top_n)
avg_TE <- getAvgPointsByRank(TE, "TE", top_n)

# Combine all positions
avg_all <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)

# Calculate VoRP (Fantasy Points - Replacement level)
calculateVoRP <- function(df, starter_count) {
    replacement_point <- df$Avg_Fantasy_Points[df$Rank == starter_count]
    df$VoRP <- df$Avg_Fantasy_Points - replacement_point
    return(df)
}

avg_QB <- calculateVoRP(avg_QB, starter_counts$QB)
avg_RB <- calculateVoRP(avg_RB, starter_counts$RB)
avg_WR <- calculateVoRP(avg_WR, starter_counts$WR)
avg_TE <- calculateVoRP(avg_TE, starter_counts$TE)

# Recombine with avg_fpts included
avg_all_vorp <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)

# Create a sortable list of all positions by VoRP
avg_all_vorp$Label <- paste0(avg_all_vorp$Position, avg_all_vorp$Rank)
avg_all_vorp_sorted <- avg_all_vorp[order(-avg_all_vorp$VoRP), c("Label", "VoRP", "Avg_Fantasy_Points")]

# Extract individual position tables from list
qb_avg_vorp <- avg_all_vorp[["QB"]]
rb_avg_vorp <- avg_all_vorp[["RB"]]
wr_avg_vorp <- avg_all_vorp[["WR"]]
te_avg_vorp <- avg_all_vorp[["TE"]]

# Build optional suffix
suffix <- if (!is.null(customTitle) && customTitle != "") paste0("_", customTitle) else ""
# Final spreadsheet name with optional suffix
spreadsheetName <- paste0("./data/VoRP_Analysis.xlsx", suffix, ".xlsx")

# Create workbook
wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "All Positions")
addWorksheet(wb, "QB")
addWorksheet(wb, "RB")
addWorksheet(wb, "WR")
addWorksheet(wb, "TE")

# Write data to sheets
writeData(wb, "All Positions", avg_all_vorp_sorted)
writeData(wb, "QB", avg_QB)
writeData(wb, "RB", avg_RB)
writeData(wb, "WR", avg_WR)
writeData(wb, "TE", avg_TE)

# Save workbook
saveWorkbook(wb, spreadsheetName, overwrite = TRUE)


# Done!
