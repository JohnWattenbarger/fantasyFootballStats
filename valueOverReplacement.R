# VoRP_analysis.R

# Assumes you have already run your main.R script to get the QB, RB, WR, TE tables with Fantasy_Points column

library(dplyr)
library(openxlsx)

# source("main.R")
# source("config") --> Might need to manually change export config to FALSE after source() call

assign_flex_starters <- function(RB, WR, TE, starter_counts) {
  # Get remaining players after starters
  RB_sorted <- RB[order(-RB$Fantasy_Points), ]
  WR_sorted <- WR[order(-WR$Fantasy_Points), ]
  TE_sorted <- TE[order(-TE$Fantasy_Points), ]

  RB_remaining <- RB_sorted[(starter_counts$RB + 1):nrow(RB_sorted), ]
  WR_remaining <- WR_sorted[(starter_counts$WR + 1):nrow(WR_sorted), ]
  TE_remaining <- TE_sorted[(starter_counts$TE + 1):nrow(TE_sorted), ]

    # Ensure all data frames have the same columns
    all_cols <- unique(c(colnames(RB_remaining), colnames(WR_remaining), colnames(TE_remaining)))
    for (col in all_cols) {
        if (!(col %in% colnames(RB_remaining))) RB_remaining[[col]] <- NA
        if (!(col %in% colnames(WR_remaining))) WR_remaining[[col]] <- NA
        if (!(col %in% colnames(TE_remaining))) TE_remaining[[col]] <- NA
    }
    RB_remaining <- RB_remaining[, all_cols]
    WR_remaining <- WR_remaining[, all_cols]
    TE_remaining <- TE_remaining[, all_cols]

    # Combine remaining flex-eligible players
    flex_pool <- rbind(RB_remaining, WR_remaining, TE_remaining)

  # Pick top N flex players
  flex_pool <- flex_pool[order(-flex_pool$Fantasy_Points), ]
  flex_starters <- flex_pool[1:starter_counts$FLEX, ]

  # Count how many flex starters per position
  flex_counts <- table(flex_starters$Pos)
  # Add to starter_counts
  starter_counts$RB <- starter_counts$RB + ifelse(!is.na(flex_counts["RB"]), flex_counts["RB"], 0)
  starter_counts$WR <- starter_counts$WR + ifelse(!is.na(flex_counts["WR"]), flex_counts["WR"], 0)
  starter_counts$TE <- starter_counts$TE + ifelse(!is.na(flex_counts["TE"]), flex_counts["TE"], 0)

  return(starter_counts)
}

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

# Calculate VoRP (Fantasy Points - Replacement level)
calculateVoRP <- function(df, starter_count) {
    replacement_point <- df$Avg_Fantasy_Points[df$Rank == starter_count]
    df$VoRP <- df$Avg_Fantasy_Points - replacement_point
    return(df)
}

# Funcion to run VORP analysis and generate Excel file
run_vorp_analysis <- function() {
    # Customize these values for your league setup
    # TODO: add these values to config.R
    # TODO: change these to have a team count multiplied by position counts, and use Flex values
    starter_counts <- list(
        QB = 24, # e.g., 2 QBs x 12 teams
        # RB = 36,
        RB = 24,
        # WR = 48,
        WR = 24,
        TE = 12,
        Flex = 24
    )

    # Get average points by rank for each position
    top_n <- 100 # can adjust based on how deep you want to go
    avg_QB <- getAvgPointsByRank(QB, "QB", top_n)
    avg_RB <- getAvgPointsByRank(RB, "RB", top_n)
    avg_WR <- getAvgPointsByRank(WR, "WR", top_n)
    avg_TE <- getAvgPointsByRank(TE, "TE", top_n)

    # Combine all positions
    avg_all <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)

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

    # Create league details dataframe
    league_details <- data.frame(
        Setting = c("Teams", "QB Starters", "RB Starters", "WR Starters", "TE Starters", "Flex Starters"),
        Value   = c(12, starter_counts$QB, starter_counts$RB, starter_counts$WR, starter_counts$TE, starter_counts$Flex)
    )
    writeData(wb, "All Positions", avg_all_vorp_sorted)

    # Apply styles for Label (1st) column in "All Positions" sheet
    style_QB <- createStyle(fontColour = "#00AA00") # green
    style_RB <- createStyle(fontColour = "#0000FF") # blue
    style_WR <- createStyle(fontColour = "#FF0000") # red
    style_TE <- createStyle(fontColour = "#FF8800") # orange

    labels <- avg_all_vorp_sorted$Label
    for (i in seq_along(labels)) {
        if (startsWith(labels[i], "QB")) {
            addStyle(wb, "All Positions", style_QB, rows = i + 1, cols = 1, gridExpand = TRUE)
        } else if (startsWith(labels[i], "RB")) {
            addStyle(wb, "All Positions", style_RB, rows = i + 1, cols = 1, gridExpand = TRUE)
        } else if (startsWith(labels[i], "WR")) {
            addStyle(wb, "All Positions", style_WR, rows = i + 1, cols = 1, gridExpand = TRUE)
        } else if (startsWith(labels[i], "TE")) {
            addStyle(wb, "All Positions", style_TE, rows = i + 1, cols = 1, gridExpand = TRUE)
        }
    }


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
}
