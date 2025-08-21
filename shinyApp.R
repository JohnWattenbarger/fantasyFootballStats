# app.R

library(shiny)
library(DT)
library(openxlsx)
# source your functions and table creation scripts
source("tableCreator.R")
source("functions.R")
source("valueOverReplacement.R")

ui <- fluidPage(
  titlePanel("Fantasy Football Stats Generator"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
          details {
            margin: 10px 0;
            padding: 5px 10px;
            border: 1px solid #ccc;
            border-radius: 6px;
            background-color: #fafafa;
          }

          details summary {
            font-weight: bold;
            cursor: pointer;
            list-style: none; /* hide default disclosure triangle */
            position: relative;
            padding-left: 1.2em;
          }

          /* custom arrow icon */
          details summary::before {
            content: '▶'; 
            position: absolute;
            left: 0;
            transition: transform 0.2s ease;
          }

          details[open] summary::before {
            transform: rotate(90deg); /* arrow rotates when open */
          }
        "))
      ),
      tags$details(
        open = TRUE,  # start expanded by default
        tags$summary("Scoring Settings"),
        # Input fields for user configuration
        numericInput("currentYear", "Current Year", value = 2024, min = 2000, max = 2100),
        helpText("Last full NFL season to collect data from (so for 2025 put '2024')"),
        numericInput("passingYardMultiplier", "Passing Yard Multiplier", value = 0.04),
        helpText(".04 = 1 point per 25 yards"),
        numericInput("passingTDMultiplier", "Passing TD Multiplier", value = 4),
        numericInput("pointsPerReceptionMultiplier", "Points Per Reception Multiplier", value = 0),
        helpText("Set to 0 for no PPR, 1 for 1 point per reception, etc.")
      ),
      tags$details(
        # open = FALSE,
        tags$summary("Advanced Scoring Settings"),
          numericInput("rushingYardMultiplier", "Rushing Yard Multiplier", value = 0.1),
          numericInput("rushingTDMultiplier", "Rushing TD Multiplier", value = 6),
          numericInput("receivingYardMultiplier", "Receiving Yard Multiplier", value = 0.1),
          numericInput("teReceivingYardMultiplier", "TE Receiving Yard Multiplier", value = 0.1),
          helpText("Most leagues this will match receivingYardMultiplier. Except in TE Premium leagues"),
          numericInput("receivingTDMultiplier", "Receiving TD Multiplier", value = 6),
          numericInput("interceptionMultiplier", "Interception Multiplier", value = -2),
          numericInput("fumbleMultiplier", "Fumble Multiplier", value = -2)
      ),
      tags$details(
        open = TRUE,
        tags$summary("VoRP Fields"),
          # VORP fields
          numericInput("leagueSize", "League Size (teams)", value = 12, min = 1),
          numericInput("qbStarters", "QBs per team", value = 1, min = 0),
          numericInput("rbStarters", "RBs per team", value = 2, min = 0),
          numericInput("wrStarters", "WRs per team", value = 2, min = 0),
          numericInput("teStarters", "TEs per team", value = 1, min = 0),
          numericInput("flexSpots", "Flex spots (RB/WR/TE) per team", value = 1, min = 0)
          # checkboxGroupInput("flexPositions", "Flex positions allowed", choices = c("RB", "WR", "TE"), selected = c("RB", "WR", "TE"))
      ),
      # Custom title for Excel files
      textInput("customTitle", "Custom Title (optional)", value = ""),
      # Action buttons for running analysis and downloading files
      actionButton("runAnalysis", "Run Analysis"),
      downloadButton("downloadExcel", "Download Excel"),
      downloadButton("downloadVorp", "Download VoRP Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("VoRP Table", DTOutput("vorpTable")),
        tabPanel("QB Table", DTOutput("qbTable")),
        tabPanel("RB Table", DTOutput("rbTable")),
        tabPanel("WR Table", DTOutput("wrTable")),
        tabPanel("TE Table", DTOutput("teTable")),
      )
    )
  )
)

runAnalysisCode <- function(input, results) {
  # Set config variables from input
  currentYear <- input$currentYear
  passingYardMultiplier <- input$passingYardMultiplier
  passingTDMultiplier <- input$passingTDMultiplier
  rushingYardMultiplier <- input$rushingYardMultiplier
  rushingTDMultiplier <- input$rushingTDMultiplier
  receivingYardMultiplier <- input$receivingYardMultiplier
  teReceivingYardMultiplier <- input$teReceivingYardMultiplier
  receivingTDMultiplier <- input$receivingTDMultiplier
  pointsPerReceptionMultiplier <- input$pointsPerReceptionMultiplier
  interceptionMultiplier <- input$interceptionMultiplier
  fumbleMultiplier <- input$fumbleMultiplier
  customTitle <- input$customTitle

  # Set config variables from input
  currentYear <- input$currentYear
  passingYardMultiplier <- input$passingYardMultiplier
  passingTDMultiplier <- input$passingTDMultiplier
  rushingYardMultiplier <- input$rushingYardMultiplier
  rushingTDMultiplier <- input$rushingTDMultiplier
  receivingYardMultiplier <- input$receivingYardMultiplier
  teReceivingYardMultiplier <- input$teReceivingYardMultiplier
  receivingTDMultiplier <- input$receivingTDMultiplier
  pointsPerReceptionMultiplier <- input$pointsPerReceptionMultiplier
  interceptionMultiplier <- input$interceptionMultiplier
  fumbleMultiplier <- input$fumbleMultiplier
  customTitle <- input$customTitle

  # Set league size and starter counts
  leagueSize <- input$leagueSize
  qbStarters <- input$qbStarters
  rbStarters <- input$rbStarters
  wrStarters <- input$wrStarters
  teStarters <- input$teStarters
  flexSpots <- input$flexSpots

  starter_counts <- list(
    QB = leagueSize * qbStarters,
    RB = leagueSize * rbStarters,
    WR = leagueSize * wrStarters,
    TE = leagueSize * teStarters,
    FLEX = leagueSize * flexSpots
  )

  # You may need to set these as global variables or pass them to your functions
  # Example: QB <- multiYearTable("passing", currentYear, passingYardMultiplier, ...)
  # For now, assuming your functions use global variables

  # Run your data processing (adapt as needed)
  passing <- multiYearTable("passing")
  rushing <- multiYearTable("rushing")
  receiving <- multiYearTable("receiving")
  kicking <- multiYearTable("kicking")

  # ...merge and process as in main.R, using the input values...

  # Example for QB (adapt for other positions)
  # mergeCategories <- c("Year", "Player", "Pos", "Team", "Age", "G", "GS")
  mergeCategories <- c("Year", "Player", "Pos", "Team", "Age", "G", "GS")
  mergeCategoriesRb <- c(mergeCategories, "Fmb")

  QB <- merge(x = passing, y = rushing, by.x = mergeCategories, by.y = mergeCategories, all.x = TRUE)
  QB <- QB[QB$Pos %in% "QB", ]
  QB[is.na(QB)] <- 0
  QB$"Fantasy_Points" <- passingYardMultiplier * QB$`Pa_Yds` +
    passingTDMultiplier * QB$`Pa_TDs` +
    interceptionMultiplier * QB$`Int` +
    rushingYardMultiplier * QB$`Ru_Yds` +
    rushingTDMultiplier * QB$`Ru_TDs` +
    fumbleMultiplier * QB$`Fmb`

  # Repeat for RB, WR, TE...
  # RB <- ...
  RB <- merge(x = rushing, y = receiving, by.x = mergeCategoriesRb, by.y = mergeCategoriesRb, all.x = TRUE)
  # RB <- merge(x = rushing, y = receiving, by.x = mergeCategories, by.y = mergeCategories, all.x = TRUE)
  RB <- RB[RB$Pos %in% "RB", ]
  RB[is.na(RB)] <- 0
  RB$"Fantasy_Points" <- rushingYardMultiplier * RB$`Ru_Yds` + rushingTDMultiplier * RB$`Ru_TDs` + fumbleMultiplier * RB$`Fmb` + receivingYardMultiplier * RB$`Re_Yds` + receivingTDMultiplier * RB$`Re_TDs` + pointsPerReceptionMultiplier * RB$`Rec`

  # WR <- ...
  WR <- receiving[receiving$Pos %in% "WR", ]
  WR$"Fantasy_Points" <- receivingYardMultiplier * WR$`Re_Yds` + receivingTDMultiplier * WR$`Re_TDs` + pointsPerReceptionMultiplier * WR$`Rec` + fumbleMultiplier * WR$`Fmb`

  # TE <- ...
  TE <- receiving[receiving$Pos %in% "TE", ]
  TE$"Fantasy_Points" <- teReceivingYardMultiplier * TE$`Re_Yds` + receivingTDMultiplier * TE$`Re_TDs` + pointsPerReceptionMultiplier * TE$`Rec` + fumbleMultiplier * TE$`Fmb`

  # Create export tables (tables from most recent year)
  QB.export <- createTableToExport(QB, currentYear)
  RB.export <- createTableToExport(RB, currentYear)
  WR.export <- createTableToExport(WR, currentYear)
  TE.export <- createTableToExport(TE, currentYear)

  # Store results
  # results$QB <- QB
  # results$RB <- RB
  # results$WR <- WR
  # results$TE <- TE
  results$QB <- QB.export
  results$RB <- RB.export
  results$WR <- WR.export
  results$TE <- TE.export
}

runVorpAnalysis <- function(input, results) {
  library(dplyr)
  QB <- results$QB
  RB <- results$RB
  WR <- results$WR
  TE <- results$TE

  # Defensive checks for input tables
  if (is.null(QB) || is.null(RB) || is.null(WR) || is.null(TE)) {
    stop("One or more position tables are missing. Please run analysis first.")
  }

  # Starter counts from input
  leagueSize <- as.numeric(input$leagueSize)
  qbStarters <- as.numeric(input$qbStarters)
  rbStarters <- as.numeric(input$rbStarters)
  wrStarters <- as.numeric(input$wrStarters)
  teStarters <- as.numeric(input$teStarters)
  flexSpots <- as.numeric(input$flexSpots)

  # Defensive checks for starter counts
  if (any(is.na(c(leagueSize, qbStarters, rbStarters, wrStarters, teStarters, flexSpots)))) {
    stop("Starter counts must be numeric and not NA.")
  }

  starter_counts <- list(
    QB = leagueSize * qbStarters,
    RB = leagueSize * rbStarters,
    WR = leagueSize * wrStarters,
    TE = leagueSize * teStarters,
    FLEX = leagueSize * flexSpots
  )

  # Assign flex starters
  starter_counts <- assign_flex_starters(RB, WR, TE, starter_counts)

  top_n <- 100
  avg_QB <- getAvgPointsByRank(QB, "QB", top_n)
  avg_RB <- getAvgPointsByRank(RB, "RB", top_n)
  avg_WR <- getAvgPointsByRank(WR, "WR", top_n)
  avg_TE <- getAvgPointsByRank(TE, "TE", top_n)

  # Defensive checks for VORP tables
  for (df in list(avg_QB, avg_RB, avg_WR, avg_TE)) {
    if (!("Rank" %in% colnames(df)) || !("Avg_Fantasy_Points" %in% colnames(df))) {
      stop("VORP calculation failed: missing columns in average points tables.")
    }
    if (!is.numeric(df$Rank) || !is.numeric(df$Avg_Fantasy_Points)) {
      stop("VORP calculation failed: Rank and Avg_Fantasy_Points must be numeric.")
    }
  }

  avg_QB <- calculateVoRP(avg_QB, starter_counts$QB)
  avg_RB <- calculateVoRP(avg_RB, starter_counts$RB)
  avg_WR <- calculateVoRP(avg_WR, starter_counts$WR)
  avg_TE <- calculateVoRP(avg_TE, starter_counts$TE)

  avg_all_vorp <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)
  avg_all_vorp$Label <- paste0(avg_all_vorp$Position, avg_all_vorp$Rank)
  avg_all_vorp_sorted <- avg_all_vorp[order(-avg_all_vorp$VoRP), c("Label", "VoRP", "Avg_Fantasy_Points")]
  avg_all_vorp_sorted$VoRP <- round(avg_all_vorp_sorted$VoRP, 1)
  avg_all_vorp_sorted$Avg_Fantasy_Points <- round(avg_all_vorp_sorted$Avg_Fantasy_Points, 1)

  results$VoRP <- avg_all_vorp_sorted
}

server <- function(input, output, session) {
  # Store results in reactive values
  results <- reactiveValues(
    QB = NULL, RB = NULL, WR = NULL, TE = NULL
  )

  observeEvent(input$runAnalysis, {
    runAnalysisCode(input, results)
    runVorpAnalysis(input, results)
  })

  # Render tables
  output$qbTable <- renderDT({
    req(results$QB)
    # datatable(results$QB)
    datatable(results$QB, options = list(pageLength = 25, order = list(list( which(colnames(results$QB) == "Fantasy_Points"), 'desc'))))
  })
  output$rbTable <- renderDT({ req(results$RB); datatable(results$RB, options = list(pageLength = 25, order = list(list( which(colnames(results$RB) == "Fantasy_Points"), 'desc')))) })
  output$wrTable <- renderDT({ req(results$WR); datatable(results$WR, options = list(pageLength = 25, order = list(list( which(colnames(results$WR) == "Fantasy_Points"), 'desc')))) })
  output$teTable <- renderDT({ req(results$TE); datatable(results$TE, options = list(pageLength = 25, order = list(list( which(colnames(results$TE) == "Fantasy_Points"), 'desc')))) })
  output$vorpTable <- renderDT({
    req(results$VoRP)
    datatable(
      results$VoRP,
      options = list(
        order = list(list(which(colnames(results$VoRP) == "VoRP"), 'desc')),
        pageLength = 25   # set default to 25 entries
      )
    )
  })

  # Download Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0(input$currentYear, "_Stats", if (input$customTitle != "") paste0("_", input$customTitle) else "", ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "QB")
      writeData(wb, "QB", results$QB)
      addWorksheet(wb, "RB"); writeData(wb, "RB", results$RB)
      addWorksheet(wb, "WR"); writeData(wb, "WR", results$WR)
      addWorksheet(wb, "TE"); writeData(wb, "TE", results$TE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Download VoRP Analysis
  output$downloadVorp <- downloadHandler(
    filename = function() {
      paste0("VoRP_Analysis_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Always run analysis before download
      runAnalysisCode(input, results)
      runVorpAnalysis(input, results)
      
      # Create league details dataframe
      league_details <- data.frame(
          Setting = c("Teams", "QBs", "RBs", "WRs", "TEs", "Flex (RB/WR/TE)"),
          Value   = c(leagueSize, qbStarters, rbStarters, wrStarters, teStarters, flexSpots)
      )

      wb <- createWorkbook()
      addWorksheet(wb, "All Positions")
      addWorksheet(wb, "QB")
      addWorksheet(wb, "RB")
      addWorksheet(wb, "WR")
      addWorksheet(wb, "TE")

      # Create the 1st "All Positions" sheet
      writeData(wb, "All Positions", avg_all_vorp_sorted)
      # Add league details data to "All Positions" starting at column E (col = 5), row 1
      writeData(wb, "All Positions", league_details, startCol = 5, startRow = 1)

      ### Add extra styling

      # All Positions sheet styling

      # Assume avg_all_vorp_sorted is your table with columns: Label, VoRP, Avg_Fantasy_Points
      # leagueSize is your number of teams

      # --- 1. Add a line after last positive VoRP player ---
      last_positive_row <- max(which(avg_all_vorp_sorted$VoRP >= 0))
      # line_style <- createStyle(border = "bottom", borderColour = "#000000", borderStyle = "thick")
      # line_style <- createStyle(border = "bottom", borderStyle = "thick", borderColour = "#000000")
      # line_style <- createStyle(border = "bottom", borderStyle = "thick")
      line_style <- openxlsx::createStyle(
        border = "BOTTOM",      # must match exactly LEFT, RIGHT, TOP, BOTTOM
        borderStyle = "thick", 
        borderColour = "#000000"
      )

      addStyle(
        wb,
        sheet = "All Positions",
        style = line_style,
        rows = last_positive_row + 1,  # +1 if header row exists
        cols = 1:ncol(avg_all_vorp_sorted),
        gridExpand = TRUE,
        stack = TRUE
      )

      # --- 2. Make every nth player bold for each position ---
      bold_style <- createStyle(textDecoration = "bold")
      positions <- unique(sub("[0-9]+$", "", avg_all_vorp_sorted$Label))  # e.g., "QB", "RB", "WR", "TE"

      for (pos in positions) {
        pos_rows <- grep(paste0("^", pos), avg_all_vorp_sorted$Label)  # indices of that position
        # Apply bold to every nth player, where n = leagueSize
        bold_rows <- pos_rows[seq(leagueSize, length(pos_rows), by = leagueSize)]
        
        addStyle(
          wb,
          sheet = "All Positions",
          style = bold_style,
          rows = bold_rows + 1,  # +1 for header row
          cols = 1:ncol(avg_all_vorp_sorted),
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      # League details table styling

      # Add border style
      # border_style <- createStyle(borderStyle = "thin")
      border_style <- createStyle(border = c("TOP","BOTTOM","LEFT","RIGHT"), borderStyle = "thin")

      # Add borders around league details table
      addStyle(
        wb,
        sheet = "All Positions",
        style = border_style,
        rows = 1:(nrow(league_details) + 1),  # +1 if you want to include header
        cols = 5:6,
        gridExpand = TRUE,
        stack = TRUE
      )

      writeData(wb, "All Positions", avg_all_vorp_sorted)
      # Apply styles for Label (1st) column based on position
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

      writeData(wb, "QB", avg_QB)
      writeData(wb, "RB", avg_RB)
      writeData(wb, "WR", avg_WR)
      writeData(wb, "TE", avg_TE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)