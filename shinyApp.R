# app.R

library(shiny)
library(DT)
library(openxlsx)
# source your functions and table creation scripts
source("tableCreator.R")
source("functions.R")

print(getwd())

ui <- fluidPage(
  titlePanel("Fantasy Football Stats Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("currentYear", "Current Year", value = 2024, min = 2000, max = 2100),
      helpText("Last full NFL season to collect data from (so for 2025 put '2024')"),
      numericInput("passingYardMultiplier", "Passing Yard Multiplier", value = 0.04),
      helpText(".04 = 1 point per 25 yards"),
      numericInput("passingTDMultiplier", "Passing TD Multiplier", value = 4),
      numericInput("rushingYardMultiplier", "Rushing Yard Multiplier", value = 0.1),
      numericInput("rushingTDMultiplier", "Rushing TD Multiplier", value = 6),
      numericInput("receivingYardMultiplier", "Receiving Yard Multiplier", value = 0.1),
      numericInput("teReceivingYardMultiplier", "TE Receiving Yard Multiplier", value = 0.1),
      helpText("Most leagues this will match receivingYardMultiplier. Except in TE Premium leagues"),
      numericInput("receivingTDMultiplier", "Receiving TD Multiplier", value = 6),
      numericInput("pointsPerReceptionMultiplier", "Points Per Reception Multiplier", value = 0),
      helpText("Set to 0 for no PPR, 1 for 1 point per reception, etc."),
      numericInput("interceptionMultiplier", "Interception Multiplier", value = -2),
      numericInput("fumbleMultiplier", "Fumble Multiplier", value = -2),
      textInput("customTitle", "Custom Title (optional)", value = ""),
      actionButton("runAnalysis", "Run Analysis"),
      downloadButton("downloadExcel", "Download Excel"),
      downloadButton("downloadVorp", "Download VoRP Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("QB Table", DTOutput("qbTable")),
        tabPanel("RB Table", DTOutput("rbTable")),
        tabPanel("WR Table", DTOutput("wrTable")),
        tabPanel("TE Table", DTOutput("teTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Store results in reactive values
  results <- reactiveValues(
    QB = NULL, RB = NULL, WR = NULL, TE = NULL
  )

  observeEvent(input$runAnalysis, {
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
  })

  # Render tables
  output$qbTable <- renderDT({
    req(results$QB)
    # datatable(results$QB)
    datatable(results$QB, options = list(order = list(list( which(colnames(results$QB) == "Fantasy_Points"), 'desc'))))
  })
  output$rbTable <- renderDT({ req(results$RB); datatable(results$RB, options = list(order = list(list( which(colnames(results$RB) == "Fantasy_Points"), 'desc')))) })
  output$wrTable <- renderDT({ req(results$WR); datatable(results$WR, options = list(order = list(list( which(colnames(results$WR) == "Fantasy_Points"), 'desc')))) })
  output$teTable <- renderDT({ req(results$TE); datatable(results$TE, options = list(order = list(list( which(colnames(results$TE) == "Fantasy_Points"), 'desc')))) })

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
      # VoRP analysis code (adapted for Shiny)
      library(dplyr)
      # Use results$QB, results$RB, etc. (already filtered for current year)
      QB <- results$QB
      RB <- results$RB
      WR <- results$WR
      TE <- results$TE

      # Helper function to get average points by rank
      getAvgPointsByRank <- function(df, pos, top_n = 100) {
        df_pos <- df[df$Pos == pos, ]
        df_pos <- df_pos[order(-df_pos$Fantasy_Points), ]
        df_pos$Rank <- rank(-df_pos$Fantasy_Points, ties.method = "first")
        df_ranked <- df_pos[df_pos$Rank <= top_n, ]
        avg_points <- df_ranked %>%
          group_by(Rank) %>%
          summarise(Avg_Fantasy_Points = mean(Fantasy_Points)) %>%
          mutate(Position = pos)
        return(avg_points)
      }

      starter_counts <- list(QB = 24, RB = 36, WR = 48, TE = 12)
      top_n <- 100

      avg_QB <- getAvgPointsByRank(QB, "QB", top_n)
      avg_RB <- getAvgPointsByRank(RB, "RB", top_n)
      avg_WR <- getAvgPointsByRank(WR, "WR", top_n)
      avg_TE <- getAvgPointsByRank(TE, "TE", top_n)

      avg_all <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)

      calculateVoRP <- function(df, starter_count) {
        replacement_point <- df$Avg_Fantasy_Points[df$Rank == starter_count]
        df$VoRP <- df$Avg_Fantasy_Points - replacement_point
        return(df)
      }

      avg_QB <- calculateVoRP(avg_QB, starter_counts$QB)
      avg_RB <- calculateVoRP(avg_RB, starter_counts$RB)
      avg_WR <- calculateVoRP(avg_WR, starter_counts$WR)
      avg_TE <- calculateVoRP(avg_TE, starter_counts$TE)

      avg_all_vorp <- bind_rows(avg_QB, avg_RB, avg_WR, avg_TE)
      avg_all_vorp$Label <- paste0(avg_all_vorp$Position, avg_all_vorp$Rank)
      avg_all_vorp_sorted <- avg_all_vorp[order(-avg_all_vorp$VoRP), c("Label", "VoRP", "Avg_Fantasy_Points")]

      wb <- createWorkbook()
      addWorksheet(wb, "All Positions")
      addWorksheet(wb, "QB")
      addWorksheet(wb, "RB")
      addWorksheet(wb, "WR")
      addWorksheet(wb, "TE")
      writeData(wb, "All Positions", avg_all_vorp_sorted)
      writeData(wb, "QB", avg_QB)
      writeData(wb, "RB", avg_RB)
      writeData(wb, "WR", avg_WR)
      writeData(wb, "TE", avg_TE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)