Files:
  - Code:
    > tableCreator.R --> contains functions to parse web and generate data tables
    > main.R --> uses tableCreator to generate different tables/visuals, and exports 2 excel files (<Year> Stats.xlsx and <Year> Data Analysis Stats.xlsx)
    > linearRegression --> python script that reads the Data Analysis file, runs linear regression on it, and generates predictive results in <Year> Predictions.xlsx
  - Data:
    > <Year> Stats.xlsx --> Raw stats gathered from web scraping (for the past year)
    > <Year> Data Analysis.xlsx --> Trimmed version of the above excel sheet. only keeps useful stats for prediction
    > <Year> Predictions.xlsx --> Contains stats, and predictions for each position. This should be used for any further data analysis

Running:
  - R:
    > open VS Code
    > open config.R --> File with current year and other values I might need to change
    > switch the "currentYear" variable (represents the last year of stats to gather. Likely will be 1 year before the actual current year)
    > switch the multiplier variables to match league settings
    > ensure the "createExcelDocs" variable is set to TRUE
    > open main.R
    > select all (ctrl + a) and run (ctrl + enter)
    > should see 2 new tables in the data folder (<current year> Data Analysis Stats which has 10 years of stats, and <current year> Stats which has just last years stats)

  - Python
    > open Atom --> linearRegression.py (now using VS Code for this as well)
    > change current year variable to match R studio's variable
    > run all (deselect any text, then ctrl + shift + b)
    > should see a new table in the data folder (<current year> Predictions which shows the current year stats and a prediction of next years stats)
    > Note: might have to install software updates using `pip install <library> --user`

