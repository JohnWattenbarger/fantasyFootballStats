# import required packages
import pandas as pd

import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import pylab as pl
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
import math

import sys
from sklearn.cluster import KMeans
from sklearn.metrics.pairwise import euclidean_distances
from numpy import inf
from mpl_toolkits.mplot3d import Axes3D

# new
from pandas import ExcelWriter
from pathlib import Path


np.set_printoptions(threshold=1000, linewidth=1000)

# get data for training
currentYear = 2024
sheetName = './data/' + str(currentYear) + ' Data Analysis Stats.xlsx'
qb = pd.read_excel(sheetName, sheet_name='QB')
rb = pd.read_excel(sheetName, sheet_name='RB')
wr = pd.read_excel(sheetName, sheet_name='WR')
te = pd.read_excel(sheetName, sheet_name='TE')

print(list(qb))

sheetName = './data/' + str(currentYear) + ' Stats.xlsx'
qb_currentYear = pd.read_excel(sheetName, sheet_name='QB')
rb_currentYear = pd.read_excel(sheetName, sheet_name='RB')
wr_currentYear = pd.read_excel(sheetName, sheet_name='WR')
te_currentYear = pd.read_excel(sheetName, sheet_name='TE')

predictionColumn = 'Next Year Fantasy Points'

def runLinearRegression(df, dependentColumnName=predictionColumn):
    # df.replace([np.inf, -np.inf], np.nan)
    # df = df.dropna()

    print("before: " + str(df.isnull().sum()))
    df = df.replace([np.inf, -np.inf], np.nan)
    # data = data.dropna()
    df = df.fillna(0)
    print("after: " + str(df.isnull().sum()))

    X = df.drop(dependentColumnName, axis=1)
    y = df[[dependentColumnName]]

    print('\n')
    splitValue = .2

    print('Linear Regression Results:')
    print('Split Size: ' + str(splitValue))

    # Splits X and y in 80-20% for training/testing
        # returns a tuple with: (matrix, matrix, vector, vector)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=splitValue, random_state=1)

    # Creates an instance of Linear Regression (that's the model we
    # are using)
    regression_model = LinearRegression()

    # invokes the fit method with the training data
    regression_model.fit(X_train, y_train)

    # prints the coefficients (rounds to 4 decimals for clarity purposes)
    for i, col_name in enumerate(X_train.columns):
        print("Coeff for {} : {}".format(col_name, round(regression_model.coef_[0][i], 4)))

    # gets the intercept
    intercept = regression_model.intercept_[0]

    print("Intercept: {}".format(intercept))

    rsquared = regression_model.score(X_test, y_test)

    y_predict = regression_model.predict(X_test)

    regression_model_mse = mean_squared_error(y_predict, y_test)

    mse =  math.sqrt(regression_model_mse)

    print('MSE: ' , math.sqrt(regression_model_mse))
    print('RSquared: ' + str(rsquared))
    print()


def getLinearRegression(df, dependentColumnName=predictionColumn):
    # df.replace([np.inf, -np.inf], np.nan)
    # df = df.dropna()

    print("before: " + str(df.isnull().sum()))
    df = df.replace([np.inf, -np.inf], np.nan)
    # data = data.dropna()
    df = df.fillna(0)
    print("after: " + str(df.isnull().sum()))

    X = df.drop(dependentColumnName, axis=1)
    y = df[[dependentColumnName]]

    splitValue = .2

    # Splits X and y in 80-20% for training/testing
        # returns a tuple with: (matrix, matrix, vector, vector)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=splitValue, random_state=1)

    # Creates an instance of Linear Regression (that's the model we
    # are using)
    regression_model = LinearRegression()

    # invokes the fit method with the training data
    regression_model.fit(X_train, y_train)

    # gets the intercept
    intercept = regression_model.intercept_[0]

    rsquared = regression_model.score(X_test, y_test)
    y_predict = regression_model.predict(X_test)
    regression_model_mse = mean_squared_error(y_predict, y_test)
    mse =  math.sqrt(regression_model_mse)

    return regression_model

def dropColumns(data, type, dropExtra=False):
    # data.replace([np.inf, -np.inf], np.nan)
    # # data = data.dropna()
    # data.fillna(0)

    # drop non-numeric columns
    if type == "QB":
        itemsToKeep = ['G', 'Cmp', 'Pa_Att', 'Cmp.', 'Pa_Yds', 'Pa_TDs', 'Int', 'Pa_Yds_G', 'ANY.A', 'Ru_Yds', 'Ru_TDs', 'Ru_Yds_G', 'Fantasy_Points', 'Next Year Fantasy Points']
        itemsToKeep = set(list(data)) &  set(itemsToKeep)
        data = data[list(itemsToKeep)]
    if type == "RB":
        itemsToKeep = ['G', 'Fmb', 'Ru_Att', 'Ru_Yds', 'Ru_TDs', 'Ru_Yds_A', 'Ru_Yds_G', 'Tgt', 'Rec', 'Re_Yds', 'Re_TDs', 'Re_Yds_G', 'Fantasy_Points', 'Next Year Fantasy Points']
        itemsToKeep = set(list(data)) &  set(itemsToKeep)
        data = data[list(itemsToKeep)]
    if type == "WR" or type == "TE":
        itemsToKeep = ['G', 'Tgt', 'Rec', 'Re_Yds', 'Yd_Rec', 'Re_TDs', 'Re_Yds_G', 'Fmb', 'Fantasy_Points', 'Next Year Fantasy Points']
        itemsToKeep = set(list(data)) &  set(itemsToKeep)
        data = data[list(itemsToKeep)]
    # if dropExtra:
    #     data = data.drop(['Height', 'Weight', 'Age'], axis=1)

    data = data.fillna(0)
    # print('Table ' + type + ', any nan? ' + str(np.any(np.isnan(data))))

    return data

# print(list(dropColumns(qb, "QB", False)))
print(dropColumns(qb, "QB", False).head(10))


def sort(data):
    # sorting data frame by name
    data.sort_values("Fantasy_Points", axis = 0, ascending = False,
                 inplace = True)
    return data

#########################################################
########################   QB   #########################
#########################################################

# get linear regression model
dropped = dropColumns(qb, "QB")
regression = getLinearRegression(dropped, predictionColumn)

# test
runLinearRegression(dropped)

# make predictions
dropped = dropColumns(qb_currentYear, "QB", True)
print("!!! DROPPED: " + str(dropped.isnull().sum()))
prediction = regression.predict(dropped)

# add predictions to original column
# convert to dataframe
df = pd.DataFrame.from_records(prediction)
qb_currentYear['Prediction'] = df.loc[:,0]

# test
print(sort(qb_currentYear).head(10))

#########################################################
########################   RB   #########################
#########################################################

# only use the top 32 players each year
# rb = setup(rb, "RB")
# rb_currentYear = setup(rb_currentYear, "RB")

# get linear regression model
dropped = dropColumns(rb, "RB")
regression = getLinearRegression(dropped)
prediction = regression.predict(dropColumns(rb_currentYear, "RB", True))

# test
runLinearRegression(dropped)

# add prediction to current year stats
df = pd.DataFrame.from_records(prediction)
rb_currentYear['Prediction'] = df.loc[:,0]

# test
print(rb_currentYear.head(10))
print("second after: " + str(len(rb_currentYear.index)))

#########################################################
########################   WR   #########################
#########################################################

# only use the top 32 players each year
# wr = setup(wr, "WR")
# wr_currentYear = setup(wr_currentYear, "WR")

# get linear regression model
dropped = dropColumns(wr, "WR")
print("dropped WR")
print(dropped.head(10))
regression = getLinearRegression(dropped)
prediction = regression.predict(dropColumns(wr_currentYear, "WR", True))

# test
runLinearRegression(dropped)

# add prediction to current year stats
df = pd.DataFrame.from_records(prediction)
wr_currentYear['Prediction'] = df.loc[:,0]

# test
print(wr_currentYear.head(10))


#########################################################
########################   TE   #########################
#########################################################

# only use the top 32 players each year
# te = setup(te, "TE")
# te_currentYear = setup(te_currentYear, "TE")

# get linear regression model
dropped = dropColumns(te, "TE")
regression = getLinearRegression(dropped)
prediction = regression.predict(dropColumns(te_currentYear, "TE", True))

# test
runLinearRegression(dropped)

# add prediction to current year stats
df = pd.DataFrame.from_records(prediction)
te_currentYear['Prediction'] = df.loc[:,0]

# test
print(te_currentYear.head(10))

#########################################################
#######################  Export  ########################
#########################################################

# NOTE: not used yet
def toExport(data, type):
    data = data.drop(['Year', 'Pos'], axis=1)
    if (type == "QB"):
        data['Tot_Yds'] = data['Pa_Yds'] + data['Ru_Yds']
        data['Tot_TDs'] = data['Pa_TDs'] + data['Ru_TDs']
        data['Turnovers'] = data['Int'] + data['Fmb']
        # data = data.drop(['Int', 'Fmb'], axis=1)
        data['TD_Ratio'] = data['Pa_TDs'] / data['Turnovers']
        # data['Pct_Ru'] = data['Ru_Yds'] / data['Tot_Yds'] * 100
        data = data.drop(['Pa_Lng'], axis=1)
    if (type == "RB"):
        # data['Fum'] = data['Ru_Fum'] + data['Re_Fum']
        # data['20_Yds'] = data['Ru_20'] + data['Re_20']
        data['Tot_Yds'] = data['Ru_Yds'] + data['Re_Yds']
        data['Tot_TDs'] = data['Ru_TDs'] + data['Re_TDs']
        data['Tot_Atts'] = data['Ru_Att'] + data['Rec']
        data['Tot_Avg'] = data['Tot_Yds'] / data['Tot_Atts']
        data['catch_rate'] = data['Rec'] / data['Tgt']
        data = data.drop(['Ru_Lng', 'Re_Lng', 'Re_1D', 'Ru_1D'], axis=1)
    if (type == "WR" or type == "TE"):
        data['catch_rate'] = data['Rec'] / data['Tgt']
        data = data.drop(['Re_Lng'], axis=1)
    return data

# Export the data to a spreadsheet
sheetName = './data/' + str(currentYear) + ' Predictions.xlsx'

# Use context manager to automatically save and close the writer
with pd.ExcelWriter(sheetName, engine='xlsxwriter') as writer:
    qb_export = toExport(qb_currentYear, "QB")
    rb_export = toExport(rb_currentYear, "RB")
    wr_export = toExport(wr_currentYear, "WR")
    te_export = toExport(te_currentYear, "TE")

    print(qb_export.head(10))

    qb_export.to_excel(writer, sheet_name="QB", index=False)
    rb_export.to_excel(writer, sheet_name="RB", index=False)
    wr_export.to_excel(writer, sheet_name="WR", index=False)
    te_export.to_excel(writer, sheet_name="TE", index=False)










# # # test
# # test = te
# # # test['Total_Yds'] = test['Ru_Yds'] + test['Re_Yds']
# # # test = test.drop(['Ru_Yds', 'Re_Yds'], axis=1)
# # test = dropColumns(test, "TE")
# # # test = test.drop(['PLAYER', 'Year', 'Position', 'TEAM'], axis=1)
# # runLinearRegression(test)
# # # runLinearRegression(dropColumns(rb, "RB"))
#
#
#
#
#
#
#
#
#
# # # test
# #
# # predictionColumn = 'Next Year Fantasy Points'
# #
# # qb_sub = qb.drop(['PLAYER', 'Year', 'Position', 'TEAM'], axis=1)
# #
# # runLinearRegression(qb_sub, predictionColumn)
# #
# # print('*****************************************')
# #
# # # try removing coefficients < .1 from 0
# # test = qb_sub.drop(['P_Yds', 'R_Yds', 'R_Yds_G', 'First_Dn'], axis=1)
# # runLinearRegression(test, predictionColumn)
# #
# # # try removing seemingly unimportant columns
# # test = qb_sub.drop(['P_Long', 'SACK', 'RATE', 'R_Long', '20', 'First_Dn'], axis=1)
# # runLinearRegression(test, predictionColumn)
# #
# #
# # # combine turnovers and remove yards
# # test['turnovers'] = test['INT'] + test['FUM']
# # test = test.drop(['INT', 'FUM'], axis=1)
# # runLinearRegression(test, predictionColumn)
# #
# # # Next: read the 2018 qb stats and try to predict
# # qb_regression = getLinearRegression(test, predictionColumn)
# #
# # current = qb_currentYear.drop(['P_Long', 'SACK', 'RATE', 'R_Long', '20', 'First_Dn'], axis=1)
# # current = current.drop(['PLAYER', 'Year', 'Position', 'TEAM'], axis=1)
# # current['turnovers'] = current['INT'] + current['FUM']
# # current = current.drop(['INT', 'FUM'], axis=1)
# #
# # prediction = qb_regression.predict(current)
# #
# # print(type(qb_currentYear))
# # print(type(prediction))
# #
# # dataframe = pd.DataFrame.from_records(prediction)
# # print(dataframe.head(5))
# #
# # print("******************")
# #
# # qb_currentYear['Prediction'] = dataframe.ix[:,0]
# # first = dataframe.ix[:,0]
# # print(qb_currentYear.head(5))
