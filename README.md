# R_Programming_Forecasting_Models
Time Series Forecasting Models In R Programming

Disclaimer: 
This code snippet is developed with an intension for generic use only. I hereby declare that the code was not part of any of my professional development work. Also, no sensitive or confidential data sources are used for this development. 

Description: 
These scripts are to be used when a user wants to select the best forecasting model among the pool of models like ARIMA, Unobserved Component Model (UCM) and Linear Prophet. These scripts are capable of handling multiple time series data on a single execution however forecasting models are restricted to univariate only. Please refer to the attached Sample Time Series Data.xlsx for the format of the input which needs to be maintained. The script will split each series in to training and testing sample in the proportion of 80:20. The best model from each genre will be selected from the training sample and the test sample will be scored using the best model. The final output will produce training and testing sample MAPE along with best fitted model name based test sample MAPE. The script will be useful in model selection in large scale forecasting exercise. However, this script is not meant for time ahead forecast generation. 

ARIMA Model Parameters: 
Following model parameters has been considered in the model development: 
1. Model with drift has been considered 
2. Non-stationary model has been considered 
3. Models with a non-zero mean has been considered 
4. Seasonal & Non-Seasonal Unit Root Tests are considered 
5. AICC, AIC, BIC are considered as information criteria based on which model selection is done. 
6. Maximum order up till 5 has been considered for non-seasonal component of AR and MA. 
7. Maximum order up till 52 has been considered for seasonal component of AR and MA assuming weekly data. This parameter must be fine-tuned accordingly. 
8. Differencing order is tried up till 3. 

UCM Model Parameters: 
Following model parameters has been considered in the model development: 
1. Log transformation before modelling is taken to make the data less volatile 
2. Irregular, Level, Slope, Seasonal components are modelled separately. 
3. Seasonal length is set as 52 assuming weekly data. This parameter must be fine-tuned accordingly. 

Linear Prophet Model Parameters: 
Following component has been considered in the model development: 
1. Yearly Seasonality 
2. Weekly Seasonality 
3. Linear Growth for trend component 

Input For The User Defined Functions: 
1. source_file_name: The character string for source xlsx file including the file extension. The First column must be having the date stamp ideally as Excel Date format, all subsequent columns should be individual time series. 
2. Date_stamp: The name of the first column which is the date stamp column 
3. n_obs: The no of rows present in the data set. Ideally this is the maximum no of date stamp available. 

Steps For Execution: 
1. Copy these codes on current working directory of R Session. 
2. Put the input file on current working directory of R Session. Please note algorithm will accept xlsx format only. Please find the attached sample data for the format that needs to be maintained. 
3. Execute following lines of codes: 
source_file_name <- "Sample Time Series Data.xlsx" ##### xlsx files only 
date_stamp <- "Date" ##### Name of The Date Stamp 
n_obs <- 215 ##### No of Rwos In Input File 
source("Model Selection.R") 
df_mape <- best_model_selection(source_file_name, date_stamp, n_obs) 

Compatibility: 
The code is developed and tested on RStudio (Version 1.0.44) using R-3.3.2
