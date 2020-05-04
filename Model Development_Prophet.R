################################################################################################
############################ Time Series Model Selection Algorithm #############################
#########################    Developer:   Debasish Dutta            ############################
#########################    Date:   	  December 2017             ############################
#################################### Helper Script 2 out of 3 ##################################
################################################################################################

############################################################################################
################### Data Preparation & Creation of Train & Test Sample #####################
############################################################################################
data_preparation <- function(ts_df, time_stamp) {
  train_df <- data.frame()
  test_df <- data.frame()
  
  for (i in names(ts_df)[-which(names(ts_df) %in% time_stamp)]) {
    temp_df <- ts_df[, c(time_stamp, i)]
    NonNAindex <- which(!is.na(temp_df[, 2]))
    series_data <- temp_df[min(NonNAindex):max(NonNAindex), ]
    names(series_data) <- c("ds", "y")
    series_data$Series_Name <- paste(i)
    
    temp_train <- series_data[1:round(nrow(series_data) * 0.8), ]
    temp_test <-
      series_data[(round(nrow(series_data) * 0.8) + 1):nrow(series_data), ]
    
    train_df <- rbind(train_df, temp_train)
    test_df <- rbind(test_df, temp_test)
    
  }
  final_result <-
    list("train_data" = train_df, "test_data" = test_df)
  return(final_result)
}

############################################################################################
#################################  Linear Prophet Model  ###################################
############################################################################################
prophet_model <- function(model_train_data, model_test_data) {
  require(prophet)
  require(dplyr)
  final_result <- data.frame()
  model_prophet <- prophet(
    model_train_data,
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE,
    growth = "linear"
  )
  forecast_train <-
    predict(model_prophet, data.frame(ds = model_train_data$ds))
  forecast_train <- forecast_train[c('ds', 'yhat')]
  forecast_test <-
    predict(model_prophet, data.frame(ds = model_test_data$ds))
  forecast_test <- forecast_test[c('ds', 'yhat')]
  
  model_train_data <-
    cbind(model_train_data[, c(1:2)], round(forecast_train$yhat))
  model_test_data <-
    cbind(model_test_data[, c(1:2)], round(forecast_test$yhat))
  names(model_train_data) <- c("Date", "Actuals", "Forecasts")
  model_train_data$MAPE <-
    abs(round(((model_train_data$Actuals - model_train_data$Forecasts) / model_train_data$Actuals
    ) * 100, digits = 2))
  model_train_data$Sample <- "Training"
  names(model_test_data) <- c("Date", "Actuals", "Forecasts")
  model_test_data$MAPE <-
    abs(round(((model_test_data$Actuals - model_test_data$Forecasts) / model_test_data$Actuals
    ) * 100, digits = 2))
  model_test_data$Sample <- "Testing"
  final_result <- rbind(model_train_data, model_test_data)
  return(final_result)
}

############################################################################################
############################### Prophet Master Script ######################################
############################################################################################
prophet_master_script <-
  function(source_file_name,
           date_stamp,
           n_obs,
           script_name) {
    require(doParallel)
    require(readxl)
    require(plyr)
    cl <- makeCluster(8)
    registerDoParallel(cl)
    df <-
      readxl::read_xlsx(
        source_file_name,
        sheet = 1,
        col_names = T,
        guess_max = n_obs
      )
    
    model_data <- data_preparation(df, date_stamp)
    dev_data <- model_data$train_data
    val_data <- model_data$test_data
    
    final_result <- data.frame()
    n_iter <- length(unique(dev_data$Series_Name))
    series_list <- unique(dev_data$Series_Name)
    
    final_result <- foreach(i = 1:n_iter) %dopar% {
      source(script_name)
      data_train <-
        dev_data[which(dev_data$Series_Name == series_list[i]),]
      data_test <-
        val_data[which(val_data$Series_Name == series_list[i]),]
      temp_result <- prophet_model(data_train, data_test)
      temp_result$Series_Name <- series_list[i]
      final_result <- rbind(final_result, temp_result)
    }
    stopCluster(cl)
    x <- ldply(final_result, data.frame)
    x$Model <- "Prophet"
    return(x)
  }
