################################################################################################
############################ Time Series Model Selection Algorithm #############################
#########################    Developer:   Debasish Dutta            ############################
#########################    Date:   	  December 2017             ############################
#################################### Helper Script 1 out of 3 ##################################
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
    series_data <- temp_df[min(NonNAindex):max(NonNAindex),]
    names(series_data) <- c("ds", "y")
    series_data$Series_Name <- paste(i)
    
    temp_train <- series_data[1:round(nrow(series_data) * 0.8),]
    temp_test <-
      series_data[(round(nrow(series_data) * 0.8) + 1):nrow(series_data),]
    
    train_df <- rbind(train_df, temp_train)
    test_df <- rbind(test_df, temp_test)
    
  }
  final_result <-
    list("train_data" = train_df, "test_data" = test_df)
  return(final_result)
}

############################################################################################
##################################### ARIMA Model ##########################################
############################################################################################
univariate_arima <- function(train_data, test_data) {
  require(forecast)
  forecast_train <- data.frame()
  forecast_test <- data.frame()
  arimamodel <- try(auto.arima(
    train_data[, 2],
    allowdrift = TRUE,
    stationary = FALSE,
    allowmean = TRUE,
    ic = c("aicc", "aic", "bic"),
    test = c("kpss", "adf", "pp"),
    seasonal.test = c("ocsb", "ch"),
    max.p = 5,
    max.q = 5,
    max.P = 52,
    max.Q = 52,
    max.d = 3,
    max.D = 3
  ),
  silent = TRUE)
  if (class(arimamodel) == "try-error") {
    forecast_train <- cbind(Date = train_data[, 1],
                            Actuals = train_data[, 2],
                            Forecasts = NA)
    forecast_test <- cbind(Date = test_data[, 1],
                           Actuals = test_data[, 2],
                           Forecasts = NA)
  } else{
    forecast_train <- cbind(
      Date = train_data[, 1],
      Actuals = train_data[, 2],
      Forecasts = as.numeric(arimamodel$fitted)
    )
    forecast_test <- cbind(
      Date = test_data[, 1],
      Actuals = test_data[, 2],
      Forecasts = as.numeric(forecast(arimamodel, nrow(test_data))$mean)
    )
  }
  
  names(forecast_train) <- c("Date", "Actuals", "Forecasts")
  names(forecast_test) <- c("Date", "Actuals", "Forecasts")
  forecast_train$MAPE <-
    abs(round(((forecast_train$Actuals - forecast_train$Forecasts) / forecast_train$Actuals
    ) * 100, digits = 2))
  forecast_train$Sample <- "Training"
  forecast_test$MAPE <-
    abs(round(((forecast_test$Actuals - forecast_test$Forecasts) / forecast_test$Actuals
    ) * 100, digits = 2))
  forecast_test$Sample <- "Testing"
  
  final_result <- rbind(forecast_train, forecast_test)
  return(final_result)
}

############################################################################################
################################# ARIMA Master Script ######################################
############################################################################################
arima_master_script <-
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
        dev_data[which(dev_data$Series_Name == series_list[i]), ]
      data_test <-
        val_data[which(val_data$Series_Name == series_list[i]), ]
      temp_result <- univariate_arima(data_train, data_test)
      temp_result$Series_Name <- series_list[i]
      final_result <- rbind(final_result, temp_result)
    }
    stopCluster(cl)
    x <- ldply(final_result, data.frame)
    x$Model <- "ARIMA"
    return(x)
  }