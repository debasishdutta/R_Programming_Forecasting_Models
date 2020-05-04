################################################################################################
############################ Time Series Model Selection Algorithm #############################
#########################    Developer:   Debasish Dutta            ############################
#########################    Date:   	  December 2017               ##########################
#########################    Input:   	1. Source File Name As Character #######################
#########################           	  2. Name of Date Stamp Column As Character ############
#########################               3. Maximum No of Date Stamp Available  #################
#########################    Output:   	1. Training & Test Sample MAPE Table   #################
################################################################################################

############################################################################################
################################# Best Model Selection #####################################
############################################################################################

best_model_selection <-
  function(source_file_name, date_stamp, n_obs) {
    require(reshape2)
    script_name_arima <- "Model Development_ARIMA.R"
    script_name_ucm <- "Model Development_UCM.R"
    script_name_prophet <- "Model Development_Prophet.R"
    source(script_name_arima)
    source(script_name_ucm)
    source(script_name_prophet)
    
    arima_output <- NULL
    ucm_output <- NULL
    prophet_output <- NULL
    
    arima_output <-
      arima_master_script(source_file_name, date_stamp, n_obs, script_name_arima)
    ucm_output <-
      ucm_master_script(source_file_name, date_stamp, n_obs, script_name_ucm)
    prophet_output <-
      prophet_master_script(source_file_name, date_stamp, n_obs, script_name_prophet)
    
    temp_out_1 <- rbind(arima_output, ucm_output, prophet_output)
    temp_out_2 <-
      aggregate(MAPE ~ Series_Name + Sample + Model, temp_out_1, mean)
    temp_out_final <-
      dcast(temp_out_2, Series_Name ~ Sample + Model)
    
    temp_out_final$Min_MAPE <-
      as.vector(apply(temp_out_final[, 1:4], 1, min))
    temp_out_final <-
      temp_out_final[order(temp_out_final$Min_MAPE, decreasing = FALSE), ]
    row.names(temp_out_final) <- NULL
    temp_out_final$Best_Model <-
      colnames(temp_out_final)[apply(temp_out_final[, 1:4], 1, which.min)]
    df <-
      within(temp_out_final, Best_Model <-
               data.frame(do.call(
                 'rbind', strsplit(as.character(Best_Model), '_', fixed = TRUE)
               )))
    df_final <- data.frame(df[, 1:8], Best_Model = df$Best_Model$X2)
    df_final$Min_MAPE <- as.numeric(df_final$Min_MAPE)
    df_final$MAPE_Range <- ifelse(df_final$Min_MAPE <= 5,
                                  "0-5%",
                                  ifelse(
                                    df_final$Min_MAPE <= 10,
                                    "5-10%",
                                    ifelse(
                                      df_final$Min_MAPE <= 15,
                                      "10-15%",
                                      ifelse(df_final$Min_MAPE <=
                                               20, "15-20%", "More Than 20%")
                                    )
                                  ))
    df_final <- df_final[,-8]
    return(df_final)
  }
