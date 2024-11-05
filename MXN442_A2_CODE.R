# 1.0 LOAD PACKAGES
## Specifies a list of packages that can be parsed to a loop to check if they are installed or not on the local drive

packages <- c("profoc",
              "tsrobprep",
              "mgcv",
              "readr",
              "readxl",
              "tidyr",
              "dplyr",
              "tsibble",
              "forecast",
              "ggplot2",
              "feasts",
              "fable",
              "glmnet",
              "splines",
              "keras") ## WARNING: keras requires tensorflow to create RNNs- you will likely need to update python and the tensorflow to run the last part of this code (following the prompts should suffice)   

for (package in packages) { ## loop for checking if the packages are installed and installing them if necessary
  if (!(package %in% installed.packages()[,1])) {
    install.packages(package)
  }
}

for (package in packages) { ## loop for loading the packages into the R environment
  library(package, character.only = TRUE)
}

# 2.0 CUSTOM FUNCTIONS
## This section mostly exists to avoid repition of basic code
## The author acknowledges other sections of the code from section 3.0 onwards could have been converted into custom functions but were left as is to help with interpretability

MXN442_folder_csv_readr <- function(folder_path, file_naming_pattern) { ## Custom function which finds all files named a specific pattern and compiling them into a single dataframe
  if (!requireNamespace("readxl", quietly = TRUE)) { ## Checks the necessary packages for reading in files has been loaded
    stop("Package 'readxl' is required but not installed. Please install it using install.packages('readxl').")
  }

  pattern <- paste0(file_naming_pattern, ".*\\.(csv|xlsx)$") ## Converts the naming pattern into a format that can be used to load in .csv and .xlsx files
  
  temp_file_list <- list.files( ## A list which stores all CSV and Excel files matching the pattern
    path = folder_path, 
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(temp_file_list) == 0) { ## Checks if any files were returned
    stop("No CSV or Excel files matching the pattern '", file_naming_pattern, "' found in the specified directory.")
  }
  
  temp_df_list <- list()   ## Stores the dataframes
  temp_first_csv_col_order <- NULL   ## Records the standard column order to help with the final rbind()
  
  for (file in temp_file_list) {   ## Reads in each file found in the folder matching the pattern
    if (grepl("\\.csv$", file, ignore.case = TRUE)) {
      temp_df <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
      temp_df <- readxl::read_excel(file)
      temp_df <- as.data.frame(temp_df)
    } else {
      stop(paste("Unsupported file type for file", basename(file)))
    }
    if (is.null(temp_first_csv_col_order)) { ## Sets the standard column order based on the first file read in
      temp_first_csv_col_order <- colnames(temp_df)
    } else {
      if (!setequal(colnames(temp_df), temp_first_csv_col_order)) { ## If-stop statement to check the csv or xlsx match the column format and will not impact the rbind()
        stop(paste("Columns in file", basename(file), "do not match the standard column names."))
      }
      temp_df <- temp_df[ , temp_first_csv_col_order] ## Ensures the file being read in has the column order set to the standard
    }
    temp_df_list[[length(temp_df_list) + 1]] <- temp_df
  }
  
  combined_temp_df <- do.call(rbind, temp_df_list) ## rbind() call to combine all dataframes loaded in by the function

  return(combined_temp_df)
}

MXN442_merge_csv_in_folder <- function(file_list, folder_path) { ## A more general version of the previous function, it reads in all files in a folder and will only retain those which contain a "Time" variable. Refer to the previous code comments if unsure how this function works
  temp_df_list <- list()
  
  all_times <- NULL

  for (file_name in file_list) {
    temp_file_path <- file.path(folder_path, file_name)
    if (!file.exists(temp_file_path)) {
      warning(paste("File not found:", file_name))
      next
    }
    
    temp_df <- read_excel(temp_file_path)
    
    if (!"Time" %in% names(temp_df)) {
      stop(paste("Column 'Time' not found in file:", file_name))
    }

    temp_df_list[[file_name]] <- temp_df
    all_times <- union(all_times, temp_df$Time)
  }
  
  for (file_name in names(temp_df_list)) {
    temp_df <- temp_df_list[[file_name]]
    missing_times <- setdiff(all_times, temp_df$Time)
    if (length(missing_times) > 0) {
      print(paste("Missing 'Time' values in file", file_name))
      print(missing_times)
    }
  }
  
  combined_temp_df <- temp_df_list[[1]]
  
  if (length(temp_df_list) > 1) {
    for (i in 2:length(temp_df_list)) {
      df_to_merge <- temp_df_list[[i]]
      combined_temp_df <- merge(combined_temp_df, df_to_merge, by = "Time", all = TRUE, suffixes = c("", paste0("_", i)))
    }
  }
  
  return(combined_temp_df)
}

transform_wind_direction <- function(df) { ## Transforms the 'Wind.Direction..deg.' variable as per Ziel's instructions: applying a sin and cosine transformation
  if (!"Wind.Direction..deg." %in% names(df)) {
    stop("The dataframe does not contain the required column 'Wind.Direction..deg.'")
  }

  theta_deg <- df$`Wind.Direction..deg.`

  if (!is.numeric(theta_deg)) {
    stop("'Wind.Direction..deg.' column must be numeric.")
  }

  theta_rad <- theta_deg * pi / 180 ## Convert the degrees to radians so that the cosine and sine transformation is between -1 and 1
  df$Wind.Direction.NS <- sin(theta_rad) ## Calculate Wind.Direction.NS: 1 at 90° (North), -1 at 270° (South)
  df$Wind.Direction.EW <- cos(theta_rad) ## Calculate Wind.Direction.EW: 1 at 0° (East), -1 at 180° (West)
  df$`Wind.Direction..deg.` <- NULL ## Remove the original Wind.Direction..deg. column

  return(df)
}

cvglmnet_plot <- function(model, x, y) { ## Exclusively used for the holiday log load adjustment. A custom function comparing the Cross-Validation Error of a LASSO model as Lambda changes whilst also showing a change in BIC 
  bic_values <- sapply(1:length(model$lambda), function(i) { ## Calculates the BIC for each lambda
    n <- length(y)
    predictions <- predict(model, s = model$lambda[i], newx = x)
    rss <- sum((predictions - y)^2)
    df <- model$glmnet.fit$df[i]
    bic <- log(n) * df + n * log(rss / n)
    return(bic)
  })
  
  best_bic_index <- which.min(bic_values) ## Identifies the lowest BIC
  best_lambda_bic <- model$lambda[best_bic_index] ## Identifies the lambda which results in the LASSO model with the lowest BIC
  assign("BestBIC_cvglmnetPlot_lambda", best_lambda_bic, envir = .GlobalEnv) ## Saves the lambda value to the R Studio environment incase it needs to be used later
  
  data <- data.frame(lambda = model$lambda, 
                     cvm = model$cvm,
                     cvsd = model$cvsd,
                     bic = bic_values)
  
  bic_scaling_factor <- max(data$cvm) / max(data$bic)  ## Scales the BIC to match CV error range so that the dual y-axis plot correctly
  
  p <- ggplot(data, aes(x = log(lambda))) +
    geom_point(aes(y = cvm), color = "red") +
    geom_line(aes(y = cvm), linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin = cvm - cvsd, ymax = cvm + cvsd), width = 0.2, color = "red") +
    geom_vline(xintercept = log(model$lambda.min), linetype = "dashed", color = "grey") +
    geom_vline(xintercept = log(model$lambda.1se), linetype = "dashed", color = "grey") +
    geom_vline(xintercept = log(best_lambda_bic), linetype = "dashed", color = "blue") +
    labs(x = "log(Lambda)", y = "Cross-Validation Error",
         title = paste("Cross-Validation Curve with BIC", deparse(substitute(model)))) +
    theme_minimal() +
    geom_line(aes(y = bic * bic_scaling_factor), color = "blue") +
    scale_y_continuous(name = "Cross-Validation Error (Left)",
                       sec.axis = sec_axis(~ . / bic_scaling_factor, name = "BIC (Right)"))
  
  return(p)
}

# 3.0 DATA IMPORT (TRAINING DATASET)
set.seed(123) ## Sets the seed early in the report for reproducibility of results

file_dir_TrainingValidation <- "C:\\Users\\james\\Documents\\MXN442\\MXN442_A2_Datatset" ## Allocate the file location as a directory that can be parsed to data importing functions
Actuals_df <- read_excel(paste0(file_dir_TrainingValidation,"\\Actuals.xlsx"), sheet = 1)
colnames(Actuals_df) <- c("Time",
                          "Load..kW.",
                          "Pressure_kpa",
                          "Cloud.Cover....",
                          "Humidity....",
                          "Temperature..C.",
                          "Wind.Direction..deg.",
                          "Wind.Speed..kmh.")
Actuals_df$Time <- as.POSIXct(Actuals_df$Time, format="%Y-%m-%d %H:%M:%S", tz="GMT") ## Chose GMT because this way, no time zone adjustments would be made to the time variable (I had issues with the data converting to the system timezone which impacted data preparations and modelling- this was the work around)

interim_Actuals_df <- read_csv(paste0(file_dir_TrainingValidation,"\\Actuals_Interim.csv"))
colnames(interim_Actuals_df) <- c("Time",
                                  "Load..kW.",
                                  "Pressure_kpa",
                                  "Cloud.Cover....",
                                  "Humidity....",
                                  "Temperature..C.",
                                  "Wind.Direction..deg.",
                                  "Wind.Speed..kmh.")
interim_Actuals_df$Time <- as.POSIXct(interim_Actuals_df$Time, format="%Y-%m-%d %H:%M", tz="GMT")

temp_list_Actuals_df <- list(Actuals_df, interim_Actuals_df)
temp_count <- 1
for (df in temp_list_Actuals_df) { ## Loop to check for the existence of NAs and if there are any, check if they all exist across the same rows (means its a blank row with no data and can be removed entirely)
  print(paste0("Checking NAs for dataframe [", temp_count,"] in list of dataframes"))
  for (col in 1:ncol(df)) {
    print(paste("# of NA values in", colnames(df)[col], "equal to:",sum(is.na(df[,col]))))
  }
  print(paste("# of rows with >=1 NA values is:", sum(!complete.cases(df))))
  temp_count <- temp_count + 1
}

temp_common_ids <- intersect(Actuals_df$Time, interim_Actuals_df$Time) ## See the report or the read_me file for information on what 'interim' and 'actuals' are: it will explain the below steps better than code notation
Actuals_df_common <- Actuals_df[Actuals_df$Time %in% temp_common_ids, ]
interim_Actuals_df_common <- interim_Actuals_df[interim_Actuals_df$Time %in% temp_common_ids, ]
Actuals_df_common <- Actuals_df_common[order(Actuals_df_common$Time), ]
interim_Actuals_df_common <- interim_Actuals_df_common[order(interim_Actuals_df_common$Time), ]
Actuals_df_diff <- Actuals_df_common
Actuals_df_diff[, -1] <- Actuals_df_common[, -1] - interim_Actuals_df_common[, -1] ## Having identified there is an overlap of observation dates, we check to see if the different datasets have any differences
colnames(Actuals_df_diff)[-1] <- paste0(colnames(Actuals_df_diff)[-1], "_diff")
View(Actuals_df_diff) ## Identifies that the 'interim' data has a constant theme of higher loads than the 'actuals'

unique_time_Actuals_df_list <- setdiff(Actuals_df$Time, interim_Actuals_df$Time)
unique_time_Actuals_df_list <- as.POSIXct(unique_time_Actuals_df_list, origin="1970-01-01", tz="GMT") 
unique_time_Actuals_df <- Actuals_df[Actuals_df$Time %in% unique_time_Actuals_df_list, ]
combined_Actuals_df <- rbind(unique_time_Actuals_df, interim_Actuals_df) ## Combines the interim and actuals data
rownames(combined_Actuals_df) <- NULL
View(combined_Actuals_df[duplicated(combined_Actuals_df$Time),]) ## Proves duplicates no longer exist

## Lines 234 to 274 essentially repeat the lines of 182 to 231
files_of_interest_list <- c("Pressure_kpa.xlsx",
                            "Cloudcover_percent.xlsx",
                            "Temperature_Celcius.xlsx",
                            "Winddirection_degree.xlsx",
                            "Windspeed_kmh.xlsx")
built_Forecasts_df <- MXN442_merge_csv_in_folder(files_of_interest_list, file_dir_TrainingValidation)
colnames(built_Forecasts_df) <- c("Time",
                                  "Pressure_kpa",
                                  "Cloud.Cover....",
                                  "Temperature..C.",
                                  "Wind.Direction..deg.",
                                  "Wind.Speed..kmh.")
for (i in 1:ncol(built_Forecasts_df)) {
  print(paste("# of NA values in", colnames(built_Forecasts_df)[i], "equal to:",sum(is.na(built_Forecasts_df[,i]))))
}
print(paste("# of rows with >=1 NA values is:", sum(!complete.cases(built_Forecasts_df))))
built_Forecasts_df$Time<- as.POSIXct(built_Forecasts_df$Time, format="%Y-%m-%d %H:%M:%S", tz="GMT")

interim_Forecasts_df <- read.csv(paste0(file_dir_TrainingValidation,"\\Forecasts_Interim.csv"))
interim_Forecasts_df <- interim_Forecasts_df[,c("Time",
                                                "Pressure_kpa",
                                                "Cloud.Cover....",
                                                "Temperature..C.",
                                                "Wind.Direction..deg.",
                                                "Wind.Speed..kmh.")]
for (i in 1:ncol(interim_Forecasts_df)) {
  print(paste("# of NA values in", colnames(interim_Forecasts_df)[i], "equal to:",sum(is.na(interim_Forecasts_df[,i]))))
}
print(paste("# of rows with >=1 NA values is:", sum(!complete.cases(interim_Forecasts_df))))
interim_Forecasts_df <- interim_Forecasts_df[complete.cases(interim_Forecasts_df),]
interim_Forecasts_df$Time<- as.POSIXct(interim_Forecasts_df$Time, format="%Y-%m-%d %H:%M:%S", tz="GMT")

temp_common_ids <- intersect(built_Forecasts_df$Time, interim_Forecasts_df$Time)
built_Forecasts_df_common <- built_Forecasts_df[built_Forecasts_df$Time %in% temp_common_ids, ]
interim_Forecasts_df_common <- interim_Forecasts_df[interim_Forecasts_df$Time %in% temp_common_ids, ]
built_Forecasts_df_common <- built_Forecasts_df_common[order(built_Forecasts_df_common$Time), ]
interim_Forecasts_df_common <- interim_Forecasts_df_common[order(interim_Forecasts_df_common$Time), ]
built_Forecasts_df_diff <- built_Forecasts_df_common
built_Forecasts_df_diff[, -1] <- built_Forecasts_df_common[, -1] - interim_Forecasts_df_common[, -1]
colnames(built_Forecasts_df_diff)[-1] <- paste0(colnames(built_Forecasts_df_diff)[-1], "_diff")
View(built_Forecasts_df_diff) ## Identifies the largest differences occur in wind direction. This warrants further investigation to ensure there is not an underlying or repeating error that could impact the accuracy of the data and our model predictions

temp_binwidth <- (max(built_Forecasts_df_diff$Wind.Direction..deg._diff) - min(built_Forecasts_df_diff$Wind.Direction..deg._diff)) / 10 ## Creates 10 bins (10 bins were chosen because it captures 0-10,..,90-100)
suspect_windDierction_diff_histogram <- ggplot(built_Forecasts_df_diff, aes(x = Wind.Direction..deg._diff)) + ## Visualising the differences between wind directions to see if there is an apparent bias
  geom_histogram(binwidth = temp_binwidth, fill = "blue", color = "black") +
  labs(title = "Histogram of Wind Direction Differences", 
       x = "Wind Direction (degrees difference)", 
       y = "Frequency") +
  theme_minimal()
plot(suspect_windDierction_diff_histogram) ## Reveals there is no pattern in the differences and that the majority are low in magnitude. We assume the minor differences will have no discernible impact on model accuracy 

suspect_windDierction_diff_autoplot <- ggplot(built_Forecasts_df_diff, aes(x = Time, y = Wind.Direction..deg._diff)) + ## Creates an autoplot of the wind direction differences 
  geom_point() +
  labs(title = "Autoplot of Wind Direction Differences over Time",
       x = "Time", 
       y = "Wind Direction Difference") +
  theme_minimal()
plot(suspect_windDierction_diff_autoplot) ## Is more specific in identifying the large differences (outliers) are localised between Nov 8 to Nov 14 and a few hours in each day. This is concerning and should be investigated further

## The following block produces 2 ARIMA models to try and forecast what value the Wind.Direction..deg. is predicted to be given all data available in that dataset
built_Forecasts_dummy <- data.frame(Time = built_Forecasts_df$Time, 
                                    Wind.Direction..deg. = built_Forecasts_df$Wind.Direction..deg.)
built_Forecasts_dummy_ts <- as_tsibble(built_Forecasts_dummy, index = Time)
built_Forecasts_WDD_arima_model <- auto.arima(built_Forecasts_dummy_ts$Wind.Direction..deg., 
                                              stepwise = FALSE, approximation = FALSE, seasonal = FALSE)
built_Forecasts_WDD_predicted <- forecast(built_Forecasts_WDD_arima_model, h = nrow(built_Forecasts_dummy))$mean
built_Forecasts_dummy$ARIMA_built_Forecasts_WDD <- built_Forecasts_WDD_predicted

interim_Forecasts_dummy <- data.frame(Time = interim_Forecasts_df$Time, 
                                      Wind.Direction..deg. = interim_Forecasts_df$Wind.Direction..deg.)
interim_Forecasts_dummy_ts <- as_tsibble(interim_Forecasts_dummy, index = Time)
interim_Forecasts_WDD_arima_model <- auto.arima(interim_Forecasts_dummy_ts$Wind.Direction..deg., 
                                                stepwise = FALSE, approximation = FALSE, seasonal = FALSE)
interim_Forecasts_WDD_predicted <- forecast(interim_Forecasts_WDD_arima_model, h = nrow(interim_Forecasts_dummy_ts))$mean
interim_Forecasts_dummy$ARIMA_interim_Forecasts_WDD <- interim_Forecasts_WDD_predicted

temp_df1 <- built_Forecasts_df[built_Forecasts_df$Time %in% temp_common_ids, ]
temp_df2 <- built_Forecasts_dummy[built_Forecasts_dummy$Time %in% temp_common_ids, ]
temp_df3 <- interim_Forecasts_df[interim_Forecasts_df$Time %in% temp_common_ids, ]
temp_df4 <- interim_Forecasts_dummy[interim_Forecasts_dummy$Time %in% temp_common_ids, ]
comparative_WDD_df <- data.frame(Time = temp_df1$Time, 
                                 Original_Built_df_WDD = temp_df1$Wind.Direction..deg.,
                                 ARIMA_Built_df_WDD = temp_df2$Wind.Direction..deg.,
                                 Original_interim_df_WDD = temp_df3$Wind.Direction..deg.,
                                 ARIMA_interim_df_WDD = temp_df4$Wind.Direction..deg.)
## Unfortunately the above ARIMAs returned similar differences in their predictions of Wind.Direction..deg. based on the interim and built forecast. This investigation was not as informative as we'd have hoped
## In line with other decisions made in this script, we preference the interim data

unique_time_built_Forecasts_list <- setdiff(built_Forecasts_df$Time, interim_Forecasts_df$Time)
unique_time_built_Forecasts_list <- as.POSIXct(unique_time_built_Forecasts_list, origin="1970-01-01", tz="GMT") 
unique_built_Forecasts_df <- built_Forecasts_df[built_Forecasts_df$Time %in% unique_time_built_Forecasts_list, ]
combined_Forecasts_df <- rbind(unique_built_Forecasts_df, interim_Forecasts_df) ## Combines the weather forecast data into a single dataframe
rownames(combined_Forecasts_df) <- NULL

combined_Forecasts_df$Time <- as.POSIXct(combined_Forecasts_df$Time, format="%Y-%m-%d %H:%M:%S", tz="GMT")
complete_Forecasts_sequence_check <- seq(min(combined_Forecasts_df$Time), max(combined_Forecasts_df$Time), by="hour") ## Checks if there is a date/ hour missing in the combined dataframe
complete_Forecasts_sequence_check <- as.POSIXct(complete_Forecasts_sequence_check, format="%Y-%m-%d %H:%M:%S", tz="GMT")
missing_dateTime_list <- setdiff(complete_Forecasts_sequence_check, combined_Forecasts_df$Time)
print(paste("Missing dates following rbind() of the built weather dataframe and interim is:", missing_dateTime_list)) ## Identifies one date is missing and that these values will need to be imputed

new_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(combined_Forecasts_df))) ## Creates a blank row for the missing date
names(new_row) <- names(combined_Forecasts_df)
new_row[["Time"]] <- missing_dateTime_list
combined_Forecasts_df <- rbind(combined_Forecasts_df, new_row) ## Adds the missing date to the combined data frame
combined_Forecasts_df <- arrange(combined_Forecasts_df, Time)
rownames(combined_Forecasts_df) <- NULL
combined_Forecasts_ts <- combined_Forecasts_df %>%
  as_tsibble(index = Time)
missing_value_columnNames <- c("Pressure_kpa",
                               "Cloud.Cover....",
                               "Temperature..C.",
                               "Wind.Direction..deg.",
                               "Wind.Speed..kmh.")
for (col in missing_value_columnNames) { ## Impliments an ARIMA model to predict the values for the missing date (ARIMA was the recommended method from our textbook- "An Introduction to Statistical Learning", 2023, James, G; Hastie, T)
  temp_ts_data <- combined_Forecasts_ts %>% select(Time, !!sym(col))
  temp_arima_model <- temp_ts_data %>%
    model(ARIMA(!!sym(col) ~ PDQ(0,1,1)))
  temp_forecast <- temp_arima_model %>%
    forecast(h = sum(is.na(combined_Forecasts_ts[[col]])))
  temp_predicted_values <- temp_forecast$.mean
  combined_Forecasts_ts[[col]][is.na(combined_Forecasts_ts[[col]])] <- temp_predicted_values
}

pre_bind_check <- setdiff(combined_Actuals_df$Time, combined_Forecasts_ts$Time) ## Used to check for any missing dates and that the above code has worked
pre_bind_check_list <- as.POSIXct(pre_bind_check, origin="1970-01-01", tz="GMT") 
pre_bind_check_list <- format(pre_bind_check_list, "%Y-%m-%d %H:%M:%S", tz="GMT")
ifelse(length(pre_bind_check_list) == 0, 
       print(paste("There is no imbalance in dates between the Actuals_df and the Forecast_df. Proceed with merge")), 
       print(paste("The following dates are missing from the Forecast_df: action is required", pre_bind_check_list)))
## The above notes there is no need for further imputation or data correction for the training dataset

Actuals_load_df <- combined_Actuals_df[,c("Time", "Load..kW.")]
TrainValid_Analysis_df <- merge(combined_Forecasts_ts, Actuals_load_df, by = "Time")
TrainValid_Analysis_df$Temperature..C. <- round(TrainValid_Analysis_df$Temperature..C.) ## Rounding has been used to keep consistency between the training and testing dataframe which will be loaded in later
TrainValid_Analysis_df <- transform_wind_direction(TrainValid_Analysis_df)
TrainValid_Analysis_ts <- TrainValid_Analysis_df %>%
  as_tsibble(index = Time)

## Below produces the rolling means for each weather variable (as specified by Ziel)
temp_df_TV_ROLLING <- subset(TrainValid_Analysis_ts, select = -Load..kW.)
temp_Pressure <- list()
temp_Cloud <- list()
temp_Temperature <- list()
temp_WindSpeed <- list()
temp_WindDirectionNS <- list()
temp_WindDirectionEW <- list()
list_of_days <- unique(as.Date(temp_df_TV_ROLLING$Time))
for (day in 1:length(list_of_days)) {
  loop_df <- temp_df_TV_ROLLING[as.Date(temp_df_TV_ROLLING$Time) == list_of_days[day], ]
  for (r in 1:nrow(loop_df)) {
    temp_Pressure[[length(temp_Pressure) + 1]] <- mean(unlist(loop_df[1:r, 2]), na.rm = TRUE)
    temp_Cloud[[length(temp_Cloud) + 1]] <- mean(unlist(loop_df[1:r, 3]), na.rm = TRUE)
    temp_Temperature[[length(temp_Temperature) + 1]] <- mean(unlist(loop_df[1:r, 4]), na.rm = TRUE)
    temp_WindSpeed[[length(temp_WindSpeed) + 1]] <- mean(unlist(loop_df[1:r, 5]), na.rm = TRUE)
    temp_WindDirectionNS[[length(temp_WindDirectionNS) + 1]] <- mean(unlist(loop_df[1:r, 6]), na.rm = TRUE)
    temp_WindDirectionEW[[length(temp_WindDirectionEW) + 1]] <- mean(unlist(loop_df[1:r, 7]), na.rm = TRUE)
  }
}
TrainValid_Analysis_ts$Pressure_RollingMean <- as.numeric(unlist(temp_Pressure))
TrainValid_Analysis_ts$Cloud_RollingMean <- as.numeric(unlist(temp_Cloud))
TrainValid_Analysis_ts$Temperature_RollingMean <- as.numeric(unlist(temp_Temperature))
TrainValid_Analysis_ts$WindSpeed_RollingMean <- as.numeric(unlist(temp_WindSpeed))
TrainValid_Analysis_ts$WindDirectionNS_RollingMean <- as.numeric(unlist(temp_WindDirectionNS))
TrainValid_Analysis_ts$WindDirectionEW_RollingMean <- as.numeric(unlist(temp_WindDirectionEW))

# 4.0 DATA IMPORT (TESTING DATASET)
## Because the below is almost identical to the above, minimal notation has been included. Please refer to the previous section if unsure
file_dir_TestingMAE <- "C:\\Users\\james\\Documents\\MXN442\\MXN442_A2_Datatset\\MAE datasets\\"
Actuals_pattern2 = "Actuals_"
TestingMAE_Actuals_df <- MXN442_folder_csv_readr(file_dir_TestingMAE, Actuals_pattern2)

for (i in 1:ncol(TestingMAE_Actuals_df)) {
  print(paste("# of NA values in", colnames(TestingMAE_Actuals_df)[i], "equal to:",sum(is.na(TestingMAE_Actuals_df[,i]))))
}
print(paste("# of rows with >=1 NA values is:", sum(!complete.cases(TestingMAE_Actuals_df))))
TestingMAE_Actuals_df <- TestingMAE_Actuals_df[complete.cases(TestingMAE_Actuals_df),]
colnames(TestingMAE_Actuals_df) <- c("Time",
                                     "Load..kW.",
                                     "Pressure_kpa",
                                     "Cloud.Cover....",
                                     "Humidity....",
                                     "Temperature..C.",
                                     "Wind.Direction..deg.",
                                     "Wind.Speed..kmh.")
TestingMAE_Actuals_df$Time <- as.POSIXct(TestingMAE_Actuals_df$Time, format="%Y-%m-%d %H:%M", tz="GMT")
TestingMAE_Actuals_df <- TestingMAE_Actuals_df %>% arrange(Time)
rownames(TestingMAE_Actuals_df) <- NULL
complete_TestingMAE_sequence_check <- seq(min(TestingMAE_Actuals_df$Time), max(TestingMAE_Actuals_df$Time), by="hour")
complete_TestingMAE_sequence_check <- as.POSIXct(complete_TestingMAE_sequence_check, format="%Y-%m-%d %H:%M:%S", tz="GMT")
missing_TestingMAE_list <- setdiff(complete_TestingMAE_sequence_check, TestingMAE_Actuals_df$Time)
missing_TestingMAE_list <- as.POSIXct(missing_TestingMAE_list, origin="1970-01-01", tz="GMT") 
missing_TestingMAE_list <- format(missing_TestingMAE_list, "%Y-%m-%d %H:%M:%S", tz="GMT")
print(paste("Missing dates following MXN442_folder_csv_readr() and developement of the results dataframe:", missing_TestingMAE_list))

Forecasts_pattern <- "Forecasts_"
TestingMAE_Forecasts_df <- MXN442_folder_csv_readr(file_dir_TestingMAE, Forecasts_pattern)

for (i in 1:ncol(TestingMAE_Forecasts_df)) {
  print(paste("# of NA values in", colnames(TestingMAE_Forecasts_df)[i], "equal to:",sum(is.na(TestingMAE_Forecasts_df[,i]))))
}
print(paste("# of rows with >=1 NA values is:", sum(!complete.cases(TestingMAE_Forecasts_df))))
TestingMAE_Forecasts_df <- TestingMAE_Forecasts_df[complete.cases(TestingMAE_Forecasts_df),]
colnames(TestingMAE_Forecasts_df) <- c("Temperature..C.",
                                       "Pressure_kpa",
                                       "Cloud.Cover....",
                                       "Wind.Direction..deg.",
                                       "Wind.Speed..kmh.",
                                       "Time")
TestingMAE_Forecasts_df$Time <- as.POSIXct(TestingMAE_Forecasts_df$Time, format="%Y-%m-%d %H:%M", tz="GMT")
TestingMAE_Forecasts_df <- TestingMAE_Forecasts_df %>% arrange(Time)
rownames(TestingMAE_Forecasts_df) <- NULL
complete_TestingForecasts_sequence_check <- seq(min(TestingMAE_Forecasts_df$Time), max(TestingMAE_Forecasts_df$Time), by="hour")
complete_TestingForecasts_sequence_check <- as.POSIXct(complete_TestingForecasts_sequence_check, format="%Y-%m-%d %H:%M:%S", tz="GMT")
missing_TestingForecasts_list <- setdiff(complete_TestingForecasts_sequence_check, TestingMAE_Forecasts_df$Time)
missing_TestingForecasts_list <- as.POSIXct(missing_TestingForecasts_list, origin="1970-01-01", tz="GMT") 
missing_TestingForecasts_list <- format(missing_TestingForecasts_list, "%Y-%m-%d %H:%M:%S", tz="GMT")
print(paste("Missing dates following MXN442_folder_csv_readr() and developement of the results dataframe:", missing_TestingForecasts_list))

common_MAE_times <- intersect(TestingMAE_Actuals_df$Time, TestingMAE_Forecasts_df$Time)
common_MAE_times <- as.POSIXct(common_MAE_times, origin="1970-01-01", tz="GMT") 
ifelse(length(common_MAE_times) != length(TestingMAE_Actuals_df$Time), 
       print(paste0("There are [",abs(length(TestingMAE_Actuals_df$Time) - length(common_MAE_times)),"] hours that are not common to both the forecasting and actual dataset. Calculation of y_lags is not possible: alternative testing dataset required")),
       print(paste("All dates and hours are shared (common) between the forecasting and actual dataset")))
## Whilst not alarming at this stage, it is important to note that there is a 48 hour (2 day) gap between the training and testing dataframe. This demonstrates our first major difference between the data we had available for this project vs Ziel 

# 5.0 EDA (EXPLORATORY DATA ANALYSIS)
unique_variables <- names(TrainValid_Analysis_ts)[names(TrainValid_Analysis_ts) != "Time"]
for (variable in unique_variables) { ## Creates visuals (STL decomposition) to give us an intuition of what the explanatory variables look like over time
  ts_data <- TrainValid_Analysis_ts %>%
    select(Time, all_of(variable)) %>%
    as_tsibble(index = Time) 
  
  stl_decomp <- ts_data %>%
    model(STL(!!sym(variable))) %>%
    components()
  
  p <- autoplot(stl_decomp) +
    labs(title = paste("STL Decomposition of", variable),
         x = "Time") +
    theme_minimal()
  
  print(p)
}

# 6.0 HOLIDAY ADJUSTMENT
## Whilst the Ziel did not make their code available for the holiday transformation, they provided an indepth description in the article. We suggest reading that section to have a greater understanding of what has been done below.
Holiday_adjusted_ts <- TrainValid_Analysis_ts

## Applying ReLU-based quantile transformation for all columns except "Time" and "Load..kW."
cols_to_transform <- setdiff(names(Holiday_adjusted_ts), c("Time", "Load..kW."))
for (col in cols_to_transform) {
  q <- quantile(Holiday_adjusted_ts[[col]], probs = seq(0, 1, by = 0.01), na.rm = TRUE)
  Holiday_adjusted_ts[[paste0(col, "_ReLU")]] <- pmax(0, Holiday_adjusted_ts[[col]] - q["50%"])
}

## Adding lagged log-load (168:510 days)
Holiday_adjusted_ts$log_load <- log(Holiday_adjusted_ts$Load..kW.)
for (lag in 168:510) {
  Holiday_adjusted_ts[[paste0("log_load_lag_", lag)]] <- dplyr::lag(Holiday_adjusted_ts$log_load, lag)
}

## Creation of dummy variables for hour of day and day in week (deterministic effects)
Holiday_adjusted_ts$day_of_week <- as.factor(weekdays(Holiday_adjusted_ts$Time))
Holiday_adjusted_ts$hour_of_day <- as.factor(format(Holiday_adjusted_ts$Time, "%H"))
Holiday_adjusted_ts_dummy <- model.matrix(~ day_of_week + hour_of_day, Holiday_adjusted_ts)
Holiday_adjusted_ts_dummy <- as.data.frame(Holiday_adjusted_ts_dummy)
Holiday_adjusted_ts_dummy <- Holiday_adjusted_ts_dummy[ , -1]

## Combining the modeled dummy variables with the original dataframe
Holiday_adjusted_ts <- bind_cols(Holiday_adjusted_ts, Holiday_adjusted_ts_dummy)

## Fitting a LASSO model tuned with BIC to predict a holiday adjusted log load
predictors <- setdiff(names(Holiday_adjusted_ts), c("Time", "Load..kW.", "log_load", "day_of_week", "hour_of_day"))
na_rows <- which(rowSums(is.na(Holiday_adjusted_ts)) > 0)
x <- as.matrix(Holiday_adjusted_ts[-na_rows, predictors])
y <- c(Holiday_adjusted_ts[-na_rows, "log_load", drop = TRUE])
lasso_fit <- cv.glmnet(x, y, alpha = 1)

## Plotting the performance of the LASSO model for visual interpretation
cv_BIC_plot <- cvglmnet_plot(lasso_fit, x, y)
cv_BIC_plot

## Applying the "optimal" LASSO model (based on the Lambda that produces the lowest BIC) to predict the log load if it WASN'T a holiday
Holiday_adjusted_ts$holiday_adjusted_log_load <- predict(lasso_fit, 
                                                         newx = as.matrix(Holiday_adjusted_ts[, predictors]), 
                                                         s = BestBIC_cvglmnetPlot_lambda)

Holiday_adjusted_ts$holiday_adjusted_log_load <- ifelse(format(Holiday_adjusted_ts$Time, "%m-%d") %in% c("12-11", "12-18"), ## Only replacing the dates explicitly identified by Ziel as holidays and therefore requiring the LASSO model intervention
                                                        Holiday_adjusted_ts$holiday_adjusted_log_load,
                                                        Holiday_adjusted_ts$log_load)
Holiday_adjusted_ts$adjusted_load <- exp(Holiday_adjusted_ts$holiday_adjusted_log_load) ## Converts the load back into the original scale (only completed for interest)

# 7.0 CREATION OF FINAL TRAINING AND TESTING DATASETS
## You should note that the tsibble from "4.0 " was not used - please refer to the report for rationale
Analysis_ts <- Holiday_adjusted_ts %>%
  dplyr::select(-c("Load..kW.", "log_load", 15:370, 373:401, "adjusted_load")) ## Removes the lags from the holiday adjusted tsibble so that the GAM specific and LASSO specific lags used in modelling (section 8.0) are created in line with Ziel's paper
Analysis_ts$hour_of_day <- as.numeric(Analysis_ts$hour_of_day)
Analysis_ts <- Analysis_ts %>%
  rename(
    Temperature_C = Temperature..C.,
    Cloud_Cover = Cloud.Cover....,
    Wind_Speed_kmh = Wind.Speed..kmh.,
    Wind_Direction_EW = Wind.Direction.EW,
    Wind_Direction_NS = Wind.Direction.NS
  )

GAM_lags <- unique(c(24, 168, 24 * c(2, 3, 8, 14, 21, 28, 35, 42, 350, 357, 364, 371, 378, 385)))
LASSO_lags <- unique(c(1:24, 24 * seq(21, 56, by = 7), 24 * c(350, 357, 364, 371)))

GAM_lags_unique <- setdiff(GAM_lags, LASSO_lags)
LASSO_lags_unique <- setdiff(LASSO_lags, GAM_lags)
common_lags <- intersect(GAM_lags, LASSO_lags)
all_lags <- unique(c(GAM_lags_unique, LASSO_lags_unique, common_lags))
for (lag in all_lags) {
  Analysis_ts[[paste0("log_load_lag_", lag)]] <- dplyr::lag(Analysis_ts$holiday_adjusted_log_load, n = lag)
}

split_index <- nrow(Analysis_ts) - (length(common_MAE_times)) 
Testing_ts <- Analysis_ts[(split_index + 1):nrow(Analysis_ts), ]
Training_ts <- Analysis_ts[1:split_index, ]

# 8.0 ENSEMBLE MODEL CREATION

start_time <- Sys.time() ## Starts a timer to record the computation time for training the 28 models

BOA_model_list <- list()

subset_days <- c(28, 56, 77, 119, 210, 393, 758)
subset_lengths <- subset_days * 24

## Below is a loop which creates a model for each subset of the training data. Much is intuitive so limited notation has been included except where a key decision was made
for (subset_length in subset_lengths) {
  subset_data <- tail(Training_ts, subset_length)
  
  ## 1. STL-decomposed exponential smoothing
  ts_data <- ts(subset_data$holiday_adjusted_log_load, frequency = 168) ## Frequency was set per Ziel's paper (note 168 hours == 1 week)
  stlm_fit <- stlm(ts_data, s.window = "periodic", method = "ets")
  model_name <- paste0("STL_ES_", subset_length/24, "days")
  BOA_model_list[[model_name]] <- stlm_fit
  
  ## 2. AR(P) model
  ar_fit <- ar(ts_data, aic = TRUE, order.max = 528) ## Order.max was set per Ziel's paper (note 528 hours == 3 weeks + 1 day)
  model_name <- paste0("ARP_", subset_length/24, "days")
  BOA_model_list[[model_name]] <- ar_fit
  
  ## 3. GAM model
  ## This modelling required adjustment from what Ziel explicitly outlined in thier methodology. Note there are 2 different GAM formulas below: motivation for this was due to the smoothing parameter and minimum degrees of freedom required (refer to the report for additional information)
  lag_terms <- paste0("s(log_load_lag_", GAM_lags, ")")
  formula_str <- paste( ## Ziel's original stated GAM
    "holiday_adjusted_log_load ~",
    "s(log_load_lag_24) +",
    "s(log_load_lag_168) +",
    paste(lag_terms, collapse = " + "), "+",
    "s(hour_of_day, bs = 'cc') +",
    "s(hour_of_day, by = day_of_week, bs = 'cc') +",
    "s(day_of_week, bs = 're') +",
    "s(Temperature_C) +",
    "s(Temperature_C, by = day_of_week) +",
    "s(Cloud_Cover) +",
    "s(Cloud_Cover, by = day_of_week) +",
    "s(Temperature_C, Cloud_Cover) +",
    "s(Pressure_kpa) +",
    "s(Wind_Speed_kmh) +",
    "s(Wind_Direction_EW) +",
    "s(Wind_Direction_NS)"
  )
  
  variables_of_interest <- unique(c(
    paste0("log_load_lag_", c(24, 168, GAM_lags)),
    "hour_of_day",
    "day_of_week",
    "Temperature_C",
    "Cloud_Cover",
    "Pressure_kpa",
    "Wind_Speed_kmh",
    "Wind_Direction_EW",
    "Wind_Direction_NS"
  ))
  
  num_unique_values <- sapply(variables_of_interest, function(var) { ## Loop to check the subset dataframe has all explanatory variables
    if (var %in% names(subset_data)) {
      length(unique(subset_data[[var]]))
    } else {
      NA
    }
  })
  
  if (any(num_unique_values < 11, na.rm = TRUE)) {  ## This loop identifies if the degrees of freedom is 10 or less. If yes, it removes the smoothing parameter. Note =<10 was used because mgcv's default (and therefore minimum requirement to effectively work) is 10
    lag_terms_nosmooth <- paste0("log_load_lag_", GAM_lags)
    formula_str_nosmooth <- paste(
      "holiday_adjusted_log_load ~",
      "log_load_lag_24 +",
      "log_load_lag_168 +",
      paste(lag_terms_nosmooth, collapse = " + "), "+",
      "hour_of_day + day_of_week +",
      "hour_of_day:day_of_week +",
      "Temperature_C +",
      "Temperature_C:day_of_week +",
      "Cloud_Cover +",
      "Cloud_Cover:day_of_week +",
      "Temperature_C:Cloud_Cover +",
      "Pressure_kpa +",
      "Wind_Speed_kmh +",
      "Wind_Direction_EW +",
      "Wind_Direction_NS"
    )
    gam_formula <- as.formula(formula_str_nosmooth)
  } else {
    gam_formula <- as.formula(formula_str)
  }

  gam_fit <- gam(gam_formula, data = subset_data, method = "REML")
  model_name <- paste0("GAM_", subset_length/24, "days")
  BOA_model_list[[model_name]] <- gam_fit
  
  ## 4. LASSO model
  exclude_vars <- c("holiday_adjusted_log_load", "day_of_week", paste0("log_load_lag_", GAM_lags_unique)) ## Excludes variables in GAM_lags_unique from the subset data so that only LASSO related lags are kept
  predictor_vars <- setdiff(names(subset_data), exclude_vars)
  x <- model.matrix(~ . - 1, data = subset_data[, predictor_vars]) ## Converts the training dataframe into a predictor matrix (requirement of the cv.glmnet() method)
  y <- subset_data$holiday_adjusted_log_load ## Converts the response into a vector (requirement of the cv.glmnet() method)
  
  lasso_fit <- cv.glmnet(x, y, alpha = 1)
  model_name <- paste0("LASSO_", subset_length/24, "days")
  BOA_model_list[[model_name]] <- lasso_fit
}

end_time <- Sys.time()
BOA_modelCreation_runTime <- end_time - start_time
print(BOA_modelCreation_runTime) ## Displays the run time for developing the ensemble (FYI- computation took 10.48651 secs on the user's laptop)

# 9.0 CREATION OF THE BOA ALGORITHM AND HELPER FUNCTION
## This section was not included in 2.0 as it made more sense to see how it accepts the testing dataframe to produce the prediction and weights
## This section was necessary because at the time of writting, there is not a package which contains either a BOA or smoothed BOA algorithm

generate_BOA_prediction <- function(model, day, horizon, testing_dataframe) { ## This is a helper function for the smoothed BOA algorithm which produces point forecasts
  prediction <- 0
  index <- ((day - 1) * horizon) + horizon ## Specifies the row index to search in the testing dataframe
  
  ## IF-ELSE statements to handle different model types
  if (inherits(model, "stlm")) {
    # Custom predict for STL models
    prediction <- as.numeric(tail(predict(model, h = index)$mean, 1))
    
  } else if (inherits(model, "ar")) {
    # Custom predict for ARP models
    prediction <- as.numeric(predict(model, h = index)[1])
    
  } else if (inherits(model, "gam")) {
    # Custom predict for GAM models
    prediction <- as.numeric(predict(model, newdata = testing_dataframe[index, ]))
    
  } else if (inherits(model, "cv.glmnet")) {
    # Custom predict for LASSO (glmnet) models
    trained_vars <- rownames(model$glmnet.fit$beta)
    prediction_row <- model.matrix(~ . - 1, data = testing_dataframe[index, ])
    prediction <- as.numeric(predict(model, newx = prediction_row[, trained_vars], s = "lambda.1se"))
    
  } else if (inherits(model, "keras.engine.training.Model")) {
    # Custom predict for keras::RNN
    timesteps <- 1
    exclude_cols <- c("Time", "holiday_adjusted_log_load")
    input_features <- testing_dataframe[index, !(names(testing_dataframe) %in% exclude_cols)]
    predictor_vars <- setdiff(names(testing_dataframe), exclude_cols)
    input_matrix <- model.matrix(~ . - 1, data = input_features)
    x_input <- array(input_matrix, dim = c(1, timesteps, ncol(input_matrix)))
    prediction <- as.numeric(predict(model, x_input, verbose = 0))
  }
  
  return(prediction)
}

## Please note the limitations of the below custom function. It has not been built to be robust to other applications, it is specific in trying to replicate Ziel's paper and performance
Smoothed_BOA_algorithm <- function(model_list, testing_dataframe, horizon = 24, response_variable, lambda) {
  start_time <- Sys.time() ## Starts a timer so that the user is aware of the computation time
  K <- length(model_list)  ## Number of models
  T <- nrow(testing_dataframe)  ## Number of observations in test data
  D <- T / horizon  ## Number of days
  
  t_weights_df <- matrix(1 / K, nrow = K * horizon, ncol = D) ## Initialise a blank matrix to store the weights as they're updated over t- predictions
  colnames(t_weights_df) <- paste0("Epoch_", 1:D)
  t_weights_df_rownames <- list()
  for (model in names(model_list)) {
    for (t in 1:horizon) { ## This loop allows the structure of the weights matrix to be simpler for the rows capture both the h & k dimension
      temp_rowname <- paste0(model, "_", t, "hour")
      t_weights_df_rownames <- append(t_weights_df_rownames, temp_rowname)
    }
  }
  rownames(t_weights_df) <- t_weights_df_rownames

  d_instantaneous_regret <- matrix(0, nrow = K * horizon, ncol = D)   ## Initialise a blank matrix for the calculation and storage of instantaneous regret
  
  t_predictions_df <- matrix(0, nrow = K * horizon, ncol = D) ## Initialise a predictions matrix which can be parsed back to the user to check the MAE calculation
  colnames(t_predictions_df) <- paste0("Epoch_", 1:D)
  rownames(t_predictions_df) <- t_weights_df_rownames
  
  B <- bs(1:K, df = K, degree = min(3, K - 1), intercept = TRUE) ## Initialise B-spline basis matrix, "B", for K models
  Delta <- diff(diag(K), differences = 1) ## Initialise a difference matrix {\delta} for K models
  
  e_range <- numeric(K * horizon) ## Initialising a blank range vector
  eta <- 0.1  ## Setting an arbitrary learning rate because it will be updated very quickly within the loop
  regret <- numeric(K * horizon) ## Initialising a blank regret vector
  
  for (d in 1:D) {   ## Loop through each day
    k_count <- 1
    for (model_name in names(model_list)) { ## Generate all predictions for each model and horizon
      k <- model_list[[model_name]]
      for (h in 1:horizon) {
        prediction <- generate_BOA_prediction(k, d, h, testing_dataframe) ## Calculates the prediction from the helper function
        t_predictions_df[((k_count - 1) * horizon) + h, d] <- prediction ## Saves the prediction
      }
      k_count <- k_count + 1
    }
    for (h in 1:horizon) { ## Compute combined forecasts and errors
      indices_h <- seq(h, K * horizon, horizon)
      weights_h <- t_weights_df[indices_h, d]
      predictions_h <- t_predictions_df[indices_h, d]
      combined_forecast <- sum(weights_h * predictions_h)
      
      ## Get the observed value for the current horizon and day
      observed_index <- ((d - 1) * horizon) + h
      observed_value <- as.numeric(testing_dataframe[observed_index, response_variable])
      
      for (k in 1:K) { ## Calculate the instantaneous regret and range for each model at this horizon
        current_index <- ((k - 1) * horizon) + h
        prediction_kh <- t_predictions_df[current_index, d]
        d_instantaneous_regret[current_index, d] <- abs(observed_value - prediction_kh) ## Instantaneous regret for model k at horizon h
        e_range[current_index] <- max(e_range[current_index] , d_instantaneous_regret[current_index, d]) ## Range for model k at horizon h
      }

      if (d > 1) { ## Accesses the previous weights appropriately (aware that if d == 1, we can't access a negative index)
        previous_weights <- t_weights_df[indices_h, d - 1]
      } else {
        previous_weights <- t_weights_df[indices_h, d]
      }
      losses_h <- d_instantaneous_regret[indices_h, d] ## Accessing the instantaneous regret for this horizon and day
      eta <- min((e_range[indices_h]/2),sqrt(log(K)/sum(d_instantaneous_regret[indices_h, 1:d]))) ## Updating the learning rate specific to d_h_k
      regret[indices_h] <-regret[indices_h] + (losses_h*(eta*losses_h - 1)/2) 
      new_weights <- previous_weights * exp(-eta * regret[indices_h]) ## Updating of weight d_h_k

      new_weights <- new_weights / sum(new_weights) ## Normalize the weights
      t_weights_df[indices_h, d] <- new_weights ## Updating t_weights_df prior to smoothing
    }
    
    for (h in 1:horizon) { ## Applying a smoothing to the weights for d_h_k
      indices_h <- seq(h, K * horizon, horizon)
      B_t_B <- t(B) %*% B ## calculating the squared B-splines grid
      smooth_term <- B_t_B + lambda * t(Delta) %*% Delta ## Calculates the smoothing term
      smoothed_weights_h <- B %*% solve(smooth_term) %*% t(B) %*% t_weights_df[indices_h, d]  ## Smoothes weight for d_h_k
      smoothed_weights_h[smoothed_weights_h < 0] <- 0 ## Ensures weights are non-negative
      t_weights_df[indices_h, d] <- smoothed_weights_h / sum(smoothed_weights_h) ## Normalises the smoothed weights whilst updating t_weights_df
    }
  }
  
  ## The below calculates the absolute error for each day and horizon
  d_h_absolute_error_df <- matrix(0, nrow = horizon, ncol = D)
  colnames(d_h_absolute_error_df) <- paste0("Day_", 1:D)
  rownames(d_h_absolute_error_df) <- paste0("Hour_", 1:horizon)
  for (d in 1:D) {
    for (h in 1:horizon) {
      indices_h <- seq(h, K * horizon, horizon)
      d_h_prediction <- sum(t_weights_df[indices_h, d] * t_predictions_df[indices_h, d])
      d_h_observation <- as.numeric(testing_dataframe[(((d - 1) * horizon) + h), response_variable])
      d_h_abs_error <- abs(d_h_observation - d_h_prediction)
      d_h_absolute_error_df[h, d] <- d_h_abs_error
    }
  }
  
  end_time <- Sys.time()
  SmoothedBOA_implementation_runTime <- end_time - start_time
  print(SmoothedBOA_implementation_runTime) ## Display to the user the function run time
  
  # Return weights and errors
  return(list(d_h_k_weights = t_weights_df, d_h_k_predictions = t_predictions_df, d_h_absoluteError = d_h_absolute_error_df))
}

# 10.0 IMPLIMENTATION OF ZIEL CODE
Smoothed_BOA_application<- Smoothed_BOA_algorithm(model_list = BOA_model_list,
                                                  testing_dataframe = Testing_ts,
                                                  horizon = 24,
                                                  response_variable = "holiday_adjusted_log_load",
                                                  lambda = 10) ## Approximate mean lambda from Figure 8 in the paper
print(paste("The MAE (kWh) from attempting to recreate Ziel's Smoothed BOA is =",exp(mean(Smoothed_BOA_application$d_h_absoluteError))))


# 11.0 ATTEMPTING SUGGESTED IMPROVEMENT 1 - TRIAL LAMBDAs
lambda_tests <- c(0, 10^seq(0, 10, 1)) ## Defining the sequence of viable lambdas

MAE_lambda <- data.frame(lambda = lambda_tests, MAE = numeric(length(lambda_tests))) ## Initialise an MAE_lambda data frame

row_count <- 1
for (i in lambda_tests) { ## Loop through each lambda and calculate the MAE
  temp_results <- Smoothed_BOA_algorithm(
    model_list = BOA_model_list,
    testing_dataframe = Testing_ts,
    horizon = 24,
    response_variable = "holiday_adjusted_log_load",
    lambda = i
  )
  MAE_lambda[row_count, "MAE"] <- mean(temp_results$d_h_absoluteError) ## Store the lambda and mean absolute error
  row_count <- row_count + 1
}

lambda_MAE_plot <- ggplot(MAE_lambda, aes(x = lambda, y = MAE)) + ## Create a plot of MAE x lambda
  geom_point() +
  scale_x_log10() +
  labs(title = "Comparison of smoothed BOA performance x Lambda (log10) penalties",
       x = "Log10_lambda", 
       y = "Mean Absolute Error (Log_load_kWh)"
  ) +
  theme_minimal()
plot(lambda_MAE_plot)

best_lambda <- MAE_lambda %>%
  filter(MAE == min(MAE, na.rm = TRUE)) %>%
  pull(lambda)

bestLambda_BOA_application<- Smoothed_BOA_algorithm(model_list = BOA_model_list, ## Creation of a BOA based on optimal lambda from plot
                                         testing_dataframe = Testing_ts,
                                         horizon = 24,
                                         response_variable = "holiday_adjusted_log_load",
                                         lambda = best_lambda) 

mean_weights <- list() ## Initialise a list to store the results of each model type in the BOA
mean_weights$STL_ES <- mean(bestLambda_BOA_application$d_h_k_weights[grepl("^STL_ES_", rownames(bestLambda_BOA_application$d_h_k_weights)), ])
mean_weights$ARP <- mean(bestLambda_BOA_application$d_h_k_weights[grepl("^ARP_", rownames(bestLambda_BOA_application$d_h_k_weights)), ])
mean_weights$GAM <- mean(bestLambda_BOA_application$d_h_k_weights[grepl("^GAM_", rownames(bestLambda_BOA_application$d_h_k_weights)), ])
mean_weights$LASSO <- mean(bestLambda_BOA_application$d_h_k_weights[grepl("^LASSO_", rownames(bestLambda_BOA_application$d_h_k_weights)), ])

## Print the results
print(paste("The average weights for STL_ES models =", mean_weights$STL_ES))
print(paste("The average weights for ARP models =", mean_weights$ARP))
print(paste("The average weights for GAM models =", mean_weights$GAM))
print(paste("The average weights for LASSO models =", mean_weights$LASSO))
lowest_contribution_model <- names(mean_weights)[which.min(unlist(mean_weights))]
print(paste("Model group with the lowest contribution (weights) to predictions =", lowest_contribution_model))
print(paste("The MAE (kWh) from using the ideal lambda to improve Ziel's Smoothed BOA is =",exp(mean(bestLambda_BOA_application$d_h_absoluteError))))
ifelse(exp(mean(bestLambda_BOA_application$d_h_absoluteError)) < exp(mean(Smoothed_BOA_application$d_h_absoluteError)),
       print(paste("The change in lambda means the updated model outperforms the original by",
                   exp(mean(bestLambda_BOA_application$d_h_absoluteError)) - exp(mean(Smoothed_BOA_application$d_h_absoluteError)), "kWh error")),
       print(paste("The original Smoothed BOA model recorded in Ziel's outperforms the updated lambda model by",
                   exp(mean(Smoothed_BOA_application$d_h_absoluteError)) - exp(mean(bestLambda_BOA_application$d_h_absoluteError)), "kWh error")))

# SECTION 12.0 ATTEMPTING SUGGESTED IMPROVEMENT 2 - RNN REPLACING AR(P)
## Creation of the new model ensemble replacing AR(P) with Recurrent Neural Networks (RNN)
## Much of the below code is identical to section 8.0, therefore notation will only be applied to the RNN developement 
start_time <- Sys.time()
updatedBOA_model_list <- list()
subset_days <- c(28, 56, 77, 119, 210, 393, 758)
subset_lengths <- subset_days * 24
for (subset_length in subset_lengths) {
  subset_data <- tail(Training_ts, subset_length)
  ts_data <- ts(subset_data$holiday_adjusted_log_load, frequency = 168)
  stlm_fit <- stlm(ts_data, s.window = "periodic", method = "ets")
  model_name <- paste0("STL_ES_", subset_length/24, "days")
  updatedBOA_model_list[[model_name]] <- stlm_fit
  
  ## RNN model
  timesteps <- 1 ## Means the RNN is built to predict hourly forecasts
  n_samples <- nrow(subset_data)
  exclude_vars <- c("holiday_adjusted_log_load", "Time")
  predictor_vars <- setdiff(names(subset_data), exclude_vars)
  RNN_subset_matrix <- model.matrix(~ . - 1, data = subset_data[, predictor_vars]) ## Similarly to the LASSO model, the data needs to be in matrix form
  n_features <- ncol(RNN_subset_matrix)
  
  x_data <- array(RNN_subset_matrix, dim = c(n_samples, timesteps, n_features)) ## the Keras package requires the dataframe to be split into arrays of observations which capture sequential data split by the timestep (in this instance, it is simply rows)
  y_data <- subset_data$holiday_adjusted_log_load
  
  model <- keras_model_sequential() %>%   ## Building the RNN model
    layer_lstm(units = n_features, input_shape = c(timesteps, n_features)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1) ## We only want point forecasts hence the output is singular
  
  model %>% compile( ## Specifying how the model should measure the training accuracy and therefore make improvements
    loss = "mae",
    optimizer = optimizer_rmsprop()
  )
  
  history <- model %>% fit( ## Training the RNN
    x_data, y_data,
    epochs = 100, ## Number of training iterations
    batch_size = 24, ## Number of observations parsed to the model at any epoch which it will be trained on. This was set to 24 to mimic the forecasting horizon and the information it would be parsed in testing
    validation_split = 0.5, ## how the original training dataset is split into training and validation
    verbose = 0 ## Stops outputs from the keras package on training time and model performance over epochs
  )

  model_name <- paste0("RNN_", subset_length/24, "days") ## Naming the RNN
  updatedBOA_model_list[[model_name]] <- model ## Saving the RNN model

  lag_terms <- paste0("s(log_load_lag_", GAM_lags, ")")
  formula_str <- paste(
    "holiday_adjusted_log_load ~",
    "s(log_load_lag_24) +",
    "s(log_load_lag_168) +",
    paste(lag_terms, collapse = " + "), "+",
    "s(hour_of_day, bs = 'cc') +",
    "s(hour_of_day, by = day_of_week, bs = 'cc') +",
    "s(day_of_week, bs = 're') +",
    "s(Temperature_C) +",
    "s(Temperature_C, by = day_of_week) +",
    "s(Cloud_Cover) +",
    "s(Cloud_Cover, by = day_of_week) +",
    "s(Temperature_C, Cloud_Cover) +",
    "s(Pressure_kpa) +",
    "s(Wind_Speed_kmh) +",
    "s(Wind_Direction_EW) +",
    "s(Wind_Direction_NS)"
  )
  variables_of_interest <- unique(c(
    paste0("log_load_lag_", c(24, 168, GAM_lags)),
    "hour_of_day",
    "day_of_week",
    "Temperature_C",
    "Cloud_Cover",
    "Pressure_kpa",
    "Wind_Speed_kmh",
    "Wind_Direction_EW",
    "Wind_Direction_NS"
  ))
  num_unique_values <- sapply(variables_of_interest, function(var) {
    if (var %in% names(subset_data)) {
      length(unique(subset_data[[var]]))
    } else {
      NA
    }
  })
  if (any(num_unique_values < 11, na.rm = TRUE)) {
    lag_terms_nosmooth <- paste0("log_load_lag_", GAM_lags)
    formula_str_nosmooth <- paste(
      "holiday_adjusted_log_load ~",
      "log_load_lag_24 +",
      "log_load_lag_168 +",
      paste(lag_terms_nosmooth, collapse = " + "), "+",
      "hour_of_day + day_of_week +",
      "hour_of_day:day_of_week +",
      "Temperature_C +",
      "Temperature_C:day_of_week +",
      "Cloud_Cover +",
      "Cloud_Cover:day_of_week +",
      "Temperature_C:Cloud_Cover +",
      "Pressure_kpa +",
      "Wind_Speed_kmh +",
      "Wind_Direction_EW +",
      "Wind_Direction_NS"
    )
    gam_formula <- as.formula(formula_str_nosmooth)
  } else {
    gam_formula <- as.formula(formula_str)
  }
  gam_fit <- gam(gam_formula, data = subset_data, method = "REML")
  model_name <- paste0("GAM_", subset_length/24, "days")
  updatedBOA_model_list[[model_name]] <- gam_fit
  exclude_vars <- c("holiday_adjusted_log_load", "day_of_week", paste0("log_load_lag_", GAM_lags_unique))
  predictor_vars <- setdiff(names(subset_data), exclude_vars)
  x <- model.matrix(~ . - 1, data = subset_data[, predictor_vars])
  y <- subset_data$holiday_adjusted_log_load
  lasso_fit <- cv.glmnet(x, y, alpha = 1)
  model_name <- paste0("LASSO_", subset_length/24, "days")
  updatedBOA_model_list[[model_name]] <- lasso_fit
}
end_time <- Sys.time()
BOA_modelCreation_runTime <- end_time - start_time
print(BOA_modelCreation_runTime)

## Creating the updated smoothed BOA to compare if replacing the AR(P) would have had a better performance (lower MAE)
updatedBOA_application<- Smoothed_BOA_algorithm(model_list = updatedBOA_model_list,
                                                testing_dataframe = Testing_ts,
                                                horizon = 24,
                                                response_variable = "holiday_adjusted_log_load",
                                                lambda = 10)
mean_weights <- list()
mean_weights$STL_ES <- mean(updatedBOA_application$d_h_k_weights[grepl("^STL_ES_", rownames(updatedBOA_application$d_h_k_weights)), ])
mean_weights$RNN <- mean(updatedBOA_application$d_h_k_weights[grepl("^RNN_", rownames(updatedBOA_application$d_h_k_weights)), ])
mean_weights$GAM <- mean(updatedBOA_application$d_h_k_weights[grepl("^GAM_", rownames(updatedBOA_application$d_h_k_weights)), ])
mean_weights$LASSO <- mean(updatedBOA_application$d_h_k_weights[grepl("^LASSO_", rownames(updatedBOA_application$d_h_k_weights)), ])

## Print the results
print(paste("The average weights for STL_ES models =", mean_weights$STL_ES))
print(paste("The average weights for RNN models =", mean_weights$RNN))
print(paste("The average weights for GAM models =", mean_weights$GAM))
print(paste("The average weights for LASSO models =", mean_weights$LASSO))
lowest_contribution_model <- names(mean_weights)[which.min(unlist(mean_weights))]
print(paste("Model group with the lowest contribution (weights) to predictions =", lowest_contribution_model))
print(paste("The MAE (kWh) from attempting to improve Ziel's model ensemble parsed to the Smoothed BOA is =",exp(mean(updatedBOA_application$d_h_absoluteError))))
ifelse(exp(mean(updatedBOA_application$d_h_absoluteError)) < exp(mean(Smoothed_BOA_application$d_h_absoluteError)),
       print(paste("The updated model ensemble provides better performance than Ziel's original by",
                   exp(mean(updatedBOA_application$d_h_absoluteError)) - exp(mean(Smoothed_BOA_application$d_h_absoluteError)), "kWh error")),
       print(paste("The original model ensemble recorded in Ziel's outperforms the suggested in this section by",
                   exp(mean(Smoothed_BOA_application$d_h_absoluteError)) - exp(mean(updatedBOA_application$d_h_absoluteError)), "kWh error")))