
### Generate the forecasts for CH4cast from SWI temp scaling model and FLARE

# set nmembers to match FLARE's default output ensemble number (210)
nmembers = 210
ebu <- read_csv("./input/observed/observed_ebu_rates.csv")

#Forecast for 03 June 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_527 <- ebu %>% filter(date == "2019-05-27")
ebu_527 <- na.omit(ebu_527)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_527 <- data.frame("full_time_day" = as.POSIXct("2019-05-27"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_527$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_527$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_603 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_527[1])
  }
  
  if(hold_methane_obs){
    curr_methane_527 <- mean(ebu_527$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_527 <- rnorm(1, ebu_527$log_ebu_rate_mg_m2_d, SE(ebu_527$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_603[m] <- curr_methane_527 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_603 <- data.frame(log_ebu_rate_forecast_603)
log_ebu_rate_forecast_603$full_time_day <- as.POSIXct("2019-06-03")
log_ebu_rate_forecast_603 <- melt(log_ebu_rate_forecast_603, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_603$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_603 <- var((log_ebu_rate_forecast_603$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 10 June 19
#############################################################################################################

# Create a current methane ebullition observation as the AR term in the model.
ebu_603 <- ebu %>% filter(date == "2019-06-03")
ebu_603 <- na.omit(ebu_603)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_603 <- data.frame("full_time_day" = as.POSIXct("2019-06-03"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_603$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_603$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_610 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_603[1])
  }
  
  if(hold_methane_obs){
    curr_methane_603 <- mean(ebu_603$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_603 <- rnorm(1, ebu_603$log_ebu_rate_mg_m2_d, SE(ebu_603$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_610[m] <- curr_methane_603 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_610 <- data.frame(log_ebu_rate_forecast_610)
log_ebu_rate_forecast_610$full_time_day <- as.POSIXct("2019-06-10")
log_ebu_rate_forecast_610 <- melt(log_ebu_rate_forecast_610, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_610$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_610 <- var((log_ebu_rate_forecast_610$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 17 June 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_610 <- ebu %>% filter(date == "2019-06-10")
ebu_610 <- na.omit(ebu_610)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_610 <- data.frame("full_time_day" = as.POSIXct("2019-06-10"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_610$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_610$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_617 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_610[1])
  }
  
  if(hold_methane_obs){
    curr_methane_610 <- mean(ebu_610$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_610 <- rnorm(1, ebu_610$log_ebu_rate_mg_m2_d, SE(ebu_610$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_617[m] <- curr_methane_610 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_617 <- data.frame(log_ebu_rate_forecast_617)
log_ebu_rate_forecast_617$full_time_day <- as.POSIXct("2019-06-17")
log_ebu_rate_forecast_617 <- melt(log_ebu_rate_forecast_617, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_617$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_617 <- var((log_ebu_rate_forecast_617$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 24 June 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_617 <- ebu %>% filter(date == "2019-06-17")
ebu_617 <- na.omit(ebu_617)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_617 <- data.frame("full_time_day" = as.POSIXct("2019-06-17"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_617$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_617$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_624 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_617[1])
  }
  
  if(hold_methane_obs){
    curr_methane_617 <- mean(ebu_617$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_617 <- rnorm(1, ebu_617$log_ebu_rate_mg_m2_d, SE(ebu_617$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_624[m] <- curr_methane_617 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_624 <- data.frame(log_ebu_rate_forecast_624)
log_ebu_rate_forecast_624$full_time_day <- as.POSIXct("2019-06-24")
log_ebu_rate_forecast_624 <- melt(log_ebu_rate_forecast_624, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_624$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_624 <- var((log_ebu_rate_forecast_624$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 01 July 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_624 <- ebu %>% filter(date == "2019-06-24")
ebu_624 <- na.omit(ebu_624)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_624 <- data.frame("full_time_day" = as.POSIXct("2019-06-24"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_624$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_624$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_701 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_624[1])
  }
  
  if(hold_methane_obs){
    curr_methane_624 <- mean(ebu_624$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_624 <- rnorm(1, ebu_624$log_ebu_rate_mg_m2_d, SE(ebu_624$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_701[m] <- curr_methane_624 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_701 <- data.frame(log_ebu_rate_forecast_701)
log_ebu_rate_forecast_701$full_time_day <- as.POSIXct("2019-07-01")
log_ebu_rate_forecast_701 <- melt(log_ebu_rate_forecast_701, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_701$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_701 <- var((log_ebu_rate_forecast_701$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 08 July 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_701 <- ebu %>% filter(date == "2019-07-01")
ebu_701 <- na.omit(ebu_701)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_701 <- data.frame("full_time_day" = as.POSIXct("2019-07-01"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_701$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_701$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_708 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_701[1])
  }
  
  if(hold_methane_obs){
    curr_methane_701 <- mean(ebu_701$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_701 <- rnorm(1, ebu_701$log_ebu_rate_mg_m2_d, SE(ebu_701$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_708[m] <- curr_methane_701 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_708 <- data.frame(log_ebu_rate_forecast_708)
log_ebu_rate_forecast_708$full_time_day <- as.POSIXct("2019-07-08")
log_ebu_rate_forecast_708 <- melt(log_ebu_rate_forecast_708, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_708$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_708 <- var((log_ebu_rate_forecast_708$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 15 July 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_708 <- ebu %>% filter(date == "2019-07-08")
ebu_708 <- na.omit(ebu_708)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_708 <- data.frame("full_time_day" = as.POSIXct("2019-07-08"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_708$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_708$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_715 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_708[1])
  }
  
  if(hold_methane_obs){
    curr_methane_708 <- mean(ebu_708$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_708 <- rnorm(1, ebu_708$log_ebu_rate_mg_m2_d, SE(ebu_708$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_715[m] <- curr_methane_708 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_715 <- data.frame(log_ebu_rate_forecast_715)
log_ebu_rate_forecast_715$full_time_day <- as.POSIXct("2019-07-15")
log_ebu_rate_forecast_715 <- melt(log_ebu_rate_forecast_715, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_715$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_715 <- var((log_ebu_rate_forecast_715$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 22 July 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_715 <- ebu %>% filter(date == "2019-07-15")
ebu_715 <- na.omit(ebu_715)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_715 <- data.frame("full_time_day" = as.POSIXct("2019-07-15"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_715$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_715$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_722 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_715[1])
  }
  
  if(hold_methane_obs){
    curr_methane_715 <- mean(ebu_715$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_715 <- rnorm(1, ebu_715$log_ebu_rate_mg_m2_d, SE(ebu_715$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_722[m] <- curr_methane_715 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_722 <- data.frame(log_ebu_rate_forecast_722)
log_ebu_rate_forecast_722$full_time_day <- as.POSIXct("2019-07-22")
log_ebu_rate_forecast_722 <- melt(log_ebu_rate_forecast_722, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_722$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_722 <- var((log_ebu_rate_forecast_722$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 29 July 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_722 <- ebu %>% filter(date == "2019-07-22")
ebu_722 <- na.omit(ebu_722)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_722 <- data.frame("full_time_day" = as.POSIXct("2019-07-22"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_722$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_722$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_729 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_722[1])
  }
  
  if(hold_methane_obs){
    curr_methane_722 <- mean(ebu_722$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_722 <- rnorm(1, ebu_722$log_ebu_rate_mg_m2_d, SE(ebu_722$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_729[m] <- curr_methane_722 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_729 <- data.frame(log_ebu_rate_forecast_729)
log_ebu_rate_forecast_729$full_time_day <- as.POSIXct("2019-07-29")
log_ebu_rate_forecast_729 <- melt(log_ebu_rate_forecast_729, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_729$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_729 <- var((log_ebu_rate_forecast_729$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 05 August 19
#############################################################################################################

# Create a current methane ebullition observation as the AR term in the model.
ebu_729 <- ebu %>% filter(date == "2019-07-29")
ebu_729 <- na.omit(ebu_729)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_729 <- data.frame("full_time_day" = as.POSIXct("2019-07-29"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_729$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_729$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_805 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_729[1])
  }
  
  if(hold_methane_obs){
    curr_methane_729 <- mean(ebu_729$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_729 <- rnorm(1, ebu_729$log_ebu_rate_mg_m2_d, SE(ebu_729$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_805[m] <- curr_methane_729 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_805 <- data.frame(log_ebu_rate_forecast_805)
log_ebu_rate_forecast_805$full_time_day <- as.POSIXct("2019-08-05")
log_ebu_rate_forecast_805 <- melt(log_ebu_rate_forecast_805, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_805$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_805 <- var((log_ebu_rate_forecast_805$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 12 August 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_805 <- ebu %>% filter(date == "2019-08-05")
ebu_805 <- na.omit(ebu_805)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_805 <- data.frame("full_time_day" = as.POSIXct("2019-08-05"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_805$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_805$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_812 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_805[1])
  }
  
  if(hold_methane_obs){
    curr_methane_805 <- mean(ebu_805$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_805 <- rnorm(1, ebu_805$log_ebu_rate_mg_m2_d, SE(ebu_805$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_812[m] <- curr_methane_805 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_812 <- data.frame(log_ebu_rate_forecast_812)
log_ebu_rate_forecast_812$full_time_day <- as.POSIXct("2019-08-12")
log_ebu_rate_forecast_812 <- melt(log_ebu_rate_forecast_812, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_812$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_812 <- var((log_ebu_rate_forecast_812$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 19 August 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_812 <- ebu %>% filter(date == "2019-08-12")
ebu_812 <- na.omit(ebu_812)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_812 <- data.frame("full_time_day" = as.POSIXct("2019-08-12"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_812$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_812$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_819 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_812[1])
  }
  
  if(hold_methane_obs){
    curr_methane_812 <- mean(ebu_812$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_812 <- rnorm(1, ebu_812$log_ebu_rate_mg_m2_d, SE(ebu_812$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_819[m] <- curr_methane_812 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_819 <- data.frame(log_ebu_rate_forecast_819)
log_ebu_rate_forecast_819$full_time_day <- as.POSIXct("2019-08-19")
log_ebu_rate_forecast_819 <- melt(log_ebu_rate_forecast_819, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_819$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_819 <- var((log_ebu_rate_forecast_819$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 28 August 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_819 <- ebu %>% filter(date == "2019-08-19")
ebu_819 <- na.omit(ebu_819)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_819 <- data.frame("full_time_day" = as.POSIXct("2019-08-19"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_819$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_819$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_828 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_819[1])
  }
  
  if(hold_methane_obs){
    curr_methane_819 <- mean(ebu_819$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_819 <- rnorm(1, ebu_819$log_ebu_rate_mg_m2_d, SE(ebu_819$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_828[m] <- curr_methane_819 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_828 <- data.frame(log_ebu_rate_forecast_828)
log_ebu_rate_forecast_828$full_time_day <- as.POSIXct("2019-08-28")
log_ebu_rate_forecast_828 <- melt(log_ebu_rate_forecast_828, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_828$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_828 <- var((log_ebu_rate_forecast_828$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 02 September 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_828 <- ebu %>% filter(date == "2019-08-28")
ebu_828 <- na.omit(ebu_828)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_828 <- data.frame("full_time_day" = as.POSIXct("2019-08-28"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_828$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_828$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_902 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_828[1])
  }
  
  if(hold_methane_obs){
    curr_methane_828 <- mean(ebu_828$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_828 <- rnorm(1, ebu_828$log_ebu_rate_mg_m2_d, SE(ebu_828$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_902[m] <- curr_methane_828 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_902 <- data.frame(log_ebu_rate_forecast_902)
log_ebu_rate_forecast_902$full_time_day <- as.POSIXct("2019-09-02")
log_ebu_rate_forecast_902 <- melt(log_ebu_rate_forecast_902, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_902$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_902 <- var((log_ebu_rate_forecast_902$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 11 September 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_902 <- ebu %>% filter(date == "2019-09-02")
ebu_902 <- na.omit(ebu_902)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_902 <- data.frame("full_time_day" = as.POSIXct("2019-09-02"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_902$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_902$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_911 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_902[1])
  }
  
  if(hold_methane_obs){
    curr_methane_902 <- mean(ebu_902$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_902 <- rnorm(1, ebu_902$log_ebu_rate_mg_m2_d, SE(ebu_902$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_911[m] <- curr_methane_902 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_911 <- data.frame(log_ebu_rate_forecast_911)
log_ebu_rate_forecast_911$full_time_day <- as.POSIXct("2019-09-11")
log_ebu_rate_forecast_911 <- melt(log_ebu_rate_forecast_911, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_911$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_911 <- var((log_ebu_rate_forecast_911$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 20 September 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_911 <- ebu %>% filter(date == "2019-09-11")
ebu_911 <- na.omit(ebu_911)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_911 <- data.frame("full_time_day" = as.POSIXct("2019-09-11"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_911$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_911$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_920 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_911[1])
  }
  
  if(hold_methane_obs){
    curr_methane_911 <- mean(ebu_911$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_911 <- rnorm(1, ebu_911$log_ebu_rate_mg_m2_d, SE(ebu_911$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_920[m] <- curr_methane_911 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_920 <- data.frame(log_ebu_rate_forecast_920)
log_ebu_rate_forecast_920$full_time_day <- as.POSIXct("2019-09-20")
log_ebu_rate_forecast_920 <- melt(log_ebu_rate_forecast_920, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_920$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_920 <- var((log_ebu_rate_forecast_920$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 27 September 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_920 <- ebu %>% filter(date == "2019-09-20")
ebu_920 <- na.omit(ebu_920)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_920 <- data.frame("full_time_day" = as.POSIXct("2019-09-20"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_920$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_920$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_927 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_920[1])
  }
  
  if(hold_methane_obs){
    curr_methane_920 <- mean(ebu_920$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_920 <- rnorm(1, ebu_920$log_ebu_rate_mg_m2_d, SE(ebu_920$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_927[m] <- curr_methane_920 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}
#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_927 <- data.frame(log_ebu_rate_forecast_927)
log_ebu_rate_forecast_927$full_time_day <- as.POSIXct("2019-09-27")
log_ebu_rate_forecast_927 <- melt(log_ebu_rate_forecast_927, id.vars = c("full_time_day"),
                                  variable.name = "Ensemble",
                                  value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_927$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_927 <- var((log_ebu_rate_forecast_927$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 02 October 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_927 <- ebu %>% filter(date == "2019-09-27")
ebu_927 <- na.omit(ebu_927)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_927 <- data.frame("full_time_day" = as.POSIXct("2019-09-27"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_927$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_927$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1002 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_927[1])
  }
  
  if(hold_methane_obs){
    curr_methane_927 <- mean(ebu_927$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_927 <- rnorm(1, ebu_927$log_ebu_rate_mg_m2_d, SE(ebu_927$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1002[m] <- curr_methane_927 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1002 <- data.frame(log_ebu_rate_forecast_1002)
log_ebu_rate_forecast_1002$full_time_day <- as.POSIXct("2019-10-02")
log_ebu_rate_forecast_1002 <- melt(log_ebu_rate_forecast_1002, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1002$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1002 <- var((log_ebu_rate_forecast_1002$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 11 October 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_1002 <- ebu %>% filter(date == "2019-10-02")
ebu_1002 <- na.omit(ebu_1002)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1002 <- data.frame("full_time_day" = as.POSIXct("2019-10-02"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1002$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_1002$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1011 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1002[1])
  }
  
  if(hold_methane_obs){
    curr_methane_1002 <- mean(ebu_1002$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1002 <- rnorm(1, ebu_1002$log_ebu_rate_mg_m2_d, SE(ebu_1002$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1011[m] <- curr_methane_1002 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1011 <- data.frame(log_ebu_rate_forecast_1011)
log_ebu_rate_forecast_1011$full_time_day <- as.POSIXct("2019-10-11")
log_ebu_rate_forecast_1011 <- melt(log_ebu_rate_forecast_1011, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1011$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1011 <- var((log_ebu_rate_forecast_1011$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 16 October 19
#############################################################################################################

# Create a current methane ebullition observation as the AR term in the model.
ebu_1011 <- ebu %>% filter(date == "2019-10-11")
ebu_1011 <- na.omit(ebu_1011)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1011 <- data.frame("full_time_day" = as.POSIXct("2019-10-11"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1011$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_1011$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1016 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1011[1])
  }
  
  if(hold_methane_obs){
    curr_methane_1011 <- mean(ebu_1011$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1011 <- rnorm(1, ebu_1011$log_ebu_rate_mg_m2_d, SE(ebu_1011$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1016[m] <- curr_methane_1011 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1016 <- data.frame(log_ebu_rate_forecast_1016)
log_ebu_rate_forecast_1016$full_time_day <- as.POSIXct("2019-10-16")
log_ebu_rate_forecast_1016 <- melt(log_ebu_rate_forecast_1016, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1016$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1016 <- var((log_ebu_rate_forecast_1016$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 23 October 19
#############################################################################################################

# Create a current methane ebullition observation as the AR term in the model.
ebu_1016 <- ebu %>% filter(date == "2019-10-16")
ebu_1016 <- na.omit(ebu_1016)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1016 <- data.frame("full_time_day" = as.POSIXct("2019-10-16"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1016$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_1016$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1023 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1016[1])
  }
  
  if(hold_methane_obs){
    curr_methane_1016 <- mean(ebu_1016$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1016 <- rnorm(1, ebu_1016$log_ebu_rate_mg_m2_d, SE(ebu_1016$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1023[m] <- curr_methane_1016 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1023 <- data.frame(log_ebu_rate_forecast_1023)
log_ebu_rate_forecast_1023$full_time_day <- as.POSIXct("2019-10-23")
log_ebu_rate_forecast_1023 <- melt(log_ebu_rate_forecast_1023, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1023$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1023 <- var((log_ebu_rate_forecast_1023$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 30 October 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_1023 <- ebu %>% filter(date == "2019-10-23")
ebu_1023 <- na.omit(ebu_1023)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1023 <- data.frame("full_time_day" = as.POSIXct("2019-10-23"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1023$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_1023$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1030 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1023[1])
  }
  
  if(hold_methane_obs){
    curr_methane_1023 <- mean(ebu_1023$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1023 <- rnorm(1, ebu_1023$log_ebu_rate_mg_m2_d, SE(ebu_1023$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1030[m] <- curr_methane_1023 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1030 <- data.frame(log_ebu_rate_forecast_1030)
log_ebu_rate_forecast_1030$full_time_day <- as.POSIXct("2019-10-30")
log_ebu_rate_forecast_1030 <- melt(log_ebu_rate_forecast_1030, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1030$ebullition_prediction, breaks = 100)
# Calculatethe variance 
var_ebu_1030 <- var((log_ebu_rate_forecast_1030$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 07 November 19
#############################################################################################################
# Create a current methane ebullition observation as the AR term in the model.
ebu_1030 <- ebu %>% filter(date == "2019-10-30")
ebu_1030 <- na.omit(ebu_1030)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1030 <- data.frame("full_time_day" = as.POSIXct("2019-10-30"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1030$log_ebu_rate_mg_m2_d), "SE" = SE(ebu_1030$log_ebu_rate_mg_m2_d), "Data" = "observation")

flare_nmembers <- 210
n_temp_members <- 210

#Uncertaintity paritioning for Ebullition
hold_methane_process <- F
hold_methane_obs <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1107 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1030[1])
  }

  if(hold_methane_obs){
    curr_methane_1030 <- mean(ebu_1030$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1030 <- rnorm(1, ebu_1030$log_ebu_rate_mg_m2_d, SE(ebu_1030$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
  
  log_ebu_rate_forecast_1107[m] <- curr_methane_1030 + process_error
  
  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1107 <- data.frame(log_ebu_rate_forecast_1107)
log_ebu_rate_forecast_1107$full_time_day <- as.POSIXct("2019-11-07")
log_ebu_rate_forecast_1107 <- melt(log_ebu_rate_forecast_1107, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

#hist(log_ebu_rate_forecast_1107$ebullition_prediction, breaks = 100)
# Calculatethe variance 
var_ebu_1107 <- var((log_ebu_rate_forecast_1107$ebullition_prediction), na = T)
#############################################################################################################

### Rbind all the ebullition forecasts together
#############################################################################################################
ensemble_forecasts_ch4_null <- rbind(log_ebu_rate_forecast_603,
                                log_ebu_rate_forecast_610,
                                log_ebu_rate_forecast_617,
                                log_ebu_rate_forecast_624,
                                log_ebu_rate_forecast_701,
                                log_ebu_rate_forecast_708,
                                log_ebu_rate_forecast_715,
                                log_ebu_rate_forecast_722,
                                log_ebu_rate_forecast_729,
                                log_ebu_rate_forecast_805,
                                log_ebu_rate_forecast_812,
                                log_ebu_rate_forecast_819,
                                log_ebu_rate_forecast_828,
                                log_ebu_rate_forecast_902,
                                log_ebu_rate_forecast_911,
                                log_ebu_rate_forecast_920,
                                log_ebu_rate_forecast_927,
                                log_ebu_rate_forecast_1002,
                                log_ebu_rate_forecast_1011,
                                log_ebu_rate_forecast_1016,
                                log_ebu_rate_forecast_1023,
                                log_ebu_rate_forecast_1030,
                                log_ebu_rate_forecast_1107,
                                deparse.level = 1)

# Remove the furthest outliers from the forecasts
ensemble_forecasts_99th_ch4_null <- ensemble_forecasts_ch4_null %>% group_by(full_time_day) %>%
  filter(ebullition_prediction < quantile(ebullition_prediction, 0.99, na.rm = T)) %>% filter(ebullition_prediction > quantile(ebullition_prediction, 0.01, na.rm = T))

# Take the mean of the forecasts
mean_forecast_ch4_null <- ensemble_forecasts_ch4_null %>% select(-Ensemble) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
SE_forecast_ch4_null <- ensemble_forecasts_ch4_null %>% select(-Ensemble) %>% group_by(full_time_day) %>% summarize_all(funs(SE))
names(SE_forecast_ch4_null)[2] <- "SE"

mean_forecast_ch4_null <- left_join(mean_forecast_ch4_null, SE_forecast_ch4_null, by = "full_time_day")
#############################################################################################################
mean_observe_all <- rbind(x_527,
                          x_603,
                          x_610,
                          x_617,
                          x_624,
                          x_701,
                          x_708,
                          x_715,
                          x_722,
                          x_729,
                          x_805,
                          x_812,
                          x_819,
                          x_828,
                          x_902,
                          x_911,
                          x_920,
                          x_927,
                          x_1002,
                          x_1011,
                          x_1016,
                          x_1023,
                          x_1030,
                          x_1107,
                          deparse.level = 1)

### Total Variance ###
var_ebu_forecast_null<- rbind(var_ebu_603,
                         var_ebu_610,
                         var_ebu_617,
                         var_ebu_624,
                         var_ebu_701,
                         var_ebu_708,
                         var_ebu_715,
                         var_ebu_722,
                         var_ebu_729,
                         var_ebu_805,
                         var_ebu_812,
                         var_ebu_819,
                         var_ebu_828,
                         var_ebu_902,
                         var_ebu_911,
                         var_ebu_920,
                         var_ebu_927,
                         var_ebu_1002,
                         var_ebu_1011,
                         var_ebu_1016, 
                         var_ebu_1023, 
                         var_ebu_1030, 
                         var_ebu_1107)

var_ebu_forecast_null <- cbind(mean_observe_all[2:24,1],
                          data.frame((var_ebu_forecast_null)))
names(var_ebu_forecast_null) <- c("date", "total_variance")




