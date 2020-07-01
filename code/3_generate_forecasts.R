### Generate the forecasts for CH4cast from SWI temp scaling model and FLARE

# set nmembers to match FLARE's default output ensemble number (210)
nmembers = 210
ebu <- read_csv("./input/observed/observed_ebu_rates.csv")

#Forecast for 03 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_05_24_2019_05_27_F_10_552020_18_35.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-05-27") %>% 
  filter(full_time_day <= "2019-06-03") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-05-27", tz = "EST"), as.POSIXct("2019-06-03", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_527 <- ebu %>% filter(date == "2019-05-27")
ebu_527 <- na.omit(ebu_527)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_527 <- data.frame("full_time_day" = as.POSIXct("2019-05-27"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_527$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_527$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_603 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_603_610 <- colMeans(master_temp_chain_603_610)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_603_610)),1)
    curr_temp_pars_603_610 <- master_temp_chain_603_610[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_603_610[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_603[m] <- curr_temp_pars_603_610[1]*flare_temp + curr_temp_pars_603_610[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_603 <- data.frame(temp_up_603)
temp_up_603 <- as.vector(temp_up_603[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_603_test <- data.frame(temp_up_603)
temp_up_603_test$full_time_day <- as.POSIXct("2019-06-03")
kf_mean_temp_up_603 <- melt(temp_up_603_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_603 <- var(kf_mean_temp_up_603$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_603 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_527 <- colMeans(master_chain_527)
  }else{
    curr_pars_index_527 <- sample(seq(1, nrow(master_chain_527)),1)
    curr_pars_527 <- master_chain_527[curr_pars_index_527,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_527[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_603))
  }else{
    sed_model_temp <- temp_up_603[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_527 <- mean(ebu_527$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_527 <- rnorm(1, ebu_527$log_ebu_rate_mg_m2_d, sd(ebu_527$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_603[m] <- curr_pars_527[1] + curr_pars_527[2] * curr_methane_527 + curr_pars_527[3] * sed_model_temp + process_error
  
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

hist(log_ebu_rate_forecast_603$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_603 <- var((log_ebu_rate_forecast_603$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 10 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_05_30_2019_06_03_F_10_552020_19_17.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:15,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:15,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:15,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:15,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-06-03") %>% 
  filter(full_time_day <= "2019-06-10") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))


temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_603 <- ebu %>% filter(date == "2019-06-03")
ebu_603 <- na.omit(ebu_603)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_603 <- data.frame("full_time_day" = as.POSIXct("2019-06-03"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_603$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_603$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_610 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_603_610 <- colMeans(master_temp_chain_603_610)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_603_610)),1)
    curr_temp_pars_603_610 <- master_temp_chain_603_610[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_603_610[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_610[m] <- curr_temp_pars_603_610[1]*flare_temp + curr_temp_pars_603_610[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_610 <- data.frame(temp_up_610)
temp_up_610 <- as.vector(temp_up_610[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_610_test <- data.frame(temp_up_610)
temp_up_610_test$full_time_day <- as.POSIXct("2019-06-10")
kf_mean_temp_up_610 <- melt(temp_up_610_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_610 <- var(kf_mean_temp_up_610$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_610 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_603 <- colMeans(master_chain_603)
  }else{
    curr_pars_index_603 <- sample(seq(1, nrow(master_chain_603)),1)
    curr_pars_603 <- master_chain_603[curr_pars_index_603,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_603[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_610))
  }else{
    sed_model_temp <- temp_up_610[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_603 <- mean(ebu_603$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_603 <- rnorm(1, ebu_603$log_ebu_rate_mg_m2_d, sd(ebu_603$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_610[m] <- curr_pars_603[1] + curr_pars_603[2] * curr_methane_603 + curr_pars_603[3] * sed_model_temp + process_error
  
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

hist(log_ebu_rate_forecast_610$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_610 <- var((log_ebu_rate_forecast_610$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 17 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_06_07_2019_06_10_F_10_552020_19_34.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-06-10") %>% 
  filter(full_time_day <= "2019-06-17") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-06-10", tz = "EST"), as.POSIXct("2019-06-17", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_610 <- ebu %>% filter(date == "2019-06-10")
ebu_610 <- na.omit(ebu_610)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_610 <- data.frame("full_time_day" = as.POSIXct("2019-06-10"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_610$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_610$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_617 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_610 <- colMeans(master_temp_chain_603_610)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_603_610)),1)
    curr_temp_pars_610 <- master_temp_chain_603_610[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_610[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_617[m] <- curr_temp_pars_610[1]*flare_temp + curr_temp_pars_610[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_617 <- data.frame(temp_up_617)
temp_up_617 <- as.vector(temp_up_617[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_617_test <- data.frame(temp_up_617)
temp_up_617_test$full_time_day <- as.POSIXct("2019-06-17")
kf_mean_temp_up_617 <- melt(temp_up_617_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_617 <- var(kf_mean_temp_up_617$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_617 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_610 <- colMeans(master_chain_610)
  }else{
    curr_pars_index_610 <- sample(seq(1, nrow(master_chain_610)),1)
    curr_pars_610 <- master_chain_610[curr_pars_index_610,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_610[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_617))
  }else{
    sed_model_temp <- temp_up_617[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_610 <- mean(ebu_610$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_610 <- rnorm(1, ebu_610$log_ebu_rate_mg_m2_d, sd(ebu_610$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model


    log_ebu_rate_forecast_617[m] <- curr_pars_610[1] + curr_pars_610[2] * curr_methane_610 + curr_pars_610[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_617$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_617 <- var((log_ebu_rate_forecast_617$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 24 June 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_06_14_2019_06_17_F_10_552020_20_49.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-06-17") %>% 
  filter(full_time_day <= "2019-06-24") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-06-17", tz = "EST"), as.POSIXct("2019-06-24", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_617 <- ebu %>% filter(date == "2019-06-17")
ebu_617 <- na.omit(ebu_617)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_617 <- data.frame("full_time_day" = as.POSIXct("2019-06-17"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_617$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_617$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_624 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_617 <- colMeans(master_temp_chain_617)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_617)),1)
    curr_temp_pars_617 <- master_temp_chain_617[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_617[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_624[m] <- curr_temp_pars_617[1]*flare_temp + curr_temp_pars_617[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_624 <- data.frame(temp_up_624)
temp_up_624 <- as.vector(temp_up_624[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_624_test <- data.frame(temp_up_624)
temp_up_624_test$full_time_day <- as.POSIXct("2019-06-24")
kf_mean_temp_up_624 <- melt(temp_up_624_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_624 <- var(kf_mean_temp_up_624$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_624 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_617 <- colMeans(master_chain_617)
  }else{
    curr_pars_index_617 <- sample(seq(1, nrow(master_chain_617)),1)
    curr_pars_617 <- master_chain_617[curr_pars_index_617,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_617[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_624))
  }else{
    sed_model_temp <- temp_up_624[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_617 <- mean(ebu_617$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_617 <- rnorm(1, ebu_617$log_ebu_rate_mg_m2_d, sd(ebu_617$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model


    log_ebu_rate_forecast_624[m] <- curr_pars_617[1] + curr_pars_617[2] * curr_methane_617 + curr_pars_617[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_624$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_624 <- var((log_ebu_rate_forecast_624$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 01 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_06_21_2019_06_24_F_10_552020_22_47.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-06-24") %>% 
  filter(full_time_day <= "2019-07-01") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-06-24", tz = "EST"), as.POSIXct("2019-07-01", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_624 <- ebu %>% filter(date == "2019-06-24")
ebu_624 <- na.omit(ebu_624)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_624 <- data.frame("full_time_day" = as.POSIXct("2019-06-24"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_624$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_624$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_701 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_624 <- colMeans(master_temp_chain_624)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_624)),1)
    curr_temp_pars_624 <- master_temp_chain_624[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_624[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_701[m] <- curr_temp_pars_624[1]*flare_temp + curr_temp_pars_624[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_701 <- data.frame(temp_up_701)
temp_up_701 <- as.vector(temp_up_701[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_701_test <- data.frame(temp_up_701)
temp_up_701_test$full_time_day <- as.POSIXct("2019-07-01")
kf_mean_temp_up_701 <- melt(temp_up_701_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_701 <- var(kf_mean_temp_up_701$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_701 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_624 <- colMeans(master_chain_624)
  }else{
    curr_pars_index_624 <- sample(seq(1, nrow(master_chain_624)),1)
    curr_pars_624 <- master_chain_624[curr_pars_index_624,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_624[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_701))
  }else{
    sed_model_temp <- temp_up_701[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_624 <- mean(ebu_624$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_624 <- rnorm(1, ebu_624$log_ebu_rate_mg_m2_d, sd(ebu_624$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model


    log_ebu_rate_forecast_701[m] <- curr_pars_624[1] + curr_pars_624[2] * curr_methane_624 + curr_pars_624[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_701$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_701 <- var((log_ebu_rate_forecast_701$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 08 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_06_28_2019_07_01_F_10_552020_23_44.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-07-01") %>% 
  filter(full_time_day <= "2019-07-08") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-07-01", tz = "EST"), as.POSIXct("2019-07-08", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_701 <- ebu %>% filter(date == "2019-07-01")
ebu_701 <- na.omit(ebu_701)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_701 <- data.frame("full_time_day" = as.POSIXct("2019-07-01"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_701$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_701$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_708 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_701 <- colMeans(master_temp_chain_701)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_701)),1)
    curr_temp_pars_701 <- master_temp_chain_701[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_701[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_708[m] <- curr_temp_pars_701[1]*flare_temp + curr_temp_pars_701[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_708 <- data.frame(temp_up_708)
temp_up_708 <- as.vector(temp_up_708[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_708_test <- data.frame(temp_up_708)
temp_up_708_test$full_time_day <- as.POSIXct("2019-07-08")
kf_mean_temp_up_708 <- melt(temp_up_708_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_708 <- var(kf_mean_temp_up_708$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_708 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_701 <- colMeans(master_chain_701)
  }else{
    curr_pars_index_701 <- sample(seq(1, nrow(master_chain_701)),1)
    curr_pars_701 <- master_chain_701[curr_pars_index_701,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_701[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_708))
  }else{
    sed_model_temp <- temp_up_708[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_701 <- mean(ebu_701$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_701 <- rnorm(1, ebu_701$log_ebu_rate_mg_m2_d, sd(ebu_701$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_708[m] <- curr_pars_701[1] + curr_pars_701[2] * curr_methane_701 + curr_pars_701[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_708$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_708 <- var((log_ebu_rate_forecast_708$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 15 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_07_05_2019_07_08_F_10_562020_0_12.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-07-08") %>% 
  filter(full_time_day <= "2019-07-15") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-07-08", tz = "EST"), as.POSIXct("2019-07-15", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_708 <- ebu %>% filter(date == "2019-07-08")
ebu_708 <- na.omit(ebu_708)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_708 <- data.frame("full_time_day" = as.POSIXct("2019-07-08"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_708$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_708$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_715 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_708 <- colMeans(master_temp_chain_708)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_708)),1)
    curr_temp_pars_708 <- master_temp_chain_708[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_708[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_715[m] <- curr_temp_pars_708[1]*flare_temp + curr_temp_pars_708[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_715 <- data.frame(temp_up_715)
temp_up_715 <- as.vector(temp_up_715[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_715_test <- data.frame(temp_up_715)
temp_up_715_test$full_time_day <- as.POSIXct("2019-07-15")
kf_mean_temp_up_715 <- melt(temp_up_715_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_715 <- var(kf_mean_temp_up_715$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_715 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_708 <- colMeans(master_chain_708)
  }else{
    curr_pars_index_708 <- sample(seq(1, nrow(master_chain_708)),1)
    curr_pars_708 <- master_chain_708[curr_pars_index_708,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_708[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_715))
  }else{
    sed_model_temp <- temp_up_715[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_708 <- mean(ebu_708$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_708 <- rnorm(1, ebu_708$log_ebu_rate_mg_m2_d, sd(ebu_708$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model


    log_ebu_rate_forecast_715[m] <- curr_pars_708[1] + curr_pars_708[2] * curr_methane_708 + curr_pars_708[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_715$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_715 <- var((log_ebu_rate_forecast_715$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 22 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_07_12_2019_07_15_F_10_562020_0_33.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-07-15") %>% 
  filter(full_time_day <= "2019-07-22") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-07-15", tz = "EST"), as.POSIXct("2019-07-22", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_715 <- ebu %>% filter(date == "2019-07-15")
ebu_715 <- na.omit(ebu_715)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_715 <- data.frame("full_time_day" = as.POSIXct("2019-07-15"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_715$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_715$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_722 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_715 <- colMeans(master_temp_chain_715)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_715)),1)
    curr_temp_pars_715 <- master_temp_chain_715[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_715[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_722[m] <- curr_temp_pars_715[1]*flare_temp + curr_temp_pars_715[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_722 <- data.frame(temp_up_722)
temp_up_722 <- as.vector(temp_up_722[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_722_test <- data.frame(temp_up_722)
temp_up_722_test$full_time_day <- as.POSIXct("2019-07-22")
kf_mean_temp_up_722 <- melt(temp_up_722_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_722 <- var(kf_mean_temp_up_722$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_722 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_715 <- colMeans(master_chain_715)
  }else{
    curr_pars_index_715 <- sample(seq(1, nrow(master_chain_715)),1)
    curr_pars_715 <- master_chain_715[curr_pars_index_715,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_715[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_722))
  }else{
    sed_model_temp <- temp_up_722[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_715 <- mean(ebu_715$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_715 <- rnorm(1, ebu_715$log_ebu_rate_mg_m2_d, sd(ebu_715$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_722[m] <- curr_pars_715[1] + curr_pars_715[2] * curr_methane_715 + curr_pars_715[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_722$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_722 <- var((log_ebu_rate_forecast_722$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 29 July 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_07_19_2019_07_22_F_10_562020_0_47.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-07-22") %>% 
  filter(full_time_day <= "2019-07-29") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-07-22", tz = "EST"), as.POSIXct("2019-07-29", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_722 <- ebu %>% filter(date == "2019-07-22")
ebu_722 <- na.omit(ebu_722)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_722 <- data.frame("full_time_day" = as.POSIXct("2019-07-22"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_722$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_722$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_729 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_722 <- colMeans(master_temp_chain_722)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_722)),1)
    curr_temp_pars_722 <- master_temp_chain_722[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_722[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_729[m] <- curr_temp_pars_722[1]*flare_temp + curr_temp_pars_722[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_729 <- data.frame(temp_up_729)
temp_up_729 <- as.vector(temp_up_729[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_729_test <- data.frame(temp_up_729)
temp_up_729_test$full_time_day <- as.POSIXct("2019-07-29")
kf_mean_temp_up_729 <- melt(temp_up_729_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_729 <- var(kf_mean_temp_up_729$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_729 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_722 <- colMeans(master_chain_722)
  }else{
    curr_pars_index_722 <- sample(seq(1, nrow(master_chain_722)),1)
    curr_pars_722 <- master_chain_722[curr_pars_index_722,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_722[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_729))
  }else{
    sed_model_temp <- temp_up_729[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_722 <- mean(ebu_722$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_722 <- rnorm(1, ebu_722$log_ebu_rate_mg_m2_d, sd(ebu_722$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_729[m] <- curr_pars_722[1] + curr_pars_722[2] * curr_methane_722 + curr_pars_722[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_729$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_729 <- var((log_ebu_rate_forecast_729$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 05 August 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_07_26_2019_07_29_F_10_562020_1_6.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-07-29") %>% 
  filter(full_time_day <= "2019-08-05") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-07-29", tz = "EST"), as.POSIXct("2019-08-05", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_729 <- ebu %>% filter(date == "2019-07-29")
ebu_729 <- na.omit(ebu_729)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_729 <- data.frame("full_time_day" = as.POSIXct("2019-07-29"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_729$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_729$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_805 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_729 <- colMeans(master_temp_chain_729)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_729)),1)
    curr_temp_pars_729 <- master_temp_chain_729[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_729[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_805[m] <- curr_temp_pars_729[1]*flare_temp + curr_temp_pars_729[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_805 <- data.frame(temp_up_805)
temp_up_805 <- as.vector(temp_up_805[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_805_test <- data.frame(temp_up_805)
temp_up_805_test$full_time_day <- as.POSIXct("2019-08-05")
kf_mean_temp_up_805 <- melt(temp_up_805_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_805 <- var(kf_mean_temp_up_805$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_805 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_729 <- colMeans(master_chain_729)
  }else{
    curr_pars_index_729 <- sample(seq(1, nrow(master_chain_729)),1)
    curr_pars_729 <- master_chain_729[curr_pars_index_729,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_729[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_805))
  }else{
    sed_model_temp <- temp_up_805[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_729 <- mean(ebu_729$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_729 <- rnorm(1, ebu_729$log_ebu_rate_mg_m2_d, sd(ebu_729$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model


    log_ebu_rate_forecast_805[m] <- curr_pars_729[1] + curr_pars_729[2] * curr_methane_729 + curr_pars_729[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_805$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_805 <- var((log_ebu_rate_forecast_805$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 12 August 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_08_02_2019_08_05_F_10_562020_1_20.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-08-05") %>% 
  filter(full_time_day <= "2019-08-12") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-08-05", tz = "EST"), as.POSIXct("2019-08-12", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_805 <- ebu %>% filter(date == "2019-08-05")
ebu_805 <- na.omit(ebu_805)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_805 <- data.frame("full_time_day" = as.POSIXct("2019-08-05"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_805$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_805$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_812 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_805 <- colMeans(master_temp_chain_805)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_805)),1)
    curr_temp_pars_805 <- master_temp_chain_805[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_805[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_812[m] <- curr_temp_pars_805[1]*flare_temp + curr_temp_pars_805[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_812 <- data.frame(temp_up_812)
temp_up_812 <- as.vector(temp_up_812[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_812_test <- data.frame(temp_up_812)
temp_up_812_test$full_time_day <- as.POSIXct("2019-08-12")
kf_mean_temp_up_812 <- melt(temp_up_812_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_812 <- var(kf_mean_temp_up_812$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_812 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_805 <- colMeans(master_chain_805)
  }else{
    curr_pars_index_805 <- sample(seq(1, nrow(master_chain_805)),1)
    curr_pars_805 <- master_chain_805[curr_pars_index_805,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_805[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_812))
  }else{
    sed_model_temp <- temp_up_812[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_805 <- mean(ebu_805$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_805 <- rnorm(1, ebu_805$log_ebu_rate_mg_m2_d, sd(ebu_805$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_812[m] <- curr_pars_805[1] + curr_pars_805[2] * curr_methane_805 + curr_pars_805[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_812$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_812 <- var((log_ebu_rate_forecast_812$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 19 August 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_08_09_2019_08_12_F_10_562020_1_31.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-08-12") %>% 
  filter(full_time_day <= "2019-08-19") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-08-12", tz = "EST"), as.POSIXct("2019-08-19", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_812 <- ebu %>% filter(date == "2019-08-12")
ebu_812 <- na.omit(ebu_812)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_812 <- data.frame("full_time_day" = as.POSIXct("2019-08-12"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_812$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_812$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_819 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_812 <- colMeans(master_temp_chain_812)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_812)),1)
    curr_temp_pars_812 <- master_temp_chain_812[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_812[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_819[m] <- curr_temp_pars_812[1]*flare_temp + curr_temp_pars_812[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_819 <- data.frame(temp_up_819)
temp_up_819 <- as.vector(temp_up_819[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_819_test <- data.frame(temp_up_819)
temp_up_819_test$full_time_day <- as.POSIXct("2019-08-19")
kf_mean_temp_up_819 <- melt(temp_up_819_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_819 <- var(kf_mean_temp_up_819$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_819 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_812 <- colMeans(master_chain_812)
  }else{
    curr_pars_index_812 <- sample(seq(1, nrow(master_chain_812)),1)
    curr_pars_812 <- master_chain_812[curr_pars_index_812,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_812[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_819))
  }else{
    sed_model_temp <- temp_up_819[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_812 <- mean(ebu_812$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_812 <- rnorm(1, ebu_812$log_ebu_rate_mg_m2_d, sd(ebu_812$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_819[m] <- curr_pars_812[1] + curr_pars_812[2] * curr_methane_812 + curr_pars_812[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_819$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_819 <- var((log_ebu_rate_forecast_819$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 28 August 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_08_16_2019_08_19_F_10_562020_1_52.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-08-19") %>% 
  filter(full_time_day <= "2019-08-28") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-08-19", tz = "EST"), as.POSIXct("2019-08-28", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_819 <- ebu %>% filter(date == "2019-08-19")
ebu_819 <- na.omit(ebu_819)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_819 <- data.frame("full_time_day" = as.POSIXct("2019-08-19"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_819$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_819$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_828 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_819 <- colMeans(master_temp_chain_819)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_819)),1)
    curr_temp_pars_819 <- master_temp_chain_819[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_819[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_828[m] <- curr_temp_pars_819[1]*flare_temp + curr_temp_pars_819[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_828 <- data.frame(temp_up_828)
temp_up_828 <- as.vector(temp_up_828[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_828_test <- data.frame(temp_up_828)
temp_up_828_test$full_time_day <- as.POSIXct("2019-08-28")
kf_mean_temp_up_828 <- melt(temp_up_828_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_828 <- var(kf_mean_temp_up_828$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_828 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_819 <- colMeans(master_chain_819)
  }else{
    curr_pars_index_819 <- sample(seq(1, nrow(master_chain_819)),1)
    curr_pars_819 <- master_chain_819[curr_pars_index_819,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_819[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_828))
  }else{
    sed_model_temp <- temp_up_828[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_819 <- mean(ebu_819$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_819 <- rnorm(1, ebu_819$log_ebu_rate_mg_m2_d, sd(ebu_819$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_828[m] <- curr_pars_819[1] + curr_pars_819[2] * curr_methane_819 + curr_pars_819[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_828$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_828 <- var((log_ebu_rate_forecast_828$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 02 September 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_08_25_2019_08_28_F_10_562020_2_5.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-08-28") %>% 
  filter(full_time_day <= "2019-09-02") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-08-28", tz = "EST"), as.POSIXct("2019-09-02", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_828 <- ebu %>% filter(date == "2019-08-28")
ebu_828 <- na.omit(ebu_828)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_828 <- data.frame("full_time_day" = as.POSIXct("2019-08-28"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_828$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_828$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_902 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_828 <- colMeans(master_temp_chain_828)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_828)),1)
    curr_temp_pars_828 <- master_temp_chain_828[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_828[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_902[m] <- curr_temp_pars_828[1]*flare_temp + curr_temp_pars_828[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_902 <- data.frame(temp_up_902)
temp_up_902 <- as.vector(temp_up_902[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_902_test <- data.frame(temp_up_902)
temp_up_902_test$full_time_day <- as.POSIXct("2019-09-02")
kf_mean_temp_up_902 <- melt(temp_up_902_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_902 <- var(kf_mean_temp_up_902$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_902 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_828 <- colMeans(master_chain_828)
  }else{
    curr_pars_index_828 <- sample(seq(1, nrow(master_chain_828)),1)
    curr_pars_828 <- master_chain_828[curr_pars_index_828,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_828[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_902))
  }else{
    sed_model_temp <- temp_up_902[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_828 <- mean(ebu_828$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_828 <- rnorm(1, ebu_828$log_ebu_rate_mg_m2_d, sd(ebu_828$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_902[m] <- curr_pars_828[1] + curr_pars_828[2] * curr_methane_828 + curr_pars_828[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_902$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_902 <- var((log_ebu_rate_forecast_902$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 11 September 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_08_30_2019_09_02_F_10_562020_2_22.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-09-02") %>% 
  filter(full_time_day <= "2019-09-11") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-09-02", tz = "EST"), as.POSIXct("2019-09-11", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_902 <- ebu %>% filter(date == "2019-09-02")
ebu_902 <- na.omit(ebu_902)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_902 <- data.frame("full_time_day" = as.POSIXct("2019-09-02"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_902$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_902$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_911 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_902 <- colMeans(master_temp_chain_902)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_902)),1)
    curr_temp_pars_902 <- master_temp_chain_902[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_902[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_911[m] <- curr_temp_pars_902[1]*flare_temp + curr_temp_pars_902[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_911 <- data.frame(temp_up_911)
temp_up_911 <- as.vector(temp_up_911[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_911_test <- data.frame(temp_up_911)
temp_up_911_test$full_time_day <- as.POSIXct("2019-09-11")
kf_mean_temp_up_911 <- melt(temp_up_911_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_911 <- var(kf_mean_temp_up_911$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_911 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_902 <- colMeans(master_chain_902)
  }else{
    curr_pars_index_902 <- sample(seq(1, nrow(master_chain_902)),1)
    curr_pars_902 <- master_chain_902[curr_pars_index_902,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_902[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_911))
  }else{
    sed_model_temp <- temp_up_911[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_902 <- mean(ebu_902$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_902 <- rnorm(1, ebu_902$log_ebu_rate_mg_m2_d, sd(ebu_902$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_911[m] <- curr_pars_902[1] + curr_pars_902[2] * curr_methane_902 + curr_pars_902[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_911$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_911 <- var((log_ebu_rate_forecast_911$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 20 September 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_09_08_2019_09_11_F_10_562020_10_15.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-09-11") %>% 
  filter(full_time_day <= "2019-09-20") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-09-11", tz = "EST"), as.POSIXct("2019-09-20", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_911 <- ebu %>% filter(date == "2019-09-11")
ebu_911 <- na.omit(ebu_911)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_911 <- data.frame("full_time_day" = as.POSIXct("2019-09-11"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_911$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_911$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_920 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_911 <- colMeans(master_temp_chain_911)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_911)),1)
    curr_temp_pars_911 <- master_temp_chain_911[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_911[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_920[m] <- curr_temp_pars_911[1]*flare_temp + curr_temp_pars_911[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_920 <- data.frame(temp_up_920)
temp_up_920 <- as.vector(temp_up_920[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_920_test <- data.frame(temp_up_920)
temp_up_920_test$full_time_day <- as.POSIXct("2019-09-20")
kf_mean_temp_up_920 <- melt(temp_up_920_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_920 <- var(kf_mean_temp_up_920$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_920 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_911 <- colMeans(master_chain_911)
  }else{
    curr_pars_index_911 <- sample(seq(1, nrow(master_chain_911)),1)
    curr_pars_911 <- master_chain_911[curr_pars_index_911,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_911[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_920))
  }else{
    sed_model_temp <- temp_up_920[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_911 <- mean(ebu_911$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_911 <- rnorm(1, ebu_911$log_ebu_rate_mg_m2_d, sd(ebu_911$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_920[m] <- curr_pars_911[1] + curr_pars_911[2] * curr_methane_911 + curr_pars_911[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_920$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_920 <- var((log_ebu_rate_forecast_920$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 27 September 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_09_17_2019_09_20_F_10_562020_10_27.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-09-20") %>% 
  filter(full_time_day <= "2019-09-27") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-09-20", tz = "EST"), as.POSIXct("2019-09-27", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_920 <- ebu %>% filter(date == "2019-09-20")
ebu_920 <- na.omit(ebu_920)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_920 <- data.frame("full_time_day" = as.POSIXct("2019-09-20"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_920$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_920$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_927 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_920 <- colMeans(master_temp_chain_920)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_920)),1)
    curr_temp_pars_920 <- master_temp_chain_920[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_920[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_927[m] <- curr_temp_pars_920[1]*flare_temp + curr_temp_pars_920[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_927 <- data.frame(temp_up_927)
temp_up_927 <- as.vector(temp_up_927[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_927_test <- data.frame(temp_up_927)
temp_up_927_test$full_time_day <- as.POSIXct("2019-09-27")
kf_mean_temp_up_927 <- melt(temp_up_927_test, id.vars = c("full_time_day"),
                            variable.name = "Ensemble",
                            value.name = "temp_prediction")
# Take the variance
variance_temp_927 <- var(kf_mean_temp_up_927$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_927 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_920 <- colMeans(master_chain_920)
  }else{
    curr_pars_index_920 <- sample(seq(1, nrow(master_chain_920)),1)
    curr_pars_920 <- master_chain_920[curr_pars_index_920,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_920[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_927))
  }else{
    sed_model_temp <- temp_up_927[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_920 <- mean(ebu_920$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_920 <- rnorm(1, ebu_920$log_ebu_rate_mg_m2_d, sd(ebu_920$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_927[m] <- curr_pars_920[1] + curr_pars_920[2] * curr_methane_920 + curr_pars_920[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_927$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_927 <- var((log_ebu_rate_forecast_927$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 02 October 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_09_24_2019_09_27_F_10_562020_10_37.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-09-27") %>% 
  filter(full_time_day <= "2019-10-02") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-09-27", tz = "EST"), as.POSIXct("2019-10-02", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_927 <- ebu %>% filter(date == "2019-09-27")
ebu_927 <- na.omit(ebu_927)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_927 <- data.frame("full_time_day" = as.POSIXct("2019-09-27"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_927$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_927$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1002 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_927 <- colMeans(master_temp_chain_927)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_927)),1)
    curr_temp_pars_927 <- master_temp_chain_927[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_927[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1002[m] <- curr_temp_pars_927[1]*flare_temp + curr_temp_pars_927[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1002 <- data.frame(temp_up_1002)
temp_up_1002 <- as.vector(temp_up_1002[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1002_test <- data.frame(temp_up_1002)
temp_up_1002_test$full_time_day <- as.POSIXct("2019-10-02")
kf_mean_temp_up_1002 <- melt(temp_up_1002_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1002 <- var(kf_mean_temp_up_1002$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1002 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_927 <- colMeans(master_chain_927)
  }else{
    curr_pars_index_927 <- sample(seq(1, nrow(master_chain_927)),1)
    curr_pars_927 <- master_chain_927[curr_pars_index_927,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_927[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1002))
  }else{
    sed_model_temp <- temp_up_1002[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_927 <- mean(ebu_927$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_927 <- rnorm(1, ebu_927$log_ebu_rate_mg_m2_d, sd(ebu_927$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1002[m] <- curr_pars_927[1] + curr_pars_927[2] * curr_methane_927 + curr_pars_927[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_1002$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1002 <- var((log_ebu_rate_forecast_1002$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 11 October 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_09_29_2019_10_02_F_10_562020_10_53.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-10-02") %>% 
  filter(full_time_day <= "2019-10-11") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-10-02", tz = "EST"), as.POSIXct("2019-10-11", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1002 <- ebu %>% filter(date == "2019-10-02")
ebu_1002 <- na.omit(ebu_1002)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1002 <- data.frame("full_time_day" = as.POSIXct("2019-10-02"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1002$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1002$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1011 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1002 <- colMeans(master_temp_chain_1002)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1002)),1)
    curr_temp_pars_1002 <- master_temp_chain_1002[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1002[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1011[m] <- curr_temp_pars_1002[1]*flare_temp + curr_temp_pars_1002[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1011 <- data.frame(temp_up_1011)
temp_up_1011 <- as.vector(temp_up_1011[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1011_test <- data.frame(temp_up_1011)
temp_up_1011_test$full_time_day <- as.POSIXct("2019-10-11")
kf_mean_temp_up_1011 <- melt(temp_up_1011_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1011 <- var(kf_mean_temp_up_1011$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1011 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1002 <- colMeans(master_chain_1002)
  }else{
    curr_pars_index_1002 <- sample(seq(1, nrow(master_chain_1002)),1)
    curr_pars_1002 <- master_chain_1002[curr_pars_index_1002,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1002[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1011))
  }else{
    sed_model_temp <- temp_up_1011[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1002 <- mean(ebu_1002$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1002 <- rnorm(1, ebu_1002$log_ebu_rate_mg_m2_d, sd(ebu_1002$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1011[m] <- curr_pars_1002[1] + curr_pars_1002[2] * curr_methane_1002 + curr_pars_1002[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_1011$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1011 <- var((log_ebu_rate_forecast_1011$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 16 October 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_10_08_2019_10_11_F_10_562020_11_25.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-10-11") %>% 
  filter(full_time_day <= "2019-10-16") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-10-11", tz = "EST"), as.POSIXct("2019-10-16", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1011 <- ebu %>% filter(date == "2019-10-11")
ebu_1011 <- na.omit(ebu_1011)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1011 <- data.frame("full_time_day" = as.POSIXct("2019-10-11"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1011$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1011$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1016 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1011 <- colMeans(master_temp_chain_1011)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1011)),1)
    curr_temp_pars_1011 <- master_temp_chain_1011[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1011[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1016[m] <- curr_temp_pars_1011[1]*flare_temp + curr_temp_pars_1011[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1016 <- data.frame(temp_up_1016)
temp_up_1016 <- as.vector(temp_up_1016[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1016_test <- data.frame(temp_up_1016)
temp_up_1016_test$full_time_day <- as.POSIXct("2019-10-16")
kf_mean_temp_up_1016 <- melt(temp_up_1016_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1016 <- var(kf_mean_temp_up_1016$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1016 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1011 <- colMeans(master_chain_1011)
  }else{
    curr_pars_index_1011 <- sample(seq(1, nrow(master_chain_1011)),1)
    curr_pars_1011 <- master_chain_1011[curr_pars_index_1011,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1011[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1016))
  }else{
    sed_model_temp <- temp_up_1016[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1011 <- mean(ebu_1011$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1011 <- rnorm(1, ebu_1011$log_ebu_rate_mg_m2_d, sd(ebu_1011$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1016[m] <- curr_pars_1011[1] + curr_pars_1011[2] * curr_methane_1011 + curr_pars_1011[3] * sed_model_temp + process_error
  
  
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

hist(log_ebu_rate_forecast_1016$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1016 <- var((log_ebu_rate_forecast_1016$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 23 October 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_10_13_2019_10_16_F_10_562020_11_38.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-10-16") %>% 
  filter(full_time_day <= "2019-10-23") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-10-16", tz = "EST"), as.POSIXct("2019-10-23", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1016 <- ebu %>% filter(date == "2019-10-16")
ebu_1016 <- na.omit(ebu_1016)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1016 <- data.frame("full_time_day" = as.POSIXct("2019-10-16"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1016$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1016$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1023 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1016 <- colMeans(master_temp_chain_1016)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1016)),1)
    curr_temp_pars_1016 <- master_temp_chain_1016[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1016[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1023[m] <- curr_temp_pars_1016[1]*flare_temp + curr_temp_pars_1016[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1023 <- data.frame(temp_up_1023)
temp_up_1023 <- as.vector(temp_up_1023[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1023_test <- data.frame(temp_up_1023)
temp_up_1023_test$full_time_day <- as.POSIXct("2019-10-23")
kf_mean_temp_up_1023 <- melt(temp_up_1023_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1023 <- var(kf_mean_temp_up_1023$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1023 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1016 <- colMeans(master_chain_1016)
  }else{
    curr_pars_index_1016 <- sample(seq(1, nrow(master_chain_1016)),1)
    curr_pars_1016 <- master_chain_1016[curr_pars_index_1016,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1016[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1023))
  }else{
    sed_model_temp <- temp_up_1023[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1016 <- mean(ebu_1016$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1016 <- rnorm(1, ebu_1016$log_ebu_rate_mg_m2_d, sd(ebu_1016$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1023[m] <- curr_pars_1016[1] + curr_pars_1016[2] * curr_methane_1016 + curr_pars_1016[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_1023$ebullition_prediction, breaks = 1000)
# Calculatethe variance 
var_ebu_1023 <- var((log_ebu_rate_forecast_1023$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 30 October 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_10_20_2019_10_23_F_10_562020_11_53.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-10-23") %>% 
  filter(full_time_day <= "2019-10-30") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-10-23", tz = "EST"), as.POSIXct("2019-10-30", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1023 <- ebu %>% filter(date == "2019-10-23")
ebu_1023 <- na.omit(ebu_1023)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1023 <- data.frame("full_time_day" = as.POSIXct("2019-10-23"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1023$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1023$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1030 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1023 <- colMeans(master_temp_chain_1023)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1023)),1)
    curr_temp_pars_1023 <- master_temp_chain_1023[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1023[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1030[m] <- curr_temp_pars_1023[1]*flare_temp + curr_temp_pars_1023[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1030 <- data.frame(temp_up_1030)
temp_up_1030 <- as.vector(temp_up_1030[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1030_test <- data.frame(temp_up_1030)
temp_up_1030_test$full_time_day <- as.POSIXct("2019-10-30")
kf_mean_temp_up_1030 <- melt(temp_up_1030_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1030 <- var(kf_mean_temp_up_1030$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1030 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1023 <- colMeans(master_chain_1023)
  }else{
    curr_pars_index_1023 <- sample(seq(1, nrow(master_chain_1023)),1)
    curr_pars_1023 <- master_chain_1023[curr_pars_index_1023,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1023[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1030))
  }else{
    sed_model_temp <- temp_up_1030[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1023 <- mean(ebu_1023$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1023 <- rnorm(1, ebu_1023$log_ebu_rate_mg_m2_d, sd(ebu_1023$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1030[m] <- curr_pars_1023[1] + curr_pars_1023[2] * curr_methane_1023 + curr_pars_1023[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_1030$ebullition_prediction, breaks = 100)
# Calculatethe variance 
var_ebu_1030 <- var((log_ebu_rate_forecast_1030$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 07 November 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_10_27_2019_10_30_F_10_562020_12_6.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:14,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-10-30") %>% 
  filter(full_time_day <= "2019-11-07") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-10-30", tz = "EST"), as.POSIXct("2019-11-07", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1030 <- ebu %>% filter(date == "2019-10-30")
ebu_1030 <- na.omit(ebu_1030)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1030 <- data.frame("full_time_day" = as.POSIXct("2019-10-30"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1030$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1030$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1107 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1030 <- colMeans(master_temp_chain_1030)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1030)),1)
    curr_temp_pars_1030 <- master_temp_chain_1030[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1030[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1107[m] <- curr_temp_pars_1030[1]*flare_temp + curr_temp_pars_1030[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1107 <- data.frame(temp_up_1107)
temp_up_1107 <- as.vector(temp_up_1107[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1107_test <- data.frame(temp_up_1107)
temp_up_1107_test$full_time_day <- as.POSIXct("2019-11-07")
kf_mean_temp_up_1107 <- melt(temp_up_1107_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1107 <- var(kf_mean_temp_up_1107$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1107 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1030 <- colMeans(master_chain_1030)
  }else{
    curr_pars_index_1030 <- sample(seq(1, nrow(master_chain_1030)),1)
    curr_pars_1030 <- master_chain_1030[curr_pars_index_1030,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1030[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1107))
  }else{
    sed_model_temp <- temp_up_1107[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1030 <- mean(ebu_1030$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1030 <- rnorm(1, ebu_1030$log_ebu_rate_mg_m2_d, sd(ebu_1030$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model

    log_ebu_rate_forecast_1107[m] <- curr_pars_1030[1] + curr_pars_1030[2] * curr_methane_1030 + curr_pars_1030[3] * sed_model_temp + process_error

  
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

hist(log_ebu_rate_forecast_1107$ebullition_prediction, breaks = 100)
# Calculatethe variance 
var_ebu_1107 <- var((log_ebu_rate_forecast_1107$ebullition_prediction), na = T)
#############################################################################################################

#Forecast for 20 November 19
#############################################################################################################
### These are the focal depths from GLM-AED FLARE RUNS ###

# Extract the nc file and pull in all the forecasted depths for all ensembles
nc <- nc_open("./input/flare_forecasts/forecast_H_2019_11_04_2019_11_07_F_16_562020_12_20.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
level <- ncvar_get(nc,'lake_depth')
nc_close(nc)

#arrange the temp and level data frames
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:20,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:20,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:20,1:210,10]))
level <- as.data.frame(cbind(time_day, level[1:20,1:210]))

temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)

#paste ens_ to every single ensemble
colnames(temp)[-1] = paste0('ens_',colnames(temp)[-1])
colnames(level)[-1] = paste0('ens_',colnames(level)[-1])

temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
level$full_time_day <- as.POSIXct(strptime(level$full_time_day, '%Y-%m-%d', tz = 'EST'))

temp_prediction <- temp %>% 
  filter(full_time_day >= "2019-11-07") %>% 
  filter(full_time_day <= "2019-11-20") %>% 
  select(-full_time_day) %>%
  summarise_all(funs(mean))

target = c(as.POSIXct("2019-11-07", tz = "EST"), as.POSIXct("2019-11-20", tz = "EST"))

level_prediction <- level %>% 
  filter(full_time_day %in% target) %>% 
  select(-full_time_day) %>%
  summarise_all(funs(diff))

temp_prediction <- as.vector(temp_prediction[1,])
level_prediction <- as.vector(level_prediction[1,])

# Create a current methane ebullition observation as the AR term in the model.
ebu_1107 <- ebu %>% filter(date == "2019-11-07")
ebu_1107 <- na.omit(ebu_1107)

# Set up a manual dataframe from the observations that can be appended to the forecasts
x_1107 <- data.frame("full_time_day" = as.POSIXct("2019-11-07"), "Ensemble" = "observed", "ebullition_prediction" = mean(ebu_1107$log_ebu_rate_mg_m2_d), "SE" = sd(ebu_1107$log_ebu_rate_mg_m2_d), "Data" = "observation")

# predict the temperature of the upstream sites using the linear model developed and then bind it with the time day column from the .nc file
# Build JAGS model for the Deep hole to shallow conversion

#Uncertaintity paritioning for Temperature
hold_temp_model_pars <- F
hold_temp_model_process <- F
hold_flare <- F

flare_nmembers <- 210
n_temp_members <- 210
temp_up_1120 <- array(NA, dim = n_temp_members)
index <- 1
for(m in 1:n_temp_members){
  
  if(hold_temp_model_pars){
    curr_temp_pars_1107 <- colMeans(master_temp_chain_1107)
  }else{
    curr_pars_temp_index <- sample(seq(0, nrow(master_temp_chain_1107)),1)
    curr_temp_pars_1107 <- master_temp_chain_1107[curr_pars_temp_index,]
  }
  
  if(hold_temp_model_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_temp_pars_1107[3])
  }
  
  if(hold_flare){
    flare_temp <- mean(as.numeric(temp_prediction))
  }else{
    flare_temp <- temp_prediction[index]#This needs to be a vector!
  }
  
  temp_up_1120[m] <- curr_temp_pars_1107[1]*flare_temp + curr_temp_pars_1107[2] + process_error
  index <- index + 1
  if(index > flare_nmembers){
    index <- 1
  }
}

temp_up_1120 <- data.frame(temp_up_1120)
temp_up_1120 <- as.vector(temp_up_1120[1,])

#Set up a TS comparison of Tempurature scaling
temp_up_1120_test <- data.frame(temp_up_1120)
temp_up_1120_test$full_time_day <- as.POSIXct("2019-11-20")
kf_mean_temp_up_1120 <- melt(temp_up_1120_test, id.vars = c("full_time_day"),
                             variable.name = "Ensemble",
                             value.name = "temp_prediction")
# Take the variance
variance_temp_1120 <- var(kf_mean_temp_up_1120$temp_prediction, na = T)

#Uncertaintity paritioning for Ebullition
hold_methane_pars <- F
hold_methane_process <- F
hold_sed_temp <- F
hold_methane_obs <- F
hold_level <- F
include_level <- F

# Here is the meat of the AR model that is using temperature and
n_methane_members <- n_temp_members
log_ebu_rate_forecast_1120 <- array(NA, dim = n_methane_members)
index <- 1
for(m in 1:n_methane_members){
  
  if(hold_methane_pars){
    curr_pars_1107 <- colMeans(master_chain_1107)
  }else{
    curr_pars_index_1107 <- sample(seq(1, nrow(master_chain_1107)),1)
    curr_pars_1107 <- master_chain_1107[curr_pars_index_1107,]
  }
  
  if(hold_methane_process){
    process_error <- 0
  }else{
    process_error <- rnorm(1, 0, curr_pars_1107[4])
  }
  
  if(hold_sed_temp){
    sed_model_temp <- mean(as.numeric(temp_up_1120))
  }else{
    sed_model_temp <- temp_up_1120[index]
  }
  
  if(hold_level){
    level_model <- mean(as.numeric(level_prediction))
  }else{
    level_model <- level_prediction[index]
  }
  
  if(hold_methane_obs){
    curr_methane_1107 <- mean(ebu_1107$log_ebu_rate_mg_m2_d)
  }else{
    curr_methane_1107 <- rnorm(1, ebu_1107$log_ebu_rate_mg_m2_d, sd(ebu_1107$log_ebu_rate_mg_m2_d))
  }
  
  # This is the actual equation that is being run for the ebullition model
log_ebu_rate_forecast_1120[m] <- curr_pars_1107[1] + curr_pars_1107[2] * curr_methane_1107 + curr_pars_1107[3] * sed_model_temp + process_error

  
  index <- index + 1
  if(index > n_temp_members){
    index <- 1
  }
}

#Set up a TS comparison of Ebullition Model
log_ebu_rate_forecast_1120 <- data.frame(log_ebu_rate_forecast_1120)
log_ebu_rate_forecast_1120$full_time_day <- as.POSIXct("2019-11-20")
log_ebu_rate_forecast_1120 <- melt(log_ebu_rate_forecast_1120, id.vars = c("full_time_day"),
                                   variable.name = "Ensemble",
                                   value.name = "ebullition_prediction")

hist(log_ebu_rate_forecast_1120$ebullition_prediction, breaks = 100)
# Calculatethe variance 
var_ebu_1120 <- var((log_ebu_rate_forecast_1120$ebullition_prediction), na = T)
#############################################################################################################

#last_observation
x_1120 <- data.frame("full_time_day" = as.POSIXct("2019-11-20"), "Ensemble" = "observed", "ebullition_prediction" = -3, "SE" = 1.1, "Data" = "observation")

### Rbind all the temperature forecasts together
#############################################################################################################
ensemble_temp_forecasts <- rbind(kf_mean_temp_up_603,
                                     kf_mean_temp_up_610,
                                     kf_mean_temp_up_617,
                                     kf_mean_temp_up_624,
                                     kf_mean_temp_up_701,
                                     kf_mean_temp_up_708,
                                     kf_mean_temp_up_715,
                                     kf_mean_temp_up_722,
                                     kf_mean_temp_up_729,
                                     kf_mean_temp_up_805,
                                     kf_mean_temp_up_812,
                                     kf_mean_temp_up_819,
                                     kf_mean_temp_up_828,
                                     kf_mean_temp_up_902,
                                     kf_mean_temp_up_911,
                                     kf_mean_temp_up_920,
                                     kf_mean_temp_up_927,
                                     kf_mean_temp_up_1002,
                                     kf_mean_temp_up_1011,
                                     kf_mean_temp_up_1016,
                                     kf_mean_temp_up_1023,
                                     kf_mean_temp_up_1030,
                                     kf_mean_temp_up_1107,
                                     kf_mean_temp_up_1120,
                                     deparse.level = 1)

# Take the mean of the forecasts
mean_temp_forecasts <- ensemble_temp_forecasts %>% select(-Ensemble) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
#############################################################################################################
write_csv(kf_mean_temp_up_603[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_03Jun19.csv")
write_csv(kf_mean_temp_up_610[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_10Jun19.csv")
write_csv(kf_mean_temp_up_617[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_17Jun19.csv")
write_csv(kf_mean_temp_up_624[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_24Jun19.csv")
write_csv(kf_mean_temp_up_701[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_01Jul19.csv")
write_csv(kf_mean_temp_up_708[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_08Jul19.csv")
write_csv(kf_mean_temp_up_715[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_15Jul19.csv")
write_csv(kf_mean_temp_up_722[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_22Jul19.csv")
write_csv(kf_mean_temp_up_729[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_29Jul19.csv")
write_csv(kf_mean_temp_up_805[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_05Aug19.csv")
write_csv(kf_mean_temp_up_812[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_12Aug19.csv")
write_csv(kf_mean_temp_up_819[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_19Aug19.csv")
write_csv(kf_mean_temp_up_828[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_28Aug19.csv")
write_csv(kf_mean_temp_up_902[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_02Sep19.csv")
write_csv(kf_mean_temp_up_911[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_11Sep19.csv")
write_csv(kf_mean_temp_up_920[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_20Sep19.csv")
write_csv(kf_mean_temp_up_927[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_27Sep19.csv")
write_csv(kf_mean_temp_up_1002[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_02Oct19.csv")
write_csv(kf_mean_temp_up_1011[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_11Oct19.csv")
write_csv(kf_mean_temp_up_1016[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_16Oct19.csv")
write_csv(kf_mean_temp_up_1023[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_23Oct19.csv")
write_csv(kf_mean_temp_up_1030[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_30Oct19.csv")
write_csv(kf_mean_temp_up_1107[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_07Nov19.csv")
write_csv(kf_mean_temp_up_1120[,c(1,3)],"./output/SWI_temp_forecast/SWI_temp_Forecast_20Nov19.csv")


### Rbind all the ebullition forecasts together
#############################################################################################################


ensemble_forecasts_ch4 <- rbind(log_ebu_rate_forecast_603,
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
                                log_ebu_rate_forecast_1120,
                                deparse.level = 1)

# Remove the furthest outliers from the forecasts
ensemble_forecasts_99th_ch4 <- ensemble_forecasts_ch4 %>% group_by(full_time_day) %>%
  filter(ebullition_prediction < quantile(ebullition_prediction, 0.99, na.rm = T)) %>% filter(ebullition_prediction > quantile(ebullition_prediction, 0.01, na.rm = T))

# Take the mean of the forecasts
mean_forecast_ch4 <- ensemble_forecasts_ch4 %>% select(-Ensemble) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
SE_forecast_ch4 <- ensemble_forecasts_ch4 %>% select(-Ensemble) %>% group_by(full_time_day) %>% summarize_all(funs(SE))
names(SE_forecast_ch4)[2] <- "SE"

mean_forecast_ch4 <- left_join(mean_forecast_ch4, SE_forecast_ch4, by = "full_time_day")
#############################################################################################################

write_csv(log_ebu_rate_forecast_603[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_03Jun19.csv")
write_csv(log_ebu_rate_forecast_610[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_10Jun19.csv")
write_csv(log_ebu_rate_forecast_617[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_17Jun19.csv")
write_csv(log_ebu_rate_forecast_624[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_24Jun19.csv")
write_csv(log_ebu_rate_forecast_701[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_01Jul19.csv")
write_csv(log_ebu_rate_forecast_708[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_08Jul19.csv")
write_csv(log_ebu_rate_forecast_715[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_15Jul19.csv")
write_csv(log_ebu_rate_forecast_722[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_22Jul19.csv")
write_csv(log_ebu_rate_forecast_729[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_29Jul19.csv")
write_csv(log_ebu_rate_forecast_805[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_05Aug19.csv")
write_csv(log_ebu_rate_forecast_812[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_12Aug19.csv")
write_csv(log_ebu_rate_forecast_819[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_19Aug19.csv")
write_csv(log_ebu_rate_forecast_828[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_28Aug19.csv")
write_csv(log_ebu_rate_forecast_902[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_02Sep19.csv")
write_csv(log_ebu_rate_forecast_911[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_11Sep19.csv")
write_csv(log_ebu_rate_forecast_920[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_20Sep19.csv")
write_csv(log_ebu_rate_forecast_927[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_27Sep19.csv")
write_csv(log_ebu_rate_forecast_1002[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_02Oct19.csv")
write_csv(log_ebu_rate_forecast_1011[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_11Oct19.csv")
write_csv(log_ebu_rate_forecast_1016[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_16Oct19.csv")
write_csv(log_ebu_rate_forecast_1023[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_23Oct19.csv")
write_csv(log_ebu_rate_forecast_1030[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_30Oct19.csv")
write_csv(log_ebu_rate_forecast_1107[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_07Nov19.csv")
write_csv(log_ebu_rate_forecast_1120[,c(1,3)],"./output/ebullition_forecast/Ebullition_Forecast_20Nov19.csv")


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
                          x_1120,
                          deparse.level = 1)

### Total Variance ###
var_ebu_forecast<- rbind(var_ebu_603,
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
                         var_ebu_1107, 
                         var_ebu_1120)

var_ebu_forecast <- cbind(mean_observe_all[2:25,1],
                          data.frame((var_ebu_forecast)))
names(var_ebu_forecast) <- c("date", "total_variance")

var_temp_forecast <- rbind(variance_temp_603,
                      variance_temp_610,
                      variance_temp_617,
                      variance_temp_624,
                      variance_temp_701,
                      variance_temp_708,
                      variance_temp_715,
                      variance_temp_722,
                      variance_temp_729,
                      variance_temp_805,
                      variance_temp_812,
                      variance_temp_819,
                      variance_temp_828,
                      variance_temp_902,
                      variance_temp_911,
                      variance_temp_920,
                      variance_temp_927,
                      variance_temp_1002,
                      variance_temp_1011,
                      variance_temp_1016, 
                      variance_temp_1023, 
                      variance_temp_1030, 
                      variance_temp_1107, 
                      variance_temp_1120)

var_temp_forecast <- cbind(mean_observe_all[2:25,1],
                          data.frame((var_temp_forecast)))
names(var_temp_forecast) <- c("date", "total_variance")

### Run the deterministic model using the observed temperatures from the SWI and the parameter estimates from 2017 (McClure et al., 2020 JGR: Biogeosciences)
det_prediction <- as.data.frame(-6.13 + 0.28803*ebullition_1120$log_ebu_mgCH4_m2_d_1 + 0.38255*ebullition_1120$temp_b_avg)
deterministic_prediction <- cbind(mean_observe_all[,1],det_prediction)
names(deterministic_prediction) <- c("full_time_day","deterministic")

