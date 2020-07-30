### STATS
### MAE, RMSE, NSE
### Set the Standard error function and specify the number of states and the 
### number of ensamble members that come from the FLARE forecast model 

stats_whole_season <- taylor_compare
stats_w_da <- taylor_compare_da



NSE_forecast_whole_season <- as.data.frame(NSE(exp(stats_whole_season$forecasts), exp(stats_whole_season$observation)))
names(NSE_forecast_whole_season)[1] <- "NSE"
NSE_forecast_past_24jun <- as.data.frame(NSE(exp(stats_w_da$forecasts), exp(stats_w_da$observation)))
names(NSE_forecast_past_24jun)[1] <- "NSE"
NSE_deterministic_whole_season <- as.data.frame(NSE(exp(stats_whole_season$deterministic), exp(stats_whole_season$observation)))
names(NSE_deterministic_whole_season)[1] <- "NSE"
NSE_deterministic_past_24jun <- as.data.frame(NSE(exp(stats_w_da$deterministic), exp(stats_w_da$observation)))
names(NSE_deterministic_past_24jun)[1] <- "NSE"


NSE_forecast_whole_season$period <- "Whole forecast period"
NSE_forecast_past_24jun$period <- "Post training forecast period"
NSE_deterministic_whole_season$period <- "Whole forecast period"
NSE_deterministic_past_24jun$period <- "Post training forecast period"

NSE_forecast_whole_season$model <- "Forecast cycles"
NSE_forecast_past_24jun$model <- "Forecast cycles"
NSE_deterministic_whole_season$model <- "Null deterministic"
NSE_deterministic_past_24jun$model <- "Null deterministic"

NSE_all <- rbind(NSE_forecast_past_24jun,NSE_deterministic_past_24jun)

NSE_all <- NSE_all[,c(3,2,1)]

names(NSE_all) <- c("Model", "Period", "NSE")
NSE_all$NSE <- signif(NSE_all$NSE, digits = 2)

tiff("./figures/NSE_OUTPUT.tiff", width=8, height=3, units="in", res = 600)
grid.table(NSE_all)
dev.off()

sum_obs_season <- as.data.frame(sum(exp(stats_whole_season$observation))*170/1.04)
names(sum_obs_season)[1] <- "mg methane"
sum_obs_season$period <- "Whole forecast period"
sum_obs_season$model <- "Observation"
sum_obs_season$per_diff <- "-"

sum_for_season <- as.data.frame(sum(exp(stats_whole_season$forecasts))*170/1.04)
names(sum_for_season)[1] <- "mg methane"
sum_for_season$period <- "Whole forecast period"
sum_for_season$model <- "Forecast cycles"
sum_for_season$per_diff <- ((abs(sum_obs_season[1,1]-sum_for_season[1,1]))/((sum_obs_season[1,1]+sum_for_season[1,1])/2))*100

sum_det_season <- as.data.frame(sum(exp(stats_whole_season$deterministic))*170/1.04)
names(sum_det_season)[1] <- "mg methane"
sum_det_season$period <- "Whole forecast period"
sum_det_season$model <- "Null deterministic"
sum_det_season$per_diff <- ((abs(sum_obs_season[1,1]-sum_det_season[1,1]))/((sum_obs_season[1,1]+sum_det_season[1,1])/2))*100

sum_obs_short <- as.data.frame(sum(exp(stats_w_da$observation))*143/1.04)
names(sum_obs_short)[1] <- "mg methane"
sum_obs_short$period <- "Post training forecast period"
sum_obs_short$model <- "Observation"
sum_obs_short$per_diff <- "-"

sum_for_short <- as.data.frame(sum(exp(stats_w_da$forecasts))*143/1.04)
names(sum_for_short)[1] <- "mg methane"
sum_for_short$period <- "Post training forecast period"
sum_for_short$model <- "Forecast cycles"
sum_for_short$per_diff <- ((abs(sum_obs_short[1,1]-sum_for_short[1,1]))/((sum_obs_short[1,1]+sum_for_short[1,1])/2))*100

sum_det_short <- as.data.frame(sum(exp(stats_w_da$deterministic))*143/1.04)
names(sum_det_short)[1] <- "mg methane"
sum_det_short$period <- "Post training forecast period"
sum_det_short$model <- "Null deterministic"
sum_det_short$per_diff <- ((abs(sum_obs_short[1,1]-sum_det_short[1,1]))/((sum_obs_short[1,1]+sum_det_short[1,1])/2))*100

total_all <- rbind(sum_obs_short,sum_for_short,sum_det_short)

total_all <- total_all[,c(3,2,1,4)]

names(total_all) <- c("Model", "Period", "Total emissions (mg Methane)", "% difference")

total_all$`Total emissions (mg Methane)` <- signif(total_all$`Total emissions (mg Methane)`, digits = 2)
total_all[c(3:6),4] <- signif(as.numeric(total_all[c(3:6),4]), digits = 2)

tiff("./figures/Summed_OUTPUT.tiff", width=8, height=3, units="in", res = 600)
grid.table(total_all)
dev.off()

NSE_forecast_whole_season_temp <- NSE((temp_eval$temp_prediction), (temp_eval$temp_b_avg))

