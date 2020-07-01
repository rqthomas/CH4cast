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


NSE_forecast_whole_season$period <- "with first month"
NSE_forecast_past_24jun$period <- "after first month"
NSE_deterministic_whole_season$period <- "with first month"
NSE_deterministic_past_24jun$period <- "after first month"

NSE_forecast_whole_season$model <- "forecast"
NSE_forecast_past_24jun$model <- "forecast"
NSE_deterministic_whole_season$model <- "deterministic"
NSE_deterministic_past_24jun$model <- "deterministic"

NSE_all <- rbind(NSE_forecast_whole_season,NSE_forecast_past_24jun,NSE_deterministic_whole_season,NSE_deterministic_past_24jun)

NSE_all <- NSE_all[,c(3,2,1)]

names(NSE_all) <- c("Model", "Period", "NSE")


tiff("./figures/ebullition_forecast/weekly_output/NSE_OUTPUT.tiff", width=4, height=2, units="in", res = 600)
grid.table(NSE_all)
dev.off()
