### STATS
### MAE, RMSE, NSE
### Set the Standard error function and specify the number of states and the 
### number of ensamble members that come from the FLARE forecast model 
SE <- function(x) sd(x)/sqrt(length(x))
rmse <- function(x) {sqrt(mean(x^2))}
mae <- function(x) {mean(abs(x))}
mean_forecasts_deterministic <- forecast_observe_compare_18[,20]

names(mean_forecasts_temp)[2] <- "ebullition_temp_model"
names(mean_forecasts_temp_level)[2] <- "ebullition_temp_level_model"
names(mean_forecasts_level)[2] <- "ebullition_level_model"
names(mean_forecasts_deterministic)[2] <- "ebullition_deterministic_model"
names(mean_forecasts_deterministic)[1] <- "full_time_day"
mean_observe_all <- mean_observe_all[,c(1,3)]

model_comparisons <- join_all(list(mean_forecasts_temp,mean_forecasts_temp_level,mean_forecasts_level),
                               by = "full_time_day", type = "left")

stats <- cbind(model_comparisons,mean_forecasts_deterministic,mean_observe_all[2:25,2])
names(stats)[6] <- "observed"


stats2 <- stats[-1,]
NSE(exp(stats2$ebullition_temp_model), exp(stats2$observed))
NSE(stats2$mean_forecasts_deterministic, exp(stats2$observed))


stats$temp_level_error <- exp(stats$observed)-exp(stats$ebullition_temp_level_model)
stats$temp_error <- exp(stats$observed)-exp(stats$ebullition_temp_model)
stats$level_error <- exp(stats$observed)-exp(stats$ebullition_level_model)
stats$determine_error <- exp(stats$observed)-stats$mean_forecasts_deterministic

rmse(stats$temp_level_error)
rmse(stats$temp_error)
rmse(stats$level_error)
rmse(stats$determine_error)

mae(stats$temp_level_error)
mae(stats$temp_error)
mae(stats$level_error)
mae(stats$determine_error)




