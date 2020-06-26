### STATS

### Total Variance ###
var_ebu_temp_level <- rbind(var_ebu_603,
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

var_ebu_temp_level <- cbind(forecast_observe_compare_18[1:24,1],
                     data.frame((var_ebu_temp_level)))
names(var_ebu_temp_level) <- c("date", "variance")


var_ebu_temp <- rbind(var_ebu_603,
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

var_ebu_temp_par
var_ebu_temp_process
var_ebu_temp_ic
var_ebu_temp_driver



var_ebu_temp <- cbind(forecast_observe_compare_18[1:24,1],
                            data.frame(var_ebu_temp))
names(var_ebu_temp) <- c("date", "var")
write_csv(var_ebu_temp, "var_partition_v2.csv")


var_ebu_level <- rbind(var_ebu_603,
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

var_ebu_level <- cbind(forecast_observe_compare_18[1:24,1],
                      data.frame((var_ebu_level)))
names(var_ebu_level) <- c("date", "variance")


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

# Posterior Predictive loss
G_temp = sum((exp(stats$ebullition_temp_model)-exp(stats$observed))^2)
P_temp = sum(var_ebu_temp$variance)
D_pl_temp = G_temp + P_temp

G_level = sum((exp(stats$ebullition_level_model)-exp(stats$observed))^2)
P_level = sum(var_ebu_level$variance)
D_pl_level = G_level + P_level

G_temp_level = sum((exp(stats$ebullition_temp_level_model)-exp(stats$observed))^2)
P_temp_level = sum(var_ebu_temp_level$variance)
D_pl_temp_level = G_temp_level + P_temp_level



