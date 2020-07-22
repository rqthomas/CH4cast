library(patchwork)

#Figures 1 and 2 are made in Arcmap, Lucidchart, and PPT. 

### Figure ### Ebullition forecasts
############################################################################################
ensemble_forecasts_ch4_short<- ensemble_forecasts_99th_ch4 %>% filter(full_time_day>="2019-06-24")

a <- ggplot(ensemble_forecasts_99th_ch4) + 
  geom_point(data = mean_forecast_ch4, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "lightblue", size = 3)+
  geom_flat_violin(data = ensemble_forecasts_99th_ch4, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = "black", fill = "lightblue", scale = "width")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 2)+
  xlab("")+
  labs(title="a")+
  ylim(-300,300)+
  geom_segment(x = as.POSIXct("2019-06-28"), y = 7.5, xend = as.POSIXct("2019-06-28"), yend = -7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-06-28"), y = 7.5, xend = as.POSIXct("2019-12-01"), yend = 7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-06-28"), y = -7.5, xend = as.POSIXct("2019-12-01"), yend = -7.5, colour = "black", lty = "dashed")+
  ylab(expression(paste("Ebullition rate ln(mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.1, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15))

b <- ggplot(ensemble_forecasts_ch4_short) + 
  geom_point(data = mean_forecast_ch4, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "blue", size = 3)+
  geom_flat_violin(data = ensemble_forecasts_ch4_short, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = NA, fill = "blue", scale = "width")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 2)+
  ylim(-7.5,7.5)+
  xlab("")+
  labs(title="b")+
  geom_segment(x = as.POSIXct("2019-06-28"), y = 7.5, xend = as.POSIXct("2019-06-28"), yend = -7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-06-28"), y = 7.5, xend = as.POSIXct("2019-12-01"), yend = 7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-06-28"), y = -7.5, xend = as.POSIXct("2019-12-01"), yend = -7.5, colour = "black", lty = "dashed")+
  ylab(expression(paste("Ebullition rate ln(mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[6]-(86400*1.5),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.1, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(), 
        title = element_text(size = 15),
        axis.title.y = element_blank())

# Figure output
tiff("./figures/EBU_FORECASTS_FIGURE_3.tiff", width = 14, height = 6, units = 'in', res = 600)
aa <- a|b
aa
dev.off()
############################################################################################

### Figure ### SWI temperature forecasts
############################################################################################
observed_temps <- as.data.frame(cbind(mean_observe_all[,1], ebullition_1120[,6]))
names(observed_temps) <- c("full_time_day", "observed_SWI_temp")

tiff("./figures/TEMPERATURE_FORECASTS_FIGURE_S2.tiff", width = 8, height = 6, units = 'in', res = 600)

v <- ggplot() + 
  geom_flat_violin(data = ensemble_temp_forecasts, aes(x = full_time_day, y = temp_prediction, group = full_time_day), size = .1, color = NA, fill = "darkorange2", scale = "width")+
  geom_point(data = mean_temp_forecasts, aes(x = full_time_day, y = temp_prediction),pch = 3, color = "darkorange2", size = 2)+
  geom_point(data = observed_temps, aes(x = full_time_day, y = observed_SWI_temp),pch = 23, color = "black", fill="red1", size = 3)+
  #geom_point(data = observed_temp, aes(x = full_time_day, y = Temp_C),pch = 23, color = "black", fill = "red1", size = 3)+
  ylim(4,30)+
  xlab("")+
  ylab(expression(paste("Temperature (C)")))+
  scale_x_datetime(limits = c(mean_temp_forecasts$full_time_day[1],mean_temp_forecasts$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_temp_forecasts$full_time_day[2]-(86400*2),
                              mean_temp_forecasts$full_time_day[6],
                              mean_temp_forecasts$full_time_day[10]+(86400*3),
                              mean_temp_forecasts$full_time_day[14]+(86400*4),
                              mean_temp_forecasts$full_time_day[18]+(86400*4),
                              mean_temp_forecasts$full_time_day[22]+(86400*9),
                              mean_temp_forecasts$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
v
dev.off()
############################################################################################

### Figure ### Temperature catwalk and HOBO loggers comparison via linear model
############################################################################################

temp_train <- temp_model_1120 %>% filter(TIMESTAMP <= "2019-01-01")
temp_forecast <- temp_model_1120 %>% filter(TIMESTAMP >= "2019-01-01")

tiff("./figures/TEMP_COMPARE_FIGURE.tiff", width=7, height=7, units = "in",res = 350)

a <- ggplot()+
  geom_point(data = temp_train,  aes(mean_ws_temp,Temp_C), pch = 21, size = 4, col = "black", fill = "cyan")+
  geom_point(data = temp_forecast,  aes(mean_ws_temp,Temp_C), pch = 21, size = 4, col = "black", fill = "red")+
  geom_smooth(data = temp_model_1120,  aes(mean_ws_temp,Temp_C),method = "lm", color = "black", fill = "grey", alpha = 0.75)+
  ylim(10,30)+
  xlim(10,30)+
  xlab(expression(paste("Catwalk Temperature (C)")))+
  ylab(expression(paste("Upstream SWI Temperature (C)")))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


a

dev.off()
############################################################################################

### Figure ### Total Ebullition forecast variance
############################################################################################
tiff("./figures/Total_forecast_variance.tiff", width = 6, height = 4, units = 'in', res = 600)
p <- ggplot(var_ebu_forecast, aes(date, total_variance))+
  geom_line(lwd = 3, color = "black")+
  theme_classic()
p
dev.off()
############################################################################################

### Figure ### Total SWI Temperature scaling model forecast variance
############################################################################################
tiff("./figures/Total_SWI_forecast_variance.tiff", width = 6, height = 4, units = 'in', res = 600)
p <- ggplot(var_temp_forecast, aes(date, total_variance))+
  geom_line(lwd = 3, color = "black")+
  theme_classic()
p
dev.off()
############################################################################################

### Figure ### Forecast evaluation time series
############################################################################################
mean_forecast_short <- mean_forecast_ch4 %>% filter(full_time_day <= "2019-06-28")

g <- ggplot() + 
  geom_line(data = mean_observe_all, aes(x = full_time_day, y = exp(ebullition_prediction)),lwd = 1.3, color = "red")+
  geom_point(data = mean_observe_all, aes(x=full_time_day,y=exp(ebullition_prediction)),pch = 23,color = "black", fill = "firebrick2", size = 5)+
  geom_point(data = mean_forecast_ch4, aes(x=full_time_day,y=exp(ebullition_prediction)),pch = 21,color = "black", fill = "blue", size = 7)+
  geom_point(data = mean_forecast_short, aes(x=full_time_day,y=exp(ebullition_prediction)),pch = 21,color = "black", fill = "lightblue", size = 7)+
  geom_point(data = deterministic_prediction, aes(x=full_time_day,y=exp(deterministic)),pch = 24,color = "black", fill = "grey", size = 6)+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lwd = 1, lty = "dashed")+
  xlab("")+
  ylab(expression(paste("Ebullition rate (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "20 Nov"))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

tiff("./figures/FORECAST_VS_DETERMINISTIC.tiff", width=10, height=7, units = "in", res = 600)
g
dev.off()
############################################################################################

### Figure ### Taylor Diagram for ebullition forecasts
############################################################################################
library("plotrix")

taylor_compare <- left_join(mean_observe_all, mean_forecast_ch4, by = "full_time_day")
taylor_compare <- left_join(taylor_compare, deterministic_prediction, by = "full_time_day")

taylor_compare <- taylor_compare %>% select(full_time_day,ebullition_prediction.x,ebullition_prediction.y,deterministic)
names(taylor_compare) <- c("full_time_day","observation","forecasts", "deterministic")

taylor_compare_da <- taylor_compare %>%
  filter(full_time_day>="2019-07-01")
names(taylor_compare_da) <- c("full_time_day","observation","forecasts", "deterministic")

taylor_compare <- na.omit(taylor_compare)
taylor_compare_da <- na.omit(taylor_compare_da)

tiff("./figures/TAYLOR_DIAGRAM.tiff", width=7, height=7, units="in", res = 600)
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$deterministic), pos.cor = T, col="grey", pcex = 3, pch = 17, main = "") ### Using just 2018 training data
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$deterministic), col="black", add = T, pcex = 3, pch = 24) ### Using just 2018 training data
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts), col="lightblue", pos.cor=T,add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_temp_level_model), col="darkorchid1", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_temp_level_model), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare_da$observation), exp(taylor_compare_da$forecasts), col="blue", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare_da$observation), exp(taylor_compare_da$forecasts), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$observation), col="red", pos.cor=T, add = T, cex = 2, pcex = 6, pch = 18) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$observation), col="black", pos.cor=T, add = T, cex = 2, pcex = 5, pch = 23) ### Just 2019 data with data assimilation

### Add legend to the plot
lpos1<-17*sd((taylor_compare_da$observation))
lpos2 <- 27*sd((taylor_compare_da$forecasts))
legend(lpos1,lpos2, box.lwd = 0, box.col = "white",legend=c("Observations","Whole forecast period", "Trained forecast period", "Null deterministic"), pch=c(18,19,19,17),pt.cex = c(3,3,3,2), cex = 1.2, col=c("red","lightblue","blue","grey"))

dev.off()
############################################################################################

### Figure ### Forecast training
############################################################################################

taylor_compare$cycle <- seq.int(nrow(taylor_compare))

taylor_compare$forecast_diff <- taylor_compare$observation - taylor_compare$forecasts
taylor_compare$determine_diff <- taylor_compare$observation - taylor_compare$deterministic

colnames(taylor_compare) <-  c('full_time_day',"observed","forecasted", "determined", "forecast_cycle","Forecast cycles", "Null determinisitc")
taylor_compare <- taylor_compare %>% select(forecast_cycle, `Forecast cycles`, `Null determinisitc`)
m <-  melt(taylor_compare,id='forecast_cycle')

colnames(m) <-  c('forecast_cycle','Model','value')


q <- ggplot(m, aes(forecast_cycle, abs(value), fill = Model))+
  geom_bar(stat = "identity", color = "black", lwd = 0.1, position='dodge')+
  scale_x_continuous(limits = c(0,25), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))+
  scale_fill_manual(values = c("blue", "grey"))+
  xlab("Forecast cycle #")+
  ylab(expression(paste("Difference ln(mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  theme_classic()+
  geom_vline(xintercept = 4.5)+
  theme(legend.position="top",
        legend.title = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12, color = "black")
  )

tiff("./figures/Mean_difference_in_models.tiff", res = 600, width = 8, height = 4, units = "in")
q
dev.off()
############################################################################################

### Figure ### Temperature Evalutation x-y plot
############################################################################################
temp_eval <- cbind(mean_temp_forecasts, ebullition_1120[2:25,6])

tiff("./figures/TEMP_EVALUATION_FIGURE.tiff", width=7, height=7, units = "in",res = 350)

m <- ggplot()+
  geom_point(data = temp_eval,  aes(temp_b_avg,temp_prediction), pch = 21, size = 4, col = "black", fill = "orange")+
  geom_smooth(data = temp_eval,  aes(temp_b_avg,temp_prediction),method = "lm", color = "black", fill = "grey", alpha = 0.75)+
  ylim(10,30)+
  xlim(10,30)+
  xlab(expression(paste("Observed Temperature (C)")))+
  ylab(expression(paste("Forecasted Temperature (C)")))+
  theme_classic()+
  theme(axis.text=element_text(size=15, color = "black"),
        axis.title=element_text(size=15, color = "black"),
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


m

dev.off()
############################################################################################



# These two figures take a loooonng time to run
# Be ready with a glass of tea, wine, coffee, beer, etc. You will be waiting a bit.
# In the meantime, I have made them captioned so they do not run when you source the code. 

# ### Figure ### SWI scaling model parameter estimates
# ############################################################################################
# mean_temp_beta_1 <- parm_temp_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b1 <- ggplot(parm_temp_all, aes(full_time_day, `beta[1]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_temp_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 3)+
#   ylab("Intercept")+
#   xlab("")+
#   theme_classic()
# 
# mean_temp_beta_2 <- parm_temp_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b2 <- ggplot(parm_temp_all, aes(full_time_day, `beta[2]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_temp_beta_2, aes(full_time_day, `beta[2]`), color = "black", lwd = 3)+
#   ylab("Temperature")+
#   xlab("")+
#   theme_classic()
# 
# 
# tiff("./figures/SBETA_temp_estimates.tiff", width = 6, height = 10, units = "in", res = 600)
# beta <- b1/b2
# beta
# dev.off()
# ############################################################################################
# 
# # ### Figure ### ebullition model parameter estimates
# ############################################################################################
# mean_ebu_beta_1 <- parm_ebu_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b1 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[1]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 1)+
#   ylab("Intercept")+
#   xlab("")+
#   theme_classic()
# 
# mean_ebu_beta_2 <- parm_ebu_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b2 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[2]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_2, aes(full_time_day, `beta[2]`), color = "black", lwd = 1)+
#   ylab("AR term")+
#   xlab("")+
#   theme_classic()
# 
# mean_ebu_beta_3 <- parm_ebu_all %>% select(`beta[3]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b3 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[3]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_3, aes(full_time_day, `beta[3]`), color = "black", lwd = 1)+
#   ylab("SWI_temp")+
#   xlab("")+
#   theme_classic()
# 
# 
# parm_ebu_short <- parm_ebu_all %>% filter(full_time_day>="2019-07-08")
# mean_ebu_beta_1_short <- parm_ebu_short %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b4 <- ggplot(parm_ebu_short, aes(full_time_day, `beta[1]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_1_short, aes(full_time_day, `beta[1]`), color = "black", lwd = 1)+
#   ylab("")+
#   xlab("")+
#   theme_classic()
# 
# mean_ebu_beta_2_short <- parm_ebu_short %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b5 <- ggplot(parm_ebu_short, aes(full_time_day, `beta[2]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_2_short, aes(full_time_day, `beta[2]`), color = "black", lwd = 1)+
#   ylab("AR ")+
#   xlab("")+
#   theme_classic()
# 
# mean_ebu_beta_3_short <- parm_ebu_short %>% select(`beta[3]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
# 
# b6 <- ggplot(parm_ebu_short, aes(full_time_day, `beta[3]`, group=as.character(variable))) +
#   geom_line(color = "grey80", lwd = 0.1)+
#   geom_line(data = mean_ebu_beta_3_short, aes(full_time_day, `beta[3]`), color = "black", lwd = 1)+
#   ylab("")+
#   xlab("")+
#   theme_classic()
# 
# tiff("./figures/BETA_ebu_estimates.tiff", width = 12, height = 14, units = "in", res = 600)
# beta <- (b1|b4)/(b2|b5)/(b3|b6)
# beta
# dev.off()
# ############################################################################################
