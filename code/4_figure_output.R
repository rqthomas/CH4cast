library(patchwork)

#Figures 1 and 2 are made in Arcmap, Lucidchart, and PPT. 

### Figure ### Ebullition forecasts
############################################################################################
ensemble_forecasts_ch4_short<- ensemble_forecasts_99th_ch4 %>% filter(full_time_day>="2019-07-08")

a <- ggplot(ensemble_forecasts_99th_ch4) + 
  geom_flat_violin(data = ensemble_forecasts_99th_ch4, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = NA, fill = "blue", scale = "width")+
  geom_point(data = mean_forecasts_ch4, aes(x = full_time_day, y = ebullition_prediction),pch = 3, color = "blue", size = 2)+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 2)+
  xlab("")+
  labs(title="A")+
  ylim(-300,300)+
  geom_segment(x = as.POSIXct("2019-07-05"), y = 7.5, xend = as.POSIXct("2019-07-05"), yend = -7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-07-05"), y = 7.5, xend = as.POSIXct("2019-12-01"), yend = 7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-07-05"), y = -7.5, xend = as.POSIXct("2019-12-01"), yend = -7.5, colour = "black", lty = "dashed")+
  ylab(expression(paste("ln(mg CH "[4]," m"^"-2"," d"^"-1",")")))+
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
a

b <- ggplot(ensemble_forecasts_99th_ch4) + 
  geom_flat_violin(data = ensemble_forecasts_99th_ch4, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = NA, fill = "blue", scale = "width")+
  geom_point(data = mean_forecasts_ch4, aes(x = full_time_day, y = ebullition_prediction),pch = 3, color = "blue", size = 2)+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 2)+
  ylim(-7.5,7.5)+
  xlab("")+
  labs(title="B")+
  geom_segment(x = as.POSIXct("2019-07-05"), y = 7.5, xend = as.POSIXct("2019-07-05"), yend = -7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-07-05"), y = 7.5, xend = as.POSIXct("2019-12-01"), yend = 7.5, colour = "black", lty = "dashed")+
  geom_segment(x = as.POSIXct("2019-07-05"), y = -7.5, xend = as.POSIXct("2019-12-01"), yend = -7.5, colour = "black", lty = "dashed")+
  ylab(expression(paste("ln(mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[7]-(86400*1),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[7],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("08 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
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
b

# Figure output
tiff("./figures/ebullition_forecast/weekly_output/EBU_FORECASTS_FIGURE_3.tiff", width = 14, height = 6, units = 'in', res = 600)
aa <- a|b
aa
dev.off()
############################################################################################

### Figure ### SWI temperature forecasts
############################################################################################
tiff("./figures/SWI_scaling_model_forecast/weekly_output/TEMPERATURE_FORECASTS_FIGURE_S2.tiff", width = 8, height = 6, units = 'in', res = 600)

v <- ggplot() + 
  geom_flat_violin(data = ensemble_temp_forecasts, aes(x = full_time_day, y = temp_prediction, group = full_time_day), size = .1, color = NA, fill = "darkorange2", scale = "width")+
  geom_point(data = mean_temp_forecasts, aes(x = full_time_day, y = temp_prediction),pch = 3, color = "black", fill = "red1", size = 2)+
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

tiff("./figures/SWI_scaling_model_forecast/daily_output/TEMP_COMPARE_FIGURE_S3.tiff", width=7, height=7, units = "in",res = 350)

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



# These two take a long time to run
# Be ready with a glass of tea, wine, coffee, beer, etc. You will be waiting a bit.

### Figure ### SWI scaling model parameter estimates
############################################################################################
mean_temp_beta_1 <- parm_temp_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

b1 <- ggplot(parm_temp_all, aes(full_time_day, `beta[1]`, group=as.character(variable))) + 
  geom_line(color = "grey80", lwd = 0.1)+
  geom_line(data = mean_temp_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 3)+
  ylab("Intercept")+
  xlab("")+
  theme_classic()

mean_temp_beta_2 <- parm_temp_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

b2 <- ggplot(parm_temp_all, aes(full_time_day, `beta[2]`, group=as.character(variable))) + 
  geom_line(color = "grey80", lwd = 0.1)+
  geom_line(data = mean_temp_beta_2, aes(full_time_day, `beta[2]`), color = "black", lwd = 3)+
  ylab("Temperature")+
  xlab("")+
  theme_classic()


tiff("./figures/SWI_scaling_model_forecast/parameter_est/BETA_temp_estimates.tiff", width = 6, height = 10, units = "in", res = 600)
beta <- b1/b2
beta
dev.off()
############################################################################################

### Figure ### ebullition model parameter estimates
############################################################################################
mean_ebu_beta_1 <- parm_ebu_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

b1 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[1]`, group=as.character(variable))) + 
  geom_line(color = "grey80")+
  geom_line(data = mean_ebu_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 3)+
  ylab("Intercept")+
  xlab("")+
  theme_classic()

mean_ebu_beta_2 <- parm_ebu_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

b2 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[2]`, group=as.character(variable))) + 
  geom_line(color = "grey80")+
  geom_line(data = mean_ebu_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 3)+
  ylab("AR term")+
  xlab("")+
  theme_classic()

mean_ebu_beta_3 <- parm_ebu_all %>% select(`beta[3]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

b3 <- ggplot(parm_ebu_all, aes(full_time_day, `beta[3]`, group=as.character(variable))) + 
  geom_line(color = "grey80")+
  geom_line(data = mean_ebu_beta_3, aes(full_time_day, `beta[3]`), color = "black", lwd = 3)+
  ylab("SWI_temp")+
  xlab("")+
  theme_classic()

tiff("./figures/ebullition_forecast/parameter_est/BETA_ebu_estimates.tiff", width = 6, height = 14, units = "in", res = 600)
beta <- b1/b2/b3
beta
dev.off()
############################################################################################
