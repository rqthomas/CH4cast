library(patchwork)

#Figures 1 and 2 are made in Arcmap, Lucidchart, and PPT. 

### Figure ### Ebullition forecasts
############################################################################################
ensemble_forecasts_ch4 <- ensemble_forecasts_ch4%>%filter(full_time_day>="2019-07-01")
mean_forecast_ch4 <- mean_forecast_ch4%>%filter(full_time_day>="2019-07-01")

a <- ggplot(ensemble_forecasts_ch4) + 
  geom_point(data = mean_forecast_ch4, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "blue", size =2)+
  geom_flat_violin(data = ensemble_forecasts_ch4, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = "black", fill = "blue", scale = "width")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 3)+
  xlab("")+
  labs(title="(a)   Forecasts with data assimilation")+
  ylim(-8,8)+
  ylab(expression(paste("ln(Ebullition rate (mg CH "[4]," m"^"-2"," d"^"-1","))")))+
  geom_vline(xintercept = as.POSIXct("2019-06-28"), lwd = 1, lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1]-(86400*1),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
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

ensemble_forecasts_ch4_nDA <- ensemble_forecasts_ch4_nDA%>%filter(full_time_day>="2019-07-01")
mean_forecast_ch4_nDA <- mean_forecast_ch4_nDA%>%filter(full_time_day>="2019-07-01")
b <- ggplot(ensemble_forecasts_ch4_nDA) + 
  geom_point(data = mean_forecast_ch4_nDA, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "magenta", size =2)+
  geom_flat_violin(data = ensemble_forecasts_ch4_nDA, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = "black", fill = "magenta", scale = "width")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 3)+
  xlab("")+
  ylab(expression(paste("ln(Ebullition rate (mg CH "[4]," m"^"-2"," d"^"-1","))")))+
  labs(title="(b)   Forecasts without data assimilation")+
  geom_vline(xintercept = as.POSIXct("2019-06-28"), lwd = 1, lty = "dotted")+
  ylim(-15,15)+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1]-(86400*1),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
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

b

ensemble_forecasts_99th_ch4_null <- ensemble_forecasts_99th_ch4_null%>%filter(full_time_day>="2019-07-01")
mean_forecast_ch4_null <- mean_forecast_ch4_null%>%filter(full_time_day>="2019-07-01")
c <- ggplot(ensemble_forecasts_99th_ch4_null) + 
  geom_point(data = mean_forecast_ch4_null, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "orange", size =2)+
  geom_flat_violin(data = ensemble_forecasts_99th_ch4_null, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = "black", fill = "orange", scale = "width")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 3)+
  xlab("")+
  ylab(expression(paste("ln(Ebullition rate (mg CH "[4]," m"^"-2"," d"^"-1","))")))+
  labs(title="(c)   Persistence null model")+
  geom_vline(xintercept = as.POSIXct("2019-06-28"), lwd = 1, lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1]-(86400*1),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
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

c


# Figure output
tiff("./figures/EBU_FORECASTS_FIGURE_3.tiff", width = 12, height = 12, units = 'in', res = 600)
aa <- (a|b)/(c|plot_spacer()) + plot_layout(nrow = 2)
aa
dev.off()
############################################################################################

### Figure ### SWI temperature forecasts
############################################################################################
observed_temps <- as.data.frame(cbind(mean_observe_all[,1], ebullition_1107[,6]))
names(observed_temps) <- c("full_time_day", "observed_SWI_temp")
ensemble_temp_forecasts <- ensemble_temp_forecasts%>%filter(full_time_day>="2019-07-01")
mean_temp_forecasts <- mean_temp_forecasts%>%filter(full_time_day>="2019-07-01")

tiff("./figures/TEMPERATURE_FORECASTS_FIGURE_S2.tiff", width = 8, height = 6, units = 'in', res = 600)

v <- ggplot(observed_temps) + 
  geom_flat_violin(data = ensemble_temp_forecasts, aes(x = full_time_day, y = temp_prediction, group = full_time_day), size = .1, color = NA, fill = "darkorange2", scale = "width")+
  geom_point(data = mean_temp_forecasts, aes(x = full_time_day, y = temp_prediction),pch = 3, color = "darkorange2", size = 2)+
  geom_point(data = observed_temps, aes(x = full_time_day, y = observed_SWI_temp),pch = 23, color = "black", fill="red1", size = 3)+
  ylim(4,30)+
  xlab("")+
  ylab(expression(paste("Temperature (C)")))+
  geom_vline(xintercept = as.POSIXct("2019-06-28"), lwd = 1, lty = "dotted")+
  xlab("")+
  ylab("")+
  scale_x_datetime(limits = c(observed_temps$full_time_day[1]-(86400*1),observed_temps$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(observed_temps$full_time_day[2]-(86400*2),
                              observed_temps$full_time_day[6],
                              observed_temps$full_time_day[10]+(86400*3),
                              observed_temps$full_time_day[14]+(86400*4),
                              observed_temps$full_time_day[18]+(86400*4),
                              observed_temps$full_time_day[22]+(86400*9),
                              observed_temps$full_time_day[25]+(86400*11)), 
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
  ylim(10,26)+
  xlim(10,26)+
  xlab(expression(paste("Dam Site (C)")))+
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

var_ebu_forecast <- var_ebu_forecast%>%filter(date>="2019-07-01")
tiff("./figures/Total_forecast_variance.tiff", width = 14, height = 6, units = 'in', res = 800)
p <- ggplot(var_ebu_forecast, aes(date, total_variance))+
  geom_line(lwd = 3, color = "black")+
  theme_classic()+
xlab("")+
  ylab("Total forecast variance")
p
dev.off()
############################################################################################

### Figure ### Total SWI Temperature scaling model forecast variance
############################################################################################
tiff("./figures/Total_SWI_forecast_variance.tiff", width = 6, height = 4, units = 'in', res = 600)
p <- ggplot(var_temp_forecast, aes(date, total_variance))+
  geom_line(lwd = 3, color = "black")+
  theme_classic()+
xlab("")+
  ylab("Total forecast variance")
p
dev.off()
############################################################################################

### Figure ### Taylor Diagram for ebullition forecasts
############################################################################################
library("plotrix")

taylor_compare <- left_join(mean_observe_all, mean_forecast_ch4, by = "full_time_day")
taylor_compare <- left_join(taylor_compare, mean_forecast_ch4_nDA, by = "full_time_day")
taylor_compare <- left_join(taylor_compare, mean_forecast_ch4_null, by = "full_time_day")

taylor_compare <- taylor_compare %>% select(full_time_day,ebullition_prediction.x,ebullition_prediction.y,deterministic)
names(taylor_compare) <- c("full_time_day","observation","forecasts", "deterministic")

taylor_compare <- taylor_compare %>%
  filter(full_time_day>="2019-07-01")
taylor_compare <- taylor_compare[,c(1,3,6,8,10)]

names(taylor_compare) <- c("full_time_day","observations","forecasts_wDA","forecasts_nDA", "null_persist")

taylor_compare <- na.omit(taylor_compare)

tiff("./figures/TAYLOR_DIAGRAM.tiff", width=7, height=7, units="in", res = 600)
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts_nDA), pos.cor = T, col="magenta", pcex = 4, pch = 17, main = "") ### Using just 2018 training data
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts_nDA), col="black", add = T, pcex = 4, pch = 24) ### Using just 2018 training data
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts_wDA), col="blue", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$forecasts_wDA), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$observation), col="red", pos.cor=T, add = T, cex = 2, pcex = 6, pch = 18) ### Just 2019 data with data assimilation
taylor.diagram(exp(taylor_compare$observation), exp(taylor_compare$observation), col="black", pos.cor=T, add = T, cex = 2, pcex = 5, pch = 23) ### Just 2019 data with data assimilation

### Add legend to the plot
lpos1<-90*sd((taylor_compare$forecasts_nDA))
lpos2 <- 270*sd((taylor_compare$forecasts_nDA))
legend(lpos1,lpos2, box.lwd = 0, box.col = "white",legend=c("Observations", "Forecasts with data assimilation", "Forecasts without data assimilation"), pch=c(18,19,17),pt.cex = c(3,3,2), cex = 1.2, col=c("red","blue","magenta"))

dev.off()
############################################################################################

### Figure ### Forecast training
############################################################################################

taylor_compare$cycle <- seq.int(nrow(taylor_compare))

taylor_compare$forecast_diff <- taylor_compare$observation - taylor_compare$forecasts
taylor_compare$determine_diff <- taylor_compare$observation - taylor_compare$deterministic

colnames(taylor_compare) <-  c('full_time_day',"observed","forecasted", "determined", "forecast_cycle","Forecast cycles", "No time update")
taylor_compare <- taylor_compare %>% select(forecast_cycle, `Forecast cycles`, `Null determinisitc`)
m <-  melt(taylor_compare,id='forecast_cycle')

colnames(m) <-  c('forecast_cycle','Model','value')


q <- ggplot(m, aes(forecast_cycle, abs(value), fill = Model))+
  geom_bar(stat = "identity", color = "black", lwd = 0.1, position='dodge')+
  scale_x_continuous(limits = c(0,24), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))+
  scale_fill_manual(values = c("blue", "grey"))+
  xlab("Forecast cycle #")+
  ylab(expression(paste("Difference (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
  theme_classic()+
  #geom_vline(xintercept = 4.5)+
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
  ylim(12,26)+
  xlim(12,26)+
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








# 
# ebu$date <- as.POSIXct(strptime(ebu$date, '%Y-%m-%d', tz = 'EST'))
# 
# 
# d <- ggplot(ebu, aes(date, exp(log_ebu_rate_mg_m2_d)), color = "black") + 
#   geom_point(size = 2, pch = 21, fill = "blue")+
#   geom_point(data = mean_observe_all, aes(x = full_time_day, y = exp(ebullition_prediction)),pch = 23, color = "black", fill = "firebrick2", size = 6)+
#   xlab("")+
#   labs(title="Observed ebullition rates")+
#   ylim(0,100)+
#   ylab(expression(paste("Ebullition rate (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
#   geom_vline(xintercept = as.POSIXct("2019-06-28"), lwd = 1, lty = "dotted")+
#   scale_x_datetime(limits = c(mean_observe_all$full_time_day[1]-(86400*1),mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
#                    breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
#                               mean_observe_all$full_time_day[6],
#                               mean_observe_all$full_time_day[10]+(86400*3),
#                               mean_observe_all$full_time_day[14]+(86400*4),
#                               mean_observe_all$full_time_day[18]+(86400*4),
#                               mean_observe_all$full_time_day[22]+(86400*9),
#                               mean_observe_all$full_time_day[25]+(86400*11)), 
#                    labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
#   theme_classic()+
#   theme(axis.text=element_text(size=15, color = "black"),
#         axis.title=element_text(size=15, color = "black"),
#         legend.position=c(.1, .9),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.title = element_blank(), 
#         title = element_text(size = 15))
# d

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
# ### Figure ### ebullition model parameter estimates
############################################################################################
parm_ebu_all <- parm_ebu_all %>% filter(full_time_day>="2019-07-01")

mean_ebu_beta_1 <- parm_ebu_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
se_ebu_beta_1 <- parm_ebu_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(sd))
names(se_ebu_beta_1)[2] <- "SD"
mean_ebu_beta_1 <- left_join(mean_ebu_beta_1, se_ebu_beta_1, by = "full_time_day")

df <- data.frame(x = c(as.POSIXct("2019-07-01"),as.POSIXct("2019-11-07")), y = c(-1.95, -1.95))



b1 <- ggplot(mean_ebu_beta_1, aes(full_time_day, `beta[1]`))+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = -1.95, xend = as.POSIXct("2019-11-07"), yend = -1.95), color = "darkblue", lwd = 1)+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 3.73, xend = as.POSIXct("2019-11-07"), yend = 3.73), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = -7.63, xend = as.POSIXct("2019-11-07"), yend = -7.63), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_ribbon(aes(ymin = `beta[1]` - SD, ymax = `beta[1]` + SD), fill = "grey70", alpha = 0.7)+
  geom_line( color = "black", lwd = 3)+
  labs(title="(a)")+
 ylab(expression(paste(beta[3], " (Intercept)")))+
  xlab("")+
  ylim(c(-10,5))+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

mean_ebu_beta_2 <- parm_ebu_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
se_ebu_beta_2 <- parm_ebu_all %>% select(`beta[2]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(sd))
names(se_ebu_beta_2)[2] <- "SD"
mean_ebu_beta_2 <- left_join(mean_ebu_beta_2, se_ebu_beta_2, by = "full_time_day")

b2 <- ggplot(mean_ebu_beta_2, aes(full_time_day, `beta[2]`))+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 0.152, xend = as.POSIXct("2019-11-07"), yend = 0.152), color = "darkblue", lwd = 1)+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 0.449, xend = as.POSIXct("2019-11-07"), yend = 0.449), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = -0.145, xend = as.POSIXct("2019-11-07"), yend = -0.145), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_ribbon(aes(ymin = `beta[2]` - SD, ymax = `beta[2]` + SD), fill = "grey70", alpha = 0.7)+
  
  geom_line( color = "black", lwd = 3)+
  labs(title="(b)")+
  ylab(expression(paste(beta[4], " (CH"[4]," Ebullition Rate"["(t)"],")")))+
  xlab("")+
  ylim(c(-0.25,0.5))+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

mean_ebu_beta_3 <- parm_ebu_all %>% select(`beta[3]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
se_ebu_beta_3 <- parm_ebu_all %>% select(`beta[3]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(sd))
names(se_ebu_beta_3)[2] <- "SD"
mean_ebu_beta_3 <- left_join(mean_ebu_beta_3, se_ebu_beta_3, by = "full_time_day")

b3 <- ggplot(mean_ebu_beta_3, aes(full_time_day, `beta[3]`))+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 0.225, xend = as.POSIXct("2019-11-07"), yend = 0.225), color = "darkblue", lwd = 1)+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 0.533, xend = as.POSIXct("2019-11-07"), yend = 0.533), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = -0.083, xend = as.POSIXct("2019-11-07"), yend = -0.083), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_ribbon(aes(ymin = `beta[3]` - SD, ymax = `beta[3]` + SD), fill = "grey70", alpha = 0.7)+
  geom_line( color = "black", lwd = 3)+
  labs(title="(c)")+
  ylab(expression(paste(beta[5], " (SWI Temperature"["(t+1)"],")")))+
  xlab("")+
  ylim(c(-0.2,0.6))+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

mean_ebu_beta_4 <- parm_ebu_all %>% select(`sigma`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))
se_ebu_beta_4 <- parm_ebu_all %>% select(`sigma`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(sd))
names(se_ebu_beta_4)[2] <- "SD"
mean_ebu_beta_4 <- left_join(mean_ebu_beta_4, se_ebu_beta_4, by = "full_time_day")

b4 <- ggplot(mean_ebu_beta_4, aes(full_time_day, `sigma`))+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 0.579, xend = as.POSIXct("2019-11-07"), yend = 0.579), color = "darkblue", lwd = 1)+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = 1.323, xend = as.POSIXct("2019-11-07"), yend = 1.323), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_segment(aes(x = as.POSIXct("2019-07-01"), y = -0.165, xend = as.POSIXct("2019-11-07"), yend = -0.165), color = "lightblue", lwd = 1, lty = "dashed")+
  geom_ribbon(aes(ymin = `sigma` - SD, ymax = `sigma` + SD), fill = "grey70", alpha = 0.7)+
  geom_line( color = "black", lwd = 3)+
  labs(title="(d)")+
  ylab(expression(paste(epsilon, " (sigma)")))+
  xlab("")+
  ylim(c(-1,2))+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

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

tiff("./figures/BETA_ebu_estimates.tiff", width = 10, height = 10, units = "in", res = 600)
beta <- (b1|b2)/(b3|b4)
beta
dev.off()
############################################################################################
