library(patchwork)

#Figures 1 and 2 are made in Arcmap, Lucidchart, and PPT. 

### Figure 3 ### Ebullition forecasts
############################################################################################

pdf("Ebullition_Rate_forecasts_wo_jitter_level_temp_14May20.pdf", width=10, height=13)

f <- ggplot(ensemble_forecasts_all_temp) + 
  #geom_jitter(data = filtered_ensemble_30_70_forecasts_all, aes(x = full_time_day, y = exp(ebullition_prediction), group = full_time_day), size=1.3, pch = 21, color = "black", fill = "white", width = .5)+
  geom_flat_violin(data = ensemble_forecasts_all_temp, aes(x = full_time_day, y = exp(ebullition_prediction), group = full_time_day), size=.1, color = NA, fill = "darkorchid1", scale = "width")+
  geom_point(data = mean_forecasts_temp, aes(x = full_time_day, y = exp(ebullition_prediction)),pch = 3, color = "darkorchid1", size = 2)+
  
  #geom_line(data = forecast_observe_compare_18, aes(x=ebu_forcast_date,y=deterministic_17),lwd = 5, color = "red3")+
  # geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=exp(ebullition_prediction)-exp(SE), ymax=exp(ebullition_prediction)+exp(SE)), width=120000,
  #               position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = exp(ebullition_prediction)),pch = 23, color = "black", fill = "firebrick2", size = 4)+
  ylim(0,1000000)+
  xlab("")+
  labs(title="Temperature + Level")+
  ylab(expression(paste("Ebullition Rate (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
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
        legend.title = element_blank())


ensemble_forecasts_all_temp_short <- ensemble_forecasts_all_temp%>% filter(full_time_day>="2019-07-08")
e <- ggplot(ensemble_forecasts_all_temp_short) + 
  #geom_jitter(data = ensemble_forecasts_all_temp_short, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=1.3, pch = 21, color = "black", fill = "white", width = .5)+
  geom_flat_violin(data = ensemble_forecasts_all_temp_short, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = NA, fill = "blue", scale = "width")+
  geom_point(data = mean_forecasts_temp, aes(x = full_time_day, y = ebullition_prediction),pch = 3, color = "blue", size = 2)+
  
  #geom_line(data = forecast_observe_compare_18, aes(x=ebu_forcast_date,y=deterministic_17),lwd = 5, color = "red3")+
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
e

# options(gganimate.dev_args = list(width = 700, height = 450))
# s <- z + transition_reveal(full_time_day)+
# shadow_mark(alpha = 0.2, size = 1)
# anim_save("Ebu_forecast_23jun20_presentation_19.gif", s)

shaded <- data.frame(x = c(as.POSIXct("2019-12-01"), as.POSIXct("2019-06-30")), y = c(10, -10))

z <- ggplot(ensemble_forecasts_all_temp) + 
  #geom_jitter(data = ensemble_forecasts_all_temp, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=1.3, pch = 21, color = "black", fill = "white", width = .5)+
  geom_flat_violin(data = ensemble_forecasts_all_temp, aes(x = full_time_day, y = ebullition_prediction, group = full_time_day), size=.1, color = NA, fill = "blue", scale = "width")+
  #geom_point(data = mean_forecasts_temp, aes(x = full_time_day, y = ebullition_prediction),pch = 3, color = "blue", size = 2)+
  
  #geom_line(data = forecast_observe_compare_18, aes(x=ebu_forcast_date,y=deterministic_17),lwd = 5, color = "red3")+
  geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=ebullition_prediction-SE, ymax=ebullition_prediction+SE), width=120000,
                  position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = ebullition_prediction),pch = 23, color = "black", fill = "firebrick2", size = 2)+
  #ylim(-200,200)+
  xlab("")+
  labs(title="A")+
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
z

tiff("FIGURE_3_EBU_FORECAST_W_JITTER.tiff", width = 14, height = 6, units = 'in', res = 1600)
aa <- z|e
aa
dev.off()




g <- ggplot(ensemble_forecasts_all_level) + 
  #geom_jitter(data = filtered_ensemble_30_70_forecasts_all, aes(x = full_time_day, y = exp(ebullition_prediction), group = full_time_day), size=1.3, pch = 21, color = "black", fill = "white", width = .5)+
  geom_flat_violin(data = ensemble_forecasts_all_level, aes(x = full_time_day, y = exp(ebullition_prediction), group = full_time_day), size=.1, color = NA, fill = "turquoise", scale = "width")+
  geom_point(data = mean_forecasts_level, aes(x = full_time_day, y = exp(ebullition_prediction)),pch = 3, color = "turquoise", size = 2)+
  
  #geom_line(data = forecast_observe_compare_18, aes(x=ebu_forcast_date,y=deterministic_17),lwd = 5, color = "red3")+
  # geom_errorbar(data = mean_observe_all,aes(x = full_time_day, ymin=exp(ebullition_prediction)-exp(SE), ymax=exp(ebullition_prediction)+exp(SE)), width=120000,
  #               position=position_dodge(0.05), color = "firebrick2", lwd = 1)+
  geom_point(data = mean_observe_all, aes(x = full_time_day, y = exp(ebullition_prediction)),pch = 23, color = "black", fill = "firebrick2", size = 4)+
  ylim(0,250)+
  xlab("")+
  labs(title="Level")+
  ylab(expression(paste("Ebullition Rate (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
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
        legend.title = element_blank())


h = f/e/g
h

dev.off()
############################################################################################

### Figure 4 ### Forecast total variance through time
############################################################################################
pdf("Variance_total_season_all_14May20.pdf", width=10, height=13)

a <- ggplot(var_ebu_temp, aes(x = date, y = abs(variance)))+
  geom_bar(stat = "identity", fill = "blue", color = "black", lwd = 0.1)+
  xlab("")+
  labs(title = "")+
  theme(legend.position="top")+
  #scale_y_log10(   breaks = scales::trans_breaks("log10", function(x) 10^x),
   #                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  ylab(expression(paste("Total Variance ln(mg CH "[4]," m"^"-2"," d"^"-1","))")))+
  # scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]), expand = c(.05,.05), 
  #                  breaks = c(mean_observe_all$full_time_day[1],
  #                             mean_observe_all$full_time_day[8],
  #                             mean_observe_all$full_time_day[16],
  #                             mean_observe_all$full_time_day[25]), 
  #                  labels = c("27-May","15-July","11-Sep","20-Nov"))+
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
        legend.title = element_blank())

a
b <- ggplot(var_ebu_temp_level, aes(x = date, y = variance))+
  geom_bar(stat = "identity", fill = "darkorchid1", color = "black", lwd = 0.1)+
  xlab("")+
  labs(title = "Temperature + Level")+
  theme(legend.position="top")+
  scale_y_log10(   breaks = scales::trans_breaks("log10", function(x) 10^x),
                   labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  ylab(expression(paste("")))+
  # scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]), expand = c(.05,.05), 
  #                  breaks = c(mean_observe_all$full_time_day[1],
  #                             mean_observe_all$full_time_day[8],
  #                             mean_observe_all$full_time_day[16],
  #                             mean_observe_all$full_time_day[25]), 
  #                  labels = c("27-May","15-July","11-Sep","20-Nov"))+
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
        legend.title = element_blank())

c <- ggplot(var_ebu_level, aes(x = date, y = variance))+
  geom_bar(stat = "identity", fill = "turquoise", color = "black", lwd = 0.1)+
  xlab("")+
  labs(title = "Level")+
  theme(legend.position="top")+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                   labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  ylab(expression(paste("")))+
  # scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]), expand = c(.05,.05), 
  #                  breaks = c(mean_observe_all$full_time_day[1],
  #                             mean_observe_all$full_time_day[8],
  #                             mean_observe_all$full_time_day[16],
  #                             mean_observe_all$full_time_day[25]), 
  #                  labels = c("27-May","15-July","11-Sep","20-Nov"))+
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
        legend.title = element_blank())

c = b/a/c
c
dev.off()
############################################################################################

### Figure 5 ### Parameter Estimates
############################################################################################


parms <- read_csv("parameter_values_ci_22Jun20.csv")
parms$date <- as.POSIXct(strptime(parms$date, '%Y-%m-%d', tz = 'EST'))

parms_short <- parms %>% filter(date >= "2019-07-01")

p0 <- ggplot(parms, aes(x = date, y=`intercept_50%`))+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_25%`), ymax=`intercept_50%`+(`intercept_75%`-`intercept_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_2.5%`), ymax=`intercept_50%`+(`intercept_97.5%`-`intercept_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`intercept_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`intercept_75%`),lwd = .5, color = "black")+
  labs(title="A")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[3], " (Intercept)")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))


p1 <- ggplot(parms, aes(x = date, y=`ar_50%`))+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_25%`), ymax=`ar_50%`+(`ar_75%`-`ar_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_2.5%`), ymax=`ar_50%`+(`ar_97.5%`-`ar_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`ar_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`ar_75%`),lwd = .5, color = "black")+
  labs(title="B")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[4], " (E"["(t)"],")")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))

p2 <- ggplot(parms, aes(x = date, y=`temp_50%`))+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_25%`), ymax=`temp_50%`+(`temp_75%`-`temp_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_2.5%`), ymax=`temp_50%`+(`temp_97.5%`-`temp_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`temp_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`temp_75%`),lwd = .5, color = "black")+
  labs(title="C")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[5], " (ST"["(t+i)"],")")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))

p3 <- ggplot(parms_short, aes(x = date, y=`intercept_50%`))+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_25%`), ymax=`intercept_50%`+(`intercept_75%`-`intercept_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_2.5%`), ymax=`intercept_50%`+(`intercept_97.5%`-`intercept_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`intercept_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`intercept_75%`),lwd = .5, color = "black")+
  labs(title="D")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[6],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[3], " (Intercept)")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))


p4 <- ggplot(parms_short, aes(x = date, y=`ar_50%`))+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_25%`), ymax=`ar_50%`+(`ar_75%`-`ar_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_2.5%`), ymax=`ar_50%`+(`ar_97.5%`-`ar_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`ar_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`ar_75%`),lwd = .5, color = "black")+
  labs(title="E")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[6],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(
                     mean_observe_all$full_time_day[6],
                     mean_observe_all$full_time_day[10]+(86400*3),
                     mean_observe_all$full_time_day[14]+(86400*4),
                     mean_observe_all$full_time_day[18]+(86400*4),
                     mean_observe_all$full_time_day[22]+(86400*9),
                     mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[4], " (E"["(t)"],")")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))

p5 <- ggplot(parms_short, aes(x = date, y=`temp_50%`))+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_25%`), ymax=`temp_50%`+(`temp_75%`-`temp_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_2.5%`), ymax=`temp_50%`+(`temp_97.5%`-`temp_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`temp_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`temp_75%`),lwd = .5, color = "black")+
  labs(title="F")+
  geom_vline(xintercept = as.POSIXct("2019-07-01"), lty = "dotted")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[6],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(
                     mean_observe_all$full_time_day[6],
                     mean_observe_all$full_time_day[10]+(86400*3),
                     mean_observe_all$full_time_day[14]+(86400*4),
                     mean_observe_all$full_time_day[18]+(86400*4),
                     mean_observe_all$full_time_day[22]+(86400*9),
                     mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[5], " (ST"["(t+i)"],")")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=18),
        axis.text = element_text(color="black", size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank(), 
        title = element_text(size = 15))

pdf("Forecasts_parameter_est_22Jun20.pdf", width=14, height=10)
plot1 <- (p0|p1|p2)/(p3|p4|p5)
plot1
dev.off()
############################################################################################

### Figure 6 ### Uncertatiny proportions
############################################################################################

prop <- read_csv("var_partition_v2.csv")

proportions_new <- melt(prop, id.vars = "date")


c <- ggplot(proportions_new, aes(x = date, y = value, fill = variable)) + 
  geom_area(position = 'stack', colour="black", size = 1)+
  xlab("")+
  ylab(c("Proportion of total variance"))+
  scale_fill_viridis(discrete = TRUE, option = "C")+
  theme_classic()+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(0,1.001), expand = c(0,0))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12),
        axis.text = element_text(color="black", size = 12),
        legend.title = element_blank())
c

plot(var_ebu_temp$date, var_ebu_temp$var, type = "l", lwd = 9, col = "grey", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(4)


############################################################################################

### Figure 7 ### Forecast evaluation
############################################################################################

pdf("Model_comparisons_time_series_22jun20.pdf", width=10, height=7)
a <- ggplot(stats) + 
  geom_point(data = stats, aes(x=full_time_day,y=mean_forecasts_deterministic),pch = 24,color = "black", fill = "grey", size = 4)+
  geom_point(data = stats, aes(x=full_time_day,y=exp(ebullition_temp_model)),pch = 21,color = "black", fill = "blue", size = 6)+
  #geom_point(data = stats, aes(x=full_time_day,y=exp(ebullition_temp_level_model)),pch = 21,color = "black", fill = "darkorchid1", size = 6)+
  #geom_point(data = stats, aes(x=full_time_day,y=exp(ebullition_level_model)),pch = 21,color = "black", fill = "turquoise", size = 6)+
  geom_line(data = stats, aes(x = full_time_day, y = exp(observed)),lwd = 0.5, color = "red")+
  geom_point(data = stats, aes(x = full_time_day, y = exp(observed)),pch = 23, color = "black", fill = "red1", size = 5)+
  ylim(0,100)+
  xlab("")+
  geom_vline(xintercept = as.POSIXct("2019-06-10"), lty = "dotted")+
  ylab(expression(paste("Ebullition (mg CH "[4]," m"^"-2"," d"^"-1",")")))+
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
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

a
dev.off()
#Taylor diagram
library("plotrix")
# show the "all correlation" display
pdf("Taylor_diagram_22jun20.pdf", width=7, height=7)

taylor.diagram(exp(stats2$observed), stats2$mean_forecasts_deterministic, pos.cor = T, col="grey", pcex = 3, pch = 17, main = "") ### Using just 2018 training data
taylor.diagram(exp(stats2$observed), stats2$mean_forecasts_deterministic, col="black", add = T, pcex = 3, pch = 24) ### Using just 2018 training data
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_temp_level_model), col="darkorchid1", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_temp_level_model), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
taylor.diagram(exp(stats2$observed), exp(stats2$ebullition_temp_model), col="blue", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
taylor.diagram(exp(stats2$observed), exp(stats2$ebullition_temp_model), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_level_model), col="turquoise", pos.cor=T, add = T, cex = 2, pcex = 4, pch = 19) ### Just 2019 data with data assimilation
#taylor.diagram(exp(stats$observed), exp(stats$ebullition_level_model), col="black", pos.cor=T, add = T, cex = 2, pcex = 4.3, pch = 21) ### Just 2019 data with data assimilation
taylor.diagram(exp(stats2$observed), exp(stats2$observed), col="red", pos.cor=T, add = T, cex = 2, pcex = 6, pch = 18) ### Just 2019 data with data assimilation
taylor.diagram(exp(stats2$observed), exp(stats2$observed), col="black", pos.cor=T, add = T, cex = 2, pcex = 5, pch = 23) ### Just 2019 data with data assimilation

### Add legend to the plot
lpos1<-17*sd((stats2$observed))
lpos2 <- 27*sd((stats2$ebullition_temp_model))
legend(lpos1,lpos2, box.lwd = 0, box.col = "white",legend=c("Observations","Forecast model", "Deterministic model"), pch=c(18,19,17),pt.cex = c(3,3,2), cex = 1.2, col=c("red","blue","grey"))

dev.off()
############################################################################################



### Figure S1 ### Water level change in FCR
############################################################################################
level <- read_csv("water_level_fst_diff_cm.csv")
level$site <- "Reservoir Intake"

pdf("water_level_TS_20Apr20.pdf", width=12, height=6)

a <- ggplot(level, aes(x = date, y = -(level+9.4)))+
  geom_line(lwd = 1, color = "black")+
  geom_point(size=2, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_classic()+
  ylim(-9.6, -9)+
  labs(y = expression(paste("Reservoir Depth (m)")), x = "", title = "")+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))

b <- ggplot(level, aes(x = site, y = -(level+9.4)))+
  geom_boxplot(color = "black", pch = 21, bg = "dodgerblue4")+
  theme_classic()+
  ylim(-9.6, -9)+
  labs(y = expression(paste("")), x = "", title = "")+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))

c = a|b
c
dev.off()
############################################################################################

### Figure S2 ### 2017 Observed vs. predicted
############################################################################################
pdf("observed_and_predicted_TS_20Apr20.pdf", width=10, height=6)

a <- ggplot(ebu_17, aes(x = date, y = exp(log_ebu_mgCH4_m2_d)))+
  geom_line(lwd = 1, color = "black")+
  geom_point(size=6, color = "black", pch = 21, bg = "grey70")+
  labs(y = expression(paste("Ebullition (mg CH"[4]," m"^"-2"," d"^"-1",")")), x = "", title = "")

b <- a + 
  geom_point(aes(x = date, y = exp(predict.lm_17.)), size=6, color = "black", pch = 21, bg = "dodgerblue4")+
  theme_classic()+
  theme(text = element_text(size=15, color = "black"),
        axis.text = element_text(size = 15, color = "black"))

b
dev.off()
############################################################################################

### Figure S3 ### Temperature
############################################################################################
pdf("Temperature_forecasts_wo_jitter_14May20.pdf", width=10, height=7)

c <- ggplot() + 
  geom_flat_violin(data = ensemble_temp_forecasts_all, aes(x = full_time_day, y = temp_prediction, group = full_time_day), size = .1, color = NA, fill = "darkorange2", scale = "width")+
  geom_point(data = temp_observed, aes(x = full_time_day, y = observed_temp),pch = 23, color = "black", fill = "red1", size = 3)+
  ylim(4,30)+
  xlab("")+
  ylab(expression(paste("Temperature (C)")))+
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
        legend.position=c(.2, .9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
c
dev.off()
############################################################################################

### Figure S4 ### Temperature catwalk and HOBO loggers
############################################################################################

temp_model <- lm(Temp_C~mean_ws_temp, data = temp_model_1030)
summary(temp_model)

temp_train <- temp_model_1030 %>% filter(TIMESTAMP <= "2019-01-01")
temp_forecast <- temp_model_1030 %>% filter(TIMESTAMP >= "2019-01-01")

tiff("Catwalk_HOBO_lm_27Apr20.tiff", width=7, height=7, units = "in",res = 350)

a <- ggplot()+
  geom_point(data = temp_train,  aes(mean_ws_temp,Temp_C), pch = 21, size = 4, col = "black", fill = "cyan")+
  geom_point(data = temp_forecast,  aes(mean_ws_temp,Temp_C), pch = 21, size = 4, col = "black", fill = "red")+
  geom_smooth(data = temp_model_1030,  aes(mean_ws_temp,Temp_C),method = "lm", color = "black", fill = "grey", alpha = 0.75)+
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

### Figure S5 ### Temperature variance parition
############################################################################################
pdf("Temp_Variance_Partition_27Apr20.pdf", width=8, height=10)

w <- ggplot(var_temp_all, aes(x = date, y = Variance))+
  geom_bar(stat = "identity", fill = "blue", color = "black", lwd = 0.1)+
  xlab("")+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(0,2), expand = c(0,0))+
  ylab(expression(paste("Temperature Total Variance (C)")))+
  # scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]), expand = c(.05,.05), 
  #                  breaks = c(mean_observe_all$full_time_day[1],
  #                             mean_observe_all$full_time_day[8],
  #                             mean_observe_all$full_time_day[16],
  #                             mean_observe_all$full_time_day[25]), 
  #                  labels = c("27-May","15-July","11-Sep","20-Nov"))+
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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12),
        axis.text = element_text(color="black", size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

x <- ggplot(proportion_var_temp_new, aes(x = date, y = value, fill = variable)) + 
  geom_area(position = 'stack')+
  xlab("")+
  ylab(c("Proportion of total variance"))+
  scale_fill_viridis(discrete = TRUE, option = "C")+
  theme_classic()+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(0,1.001), expand = c(0,0))+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(),legend.background = element_blank(),
        legend.key.size = unit(0.7,"cm"),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12),
        axis.text = element_text(color="black", size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2),
        legend.title = element_blank())

z = w/x

z
dev.off()
############################################################################################

### Figure S6 ### Parameter Estimates
############################################################################################
parms <- read_csv("parameter_values_ci_27Apr20.csv")
parms$date <- as.POSIXct(strptime(parms$date, '%Y-%m-%d', tz = 'EST'))

p0 <- ggplot(parms, aes(x = date, y=`intercept_50%`))+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_25%`), ymax=`intercept_50%`+(`intercept_75%`-`intercept_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`intercept_50%`-(`intercept_50%`-`intercept_2.5%`), ymax=`intercept_50%`+(`intercept_97.5%`-`intercept_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`intercept_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`intercept_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`intercept_75%`),lwd = .5, color = "black")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[0], " (Intercept)")))+
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


p1 <- ggplot(parms, aes(x = date, y=`ar_50%`))+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_25%`), ymax=`ar_50%`+(`ar_75%`-`ar_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`ar_50%`-(`ar_50%`-`ar_2.5%`), ymax=`ar_50%`+(`ar_97.5%`-`ar_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`ar_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`ar_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`ar_75%`),lwd = .5, color = "black")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[1], " (CH"[4]," Ebullition Rate"["(t)"],")")))+
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

p2 <- ggplot(parms, aes(x = date, y=`temp_50%`))+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_25%`), ymax=`temp_50%`+(`temp_75%`-`temp_50%`)), fill = "grey25", alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin=`temp_50%`-(`temp_50%`-`temp_2.5%`), ymax=`temp_50%`+(`temp_97.5%`-`temp_50%`)), fill = "grey70", alpha = 0.5)+
  geom_line(lwd = 2, color = "black")+
  geom_line(aes(x=date,y=`temp_2.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_97.5%`),lwd = .3, color = "black")+
  geom_line(aes(x=date,y=`temp_25%`),lwd = .5, color = "black")+
  geom_line(aes(x=date,y=`temp_75%`),lwd = .5, color = "black")+
  scale_x_datetime(limits = c(mean_observe_all$full_time_day[1],mean_observe_all$full_time_day[25]+(86400*11)), expand = c(.05,.05), 
                   breaks = c(mean_observe_all$full_time_day[2]-(86400*2),
                              mean_observe_all$full_time_day[6],
                              mean_observe_all$full_time_day[10]+(86400*3),
                              mean_observe_all$full_time_day[14]+(86400*4),
                              mean_observe_all$full_time_day[18]+(86400*4),
                              mean_observe_all$full_time_day[22]+(86400*9),
                              mean_observe_all$full_time_day[25]+(86400*11)), 
                   labels = c("01 June","01 July","01 Aug","01 Sep", "01 Oct", "01 Nov", "01 Dec"))+
  xlab("")+
  ylab(expression(paste(beta[2], " (SWI Temperature"["(t+1)"],")")))+
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

pdf("Forecasts_parameter_est_23Apr20.pdf", width=14, height=4)
plot1 <- p0|p1|p2
plot1
dev.off()
############################################################################################