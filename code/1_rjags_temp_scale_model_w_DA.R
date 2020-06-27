### RJAGS temperature model using hobo and catwalk data going back to deployment of the catwalk of FLARE in 2018

cat <- read_csv("./input/DA_temp_scale_model/Catwalk.csv", skip = 1)

cat_sum <- cat %>% filter(TIMESTAMP >= "2018-07-05 12:00:00") %>%
  select(TIMESTAMP, wtr_1, wtr_2, wtr_3) %>%
  #filter(wtr_1 != "NAN") %>%
  filter(wtr_2 != "NAN") %>%
  filter(wtr_3 != "NAN") %>%
  filter(TIMESTAMP != "NAN") %>%
  filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")%>%
  mutate(mean_ws_temp = (as.numeric(wtr_2)+as.numeric(wtr_3))/2) %>%
  select(TIMESTAMP, mean_ws_temp)

cat_sum$TIMESTAMP <- as.POSIXct(strptime(cat_sum$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### 03June19 and 10June19 Temperature JAGS model ###
###########################################################################################

### READ IN THE HOBO DATA
hobo_603_610 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_603_610.csv")
names(hobo_603_610)[1] <- "TIMESTAMP"

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_603_610 <- left_join(hobo_603_610, cat_sum, by = "TIMESTAMP")
temp_model_603_610 <- na.omit(temp_model_603_610)

temp_model_603_610$TIMESTAMP <- as.POSIXct(strptime(temp_model_603_610$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_603_610 <- temp_model_603_610 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_603_610$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_603_610$Temp_C,
                               'catwalk' = temp_model_603_610$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma',"tau"),
                       n.iter = 5000)
summary(samples)
gelman.diag(samples)

master_temp_chain_603_610 <- combine.mcmc(samples)

curr_temp_pars_603_610 <- sample(seq(0, nrow(master_temp_chain_603_610)),1)

mean_pars_temp_603_610 <- colMeans(master_temp_chain_603_610)
###########################################################################################

### 17June19 Temperature JAGS model ###
###########################################################################################

### READ IN THE HOBO DATA
hobo_617 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_617.csv")
names(hobo_617)[1] <- "TIMESTAMP"
hobo_617$TIMESTAMP <- as.POSIXct(strptime(hobo_617$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_617 <- left_join(hobo_617, cat_sum, by = "TIMESTAMP")
temp_model_617 <- na.omit(temp_model_617)

temp_model_617$TIMESTAMP <- as.POSIXct(strptime(temp_model_617$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_617 <- temp_model_617 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_617$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_617$Temp_C,
                               'catwalk' = temp_model_617$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)
gelman.diag(samples)

master_temp_chain_617 <- combine.mcmc(samples)

curr_temp_pars_617 <- sample(seq(0, nrow(master_temp_chain_617)),1)

mean_pars_temp_617 <- colMeans(master_temp_chain_617)
###########################################################################################

### 24June19 Temperature JAGS model ###
###########################################################################################

### READ IN THE HOBO DATA
hobo_624 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_624.csv")
names(hobo_624)[1] <- "TIMESTAMP"
hobo_624$TIMESTAMP <- as.POSIXct(strptime(hobo_624$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_624 <- left_join(hobo_624, cat_sum, by = "TIMESTAMP")
temp_model_624 <- na.omit(temp_model_624)

temp_model_624$TIMESTAMP <- as.POSIXct(strptime(temp_model_624$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_624 <- temp_model_624 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_624$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_624$Temp_C,
                               'catwalk' = temp_model_624$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_624 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_624 <- colMeans(master_temp_chain_624) ### Calculate the means of the parameter distribution
###########################################################################################

### 01July19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_701 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_701.csv")
names(hobo_701)[1] <- "TIMESTAMP"
hobo_701$TIMESTAMP <- as.POSIXct(strptime(hobo_701$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_701 <- left_join(hobo_701, cat_sum, by = "TIMESTAMP")
temp_model_701 <- na.omit(temp_model_701)

temp_model_701$TIMESTAMP <- as.POSIXct(strptime(temp_model_701$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_701 <- temp_model_701 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_701$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_701$Temp_C,
                               'catwalk' = temp_model_701$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_701 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_701 <- colMeans(master_temp_chain_701) ### Calculate the means of the parameter distribution
###########################################################################################

### 08July19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_708 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_708.csv")
names(hobo_708)[1] <- "TIMESTAMP"
hobo_708$TIMESTAMP <- as.POSIXct(strptime(hobo_708$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_708 <- left_join(hobo_708, cat_sum, by = "TIMESTAMP")
temp_model_708 <- na.omit(temp_model_708)

temp_model_708$TIMESTAMP <- as.POSIXct(strptime(temp_model_708$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_708 <- temp_model_708 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_708$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_708$Temp_C,
                               'catwalk' = temp_model_708$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_708 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_708 <- colMeans(master_temp_chain_708) ### Calculate the means of the parameter distribution
###########################################################################################

### 15July19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_715 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_715.csv")
names(hobo_715)[1] <- "TIMESTAMP"
hobo_715$TIMESTAMP <- as.POSIXct(strptime(hobo_715$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_715 <- left_join(hobo_715, cat_sum, by = "TIMESTAMP")
temp_model_715 <- na.omit(temp_model_715)

temp_model_715$TIMESTAMP <- as.POSIXct(strptime(temp_model_715$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_715 <- temp_model_715 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_715$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_715$Temp_C,
                               'catwalk' = temp_model_715$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_715 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_715 <- colMeans(master_temp_chain_715) ### Calculate the means of the parameter distribution
###########################################################################################

### 22July19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_722 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_722.csv")
names(hobo_722)[1] <- "TIMESTAMP"
hobo_722$TIMESTAMP <- as.POSIXct(strptime(hobo_722$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_722 <- left_join(hobo_722, cat_sum, by = "TIMESTAMP")
temp_model_722 <- na.omit(temp_model_722)

temp_model_722$TIMESTAMP <- as.POSIXct(strptime(temp_model_722$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_722 <- temp_model_722 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_722$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_722$Temp_C,
                               'catwalk' = temp_model_722$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_722 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_722 <- colMeans(master_temp_chain_722) ### Calculate the means of the parameter distribution
###########################################################################################

### 29July19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_729 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_729.csv")
names(hobo_729)[1] <- "TIMESTAMP"
hobo_729$TIMESTAMP <- as.POSIXct(strptime(hobo_729$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_729 <- left_join(hobo_729, cat_sum, by = "TIMESTAMP")
temp_model_729 <- na.omit(temp_model_729)

temp_model_729$TIMESTAMP <- as.POSIXct(strptime(temp_model_729$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_729 <- temp_model_729 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_729$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_729$Temp_C,
                               'catwalk' = temp_model_729$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_729 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_729 <- colMeans(master_temp_chain_729) ### Calculate the means of the parameter distribution
###########################################################################################

### 05Aug19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_805 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_805.csv")
names(hobo_805)[1] <- "TIMESTAMP"
hobo_805$TIMESTAMP <- as.POSIXct(strptime(hobo_805$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_805 <- left_join(hobo_805, cat_sum, by = "TIMESTAMP")
temp_model_805 <- na.omit(temp_model_805)

temp_model_805$TIMESTAMP <- as.POSIXct(strptime(temp_model_805$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_805 <- temp_model_805 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_805$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_805$Temp_C,
                               'catwalk' = temp_model_805$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_805 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_805 <- colMeans(master_temp_chain_805) ### Calculate the means of the parameter distribution
###########################################################################################

### 12Aug19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_812 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_812.csv")
names(hobo_812)[1] <- "TIMESTAMP"
hobo_812$TIMESTAMP <- as.POSIXct(strptime(hobo_812$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_812 <- left_join(hobo_812, cat_sum, by = "TIMESTAMP")
temp_model_812 <- na.omit(temp_model_812)

temp_model_812$TIMESTAMP <- as.POSIXct(strptime(temp_model_812$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_812 <- temp_model_812 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_812$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_812$Temp_C,
                               'catwalk' = temp_model_812$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_812 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_812 <- colMeans(master_temp_chain_812) ### Calculate the means of the parameter distribution
###########################################################################################

### 19Aug19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_819 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_819.csv")
names(hobo_819)[1] <- "TIMESTAMP"
hobo_819$TIMESTAMP <- as.POSIXct(strptime(hobo_819$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_819 <- left_join(hobo_819, cat_sum, by = "TIMESTAMP")
temp_model_819 <- na.omit(temp_model_819)

temp_model_819$TIMESTAMP <- as.POSIXct(strptime(temp_model_819$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_819 <- temp_model_819 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_819$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_819$Temp_C,
                               'catwalk' = temp_model_819$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_819 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_819 <- colMeans(master_temp_chain_819) ### Calculate the means of the parameter distribution
###########################################################################################

### 28Aug19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_828 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_828.csv")
names(hobo_828)[1] <- "TIMESTAMP"
hobo_828$TIMESTAMP <- as.POSIXct(strptime(hobo_828$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_828 <- left_join(hobo_828, cat_sum, by = "TIMESTAMP")
temp_model_828 <- na.omit(temp_model_828)

temp_model_828$TIMESTAMP <- as.POSIXct(strptime(temp_model_828$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_828 <- temp_model_828 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_828$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_828$Temp_C,
                               'catwalk' = temp_model_828$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_828 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_828 <- colMeans(master_temp_chain_828) ### Calculate the means of the parameter distribution
###########################################################################################

### 02Sep19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_902 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_902.csv")
names(hobo_902)[1] <- "TIMESTAMP"
hobo_902$TIMESTAMP <- as.POSIXct(strptime(hobo_902$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_902 <- left_join(hobo_902, cat_sum, by = "TIMESTAMP")
temp_model_902 <- na.omit(temp_model_902)

temp_model_902$TIMESTAMP <- as.POSIXct(strptime(temp_model_902$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_902 <- temp_model_902 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_902$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_902$Temp_C,
                               'catwalk' = temp_model_902$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_902 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_902 <- colMeans(master_temp_chain_902) ### Calculate the means of the parameter distribution
###########################################################################################

### 11Sep19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_911 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_911.csv")
names(hobo_911)[1] <- "TIMESTAMP"
hobo_911$TIMESTAMP <- as.POSIXct(strptime(hobo_911$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_911 <- left_join(hobo_911, cat_sum, by = "TIMESTAMP")
temp_model_911 <- na.omit(temp_model_911)

temp_model_911$TIMESTAMP <- as.POSIXct(strptime(temp_model_911$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_911 <- temp_model_911 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_911$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_911$Temp_C,
                               'catwalk' = temp_model_911$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_911 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_911 <- colMeans(master_temp_chain_911) ### Calculate the means of the parameter distribution
###########################################################################################

###20Sep19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_920 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_920.csv")
names(hobo_920)[1] <- "TIMESTAMP"
hobo_920$TIMESTAMP <- as.POSIXct(strptime(hobo_920$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_920 <- left_join(hobo_920, cat_sum, by = "TIMESTAMP")
temp_model_920 <- na.omit(temp_model_920)

temp_model_920$TIMESTAMP <- as.POSIXct(strptime(temp_model_920$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_920 <- temp_model_920 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_920$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_920$Temp_C,
                               'catwalk' = temp_model_920$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_920 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_920 <- colMeans(master_temp_chain_920) ### Calculate the means of the parameter distribution
###########################################################################################

###27Sep19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_927 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_927.csv")
names(hobo_927)[1] <- "TIMESTAMP"
hobo_927$TIMESTAMP <- as.POSIXct(strptime(hobo_927$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_927 <- left_join(hobo_927, cat_sum, by = "TIMESTAMP")
temp_model_927 <- na.omit(temp_model_927)

temp_model_927$TIMESTAMP <- as.POSIXct(strptime(temp_model_927$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_927 <- temp_model_927 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_927$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_927$Temp_C,
                               'catwalk' = temp_model_927$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_927 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_927 <- colMeans(master_temp_chain_927) ### Calculate the means of the parameter distribution
###########################################################################################

###02Oct19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1002 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1002.csv")
names(hobo_1002)[1] <- "TIMESTAMP"
hobo_1002$TIMESTAMP <- as.POSIXct(strptime(hobo_1002$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1002 <- left_join(hobo_1002, cat_sum, by = "TIMESTAMP")
temp_model_1002 <- na.omit(temp_model_1002)

temp_model_1002$TIMESTAMP <- as.POSIXct(strptime(temp_model_1002$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1002 <- temp_model_1002 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1002$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1002$Temp_C,
                               'catwalk' = temp_model_1002$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1002 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1002 <- colMeans(master_temp_chain_1002) ### Calculate the means of the parameter distribution
###########################################################################################

###11Oct19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1011 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1011.csv")
names(hobo_1011)[1] <- "TIMESTAMP"
hobo_1011$TIMESTAMP <- as.POSIXct(strptime(hobo_1011$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1011 <- left_join(hobo_1011, cat_sum, by = "TIMESTAMP")
temp_model_1011 <- na.omit(temp_model_1011)

temp_model_1011$TIMESTAMP <- as.POSIXct(strptime(temp_model_1011$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1011 <- temp_model_1011 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1011$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1011$Temp_C,
                               'catwalk' = temp_model_1011$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1011 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1011 <- colMeans(master_temp_chain_1011) ### Calculate the means of the parameter distribution
###########################################################################################

###16Oct19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1016 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1016.csv")
names(hobo_1016)[1] <- "TIMESTAMP"
hobo_1016$TIMESTAMP <- as.POSIXct(strptime(hobo_1016$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1016 <- left_join(hobo_1016, cat_sum, by = "TIMESTAMP")
temp_model_1016 <- na.omit(temp_model_1016)

temp_model_1016$TIMESTAMP <- as.POSIXct(strptime(temp_model_1016$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1016 <- temp_model_1016 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1016$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1016$Temp_C,
                               'catwalk' = temp_model_1016$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1016 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1016 <- colMeans(master_temp_chain_1016) ### Calculate the means of the parameter distribution
###########################################################################################

###23Oct19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1023 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1023.csv")
names(hobo_1023)[1] <- "TIMESTAMP"
hobo_1023$TIMESTAMP <- as.POSIXct(strptime(hobo_1023$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1023 <- left_join(hobo_1023, cat_sum, by = "TIMESTAMP")
temp_model_1023 <- na.omit(temp_model_1023)

temp_model_1023$TIMESTAMP <- as.POSIXct(strptime(temp_model_1023$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1023 <- temp_model_1023 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1023$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1023$Temp_C,
                               'catwalk' = temp_model_1023$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1023 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1023 <- colMeans(master_temp_chain_1023) ### Calculate the means of the parameter distribution
###########################################################################################

###30Oct19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1030 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1030.csv")
names(hobo_1030)[1] <- "TIMESTAMP"
hobo_1030$TIMESTAMP <- as.POSIXct(strptime(hobo_1030$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1030 <- left_join(hobo_1030, cat_sum, by = "TIMESTAMP")
temp_model_1030 <- na.omit(temp_model_1030)

temp_model_1030$TIMESTAMP <- as.POSIXct(strptime(temp_model_1030$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1030 <- temp_model_1030 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1030$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1030$Temp_C,
                               'catwalk' = temp_model_1030$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1030 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1030 <- colMeans(master_temp_chain_1030) ### Calculate the means of the parameter distribution
###########################################################################################

###07Nov19 Temperature JAGS model ###
#######################################################################################

### READ IN THE HOBO DATA
hobo_1107 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1107.csv")
names(hobo_1107)[1] <- "TIMESTAMP"
hobo_1107$TIMESTAMP <- as.POSIXct(strptime(hobo_1107$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1107 <- left_join(hobo_1107, cat_sum, by = "TIMESTAMP")
temp_model_1107 <- na.omit(temp_model_1107)

temp_model_1107$TIMESTAMP <- as.POSIXct(strptime(temp_model_1107$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1107 <- temp_model_1107 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1107$TIMESTAMP)
sink("jags_model.bug")

cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1107$Temp_C,
                               'catwalk' = temp_model_1107$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1107 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1107 <- colMeans(master_temp_chain_1107) ### Calculate the means of the parameter distribution
parameters_temp_1107 <- as.data.frame(master_temp_chain_1107[,1:3])
parameters_temp_1107$variable <- 1:nrow(parameters_temp_1107) 
parameters_temp_1107$full_time_day <- as.POSIXct("2019-11-07")
###########################################################################################

###20Nov19 Temperature JAGS model ###
#######################################################################################
### READ IN THE HOBO DATA
hobo_1120 <- read_csv("./input/DA_temp_scale_model/HOBO_CATWALK_MODEL_1120.csv")
names(hobo_1120)[1] <- "TIMESTAMP"
hobo_1120$TIMESTAMP <- as.POSIXct(strptime(hobo_1120$TIMESTAMP, '%Y-%m-%d %H:%M:%S', tz = 'EST'))

### BIND IT TO THE CATWALK DATA AND OMIT ALL NANS
temp_model_1120 <- left_join(hobo_1120, cat_sum, by = "TIMESTAMP")
temp_model_1120 <- na.omit(temp_model_1120)

temp_model_1120$TIMESTAMP <- as.POSIXct(strptime(temp_model_1120$TIMESTAMP, '%Y-%m-%d', tz = 'EST'))

temp_model_1120 <- temp_model_1120 %>% group_by(TIMESTAMP) %>% summarise_all(funs(mean))

N <- length(temp_model_1120$TIMESTAMP)
cat('model {
    for (i in 1:N) {
    temp[i] ~ dnorm(temp.hat[i], tau)
    temp.hat[i] <- beta[1]*catwalk[i] + beta[2]
    }
    
    #Vague priors on the beta
    for(j in 1:2){
    beta[j] ~ dnorm(0,1/10000)
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)

sink()
jags <- jags.model('jags_model.bug',
                   data = list('temp' = temp_model_1120$Temp_C,
                               'catwalk' = temp_model_1120$mean_ws_temp,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 5000)
samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 5000)

gelman.diag(samples) ### make sure the MCMC have converged

master_temp_chain_1120 <- combine.mcmc(samples) ### combine the chains

mean_pars_temp_1120 <- colMeans(master_temp_chain_1120) ### Calculate the means of the parameter distribution

parameters_temp_1120 <- as.data.frame(master_temp_chain_1120[,1:3])
parameters_temp_1120$variable <- 1:nrow(parameters_temp_1120) 
parameters_temp_1120$full_time_day <- as.POSIXct("2019-11-20")
###########################################################################################

parm_temp_all <- rbind(parameters_temp_1107,
                       parameters_temp_1120)



mean_temp_beta_1 <- parm_temp_all %>% select(`beta[1]`, full_time_day, variable) %>% group_by(full_time_day) %>% summarize_all(funs(mean))

ggplot(parm_temp_all, aes(full_time_day, `beta[1]`, group=as.character(variable))) + 
  geom_line(color = "grey80")+
  geom_line(data = mean_temp_beta_1, aes(full_time_day, `beta[1]`), color = "black", lwd = 3)+
  theme_classic()
