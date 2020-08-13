
### 27May19 forecast JAGS model ###
###########################################################################################
ebullition_527 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_527.csv")


N <- length(ebullition_527$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_527$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_527$log_ebu_mgCH4_m2_d_1,

                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags, n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples)
summary(samples)
#plot(samples)
master_chain_527 <- combine.mcmc(samples)
summary(master_chain_527)

curr_pars_527 <- sample(seq(0, nrow(master_chain_527)),1)
master_chain_527[curr_pars_527,]

mean_pars_527 <- colMeans(master_chain_527) 

parameters_ebu_527 <- as.data.frame(master_chain_527[,1:2])
parameters_ebu_527$variable <- 1:nrow(parameters_ebu_527) 
parameters_ebu_527$full_time_day <- as.POSIXct("2019-05-27")
###########################################################################################

### 03June19 forecast JAGS model ###
###########################################################################################
ebullition_603 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_603.csv")


N <- length(ebullition_603$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
    ebu.hat[i] <- ebu_lag[i]
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_603$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_603$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 5,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples)
summary(samples)
master_chain_603 <- combine.mcmc(samples)
summary(master_chain_603)
curr_pars_603 <- sample(seq(0, nrow(master_chain_603)),1)
master_chain_603[curr_pars_603,]

mean_pars_603 <- colMeans(master_chain_603)

parameters_ebu_603 <- as.data.frame(master_chain_603[,1:2])
parameters_ebu_603$variable <- 1:nrow(parameters_ebu_603) 
parameters_ebu_603$full_time_day <- as.POSIXct("2019-06-03")
###########################################################################################

### 10June19 AR TS JAGS model ###
###########################################################################################
ebullition_610 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_610.csv")

N <- length(ebullition_610$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_610$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_610$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples)
summary(samples)
master_chain_610 <- combine.mcmc(samples)
summary(master_chain_610)
curr_pars_610 <- sample(seq(0, nrow(master_chain_610)),1)
master_chain_610[curr_pars_610,]

mean_pars_610 <- colMeans(master_chain_610)
parameters_ebu_610 <- as.data.frame(master_chain_610[,1:2])
parameters_ebu_610$variable <- 1:nrow(parameters_ebu_610) 
parameters_ebu_610$full_time_day <- as.POSIXct("2019-06-10")
###########################################################################################

### 17June19 AR TS JAGS model ###
###########################################################################################
ebullition_617 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_617.csv")

N <- length(ebullition_617$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_617$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_617$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 50000)
gelman.diag(samples)
summary(samples)
master_chain_617 <- combine.mcmc(samples)
summary(master_chain_617)
curr_pars_617 <- sample(seq(0, nrow(master_chain_617)),1)
master_chain_617[curr_pars_617,]

mean_pars_617 <- colMeans(master_chain_617)
parameters_ebu_617 <- as.data.frame(master_chain_617[,1:2])
parameters_ebu_617$variable <- 1:nrow(parameters_ebu_617) 
parameters_ebu_617$full_time_day <- as.POSIXct("2019-06-17")
###########################################################################################

### 24June19 AR TS JAGS model ###
###########################################################################################
ebullition_624 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_624.csv")

N <- length(ebullition_624$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_624$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_624$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 
summary(samples)
master_chain_624 <- combine.mcmc(samples)
summary(master_chain_624)
curr_pars_624 <- sample(seq(0, nrow(master_chain_624)),1)
master_chain_624[curr_pars_624,]

mean_pars_624 <- colMeans(master_chain_624)

parameters_ebu_624 <- as.data.frame(master_chain_624[,1:2])
parameters_ebu_624$variable <- 1:nrow(parameters_ebu_624) 
parameters_ebu_624$full_time_day <- as.POSIXct("2019-06-24")
###########################################################################################

### 01July19 AR TS JAGS model ###
###########################################################################################
ebullition_701 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_701.csv")

N <- length(ebullition_701$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_701$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_701$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 30000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 100000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_701 <- combine.mcmc(samples)
summary(master_chain_701)
curr_pars_701 <- sample(seq(0, nrow(master_chain_701)),1)
master_chain_701[curr_pars_701,]

mean_pars_701 <- colMeans(master_chain_701)

parameters_ebu_701 <- as.data.frame(master_chain_701[,1:2])
parameters_ebu_701$variable <- 1:nrow(parameters_ebu_701) 
parameters_ebu_701$full_time_day <- as.POSIXct("2019-07-01")
###########################################################################################

### 08July19 AR TS JAGS model ###
###########################################################################################
ebullition_708 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_708.csv")

N <- length(ebullition_708$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_708$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_708$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_708 <- combine.mcmc(samples)
summary(master_chain_708)
curr_pars_708 <- sample(seq(0, nrow(master_chain_708)),1)
master_chain_708[curr_pars_708,]

mean_pars_708 <- colMeans(master_chain_708)

parameters_ebu_708 <- as.data.frame(master_chain_708[,1:2])
parameters_ebu_708$variable <- 1:nrow(parameters_ebu_708) 
parameters_ebu_708$full_time_day <- as.POSIXct("2019-07-08")
###########################################################################################

### 15July19 AR TS JAGS model ###
###########################################################################################
ebullition_715 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_715.csv")

N <- length(ebullition_715$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_715$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_715$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_715 <- combine.mcmc(samples)
summary(master_chain_715)
curr_pars_715 <- sample(seq(0, nrow(master_chain_715)),1)
master_chain_715[curr_pars_715,]

mean_pars_715 <- colMeans(master_chain_715)

parameters_ebu_715 <- as.data.frame(master_chain_715[,1:2])
parameters_ebu_715$variable <- 1:nrow(parameters_ebu_715) 
parameters_ebu_715$full_time_day <- as.POSIXct("2019-07-15")
###########################################################################################

### 22July19 AR TS JAGS model ###
###########################################################################################
ebullition_722 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_722.csv")

N <- length(ebullition_722$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_722$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_722$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_722 <- combine.mcmc(samples)
summary(master_chain_722)
curr_pars_722 <- sample(seq(0, nrow(master_chain_722)),1)
master_chain_722[curr_pars_722,]

mean_pars_722 <- colMeans(master_chain_722)

parameters_ebu_722 <- as.data.frame(master_chain_722[,1:2])
parameters_ebu_722$variable <- 1:nrow(parameters_ebu_722) 
parameters_ebu_722$full_time_day <- as.POSIXct("2019-07-22")
###########################################################################################

### 29July19 AR TS JAGS model ###
###########################################################################################
ebullition_729 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_729.csv")

N <- length(ebullition_729$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_729$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_729$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 50000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_729 <- combine.mcmc(samples)
summary(master_chain_729)
curr_pars_729 <- sample(seq(0, nrow(master_chain_729)),1)
master_chain_729[curr_pars_729,]

mean_pars_729 <- colMeans(master_chain_729)

parameters_ebu_729 <- as.data.frame(master_chain_729[,1:2])
parameters_ebu_729$variable <- 1:nrow(parameters_ebu_729) 
parameters_ebu_729$full_time_day <- as.POSIXct("2019-07-29")
###########################################################################################

### 05Aug19 AR TS JAGS model ###
###########################################################################################
ebullition_805 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_805.csv")

N <- length(ebullition_805$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_805$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_805$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 50000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_805 <- combine.mcmc(samples)
summary(master_chain_805)
curr_pars_805 <- sample(seq(0, nrow(master_chain_805)),1)
master_chain_805[curr_pars_805,]

mean_pars_805 <- colMeans(master_chain_805)

parameters_ebu_805 <- as.data.frame(master_chain_805[,1:2])
parameters_ebu_805$variable <- 1:nrow(parameters_ebu_805) 
parameters_ebu_805$full_time_day <- as.POSIXct("2019-08-05")
###########################################################################################

### 12Aug19 AR TS JAGS model ###
###########################################################################################
ebullition_812 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_812.csv")

N <- length(ebullition_812$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_812$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_812$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_812 <- combine.mcmc(samples)
summary(master_chain_812)
curr_pars_812 <- sample(seq(0, nrow(master_chain_812)),1)
master_chain_812[curr_pars_812,]

mean_pars_812 <- colMeans(master_chain_812)

parameters_ebu_812 <- as.data.frame(master_chain_812[,1:2])
parameters_ebu_812$variable <- 1:nrow(parameters_ebu_812) 
parameters_ebu_812$full_time_day <- as.POSIXct("2019-08-12")
###########################################################################################

### 19Aug19 AR TS JAGS model ###
###########################################################################################
ebullition_819 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_819.csv")

N <- length(ebullition_819$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_819$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_819$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_819 <- combine.mcmc(samples)
summary(master_chain_819)
curr_pars_819 <- sample(seq(0, nrow(master_chain_819)),1)
master_chain_819[curr_pars_819,]

mean_pars_819 <- colMeans(master_chain_819)

parameters_ebu_819 <- as.data.frame(master_chain_819[,1:2])
parameters_ebu_819$variable <- 1:nrow(parameters_ebu_819) 
parameters_ebu_819$full_time_day <- as.POSIXct("2019-08-19")
###########################################################################################

### 28Aug19 AR TS JAGS model ###
###########################################################################################
ebullition_828 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_828.csv")

N <- length(ebullition_828$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_828$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_828$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_828 <- combine.mcmc(samples)
summary(master_chain_828)
curr_pars_828 <- sample(seq(0, nrow(master_chain_828)),1)
master_chain_828[curr_pars_828,]

mean_pars_828 <- colMeans(master_chain_828)

parameters_ebu_828 <- as.data.frame(master_chain_828[,1:2])
parameters_ebu_828$variable <- 1:nrow(parameters_ebu_828) 
parameters_ebu_828$full_time_day <- as.POSIXct("2019-08-28")
###########################################################################################

### 02Sep19 AR TS JAGS model ###
###########################################################################################
ebullition_902 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_902.csv")

N <- length(ebullition_902$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_902$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_902$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_902 <- combine.mcmc(samples)
summary(master_chain_902)
curr_pars_902 <- sample(seq(0, nrow(master_chain_902)),1)
master_chain_902[curr_pars_902,]

mean_pars_902 <- colMeans(master_chain_902)

parameters_ebu_902 <- as.data.frame(master_chain_902[,1:2])
parameters_ebu_902$variable <- 1:nrow(parameters_ebu_902) 
parameters_ebu_902$full_time_day <- as.POSIXct("2019-09-02")
###########################################################################################

### 11Sep19 AR TS JAGS model ###
###########################################################################################
ebullition_911 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_911.csv")

N <- length(ebullition_911$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_911$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_911$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_911 <- combine.mcmc(samples)
summary(master_chain_911)
curr_pars_911 <- sample(seq(0, nrow(master_chain_911)),1)
master_chain_911[curr_pars_911,]

mean_pars_911 <- colMeans(master_chain_911)

parameters_ebu_911 <- as.data.frame(master_chain_911[,1:2])
parameters_ebu_911$variable <- 1:nrow(parameters_ebu_911) 
parameters_ebu_911$full_time_day <- as.POSIXct("2019-09-11")
###########################################################################################

### 20Sep19 AR TS JAGS model ###
###########################################################################################
ebullition_920 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_920.csv")

N <- length(ebullition_920$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_920$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_920$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_920 <- combine.mcmc(samples)
summary(master_chain_920)
curr_pars_920 <- sample(seq(0, nrow(master_chain_920)),1)
master_chain_920[curr_pars_920,]

mean_pars_920 <- colMeans(master_chain_920)

parameters_ebu_920 <- as.data.frame(master_chain_920[,1:2])
parameters_ebu_920$variable <- 1:nrow(parameters_ebu_920) 
parameters_ebu_920$full_time_day <- as.POSIXct("2019-09-20")
###########################################################################################

### 27Sep19 AR TS JAGS model ###
###########################################################################################
ebullition_927 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_927.csv")

N <- length(ebullition_927$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }
 
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_927$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_927$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_927 <- combine.mcmc(samples)
summary(master_chain_927)
curr_pars_927 <- sample(seq(0, nrow(master_chain_927)),1)
master_chain_927[curr_pars_927,]

mean_pars_927 <- colMeans(master_chain_927)

parameters_ebu_927 <- as.data.frame(master_chain_927[,1:2])
parameters_ebu_927$variable <- 1:nrow(parameters_ebu_927) 
parameters_ebu_927$full_time_day <- as.POSIXct("2019-09-27")
###########################################################################################

### 02Oct19 AR TS JAGS model ###
###########################################################################################
ebullition_1002 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1002.csv")

N <- length(ebullition_1002$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1002$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1002$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1002 <- combine.mcmc(samples)
summary(master_chain_1002)
curr_pars_1002 <- sample(seq(0, nrow(master_chain_1002)),1)
master_chain_1002[curr_pars_1002,]

mean_pars_1002 <- colMeans(master_chain_1002)

parameters_ebu_1002 <- as.data.frame(master_chain_1002[,1:2])
parameters_ebu_1002$variable <- 1:nrow(parameters_ebu_1002) 
parameters_ebu_1002$full_time_day <- as.POSIXct("2019-10-02")
###########################################################################################

### 11Oct19 AR TS JAGS model ###
###########################################################################################
ebullition_1011 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1011.csv")

N <- length(ebullition_1011$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }
    
    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1011$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1011$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 50000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1011 <- combine.mcmc(samples)
summary(master_chain_1011)
curr_pars_1011 <- sample(seq(0, nrow(master_chain_1011)),1)
master_chain_1011[curr_pars_1011,]

mean_pars_1011 <- colMeans(master_chain_1011)

parameters_ebu_1011 <- as.data.frame(master_chain_1011[,1:2])
parameters_ebu_1011$variable <- 1:nrow(parameters_ebu_1011) 
parameters_ebu_1011$full_time_day <- as.POSIXct("2019-10-11")
###########################################################################################

### 16Oct19 AR TS JAGS model ###
###########################################################################################
ebullition_1016 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1016.csv")

N <- length(ebullition_1016$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1016$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1016$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1016 <- combine.mcmc(samples)
summary(master_chain_1016)
curr_pars_1016 <- sample(seq(0, nrow(master_chain_1016)),1)
master_chain_1016[curr_pars_1016,]

mean_pars_1016 <- colMeans(master_chain_1016)

parameters_ebu_1016 <- as.data.frame(master_chain_1016[,1:2])
parameters_ebu_1016$variable <- 1:nrow(parameters_ebu_1016) 
parameters_ebu_1016$full_time_day <- as.POSIXct("2019-10-16")
###########################################################################################

### 23Oct19 AR TS JAGS model ###
###########################################################################################
ebullition_1023 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1023.csv")

N <- length(ebullition_1023$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1023$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1023$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1023 <- combine.mcmc(samples)
summary(master_chain_1023)
curr_pars_1023 <- sample(seq(0, nrow(master_chain_1023)),1)
master_chain_1023[curr_pars_1023,]

mean_pars_1023 <- colMeans(master_chain_1023)

parameters_ebu_1023 <- as.data.frame(master_chain_1023[,1:2])
parameters_ebu_1023$variable <- 1:nrow(parameters_ebu_1023) 
parameters_ebu_1023$full_time_day <- as.POSIXct("2019-10-23")
###########################################################################################

### 30Oct19 AR TS JAGS model ###
###########################################################################################
ebullition_1030 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1030.csv")

N <- length(ebullition_1030$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1030$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1030$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1030 <- combine.mcmc(samples)
summary(master_chain_1030)
curr_pars_1030 <- sample(seq(0, nrow(master_chain_1030)),1)
master_chain_1030[curr_pars_1030,]

mean_pars_1030 <- colMeans(master_chain_1030)

parameters_ebu_1030 <- as.data.frame(master_chain_1030[,1:2])
parameters_ebu_1030$variable <- 1:nrow(parameters_ebu_1030) 
parameters_ebu_1030$full_time_day <- as.POSIXct("2019-10-30")
###########################################################################################

### 07Nov19 AR TS JAGS model ###
###########################################################################################
ebullition_1107 <- read_csv("./input/DA_AR_forecast_model/EBU_JAGS_1107.csv")

N <- length(ebullition_1107$date)

sink("jags_model.bug")
cat('model {
    for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
ebu.hat[i] <- ebu_lag[i]
    }

    # Prior for the inverse variance
    sigma ~ dunif(0, 100) # standard deviation
    tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
    }'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = ebullition_1107$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = ebullition_1107$log_ebu_mgCH4_m2_d_1,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 10000)

samples = coda.samples(model = jags,
                       variable.names = c('sigma','tau'),
                       n.iter = 60000)
gelman.diag(samples) ### Check to make sure that the chains converge! 

master_chain_1107 <- combine.mcmc(samples)
summary(master_chain_1107)
curr_pars_1107 <- sample(seq(0, nrow(master_chain_1107)),1)
master_chain_1107[curr_pars_1107,]

mean_pars_1107 <- colMeans(master_chain_1107)

parameters_ebu_1107 <- as.data.frame(master_chain_1107[,1:2])
parameters_ebu_1107$variable <- 1:nrow(parameters_ebu_1107) 
parameters_ebu_1107$full_time_day <- as.POSIXct("2019-11-07")
###########################################################################################

