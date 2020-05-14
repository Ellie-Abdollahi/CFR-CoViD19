load_data <- function(region) {
    data.folder = '../Data'
    data.file   = paste0("CFR_Data_",region,".csv")
    path        = file.path(data.folder, data.file)
    my_data     = read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    # Deal with US date format:
    if(region != 'Canada'){
        my_data$Date = lubridate::mdy(my_data$Date)
    }
    
    return(my_data)
}

load_r <- function(region) {
    r.folder = '../r_estimate'
    r.file   = paste0("r_estimate_",region,".csv")   # NYC, Canada
    path2    = file.path(r.folder, r.file)
    my_r     = read.table(path2, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    return(my_r$r)
}


b_CFR <-function(cum.death,cum.case){
    x = cum.death / cum.case
    return(x)
}


# Estimating U based on gamma distribution Moment Generating Function
U <- function(nsim, r, pre.mu, pre.sd){
    x = array()
    for (i in 1:nsim){
        mu = rnorm(1, pre.mu, 2) #Sampling from normal for mean of gamma
        sd = rnorm(1, pre.sd, 1) #Sampling from normal for sd of gamma
        cv = sd/mu
        cv2 = cv^2
        x[i] = 1/((1+(r*mu*cv2))^(cv2))
    }
    return(x)
}

# Estimating U_CFR

U_est <- function (nsim, my_data, r, pre.mu, pre.sd){
    
    t = nrow(my_data)
    
    mass_U     = matrix(data=NA, nrow= t, ncol = nsim)
    mass_U_CFR = matrix(data=NA, nrow= t, ncol = nsim)
    for (i in 1:t)
    {
        mass_U[i,]      = U(nsim, r[i], pre.mu, pre.sd)
        mass_U_CFR[i, ] = my_data$b_CFR[i] / mass_U[i,]
    }
    
    return(mass_U_CFR)
}


# Calculating Bias CFR (Crude CFR)

calc_adj_CFR<- function(nsim, my_data, r, pre.mu, pre.sd ){
    
    t = nrow(my_data)
    
    # Setting up the dataframe for results:
    results <- as.data.frame(matrix(NA, nrow = t, ncol = 6))
    names(results) <- c("Time", "Date", "b_CFR", "U_CFR", "Low_U_CFR", "Up_U_CFR")
    
    my_data$b_CFR = b_CFR(my_data$cum.death, my_data$cum.case)
    U.est = U_est(nsim, my_data, r, pre.mu, pre.sd)
    
    results$Time  = 1:t
    results$Date  = lubridate::dmy(my_data$Date)
    results$b_CFR = b_CFR(my_data$cum.death, my_data$cum.case)
    results$U_CFR = apply(U.est, MARGIN = 1, FUN = mean) 
    
    for (i in 1:t) {
        interval_CFR         <- as.numeric(hdi(U.est[i,]))
        results$Low_U_CFR[i] <- interval_CFR[1]
        results$Up_U_CFR[i]  <- interval_CFR[2]
    }
    return(results)
}


plot_results <- function(res, region) {
    g = ggplot(data = res)+
        geom_line(data=res, aes(Time, b_CFR), color = 'red')+
        geom_line(data=res, aes(Time, U_CFR)) +labs(title = "") + 
        ggtitle(region)
    plot(g)
}

