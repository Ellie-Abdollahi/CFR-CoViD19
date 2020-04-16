## Main run file for estimating CFR in Canada

rm(list = ls())
library(stats)
library(utils)
library(ggplot2)
library(lubridate)
library(HDInterval)
library(fitdistrplus)

### Loading Data ###

# path<-file.path("~","Documents", "CFR_Data_Canada.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# path<-file.path("~","Documents", "CFR_Data_US.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

path<-file.path("~","Documents", "CFR_Data_NY.csv")
my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

### Loading r estimate, which is the ML of r at each time ###

# path2<-file.path("~","Documents", "r_estimate_Canada.csv")
# my_r=read.table(path2, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# path2<-file.path("~","Documents", "r_estimate_NYC.csv")
# my_r=read.table(path2, header = TRUE, sep = ",", stringsAsFactors = FALSE)

path2<-file.path("~","Documents", "r_estimate_US.csv")
my_r=read.table(path2, header = TRUE, sep = ",", stringsAsFactors = FALSE)

r=my_r$r

t = length(my_data$Date)
nsim = 1000
pre.mu = 13.59
pre.sd = 7.85


results <- as.data.frame(matrix(NA, nrow = t, ncol = 6))
names(results) <- c("Time", "Date", "b_CFR", "U_CFR", "Low_U_CFR", "Up_U_CFR")

# Calculating Bias CFR (Crude CFR)

b_CFR <-function(cum.death,cum.case){
  b_CFR = cum.death/(cum.case)
  return(b_CFR)
}
for (i in 1:t){
  cum.death = my_data$cum.death[i]
  cum.case = my_data$cum.case [i]
  my_data$b_CFR[i] = b_CFR(cum.death, cum.case)
  results[i,3] = b_CFR(cum.death, cum.case)
  
}

#Estimating U based on gamma distribution Moment Generating Function

U <- function(nsim,r){
  U = array()
  
  for (i in 1:nsim){
    mu = rnorm(1,pre.mu,2) #Sampling from normal for mean of gamma
    sd = rnorm(1,pre.sd,1) #Sampling from normal for sd of gamma
    cv = sd/mu
    cv2 = cv^2
    U[i] = 1/((1+(r*mu*cv2))^(cv2))
  }
  return(U)
}

#Estimating U_CFR

U_est <- function (nsim){
  
  mass_U = matrix(data=NA, nrow= t, ncol = nsim)
  mass_U_CFR = matrix(data=NA, nrow= t, ncol = nsim)
  for (i in 1:t)
  {
    mass_U[i,] = U(nsim,r[i])
    mass_U_CFR[i, ] = (my_data$b_CFR[i])/mass_U[i,]
  }
  
  return(mass_U_CFR)
  
}




#building results
main_CFR<- function(nsim){
  U_est = U_est(nsim)
  
  for (i in 1:t) {
    
    results[i,1] <- i
    results[i,2] <- my_data$Date[i]
    results[i,3] = results[i,3]
    results[i,4] <- mean(U_est[i,])
    interval_CFR <- as.numeric(hdi(U_est[i,]))
    results[i,5] <- interval_CFR[1]
    results[i,6] <- interval_CFR[2]
  }
  return(results)
}

results = main_CFR(nsim)

# results$Date = as.Date(result$Date,format = "%m/%d/%y" )

# ggplot(data = results)+
# geom_line(data=results, aes(Time, b_CFR), color = 'red')+
# geom_line(data=results, aes(Time, U_CFR)) +labs(title = "")

write.table(results, file = "CFR_Canada_Confirmed.csv", sep=",")
# write.table(results, file = "CFR_NY_Confirmed.csv", sep=",")
# write.table(results, file = "CFR_US_Confirmed.csv", sep=",")


