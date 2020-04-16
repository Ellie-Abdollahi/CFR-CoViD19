## Main run file for estimating CFR in Canada - UNDERREPORT

rm(list = ls())
library(stats)
library(utils)
library(ggplot2)
library(lubridate)
library(HDInterval)
library(fitdistrplus)

# path<-file.path("~","Documents", "CFR_Data_Canada.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# path<-file.path("~","Documents", "CFR_Data_NY.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

path<-file.path("~","Documents", "CFR_Data_US.csv")
my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)


#Loading r estimate, which is the ML of r at each time
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


results <- as.data.frame(matrix(NA, nrow = t, ncol = 2))
names(results) <- c( "Date", "U_CFR")

# Calculating Bias CFR (Crude CFR)
b_CFR <-function(cum.death,cum.case,p){

  b_CFR = cum.death/((cum.case)/(1-p))
  return(b_CFR)
}


b_CFR_main <- function(){
  p = rbeta(1,10,4)
  my_b_CFR = array()
  for (i in 1:t){
  cum.death = my_data$cum.death[i]
  cum.case = my_data$cum.case [i]
  my_b_CFR[i] = b_CFR(cum.death, cum.case,p)
  }
return(my_b_CFR)
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
    mass_U_CFR[i, ] = (b_CFR_main()[i])/mass_U[i,]
  }
  
  return(mass_U_CFR)
  
}




#building results
main_CFR<- function(nsim){
  U_est = U_est(nsim)
  
  for (i in 1:t) {
    
    results[i,1] <- i
    results[i,2] <- mean(U_est[i,])
      }
  return(results)
}

output = matrix(data = NA, nrow = t, ncol = 1000)

for (j in 1:1000){
output[,j] = main_CFR(nsim)[[2]]
}

#fitting LogNormal to the last data point CFR
lnorm_fit <- fitdist(output[t,],"lnorm")

write.table(output, file = "UnderReport_US.csv", sep=",")
#write.table(output, file = "UnderReport_Canada.csv", sep=",")
#write.table(output, file = "UnderReport_NYC.csv", sep=",")
