#Maximum Likelihood to estimate r

rm(list = ls())
library(Stats)
library(utils)

# path<-file.path("~","Documents", "CFR_Data_US.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

path<-file.path("~","Documents", "CFR_Data_NY.csv")
my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Calculating cumulatives for NYC
#We use this part when number of incidence is available

# my_data$cum.case[1]=my_data$case[1]
# my_data$cum.death[1]=my_data$death[1]
# 
# 
# for (i in 2:length(my_data$Date)){
#   my_data$cum.case[i] = my_data$cum.case[i-1]+my_data$case[i]
#   my_data$cum.death[i] = my_data$cum.death[i-1]+my_data$death[i]
#  }



t = length(my_data$cum.case)
rmax = array()
C= array()
C2 = array()

for (i in 1:t){
  C[i] =my_data$cum.case[i]
}

for (i in 1:t){
  C2[i] = sum((my_data$cum.case[1:(i-1)]))
}


rmax = array()

#For each data point, substitute "t" with that day
ll <- function(r){
  
    ((-C2[t])*(1-exp(-r))+ (C[t]-C[1])*exp(-r))
}

rmax[t] <- uniroot(ll, lower = 0 , upper = 1)[[1]]

rmax[1]=0

# write.table(my_data, file = "CFR_Data_NY.csv", sep=",")
# write.table(rmax, file = "r_estimate_US.csv", sep = ",")
# write.table(rmax, file = "r_estimate_NYC.csv", sep = ",")
