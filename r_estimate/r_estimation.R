#Maximum Likelihood to estimate r (Hiroshi Nishiura et al. 2009)

rm(list = ls())
library(stats)
library(utils)


path<-file.path("~","Documents", "CFR_Data_Canada.csv")
my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# path<-file.path("~","Documents", "CFR_Data_US.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# path<-file.path("~","Documents", "CFR_Data_NY.csv")
# my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)



t = length(my_data$Date)

C= array()
C2 = array()

for (i in 1:t){
  C[i] =my_data$cum.case[i]
}

for (i in 1:t){
  C2[i] = sum((my_data$cum.case[1:(i-1)]))
}


rmax = array() #Some of them should be set as zero manually, the ML function throw error for them

#For each data point, substitute "t" with that day

ll <- function(r){
  
    ((-C2[t])*(1-exp(-r))+ (C[t]-C[1])*exp(-r))
}

rmax[t] <- uniroot(ll, lower = 0 , upper = 1)[[1]] 

#We will use this vector in the main function, when estimating CFR at each time

write.table(rmax, file = "r_estimate_Canada.csv", sep = ",")
#write.table(rmax, file = "r_estimate_US.csv", sep = ",")
#write.table(rmax, file = "r_estimate_NYC.csv", sep = ",")
