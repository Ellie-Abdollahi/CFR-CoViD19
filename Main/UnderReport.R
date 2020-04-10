#Sampling from poisson(C/(1-p)) to generate data for under-reporting

rm(list = ls())
library(stats)
library(utils)


path<-file.path("~","Documents", "CFR_Data_NY.csv")
my_data=read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

#case: incidence at each day
#cum.case: cumulative incidence
#death: death at each day (We don't change death)
#cum.death: cumulative death

t = length(my_data$Date)
p = 0.5 #Probability of under-reporting

Mass.Case = matrix(data = NA, nrow = t, ncol = 500)
Mass.Cum.Case = matrix(data = NA, nrow = t, ncol = 500)

#Sampling from Poisson

for (i in 1:t){
  Mass.Case[i,] = rpois(500, (my_data$case[i]/(1-p)))
}
write.table(Mass.Case, file = "Mass_NYC.csv", sep=",")


#Calculating cumulatives for newly generated data set

Mass.Cum.Case[1,]=Mass.Case[1,]

for (i in 2:t){
  Mass.Cum.Case[i,] = Mass.Cum.Case[i-1,]+Mass.Case[i,]
}


#Save the output to use it later for CFR calculation
write.table(Mass.Cum.Case, file = "Mass_Cum_NYC.csv", sep=",")


#### This is code for generating C2 and C to use in likelihood function of r

# C= matrix(data = NA, nrow = t, ncol = 500)
# C2 = matrix(data = NA, nrow = t, ncol = 500)
# 
# for (i in 1:t){
#   for(j in 1:500){
#     C[i,j] =Mass.Cum.Case[i,j]
#       }
# }
# 
# for (i in 1:t){
#   for(j in 1:500){
#     C2[i,j] = sum((Mass.Cum.Case[1:(i-1),j]))
#   }
# }


