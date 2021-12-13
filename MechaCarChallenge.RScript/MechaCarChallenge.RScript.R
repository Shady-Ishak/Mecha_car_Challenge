setwd("/Users/shadyishak/Desktop/Class.Folder/15R/challenge/MechaCarChallenge.RScript")
library(tidyverse)
library(dplyr)

#Import and read in the MechaCar_mpg.csv file as a dataframe.
MechaCar_mpg <- read_csv("MechaCar_mpg.csv")

names(MechaCar_mpg)

#Perform linear regression using the lm() function.
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg)

#Using the summary() function, determine the p-value and the r-squared value for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg))



###Deliverable 2: Create Visualizations for the Trip Analysis (30 points)
Suspension_Coil <- read_csv("Suspension_Coil.csv")
View(Suspension_Coil)

#creates a total_summary dataframe using the summarize() function to get the mean, median, variance, and standard deviation
total_summary <- Suspension_Coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

#creates a lot_summary dataframe
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

#Go deeper into Lot3
Lot3 <- subset(Suspension_Coil, Manufacturing_Lot %in% c("Lot3"))
ggplot(Lot3, aes(x=PSI)) + geom_density()
ggplot(Suspension_Coil, aes(x=Manufacturing_Lot, y=PSI)) + geom_boxplot()



###Deliverable 3: T-Tests on Suspension Coils

##A) test that compares all manufacturing lots against mean PSI of the population

#take a sample
total_sample <- Suspension_Coil %>% sample_n(50)

#compare sample mean to the population mean
t.test(total_sample$PSI, mu=mean(Suspension_Coil$PSI))


##B) Compare each manufacturing lot against mean PSI of the population

#create a sample for Lot1
lot1_sample <- Suspension_Coil %>% subset(Manufacturing_Lot=='Lot1') %>% sample_n(25)

#compare Lot1 sample mean to the population mean PSI
t.test(lot1_sample$PSI, mu=mean(Suspension_Coil$PSI))

#create a sample for Lot2
lot2_sample <- Suspension_Coil %>% subset(Manufacturing_Lot=='Lot2') %>% sample_n(25)

#compare Lot2 sample mean to the population mean PSI
t.test(lot2_sample$PSI, mu=mean(Suspension_Coil$PSI))

#create a sample for Lot3
lot3_sample <- Suspension_Coil %>% subset(Manufacturing_Lot=='Lot3') %>% sample_n(25)

#compare Lot2 sample mean to the population mean PSI
t.test(lot3_sample$PSI, mu=mean(Suspension_Coil$PSI))

