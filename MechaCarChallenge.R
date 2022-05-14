#D1

#Read in dplyr
library(dplyr)

#Read in file
mechampg_table <- read.csv(file='mechacar_mpg.csv',check.names=F,stringsAsFactors = F)

# Linear regression for mpg
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechampg_table)

#Determine p-value and r-square value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechampg_table))

#D2
#Read in file
suspension_table <- read.csv(file='Suspension_coil.csv', check.names=F, stringsAsFactors = F)

#Total summary
total_summary <- suspension_table %>% summarize(Mean=mean(PSI), median=(PSI), Variance=var(PSI), SD=sd(PSI))

#lot summary
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')
#D3
#PSI T-test for all
t.test(suspension_table$PSI, mu = 1500)

#PSI t-test for each individual lot
#Lot 1
t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

#Lot 2
t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

#Lot 3
t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

