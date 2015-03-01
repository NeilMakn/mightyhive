setwd("datatau/teamleada/mightyhive")
aband_data <- read.csv("Abandoned_Data_Seed.csv",header=T)
reserv_data <- read.csv("Reservation_Data_Seed.csv",header=T)

nrow(aband_data)

test_rows <- which(reserv_data$Test_Control == 'test')

#H1: The difference in reservations made is greater with
#re-targeted ads.

#H0: There is no observed difference in reservations
# made between re-targeted ads and non-retargeted ads.



#DATA CLEANING
#find matches of people in both data sets.
#define which was a test group, and which was a control group.
#determine changes in reservation between groups.


# thoughts:
#Identifying by name/lastname is difficult
#because of incomplete information.
#Using subject phone numbers is best option.
#Assumptions:
#Same person will call back on the same number
#

#remove duplicates. customers only need to call
#once. This also removes blanks, which are diffcult to match.

dupli_IncomingNumbers <- duplicated(aband_data$Incoming_Phone)
data_no_dupli_numbers <- aband_data[!dupli_IncomingNumbers,]
#double check there are only unique numbers
sum(duplicated(data_no_dupli_numbers$Incoming_Phone))

#separate test and control group in abandoned calls
#match each group to reservations table
#compare difference

test_cust_aband_rows <- which(data_no_dupli_numbers$Test_Control == 'test')
control_cust_aband_rows <- which(data_no_dupli_numbers$Test_Control == 'control' )

test_group <- data_no_dupli_numbers[test_cust_aband_rows,]
control_group <-data_no_dupli_numbers[control_cust_aband_rows,]

#match test group with reservation table
matched_test_group <- test_group$Incoming_Phone %in% reserv_data$Incoming_Phone

#match control group with reservation table
matched_control_group <- control_group$Incoming_Phone %in% reserv_data$Incoming_Phone

#do statistical analysis by hand then check with t.test()
test_prop <- sum(matched_test_group) / length(matched_test_group)
control_prop <- sum(matched_control_group) / length(matched_control_group)

test_stat <- test_prop - control_prop

pooled_prop <- (sum(matched_test_group) + sum(matched_control_group))/(length(matched_test_group)+length(matched_control_group))

SE <- sqrt(pooled_prop*(1-pooled_prop)*((1/length(matched_test_group))+(1/length(matched_control_group))))

z_score <- test_stat / SE

#z score is how many std deviations
#away from the mean
#can use normal distribution to determine
# probability of getting z-score more extreme
#than the one we observed

p_value <- pnorm(z_score,lower.tail = F)

#p_value is chance of getting this result in the data
#amount of risk set by alpha level, commonly at 5%
#Assuming 5% for this analysis
#the p-value is very low, and less than 5%. So we, can
#reject h0 on those terms.

#explain which test was chosen

t.test(matched_test_group, matched_control_group)
