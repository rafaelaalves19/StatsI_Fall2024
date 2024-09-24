#####################
# Problem 1
#####################


#A school counselor was curious about the average of IQ of the students in her school and
#took a random sample of 25 students’ IQ scores. The following is the data set:
# y <− c ( 1 0 5 , 6 9 , 8 6 , 1 0 0 , 8 2 , 1 1 1 , 1 0 4 , 1 1 0 , 8 7 , 1 0 8 , 8 7 , 
#9 0 , 9 4 , 1 1 3 , 1 1 2 , 9 8 , 8 0 , 9 7 , 9 5 , 1 1 1 , 1 1 4 , 8 9 , 9 5 , 1 2 6 , 9 8 )

# 1. Find a 90% confidence interval for the average student IQ in the school.


# data set:
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)


# mean and standard deviation
mean_y <- mean(y)
sd_y <- sd(y)


# 90% confidence interval using t.test
confidence_interval <- t.test(y, conf.level = 0.90)
confidence_interval$conf.int




# 2. Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α = 0.05.


# data set:
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)


# one-sample t-test:
t_test_result <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.05)
t_test_result


# Conclusion:
# Because the p-value is 0.7215 and it's greater than 0.05, I do not have enough evidence 
# to reject the null hypothesis that average student IQ in the school is equal to the 
# national average of 100 IQ score.


