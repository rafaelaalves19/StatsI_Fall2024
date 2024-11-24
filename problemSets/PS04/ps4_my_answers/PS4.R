# installing packages and libraries
install.packages("car")    # car = Companion to Applied Regression
library(car)
data("Prestige")  
help(Prestige)           
head(Prestige)
View(Prestige)



######################################## Question 1 ########################################
############################################################################################

# (a) Create a new variable professional by recoding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).

# creating a new variable "professional" assigning 1 for 'prof' and 0 for 'wc' and 'bc'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)



# (b) Run a linear model with prestige as an outcome and income, professional, and the
# interaction of the two as predictors (Note: this is a continuous × dummy interaction.)

# in order to run a linear model with a dummy interaction as a predictor, 
# I created an interaction term called inter_1 (income * professional)
Prestige$inter_1 <- Prestige$income * Prestige$professional

# running the linear model including the interaction term:
linear_model1 <- lm(prestige ~ income + professional + inter_1, data=Prestige)
summary(linear_model1)


# (c) Write the prediction equation based on the result.
# prestige = 21.142 + 0.003 * income + 37.781 * professional - 0.002 * inter_1



# (d) Interpret the coefficient for income.
#The coefficient for income (0.003) represents the effect of income on prestige when the professional 
#is = 0 (blue-collar or white-collar workers).
#For every 1-dollar-unit increase in income, the prestige increases 0.003 for bc and wc.
# But when professional = 1, the effect of income on prestige changes because of the interaction term coefficient (- 0.002 for inter_1)
# The combined effect of income for professionals is: 0.003 − 0.002 = 0.001
# For bc and wc workers, each additional dollar in income increases prestige by 0.003.
# For professionals, each additional dollar in income increases prestige by 0.001



# (e) Interpret the coefficient for professional.
# Being a professional (professional = 1) adds 37.781 points to the prestige score compared to (blue-collar or white-collar), holding income constant (= 0).
# In another words, when income = 0, professionals have 37.781 points higher prestige than wc and bc.



#(f) What is the effect of a $1,000 increase in income on prestige score for professional occupations? 
# Calculate the change in ˆy associated with a $1,000 increase in income based on your answer for (c).
### For professionals (=1), each additional 1-unit-dollar in income increases prestige by 0.001 points.
# If we consider 1,000 dollars increase in income, we have: 1.000 * 0.001 = 1.
# Conclusion: $1,000 increase in income is associated with a 1 point increase in the prestige score.



# (g) What is the effect of changing one’s occupations from non-professional to professional when her income is $6,000? 
# We are interested in the marginal effect of professional jobs when the variable income takes the value of 6, 000. 
# Calculate the change in ˆy based on your answer for (c).

prestige = 21.142 + 0.003 * income + 37.781 * professional - 0.002 * inter_1

# when non-professional (= 0), the inter_1 = 0
# prestige (non-professional) = 21.142 + 0.003 * 6000  =  21.142 + 18 = 39.142

# when professional (= 1), we include inter_1
# prestige (professional) = 21.142 + 0.003 * 6000 +37.781 − 0.002 *(6000) = 21.142 + 18 + 37.781 − 12 = 64.923

# The marginal effect is the difference between the two prestige scores: 
# prestige (professional) - prestige (non-professional) = 64.923 - 39.142 = 25.781
# Conclusion: if we change the occupation from a non-professional to a professional when income is $6,000, it means 
# an increase of 25.781 points in the prestige score.



######################################## Question 2 ########################################
############################################################################################


# (a) ---------------------------------------------------------------------------------
# n = 131 / treatment + control group / McAuliffe X Cuccinelli
# dependent variable Y = votes on Cuccinelli
# first variable = precinct with signs / n = 30
# second variable = precincts adjacent to a precinct with sign


# Setting the hypotheses:
# Null Hypothesis: Yard signs have no effect on vote share
# Alternative Hypothesis: Yard signs affect vote share


# coefficient and standard error for the variable "Precinct assigned lawn signs":
coef_assnglawn <- 0.042      
se_assnglawn <- 0.016

# calculating t-statistic
t_stat <- coef_assnglawn / se_assnglawn
cat("t-statistic:", t_stat, "\n")

# calculating two-tailed p-value using the t-distribution and degrees of freedom
n <- 131     
predictors <- 2  
df <- n - predictors - 1  # degrees of freedom

p_value <- 2 * (1 - pt(abs(t_stat), df = df))
cat("p-value:", p_value, "\n")

# conclusion: 
# As the p-value is < 0.05, we can reject the null-hyp and affirm that lawn signs have a significant effect on vote share.


# (b) ---------------------------------------------------------------------------------

# Set the hypotheses:
# Null Hypothesis: being next to precincts with yard signs have no effect on vote share
# Alternative Hypothesis: being next to precincts with yard signs affect vote share

# coefficient and standard error for the variable "Precinct adjacent to lawn signs":
coef_adjacentlawn <- 0.042      
se_adjacentlawn <- 0.013

# calculating t-statistic
t_stat2 <- coef_adjacentlawn / se_adjacentlawn
cat("t-statistic:", t_stat2, "\n")

# calculating two-tailed p-value using the t-distribution and degrees of freedom
n <- 131     
predictors <- 2  
df <- n - predictors - 1  # degrees of freedom

p_value2 <- 2 * (1 - pt(abs(t_stat2), df = df))
cat("p-value:", p_value2, "\n")

# conclusion: 
# As the p-value is < 0.05, reject null hypothesis. 
# There is sufficient evidence to conclude that being in a precinct adjacent to lawn signs 
# significantly affects vote share at the 5% significance level


# (c) ---------------------------------------------------------------------------------

#The constant term (intercept) represents the predicted value of the dependent variable when all predictors are set to 0.
# Considering the regression: 'vote share = 0.302 + 0.042 * assigned signs + 0.042 * adjacent signs', we can conclude
# that the predicted vote share proportion for McAuliff’s opponent, Ken Cuccinelli, in a precinct with no lawn signs 
# (assigned or adjacent) is expected to be 30.2%.


# (d) ---------------------------------------------------------------------------------
# Considering the data that was given, we can use the R2 value (=0.094) to check if the model fits the regression.
# The R2 = 0.094 means that only 9.4% of the variation in vote share is explained by the presence of lawn signs and 
# adjacency to precincts with lawn signs. The remaining 90.6% (100% − 9.4%) of the variation in vote share 
# can be explained by other factors not included in the model (ex: campaign strategies, demographics issues, etc).
# Even though lawn signs and adjacency significantly affect vote share as we saw before, 
# their overall impact is small and they are not a major factor in determining vote share.








