#Required libraries
library(dplyr)
library(ggplot2)
library(stats)
library(car)       
library(lmtest)    
library(nortest)   

# Load the dataset
data <- read.csv("test.csv")

# verifying the column names
print(colnames(data))

# the column names are renamed as it's easy to use
data <- data %>%
  rename(
    Dep_Delay = Departure.Delay.in.Minutes,
    Arr_Delay = Arrival.Delay.in.Minutes,
    Inf_Ent = Inflight.entertainment,
    Satisfaction = satisfaction
  )

# Preprocess Satisfaction - Converting to the binary factor
data$Satisfaction <- ifelse(data$Satisfaction == "satisfied", 1, 0)
data$Satisfaction <- as.factor(data$Satisfaction)

# Verify the Preprocessing
print(table(data$Satisfaction))  
# It Shows the distribution as 0 if it's neutral or dissatisfied and 1 if satisfied.

# A/B Testing 

# 1 t-test - Comparing Departure Delay 
ttest_res <- t.test(data$Dep_Delay ~ data$Satisfaction, var.equal = FALSE)
print(ttest_res)
# If p-value < 0.05 then departure Delay significantly differs b/w satisfaction groups.

# 2 Chi-squared test - Customer type vs Satisfaction
chisq_table <- table(data$Customer.Type, data$Satisfaction)
chtest_res <- chisq.test(chisq_table)
print(chtest_res)
# Interpretation - If p-value < 0.05, Customer Type is associated with Satisfaction.

# 3 ANOVA - Departure Delay across the Class
anova_res <- aov(Dep_Delay ~ Class, data = data)
print(summary(anova_res))
# If p-value < 0.05, Departure Delay differs and if >0.05 then there will be no significant different.

# Regression Analysis

# Logistic regression - considering multiple predictors
model <- glm(Satisfaction ~ Dep_Delay + Inf_Ent + Class + Age + Flight.Distance, 
             data = data, family = "binomial")
summary(model)

# Improving the model
step_mod <- step(model, direction = "backward")
summary(step_mod)
# It will remove all the non-significant variables to improve the model.

# Diagnostic Tests
# 1 Variance Inflation Factor(VIF)
vif_res <- vif(step_mod)
print(vif_res)
# VIF < 5 indicates that there are no severe multicollinearity.

# 2 Breusch-Pagan Test
bp_test <- bptest(step_mod)
print(bp_test)
# If p-value > 0.05 means no evidence of heteroskedasticity.
# As per the obtained result, heteroscedasticity is present.

# 3 RESET Test - Checking the  model specification
reset_test <- resettest(step_mod)
print(reset_test)
# If p-value > 0.05 then it indicates that model is correctly specified.
# Since the p-value is less than 0.05, the RESET test suggests that the model is likely misspecified. 
# This means that important variables might be missing, or the functional form of the model may not be appropriate.


# 4 Normality Test on residuals
resid <- residuals(step_mod, type = "deviance")
norm_test <- ad.test(resid)  
print(norm_test)
# If p-value > 0.05 - residuals are normally distributed.
# The data do not follow a normal distribution.

# Data Visualization 

# 1 Univariate chart - Histogram for Departure Delay
ggplot(data, aes(x = Dep_Delay)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Departure Delays", x = "Departure Delay (minutes)", y = "Count") +
  theme_minimal()
ggsave("Histogram_Departure_Delay.png")

# 2 Univariate chart - Bar plot Satisfaction
ggplot(data, aes(x = Satisfaction)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Satisfaction Distribution", x = "Satisfaction (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()
ggsave("Bar_Satisfaction.png")

# 3 Bivariate chart - Scatterplot of Departure and Arrival Delay by Satisfaction
ggplot(data, aes(x = Dep_Delay, y = Arr_Delay, color = Satisfaction)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Departure vs. Arrival Delay by Satisfaction", 
       x = "Departure Delay (minutes)", y = "Arrival Delay (minutes)") +
  theme_minimal()
ggsave("Scatterplot_Satisfaction.png")

# 4 Complex chart - Boxplot of Departure Delay by Class and Satisfaction
ggplot(data, aes(x = Class, y = Dep_Delay, fill = Satisfaction)) +
  geom_boxplot() +
  labs(title = "Departure Delay by Class and Satisfaction", 
       x = "Class", y = "Departure Delay (minutes)") +
  theme_minimal()
ggsave("Boxplot_Class_Satisfaction.png")


# Notes for Report
# The dataset identifies the main factors influencing passenger happiness:-
# 1) According to regression and t-test results, departure delays dramatically lower satisfaction.
# 2) According to the regression coefficient, in-flight entertainment increases customer satisfaction.
# 3) Delay experiences are impacted by class disparities (ANOVA and boxplot).
# Recommendations: Reduce wait times and improve entertainment to increase customer happiness.