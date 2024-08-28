# Author : Taraka Paruchuru
# Data Source : https://www.kaggle.com/datasets/thebumpkin/auto-insurance-claims-updated-to-2024/data
# About DataSource: The Dataset is about auto insurance claims, Monthly insurance Premiums, Gender, Income, and Education, along with policy-specific data like Coverage, Policy type, Vehicle Class, state, location...etc
# Research Questions: accuracy in Predicting the Claim amounts, factors influencing Monthly Premium Amounts, examine the relationship between coverage type and other factors

# Install all the requried Libraries
#Libraries
library(readr) # Function to read the files
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(DescTools) #VIF Function
library(leaps) #Best Set Linear Regression Functions


#Set Working Directory
getwd() # to look into your current working dir
setwd("/Users/tarakaparuchuru/Downloads") # set the current working dir to path required
# data <- read.csv("AutoInsuranceClaims2024.csv", header=TRUE, sep=",") # Load the Data or CSV file


data <- read.csv("AutoInsuranceClaims2024_missing.csv", header=TRUE, sep=",")
missing_values = sum(is.na(data)) #missing values are 'AAA' and '999' not NA or null
data <- read.csv(file = "AutoInsuranceClaims2024_missing.csv",header=TRUE, sep=",", na = c("999", "AAA"))
missing_values = sum(is.na(data))

# # Handling missing values
# 
#Treat Missing Values
# 
#Listwise Deletion
 data <- na.omit(data) # remove all rows in the data if any cell contain any NA (missing) values.
# 
missing_values = sum(is.na(data))

#Check Sample Size and Number of Variables
dim(data)
#9134-Sample Size and 34 variables

View(data) # view data as a table

#Show for first 6 rows of data
head(data)

#Show last 6 rows of data
tail(data)

#Show the structure of the data

str(data) # look into all variables into the dataset, data type...


#Show names of the variables and column numbers of variables

names(data) 


###########################################################

#Show column Numbers

library(psych)
describe(data) # to see the descriptive statistiscs  like mean, median, sd, mad(mean abosolute deviation), trimmed after trimmig extreme values

# Here we are focusing on 2 variables as target 1) Monthly.Premium with number '#' included to the model 2)Total.Claim.Amount with 'a' included

model1 = lm(Monthly.Premium.Auto  ~ ., data=data)
summary(model1) # Model to predict Monthly.Premium

modela = lm(Total.Claim.Amount  ~ ., data=data)
summary(modela) # Model to predict Claim.Amount

# problem 1 the lm() considers each unique customer as a categorical variable,R is creating a dummy variable for each customer. 
#Categorical variable like Customer in a regression model, R automatically creates dummy variables (one for each level of the categorical variable)
#resulting in more predictors than observations

# #(Intercept)                     10667.35        NaN     NaN      NaN
# CustomerAA11235                 -7199.41        NaN     NaN      NaN
# CustomerAA16582                 21904.78        NaN     NaN      NaN

# Problem 2: Overfitting and Perfect Multicollinearity:
#Including a categorical variable with a large number of unique values can cause perfect multicollinearity, where the predictors are perfectly correlated with each other
# When all dummy variables for a categorical variable are included, it can lead to perfect multicollinearity (also known as the dummy variable trap)

# Solution - excude customerID as its just and identifier

# # Problem, still the Model shows a lot 'NA's which means multicollinearity as verified, we see "Index" variable
#Index seem to correspond to their categorical counterparts (e.g., Coverage.Index, Education.Index, etc.). If these indices are perfectly correlated with the categorical variables they represent
# Vehicle.Size.L                 -4.260e+02  2.443e+02  -1.744 0.081195 .  
# Vehicle.Size.Q                 -9.890e+01  1.666e+02  -0.594 0.552860    
# Vehicle.Size.Index1                    NA         NA      NA       NA    
# Vehicle.Size.Index2                    NA         NA      NA       NA  

# Solution - excude Index col's 

# Issue is resolved for CustomerID,Index but still some categorical variables cause 'NA' ,Date which is char create too many dummy variables:

# Coverage.Index                         NA         NA      NA       NA    
# EducationCollege                1.049e+01  2.322e+02   0.045 0.963983    
# EducationDoctor                -2.538e+02  4.923e+02  -0.516 0.606185    
# EducationHigh School or Below   5.047e+02  2.349e+02   2.149 0.031682 *  
#   EducationMaster                 1.277e+02  3.563e+02   0.358 0.719983    
# Education.Index                        NA         NA      NA       NA  
# Effective.To.Date1/13/11       -1.868e+03  9.943e+02  -1.879 0.060323 .  
# Effective.To.Date1/14/11       -1.704e+01  9.836e+02  -0.017 0.986180    
# Effective.To.Date1/15/11       -9.855e+02  9.795e+02  -1.006 0.314336   


# Covert Effective Date into a numeric format as Days.Since.Start and remove Effective.To.Date
data$Effective.To.Date <- as.Date(data$Effective.To.Date, format="%m/%d/%Y") # first convert into date object
data$Days.Since.Start <- as.numeric(data$Effective.To.Date - min(data$Effective.To.Date)) # to number days
str(data) # we don't use factors are they create too many unique dates treating as categories

model2 <- lm(Monthly.Premium.Auto ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(model2)

modela2 = lm(Total.Claim.Amount ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(modela)


################################
# Scale conversion of numeric data, we do this for graphicaly presenting the Beta C0efficients of variables 

# Separate numeric and non-numeric columns
numeric_columns <- sapply(data, is.numeric)
non_numeric_columns <- !numeric_columns

# Scale only the numeric columns
scaled_numeric_data <- as.data.frame(scale(data[, numeric_columns]))

# Keep the non-numeric columns as they are
non_numeric_data <- data[, non_numeric_columns]

# Recombine the scaled numeric data with the non-numeric data
data <- cbind(scaled_numeric_data, non_numeric_data)

# View the structure of the scaled data
str(data)

################################

# Incase if you still see NA's run the below code to convert Categorical to factors, esle ingore Model will convert them


# Factor Conversion is done on categorical vairabales, ensuring that each level of the factor is treated as a distinct category 
data$Vehicle.Size.Index  <- as.factor(data$Vehicle.Size.Index)
data$State <- as.factor(data$State)
data$Response <- as.factor(data$Response)
data$Employment.Status <- as.factor(data$Employment.Status)
data$Gender <- as.factor(data$Gender)
data$Location <- as.factor(data$Location)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Policy.Type <- as.factor(data$Policy.Type)
data$Policy <- as.factor(data$Policy)
data$Sales.Channel <- as.factor(data$Sales.Channel)
data$Vehicle.Class <- as.factor(data$Vehicle.Class)


# use unique value to get values of each column to add in levels or factor order
unique_values <- lapply(data, unique)
print(unique_values)


# Factor Conversion for ordered factor, where order is to be maintained
data$Coverage <- factor(data$Coverage, levels = c("Basic", "Extended", "Premium"), ordered = TRUE)
data$Education <- factor(data$Education, levels = c("High School or Below", "College", "Bachelor", "Master", "Doctor"), ordered = TRUE)
data$Vehicle.Size <- factor(data$Vehicle.Size, levels = c("Small", "Medsize", "Large"), ordered = TRUE)


# To check Multicollinearity

library(car)
vif(model2)
vif(modela2)
# We got aliased coefficients
# Error in vif.default(model4) : 
#   there are aliased coefficients in the model

alias(model2) #function to show you which coefficients are aliased (i.e., perfectly collinear).
#occur when there is perfect multicollinearity in the model. This means that one or more predictor variables in your model can be expressed as an exact linear combination of other variables
#the regression model cannot uniquely estimate the coefficients for these variables, leading to "aliased" coefficients.

# solution :You may need to remove or combine these variables to resolve the issue. 
# here looking into the alias the columns of left PolicyPersonal L3,PolicySpecial L3 are being removed

# PolicyCorporate L3 PolicyPersonal L1 PolicyPersonal L2 PolicySpecial L1
# PolicyPersonal L3  0                 -1                -1                 0              
# PolicySpecial L3   0                  0                 0                -1      

str(data)
model3 <- lm(Monthly.Premium.Auto ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(model3)

str(data)
model3a <- lm(Total.Claim.Amount ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(model3a)

# Still use with NA's `PolicyPersonal L3` - `PolicySpecial L3` are the culprits, confirmed with VIF and alias
# so we have to exclude the levels from Policy factor variable of data
data <- subset(data, !Policy %in% c("Personal L3", "Special L3","Personal L2","Special L2"))

vif(model3)
alias(model3) 
vif(model3a)
alias(model3a) 

# mutiple levels of Policy are causing alias (correlating ) so exluded individually but, finally removed Policy from out model4
model4 <- lm(Monthly.Premium.Auto ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index -Policy - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(model4)
model4a <- lm(Total.Claim.Amount ~ . - Customer - Effective.To.Date - Coverage.Index - Education.Index - Employment.Status.Index - Location.Index - Marital.Status.Index - Policy.Type.Index - Policy.Index -Policy - Sales.Channel.Index - Vehicle.Class.Index - Vehicle.Size.Index, data = data)
summary(model4a)

vif(model4) #GVIF (Generalized Variance Inflation Factor): Used when the predictor variable is a categorical variable with multiple levels.
alias(model4) 
vif(model4a) #GVIF (Generalized Variance Inflation Factor): Used when the predictor variable is a categorical variable with multiple levels.
alias(model4a) 
# here we have high corelation above 10 , so we need to consider them
# Monthly.Premium.Auto          27.112649
# Vehicle.Class                 22.457398 

# These indicate high multicollinearity , but we can't remove them due to importance of those variables, so we use rgaulirization techniques to handles those.


# Finally we are left with somany variables, with least significance looking into the global values and local values.

library(dplyr) # subset from dplyr
# Remove specific columns by name using the 'subset()' function:
clean_data <- subset(data, select = -c(Customer, Effective.To.Date, Coverage.Index, Education.Index, 
                                       Employment.Status.Index, Location.Index, Marital.Status.Index, 
                                       Policy.Type.Index, Policy.Index, Policy, 
                                       Sales.Channel.Index, Vehicle.Class.Index, Vehicle.Size.Index))

str(clean_data)

model5 <- lm(Monthly.Premium.Auto ~ . , data = clean_data)
summary(model5)

model5a <- lm(Total.Claim.Amount ~ . , data = clean_data)
summary(model5a)


# Now we have cleaned the data, so we can check the assumptions of linear regression, though here we pefrom multiple regression later we go with mutivariate analysis

# Linearity: Linear regression is based on the assumption that your model is linear
# Independence:The observations should be independent of each other. This means that the value of one observation should not influence the value of another
# Homoscedasticity: The variance of the residuals (the errors between the predicted and actual values) should be constant across all levels of the independent variables
# Normality of residual:The residuals should be normally distributed. This assumption is particularly important for hypothesis testing and confidence interval construction.Outliers can significantly affect the normality assumption. 
# No Multicollinearity:The independent variables should not be highly correlated with each other

# Plotting for Monthly.Premium.Auto
pairs(clean_data[, c("Monthly.Premium.Auto", "Customer.Lifetime.Value", "Income", 
                     "Months.Since.Last.Claim", "Months.Since.Policy.Inception", 
                     "Number.of.Open.Complaints", "Number.of.Policies")])

# Plotting for Total.Claim.Amount
pairs(clean_data[, c("Total.Claim.Amount", "Customer.Lifetime.Value", "Income", 
                     "Months.Since.Last.Claim", "Months.Since.Policy.Inception", 
                     "Number.of.Open.Complaints", "Number.of.Policies")])

# Fit the models
model11 <- lm(Monthly.Premium.Auto ~ . - Total.Claim.Amount, data = clean_data)
model22 <- lm(Total.Claim.Amount ~ . - Monthly.Premium.Auto, data = clean_data)


#1. Linearity - Residual Plots
# Plot Residuals vs. Fitted Values for both models side by side
# fitted values are predicted values, residuals are diff btw observed and predicted values.
par(mfrow = c(1, 2))  # Set up a 1x2 plotting area 1 row ,2 cloumns 

plot(model11$fitted.values, residuals(model11), #extract fitted and residuals from model
     main = "Residuals vs Fitted for Monthly.Premium.Auto", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red") #adds a horizontal line at y = 0. in red color

# here the plot shows the pattern of clusters in a specific band, the monthly premium might not be linear with dependent varaibles

plot(model22$fitted.values, residuals(model22), 
     main = "Residuals vs Fitted for Total.Claim.Amount", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# here the plot has some canonical shape which non linear for Total Claim amount and other Predictors


#2. Independence - Durbin-Watson Test
# Ranging between 0 and 4. A value of 2.0 indicates there is no autocorrelation detected in the sample. Values from 0 to less than 2 point to positive autocorrelation and values from 2 to 4 means negative autocorrelation
# The p-value associated with the test helps determine the significance of the autocorrelation > 0.05 not significant, <0.05 significant
library(lmtest)
dwtest(model11) # For Monthly.Premium.Auto, no auto correlation
dwtest(model22) # For Total.Claim.Amount, no auto correlation

#This means that the residuals are likely independent, which is a key assumption for the validity of linear regression models

#3.Homoscedasticity - Ensure that the residuals have constant variance - Breusch-Pagan Test: Use this statistical test to formally test for heteroscedasticity.
library(lmtest)
# Breusch-Pagan Test
bptest(model11) # For Monthly.Premium.Auto
bptest(model22) # For Total.Claim.Amount

#4. Normality of Residuals 

# Q-Q Plot
qqnorm(residuals(model1), main = "Q-Q Plot for Monthly.Premium.Auto")
qqline(residuals(model1), col = "red")

qqnorm(residuals(model22), main = "Q-Q Plot for Total.Claim.Amount")
qqline(residuals(model22), col = "red")

# Shapiro-Wilk Test
shapiro.test(residuals(model11)) # For Monthly.Premium.Auto
shapiro.test(residuals(model22)) # For Total.Claim.Amount

#5. Multicollinearity

library(car)
# Calculate VIF
vif(model11) # For Monthly.Premium.Auto
vif(model22) # For Total.Claim.Amount

# significant variables selection

# Extract the coefficients and their summary for model11 and model22
coef_summary11 <- summary(model11)$coefficients
coef_summary22 <- summary(model22)$coefficients

# Filter out only significant coefficients (p-value < 0.05) for model11
significant_coef11 <- coef_summary11[coef_summary11[, 4] < 0.05, ]

# Filter out only significant coefficients (p-value < 0.05) for model22
significant_coef22 <- coef_summary22[coef_summary22[, 4] < 0.05, ]

# View the significant coefficients
significant_coef11 # significant coefficients for Monthly Premium
significant_coef22 # significant coefficients for Total Claim Amount

# Convert significant coefficients of Monthly Premium to a data frame for plotting 
df_significant11 <- data.frame(
  Predictor = rownames(significant_coef11),
  Coefficient = significant_coef11[, 1]
)

# Print the data frame to ensure correctness
print(df_significant11)


# Convert significant coefficients of Total Claim Amount to a data frame for plotting of
df_significant22 <- data.frame(
  Predictor = rownames(significant_coef22),
  Coefficient = significant_coef22[, 1]
)

# Print the data frame to ensure correctness
print(df_significant22)

# Plot the bar plot using ggplot2
ggplot(df_significant11, aes(x = Coefficient, y = reorder(Predictor, Coefficient), fill = Coefficient > 0)) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Impact of Significant Predictors on Monthly Auto Premium',
    x = 'Beta Coefficient Value',
    y = 'Predictor variable'
  ) +
  scale_fill_manual(values = c("red", "green"), 
                    name = "Direction", 
                    labels = c("Below 0", "Above 0")) +
  theme(
    axis.title = element_text(size = 12), 
    plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), 
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 49, hjust = 1)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip()  # Flip to horizontal bars



ggplot(df_significant22, aes(x = Coefficient, y = reorder(Predictor, Coefficient), fill = Coefficient > 0)) +
  geom_bar(stat = 'identity') +
  labs(
    title = 'Impact of Significant Predictors on Monthly Auto Premium',
    x = 'Beta Coefficient Value',
    y = 'Predictor variable'
  ) +
  scale_fill_manual(values = c("red", "green"), 
                    name = "Direction", 
                    labels = c("Below 0", "Above 0")) +
  theme(
    axis.title = element_text(size = 12), 
    plot.title = element_text(size = 12, hjust = 0.5,face = "bold"), 
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 49, hjust = 1)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip()  # Flip to horizontal bars




# plot the heatmap for correlation on numeric data
numeric_data <- clean_data[sapply(clean_data, is.numeric)] # filter the numeric data for co-relation

# Cor plot
library(corrplot)
corrplot(M, method = "square")
M<-cor(numeric_data, method="spearman")
M

library(GGally) 
#GGplot Correlation
ggcorr(clean_data)


#corrplot for numerical co-relation for numerical variables only.
corrplot(cor(numeric_data,method="spearman"), method = "number")

str(data)


#No heave corelations so we proceed to Automatic Regression Models 
#Creating Automatic Models


# null model has only the constant y-intercept included, no predictors yet
# note we have clean_data dataset which has only filtered variables from fullset ,  based on above operations
null = lm(Monthly.Premium.Auto ~ 1, data=clean_data)
null
# full model has all x variables included.
full = lm(Monthly.Premium.Auto ~ ., data=clean_data)
full


#Forward Regression
train_Forward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(train_Forward)

#Backward Regression
train_Backward = step(full, direction="backward")
summary(train_Backward)


#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
#options(scipen = 999)
options(scipen = 0)# to get standard numeric notation
summary(train_Step)


#Best Sets Linear Regressions
library(leaps)
# regsubsets inbuilt function finds the top  variables that  best fit in lm models based on parameters such as R-sqaure, AIC .. scores
exactpriceSubsets = regsubsets(Monthly.Premium.Auto ~ ., data=clean_data, nbest=10) #nbest tells how many of the "best" models to build and see
exactpriceSubsets

# Result: 10 model subsets of each size up to 8 variables, Selection Algorithm: exhaustive

#plotting the exactpriceSubsets 
plot(exactpriceSubsets, scale="adjr2") # Adjusted R-squared 

plot(exactpriceSubsets, scale="r2") # R-squared 

str(clean_data)

# Load necessary libraries
library(ggplot2)

# Coefficients and corresponding variables based on the earlier regression summary
coefficients <- c(
  'Customer.Lifetime.Value' = 1.378e-04,
  'Coverage.L' = 4.328e+01,
  'Coverage.Q' = 3.436e+00,
  'Income' = 1.342e-05,
  'LocationSuburban' = -4.393e+00,
  'LocationUrban' = -2.878e+00,
  'Total.Claim.Amount' = 1.000e-02,
  'Vehicle.ClassLuxury Car' = 1.632e+02,
  'Vehicle.ClassLuxury SUV' = 1.691e+02,
  'Vehicle.ClassSports Car' = 5.680e+01,
  'Vehicle.ClassSUV' = 5.496e+01
)

# Convert to a data frame for easier plotting
df <- data.frame(
  Predictor = names(coefficients),
  Coefficient = coefficients
)

# Plotting the bar plot using ggplot2
ggplot(df, aes(x = Coefficient, y = reorder(Predictor, Coefficient))) +
  geom_bar(stat = 'identity', fill = 'skyblue') +
  labs(
    title = 'Impact of Significant Predictors on Monthly Auto Premium',
    x = 'Coefficient Value',
    y = 'Predictor'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip()  # Flip to horizontal bars



############ Plot 2################

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your data is stored in a dataframe called `data`
# If the dataset is named differently, replace `data` with the actual dataset name

# Plot 1: Average Monthly Premium by Coverage
p1 <- ggplot(data, aes(x = Coverage, y = Monthly.Premium.Auto)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(title = "Average Monthly Premium by Coverage") +
  theme_minimal()

# Plot 2: Average Monthly Premium by Vehicle Class
p2 <- ggplot(data, aes(x = Vehicle.Class, y = Monthly.Premium.Auto)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(title = "Average Monthly Premium by Vehicle Class") +
  theme_minimal()

# Plot 3: Average Monthly Premium by Location
p3 <- ggplot(data, aes(x = Location, y = Monthly.Premium.Auto)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(title = "Average Monthly Premium by Location") +
  theme_minimal()

# Plot 4: Relationship between Total Claim Amount and Monthly Premium
p4 <- ggplot(data, aes(x = Total.Claim.Amount, y = Monthly.Premium.Auto)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Monthly Premium vs Total Claim Amount") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

