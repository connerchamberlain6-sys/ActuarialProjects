#BODILY INJURY CLAIMS ANALYSIS PROJECT

#PART 1: EXPLORING DATA

#Here I read in the data from the file and
#check on the data types these loaded in as.

insurance_data<- read.csv("injury_claims.csv", stringsAsFactors = TRUE)
str(insurance_data)

#I check here to see if there are any missing values and notice there are none.
colSums(is.na(insurance_data))

#Here I run a summary on the data to see if there is anything that sticks out.
summary(insurance_data)

#I notice that Claims and BodilyInjury have minimums of 0. I check the 
#combinations of these 0's to see if there is anything to note. I remove these 
#once I am done checking them

NoBodilyInjury<-ifelse(insurance_data$BodilyInjury == 0, 0, 1)
NoClaims<- ifelse(insurance_data$Claims == 0, 0, 1)
table(NoBodilyInjury,NoClaims)

rm(NoBodilyInjury)
rm(NoClaims)

#There are 4070 instances of no claims or payments and 28 instances of no claims
#yet some payments. I will remove all of these cases. I will also remove
#the claims variable since the number of claims may not be known
#when predicting future bodily injury costs. I then run a summary to check my 
#updated information.

insurance_data <- insurance_data[insurance_data$BodilyInjury!=0,]
insurance_data <- insurance_data[insurance_data$Claims!=0,]
insurance_data$Claims <- NULL
summary(insurance_data)

#I now make Histograms specifically for BodilyInjury, Miles and Value. I 
#look to see if any sort of transformation should be done to these
#distributions to be added to the dataframe.

library(ggplot2)

ggplot(insurance_data, aes(x = BodilyInjury)) +
  geom_histogram()

ggplot(insurance_data, aes(x = log(BodilyInjury))) +
  geom_histogram()

#The BodilyInjury histogram was right-skewed so I took a look at the natural log
#of that data which made it more normally distributed.

ggplot(insurance_data, aes(x = Miles)) +
  geom_histogram()

#The histogram for Miles looks normally distributed so I leave that data alone.

ggplot(insurance_data, aes(x = Value)) +
  geom_histogram()

ggplot(insurance_data, aes(x = log(Value))) +
  geom_histogram()


#Much like BodilyInjury, the Value data was right skewed and checking the
#log made it more normally distributed.

#I decide to add the natural log of BodilyInjury and Value to my data in new columns
#based off of these histograms. Checking the summary again, I note that the 
#ranges of my other columns seem reasonable so no adjustments are needed

insurance_data$logBodilyInjury <- log(insurance_data$BodilyInjury)
insurance_data$logValue <- log(insurance_data$Value)
summary(insurance_data)


#PART 2: EXPLORE RELATIONSHIPS TO BODILY INJURY COSTS

#Here I compare predictor variables versus the natural log of Bodily Injury. I
#use boxplots except for Value and Age since those have so many unique values.

ggplot(insurance_data, aes(x = factor(Year), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = Credit, y =logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(Sex), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(MaritalStatus), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(Miles), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(YearsOwned), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(CarAge), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = factor(Drivers), y = logBodilyInjury)) +
  geom_boxplot()

ggplot(insurance_data, aes(x = Age, y = logBodilyInjury)) +
  geom_point()

ggplot(insurance_data, aes(x = logValue, y = logBodilyInjury)) +
  geom_point()

#I notice that their seems to be no real change in the natural log of 
#BodilyInjury no matter which predictor variable you use with it.

#Now I check to see the mean per group for each variable to see if there
#is anything of note.
library(dplyr)

insurance_data %>%
  group_by(Year) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(Credit) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(Sex) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(MaritalStatus) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(Miles) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(YearsOwned) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(CarAge) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data %>%
  group_by(Drivers) %>%
  summarise(mean = mean(BodilyInjury), n = n())

#In order to check Value and Age I put them into bins.

insurance_data$CutValue <- cut(insurance_data$logValue, 10)
insurance_data$CutAge <- cut(insurance_data$Age, 10)
 
insurance_data %>%
  group_by(CutValue) %>%
  summarise(mean = mean(BodilyInjury))

insurance_data %>%
  group_by(CutAge) %>%
  summarise(mean = mean(BodilyInjury), n = n())

insurance_data$CutValue <- NULL
insurance_data$CutAge <- NULL

#When it comes to Year, there appears to be a bit of an upward trend as the 
#years get bigger so I will keep this variable.

#When it comes to Credit, there is a very slight upward trend from A to H. To 
#make this more readable, I will reassign it as 1-8 instead of A-H.

#There is a clear difference in mean cost for Males compared to Females,
#so we will keep the Gender variable.

#There is a difference in mean based on Marital Status so we will keep this
#variable.

#Costs rise as Years Owned, Car Age and number of Drivers increase, so we will
#keep these variables.

#As miles increase, cost decreases so we will keep this variable.

#No clear pattern is seen based off of the value of the car, so we will
#remove this variable.

#Age has an interesting pattern where it starts very high, then it drops and 
#ends up growing from there. I will investigate this data by individual age and 
#see if there is any way I could possibly split this data.

Mean_By_Age <- insurance_data %>%
  group_by(Age) %>%
  summarise(mean = mean(BodilyInjury)) %>%
  as.data.frame()

Mean_By_Age

ggplot(Mean_By_Age, aes(x = Age, y = mean)) +
  geom_point()

#There is a strong linear decrease until 27, then increases at a lower rate 
#linearly. I will split this into two groups of LowAge for under 27 or 27 
#exactly, and HighAge for above 27 or 27 exactly. I will also remove the Value 
#variable as mentioned earlier.

insurance_data$Value <- NULL
insurance_data$logValue <- NULL
insurance_data$LowAge <- ifelse(insurance_data$Age < 27, insurance_data$Age, 27)
insurance_data$HighAge <- ifelse(insurance_data$Age > 27, insurance_data$Age, 27)
summary(lm(BodilyInjury ~ LowAge + HighAge, insurance_data))


#The given slopes seem consistent with the plot of all ages put together.

#I will now numerize the Credit column.

insurance_data$NumericCredit <- as.numeric(insurance_data$Credit)
insurance_data$Credit <- NULL
summary(insurance_data)

#PART 3: DECIDING ON A MODELING METHOD

#When deciding what type of model to use between a Decision Tree and 
#GLM, there are a few things that I look at to weigh my options.

# 1. Feature selection: Decision trees automatically select variables through 
#their splitting process, while GLMs require the analyst to evaluate variable 
#significance through statistical testing.

# 2. Linear relationship possibilities: GLMs can efficiently model linear 
#relationships between predictors and the outcome. Decision trees may require 
#multiple splits to approximate these relationships, potentially increasing 
#model complexity and overfitting risk.

# 3. Nonlinear relationship possibilities: Decision trees naturally capture 
#nonlinear relationships, while GLMs may require transformations or additional 
#terms to accommodate them.

# 4. Interpretability: Decision trees allow us to understand combinations and 
#interactions between variables through their splitting rules. GLMs use 
#coefficients to explain the relationships between individual predictors and the 
#outcome.

# 5. Overfitting risk: Decision trees can become overfitted when they create too 
#many splits or overly specific groups. GLMs can also overfit when too many 
#unnecessary predictors or complex terms are included.

#I chose to proceed with a GLM because several of the numeric predictors
#showed identifiable relationships with BodilyInjury during exploratory
#data analysis. A GLM can efficiently model these relationships through
#coefficients without requiring numerous splits like a decision tree
#might. Additionally, the nonlinear relationship observed with Age can
#be accommodated through separate variables (HighAge and LowAge).
#GLMs also allow for statistical testing to evaluate predictor
#significance and provide interpretable results through their coefficients.
#For these reasons, a GLM appears to be an appropriate starting point
#for modeling BodilyInjury.

#PART 4: FITTING THE BEST MODEL

#After deciding to go with a GLM, there are a few options to consider:

# 1. Normal distribution with identity link (standard regression):
#This is a solid starting point to potentially build off of.

# 2. Normal distribution with a log link: This allows us to have only positive
#predictions, but we may not have an accurate distribution.

# 3. Gamma distribution with a log link: This helps account for skewness.

#I will split the data into training, testing, and holdout.

library(caret)
set.seed(1000)
train_indices <- createDataPartition(insurance_data$BodilyInjury, p = .7, list = FALSE)
insurance_data_train <- insurance_data[train_indices,]
insurance_data_test <- insurance_data[-train_indices,]

test_indices <- createDataPartition(insurance_data_test$BodilyInjury, p = 2/3, list = FALSE)
insurance_data_hold <- insurance_data[-test_indices,]
insurance_data_test <- insurance_data[test_indices,]

rm(train_indices)
rm(test_indices)

#MODEL 1: Normal identity
#The only data not used is logBodilyInjury since we are predicting 
#Bodily Injury, and Age is not used since we split it into the LowAge
#and HighAge

GLM1 <- glm(BodilyInjury ~ . - logBodilyInjury - Age, data = insurance_data_train, family = gaussian(link = "identity"))
d <- drop1(GLM1, test = "LRT")
d
d[which.max(d[, 5]), ]

#NumericCredit should be dropped

GLM1 <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit, data = insurance_data_train, family = gaussian(link = "identity"))
d <- drop1(GLM1, test = "LRT")
d
d[which.max(d[, 5]), ]

#YearsOwned should be dropped

GLM1 <- glm(BodilyInjury ~ . -NumericCredit - logBodilyInjury - Age -YearsOwned, data = insurance_data_train, family = gaussian(link = "identity"))
d <- drop1(GLM1, test = "LRT")
d
d[which.max(d[, 5]), ]

#Everything else is statistically significant. I will now test this model.
#I use RMSE to evaluate this test.

prediction <- predict(GLM1, newdata = insurance_data_test, type = "response")
sqrt(sum((prediction-insurance_data_test$BodilyInjury)^2)/ nrow(insurance_data_test))
sqrt(mean(insurance_data_test$BodilyInjury^2))

#The RMSE is better than the baseline.

ggplot() +
  geom_point(aes(x = GLM1$fitted.values, y = residuals(GLM1)))

plot_data <- data.frame(residuals = residuals(GLM1), fitted = GLM1$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()
grid.arrange(p1, p2, ncol = 2)


#The plots show that this is not the correct model as it veers way off of the 
#desired results.

#MODEL 2: Normal, log
GLM2 <- glm(BodilyInjury ~ . - logBodilyInjury - Age, data = insurance_data_train, family = gaussian(link = "log"))
d <- drop1(GLM2, test = "LRT")
d
d[which.max(d[, 5]), ]

#Need to drop NumericCredir.

GLM2 <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit, data = insurance_data_train, family = gaussian(link = "log"))
d <- drop1(GLM2, test = "LRT")
d
d[which.max(d[, 5]), ]

#Need to drop YearsOwned.

GLM2 <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit - YearsOwned, data = insurance_data_train, family = gaussian(link = "log"))
d <- drop1(GLM2, test = "LRT")
d
d[which.max(d[, 5]), ]

#Everything remaining appears statistically significant.

prediction <- predict(GLM2, newdata = insurance_data_test, type = "response")
sqrt(sum((prediction - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

ggplot() +
  geom_point(aes(x = GLM2$fitted.values, y = residuals(GLM2)))

plot_data <- data.frame(residuals = residuals(GLM2), fitted = GLM2$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()

grid.arrange(p1, p2, ncol = 2)

#Still not quite what we are looking for.

#MODEL 3: Gamma, log

GLM3 <- glm(BodilyInjury ~ . - logBodilyInjury - Age, data = insurance_data_train, family = Gamma(link = "log"))
d <- drop1(GLM3, test = "LRT")
d
d[which.max(d[, 5]), ]

#Once again, NumericCredit goes first.

GLM3 <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit, data = insurance_data_train, family = Gamma(link = "log"))
d <- drop1(GLM3, test = "LRT")
d
d[which.max(d[, 5]), ]

#YearsOwned needs to go.

GLM3 <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit - YearsOwned, data = insurance_data_train, family = Gamma(link = "log"))
d <- drop1(GLM3, test = "LRT")
d
d[which.max(d[, 5]), ]

#All remaining measures appear statistically significant.

prediction <- predict(GLM3, newdata = insurance_data_test, type = "response")
sqrt(sum((prediction - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

ggplot() +
  geom_point(aes(x = GLM3$fitted.values, y = residuals(GLM3)))

plot_data <- data.frame(residuals = residuals(GLM3), fitted = GLM3$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()
grid.arrange(p1, p2, ncol = 2)

#Overall closer than the last 2 but still some skewness, so we will see if there 
#are any improvements that can be made with a different model.

#MODEL 4: Inverse Gaussian, Log
#All previous versions have needed NumericCredit and YearsOwned dropped so I 
#will begin with those already dropped.
GLM4 <- glm(BodilyInjury ~ . - logBodilyInjury - Age -NumericCredit - YearsOwned, data = insurance_data_train, family = inverse.gaussian(link = "log"))
d <- drop1(GLM4, test = "LRT")
d
d[which.max(d[, 5]), ]

#Everything appears statistically significant.

prediction <- predict(GLM4, newdata = insurance_data_test, type = "response")
sqrt(sum((prediction - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

ggplot() +
  geom_point(aes(x = GLM4$fitted.values, y = residuals(GLM4)))

plot_data <- data.frame(residuals = residuals(GLM4), fitted = GLM4$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()
grid.arrange(p1, p2, ncol = 2)

#This is so far our closest model.

#MODEL 5: Normal, identity, but predicting the log

GLM5 <- lm(logBodilyInjury ~ . - BodilyInjury - Age, data = insurance_data_train)
d <- drop1(GLM5, test = "Chisq") 
d
d[which.max(d[, 5]), ]

# Drop NumericCredit

GLM5 <- lm(logBodilyInjury ~ . - BodilyInjury - Age - NumericCredit, data = insurance_data_train)
d <- drop1(GLM5, test = "Chisq")
d
d[which.max(d[, 5]), ]

# Drop YearsOwned

GLM5 <- lm(logBodilyInjury ~ . - BodilyInjury - Age - NumericCredit - YearsOwned, data = insurance_data_train)
d <- drop1(GLM5, test = "Chisq")
d
d[which.max(d[, 5]), ]

#All variables appear very statistically significant

#Diagnostics - Note that the predictions are now the logarithm of the target, 
#so predicted values need to be exponentiated with the variance added (recall 
#that we are essentially assuming a lognormal distribution, so the mean is 
#exp(mu + 0.5sigmasq)). The diagnostics should be on the model as fit.

prediction <- exp(predict(GLM5, newdata = insurance_data_test) + 0.5 * (summary(GLM5)$sigma)^2)
sqrt(sum((prediction - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

ggplot() +
  geom_point(aes(x = GLM5$fitted.values, y = residuals(GLM5)))

plot_data <- data.frame(residuals = residuals(GLM5), fitted = GLM5$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()
grid.arrange(p1, p2, ncol = 2)

#While this looks generally solid as well, the last test (Inverse Gaussian)
#has a better RMSE and was also fitted well, making it the winner.

#Within the Inverse Gaussian (GLM4) model I will check for interactions.

add1(GLM4, ~ .^2, test = "LRT")

#The most significant pairing appears to be LowAge and Sex. This implies that
#the relationship between LowAge And the BodilyInjury costs varies depending
#on if the person is male or female. We will add this interaction to the 
#model.

GLM4_int <- glm(BodilyInjury ~ . - logBodilyInjury - Age - NumericCredit - YearsOwned + Sex:LowAge, data = insurance_data_train, family = inverse.gaussian(link = "log"))

prediction <- predict(GLM4_int, newdata = insurance_data_test, type = "response")
sqrt(sum((prediction - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

ggplot() +
  geom_point(aes(x = GLM4_int$fitted.values, y = residuals(GLM4_int)))

plot_data <- data.frame(residuals = residuals(GLM4_int), fitted = GLM4_int$fitted.values)

library(gridExtra)

p1 <- ggplot(data = plot_data, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()
p2 <- ggplot(data = plot_data, aes(residuals, after_stat(density))) +
  geom_histogram()
grid.arrange(p1, p2, ncol = 2)


#Finally I will check the measure of error using the holdout data.

prediction_hold <- predict(GLM4_int, newdata = insurance_data_hold, type = "response")
sqrt(sum((prediction_hold - insurance_data_hold$BodilyInjury)^2) / nrow(insurance_data_hold))

#This is roughly around the other values I have gotten.

#PART 5: FIT THE OTHER MODEL

#Now I will look at this with a Tree.

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree_BodilyInjury <- rpart(BodilyInjury ~ . - logBodilyInjury - HighAge - LowAge,
                           data = insurance_data_test,
                           method = "anova",
                           control = rpart.control(cp = 0, maxdepth = 10)
)
tree_BodilyInjury_pruned <- prune(tree_BodilyInjury, cp = tree_BodilyInjury$cptable[which.min(tree_BodilyInjury$cptable[, "xerror"]), "CP"])
rpart.plot(tree_BodilyInjury_pruned)

prediction_test <- predict(tree_BodilyInjury_pruned, newdata = insurance_data_test)
sqrt(sum((prediction_test - insurance_data_test$BodilyInjury)^2) / nrow(insurance_data_test))

#My best GLM model was GLM4 with the Inverse Gaussian model. This model
#used Year, Sex, MaritalStatus, Miles, CarAge, Drivers, LowAge, and HighAge.
#The Decision Tree had a lower RMSE by 10 and used Age, CarAge, Sex and Drivers.
#There is not enough of a difference to consider switching to the Decision
#Tree. There is an argument for the Decision Tree with less features but
#I will stick with the GLM.

# Task 7 - Interpret the model

summary(GLM4_int)
