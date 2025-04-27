##############################################################################
## Title:         SPPS 225 - Linear Regression Example
## Programmer:    Mark Bounthavong
## Date:          27 April 2025
## Updated:       NA
## Updated by:    NA
##############################################################################

#### CLEAR ENVIRONMENT
rm(list=ls())		


#### LOAD LIBRARIES
library("ggplot2")
# install.packages("devtools")
library("devtools")
# install.packages("predict3d")
## Note: 02-11-2023: This has been removed from CRAN, so you need to use the following code to install predict3d:
## devtools::install_github("cardiomoon/predict3d") ## Make sure to have the devtools installed
library("predict3d")
# install.packages("psych")
library("psych")
# install.packages("magrittr")
# library("magrittr") ## allows for rounding using the %>%
library("dplyr")
# install.packages("gtsummary") ## Allows for publication ready tables
library("gtsummary")
# install.packages("DescTools")
library("DescTools") ## Needed for normality testing
# install.packages("nortest")
library("nortest") ## Needed for normality testing
# install.packages("lmtest")
library("lmtest") ## Need for heteroskedasticity testing
# install.packages("sandwich")
library("sandwich")  ## Needed for estimating Huber-White sandwich standard errors
# install.packages("broom")
library("broom")  ## Needed package to generate tidy tables
# install.packages("broom.helpers)
library("broom.helpers")  ## Needed to create regression coefficient tibbles


#### LOAD DATA
diabetes.data <- read.csv("https://raw.githubusercontent.com/mbounthavong/R-tutorials/refs/heads/main/Data/diabetes.csv")

#### VIEW DATA
knitr::kable(
  head(diabetes.data), caption = "Table 1. First six rows of the Pima Indians Diabetes Dataset"
)


knitr::kable(
  describeBy(diabetes.data) %>% round(2) 
)


### Plot the association between the subject's age and glucose level
ggplot(diabetes.data, aes(x = Age, y = Glucose)) +
  geom_point() +
  stat_smooth()


#### Linear regression model (Y = Glucose, X = Age)
linear.model1 <- lm(Glucose ~ Age, data = diabetes.data)
summary(linear.model1)

#### Generate the 95% CI
confint(linear.model1)


#### Present the output in a table
model1 <- tbl_regression(linear.model1, intercept = TRUE)
as_gt(model1) %>% 
  gt::tab_header("Table 2. Linear regression model output (Glucose ~ Age)") %>% 
  gt::tab_options(table.align='left')

#### PREDICTIONS PLOT
ggPredict(linear.model1, digits = 1, show.point = TRUE, se = TRUE, xpos = 0.5)

ggPredict(linear.model1, digits = 1, show.point = FALSE, se = TRUE, xpos = 0.5)



#### Generate groups based on pregnancy history (Group 0 = 0, Group 1 = 1 or more pregnancies)
diabetes.data$pregnancy.history[diabetes.data$Pregnancies == 0] = 0
diabetes.data$pregnancy.history[diabetes.data$Pregnancies > 0] = 1

table(diabetes.data$pregnancy.history)


#### Linear regression model (Y = Glucose, X1 = Age, X2 = Pregnancy History)
linear.model2 <- lm(Glucose ~ Age + pregnancy.history, data = diabetes.data)
summary(linear.model2)
confint(linear.model2)


#### Present the output in a table
model2 <- tbl_regression(linear.model2, intercept = TRUE)
as_gt(model2) %>% 
  gt::tab_header("Table 3. Linear regression model output with confounder (Glucose ~ Age + Pregnancy History)") %>% 
  gt::tab_options(table.align='left')

#### Merge the two linear regression model's outputs
model1 <- tbl_regression(linear.model1, intercept = TRUE)
model2 <- tbl_regression(linear.model2, intercept = TRUE)
table1 <- tbl_merge(tbls = list(model1, model2),
                    tab_spanner = c("**Model 1**", "**Model 2**"))
as_gt(table1) %>% 
  gt::tab_header("Table 4. Comparison between linear regression models [Model 1 (crude) v. Model 2 (adjusted)]") %>% 
  gt::tab_options(table.align='left')



#### PREDICT PLOTS
ggPredict(linear.model2, digits = 1, show.point = FALSE, se = TRUE, xpos = 0.5)



#### LINEAR REGRESSION with ROBUST STANDARD ERRORS
linear.model1 <- lm(Glucose ~ Age, data = diabetes.data)
summary(linear.model1)
confint(linear.model1)

#### Huber-White sandwich estimation
robust1 <- coeftest(linear.model1, vcov = vcovHC(linear.model1, type = "HC1"))
robust1
confint(robust1)

#### Set up the matrix
par(mfrow = c(1, 2))

#### Histogram of the residuals
hist(linear.model1$res)

#### QQ-plot of the residuals against the QQ line
qqnorm(linear.model1$res); qqline(linear.model1$res, col = "2", lwd = 1, lty = 1)

#### Test normality using Shapiro-Wilk's test
shapiro.test(linear.model1$res)

#### Test normality using Jarque Bera test
JarqueBeraTest(linear.model1$res, robust = FALSE) ### Does not use robust method

#### Test normality using the Kolmogorov-Smirnov test
lillie.test(linear.model1$res)










