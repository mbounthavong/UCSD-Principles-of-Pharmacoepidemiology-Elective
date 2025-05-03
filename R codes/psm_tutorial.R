##############################################################################
## Title:         Propensity Score Matching Tutorial in R
## Programmer:    Mark Bounthavong
## Date:          25 February 2025
## Updated:       03 May 2025
## Updated by:    Mark Bounthavong
##############################################################################


## Resources:
# URL: https://kosukeimai.github.io/MatchIt/
# URL: https://evalsp21.classes.andrewheiss.com/example/matching-ipw/
# URL: https://livefreeordichotomize.com/posts/2019-01-17-understanding-propensity-score-weighting/
# URL: https://www.franciscoyira.com/post/matching-in-r-3-propensity-score-iptw/
# URL: https://ngreifer.github.io/WeightIt/articles/WeightIt.html



## Clear environment
rm(list=ls())				


## Load libraries
if (!require("pacman")) install.packages("pacman"); library("pacman")

p_load("MEPS",
       "tidyverse",
       "MatchIt",
       "cobalt",
       "broom",
       "modelsummary",
       "sandwich")

### Load data
### Load 2021 Full Year Consolidated Data File
hc2021 = read_MEPS(file = "h233")
names(hc2021) <- tolower(names(hc2021))   ## Convert column headers to lower case


## View first fix rows
head(hc2021)


## Rename columns / Reduce data to only a few variables
hc2021p = hc2021 %>%
  rename(
    age = age21x, 
    totexp = totexp21,
    ertexp = ertexp21,
    diabetes = diabdx_m18,
    race = racev1x,
    poverty = povcat21,
    marital_status = marry21x) %>%
  select(
    dupersid, 
    age,
    sex,
    race,
    poverty,
    diabetes,
    marital_status,
    totexp,
    ertexp)
hc2021p$year <- 2021



## Subset data for adults (age >= 18 years)
hc2021p <- subset(hc2021p, age >= 18)
summary(hc2021p$age)



## Subset data for adults with marital status information
hc2021p <- subset(hc2021p, marital_status >= 1)
table(hc2021p$marital_status)


## Label values for variables
table(hc2021p$sex)
hc2021$sex <- factor(hc2021$sex, levels = c(1, 2), labels = c("Male", "Female"))

table(hc2021p$race)
hc2021p$race <- factor(hc2021p$race, levels = c(1, 2, 3, 4, 6), labels = c("White", "Black", "AI/AN", "Asian", "Multiple"))

table(hc2021p$poverty)
hc2021p$poverty <- factor(hc2021p$poverty, levels = c(1, 2, 3, 4, 5), labels = c("Poor", "Near Poor", "Low Income", "Middle Income", "High Income"))

table(hc2021p$marital_status)
hc2021p$marital_status <- factor(hc2021p$marital_status, levels = c(1, 2, 3, 4, 5), labels = c("Married", "Widowed", "Divorced", "Separated", "Never Married"))


## Identify respondents with diabetes
table(hc2021p$diabetes)

## Keep respondents with diabetes indicator for 1 = Yes and 2 = No.
hc2021p <- hc2021p[hc2021p$diabetes %in% c(1, 2), ]

## Change diabetes indicator to 1 = Yes, 0 = No.
hc2021p$diabetes[hc2021p$diabetes == 1] = 1
hc2021p$diabetes[hc2021p$diabetes == 2] = 0
table(hc2021p$diabetes)
hc2021p$diabetes_new <- factor(hc2021p$diabetes, levels = c(0, 1), labels = c("No", "Yes"))



##########################################################
## Propensity Score Matching - Nearest Neighbor Approach
##########################################################
## Propensity score matching
match1 <- matchit(diabetes ~ age + sex + race + poverty + marital_status,
                  data = hc2021p,
                  method = "nearest",
                  discard = "both",
                  caliper = 0.01)


## Inspect matching
summary(match1)

## Visual inspection using Love plot
#### Method 1: 
love.plot(match1, 
          stats = c("m", "ks"), ### m = mean difference; ks = Kolmogorov-Smirnov
          thresholds = c(m = 0.1, ks = 0.05), 
          drop.distance = TRUE, 
          colors = c("dodgerblue4", "firebrick"),
          shapes = c(16, 17),
          stars = "none")

#### Method 2:
match1.out <- summary(match1, standardize = TRUE)
plot(match1.out, 
     var.order = "unmatched", 
     abs = FALSE)



### Convert to data
match_data1 <- match.data(match1)
head(match_data1)



##########################################################
## Propensity Score Matching - Mahalanobis
##########################################################
## Propensity score matching
match2 <- matchit(diabetes ~ age + sex + race + poverty + marital_status,
                  data = hc2021p,
                  distance = "mahalanobis",
                  replace = TRUE)


## Inspect matching
summary(match2)

## Visual inspection using Love plot
#### Method 1: 
love.plot(match2, 
          stats = c("m", "ks"), ### m = mean difference; ks = Kolmogorov-Smirnov
          thresholds = c(m = 0.1, ks = 0.05), 
          drop.distance = TRUE, 
          colors = c("dodgerblue4", "firebrick"),
          shapes = c(16, 17),
          stars = "none")

#### Method 2:
match2.out <- summary(match2, standardize = TRUE)
plot(match2.out, 
     var.order = "unmatched", 
     abs = FALSE)



### Convert to data
match_data2 <- match.data(match1)
head(match_data2)




##########################################################
## Propensity Score Matching - IPW
##########################################################
### Construct a logistic regression model
logit1 <- glm(diabetes ~ age + sex + race + poverty + marital_status, 
              data = hc2021p,
              family = "binomial"(link = "logit"))
summary(logit1)

## Method 1: Using augment_columns
prob_fitted <- augment_columns(logit1, 
                               data = hc2021p,
                               type.predict = "response") %>%
                rename(propensity_score = .fitted)

## Inspect propensity scores (propensity_score)
summary(prob_fitted$propensity_score)


## Method 2: Using mutate
prob_fitted2 <- hc2021p %>%
    mutate(
        propensity_score =  glm(diabetes ~ age + sex + race + poverty + marital_status, 
                                data = hc2021p,
                                family = "binomial"(link = "logit")) %>%
          predict(type = "response")
          )

## Inspect propensity scores (propensity_score)
summary(prob_fitted2$propensity_score)

## Visualize the propensity scores between the diabetes and no-diabetes groups
ps_fitted <- prob_fitted2 %>%
    tidyr::spread(diabetes, propensity_score, sep = "_")

ggplot(ps_fitted) + 
  geom_histogram(bins = 50, aes(diabetes_0)) + 
  geom_histogram(bins = 50, aes(x = diabetes_1, y = -after_stat(count))) + 
  ylab("count") + xlab("p") +
  geom_hline(yintercept = 0, lwd = 0.5) +
  scale_y_continuous(label = abs) 



##########################################################
## Calculate the inverse probability weight (IPW)
##########################################################

##############
#### ATE IPW
##############
ate_fitted <- prob_fitted2 %>%
    mutate(
        ipw_ate = (diabetes / propensity_score) + ((1 - diabetes) / (1 - propensity_score))
          )

## Visualize the propensity scores between the diabetes and no-diabetes groups
ps_ate <- ate_fitted %>%
  tidyr::spread(diabetes, propensity_score, sep = "_")

ggplot(ps_ate) +
  geom_histogram(bins = 50, aes(diabetes_1), alpha = 0.5) + 
  geom_histogram(bins = 50, aes(diabetes_1, weight = ipw_ate), fill = "green", alpha = 0.5) + 
  geom_histogram(bins = 50, alpha = 0.5, aes(x = diabetes_0, y = -..count..)) + 
  geom_histogram(bins = 50, aes(x = diabetes_0, weight = ipw_ate, y = -..count..), fill = "blue", alpha = 0.5) + 
  ylab("count") + xlab("p") +
  geom_hline(yintercept = 0, lwd = 0.5) +
  scale_y_continuous(label = abs) 


##############
#### ATT IPW
##############
att_fitted <- prob_fitted2 %>%
  mutate(
    ipw_att = ((propensity_score * diabetes) / propensity_score) + ((propensity_score * (1 - diabetes)) / (1 - propensity_score))
        )

## Visualize the propensity scores between the diabetes and no-diabetes groups
ps_att <- att_fitted %>%
  tidyr::spread(diabetes, propensity_score, sep = "_")

ggplot(ps_att) +
  geom_histogram(bins = 50, aes(diabetes_1), alpha = 0.5) + 
  geom_histogram(bins = 50, aes(diabetes_1, weight = ipw_att), fill = "green", alpha = 0.5) + 
  geom_histogram(bins = 50, alpha = 0.5, aes(x = diabetes_0, y = -..count..)) + 
  geom_histogram(bins = 50, aes(x = diabetes_0, weight = ipw_att, y = -..count..), fill = "blue", alpha = 0.5) + 
  ylab("count") + xlab("p") +
  geom_hline(yintercept = 0, lwd = 0.5) +
  scale_y_continuous(label = abs) 



#### Compare the models

## No matching
model0 <- glm(totexp ~ diabetes,
              data = hc2021p)
summary(model0)


## Nearest neighbor matching
model1 <- glm(totexp ~ diabetes,
              data = match_data1)
summary(model1)


model2 <- glm(totexp ~ diabetes,
              data = match_data2)
summary(model2)



## IPW with ATE
model3 <- glm(totexp ~ diabetes, 
              data = ate_fitted, 
              weight = ipw_ate)
summary(model3)


## IPW with ATT
model4 <- glm(totexp ~ diabetes, 
              data = att_fitted, 
              weight = ipw_att)
summary(model4)


#### Comparing the models together
model_list <- list("No matching" = model0,
                   "Nearest neighbor matching" = model1,
                   "Mahalanobis with replacement" = model2,
                   "IPW wih ATE" = model3,
                   "IPW with ATT" = model4)

modelsummary1 <- modelsummary(model_list,
             stars = TRUE,
             gof_omit = ".*IC",
             fmt = fmt_decimal(digits = 0, pdigits = 3),
             statistic = NULL,
             conf_level = 0.95,
             vcov = "robust",
             estimate = "{estimate} [{conf.low}, {conf.high}] {stars}",
             notes = list("*** P<0.001")) 
modelsummary1




