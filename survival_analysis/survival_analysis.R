library(tidyverse)
library(ggthemes)
library(janitor)
library(survival)
library(rms)

# Get Data ----

# Source: Data Camp
file_url <- "https://assets.datacamp.com/production/repositories/1861/datasets/284a58cc6f9c5873937a97eca0420b980b5e5b23/survivalDataExercise.csv"
dat <- read_csv(file_url)

# EDA ----

# overview
dat %>% 
  ggplot(aes(daysSinceFirstPurch, fill = factor(boughtAgain))) + 
  geom_histogram() +
  facet_wrap(~ boughtAgain, ncol = 1) + 
  labs(x = "Days", y = "Count", fill = "Repurchase")

# Kaplan-Meier Analysis ----

# create a survival object
surv_obj <- Surv(dat$daysSinceFirstPurch, dat$boughtAgain)
str(surv_obj)
head(surv_obj)

# null model
fit.null <- survfit(surv_obj ~ 1, type = "kaplan-meier")
plot(fit.null, conf.int = FALSE)

# fit to gender
fit.gender <- survfit(surv_obj ~ gender, data = dat)
plot(fit.gender, conf.int = FALSE, lty = 2:3)
legend(120, .9, c("male", "female"), lty = 2:3)

# fit to voucher
fit.gender <- survfit(surv_obj ~ voucher, data = dat)
plot(fit.gender, conf.int = FALSE, lty = 2:3)
legend(120, .9, c("no", "yes"), lty = 2:3)

# Cox Proportional Hazards Regression ----

# fit a Cox model
fit.cph <- survival::coxph(surv_obj ~ gender + voucher + returned + shoppingCartValue, data = dat)
summary(fit.cph)

# visualize model better
survminer::ggforest(fit.cph, data = dat)

# Validate Model Assumptions ----

# test the proportional hazards assumption for a Cox regression model fit  
fit.coxzph <- cox.zph(fit.cph)
fit.coxzph

# ideally a straightline
plot(fit.coxzph, var = "gendermale")

# Make Prediction ----

# dummy data
new <-  data.frame(daysSinceFirstPurcch = 40, shoppingCartValue = 55.5,
                     gender = "male", voucher = 1, returned = 0)
# make prediction
pred <- survfit(fit.cph, newdata = new)
plot(pred, conf.int = FALSE)

