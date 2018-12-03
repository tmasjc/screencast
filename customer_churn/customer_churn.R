library(tidyverse)
library(ggthemes)
library(janitor)
library(rsample)
library(recipes)
library(yardstick)
library(broom)

# Set theme 绘图的主题
old <- theme_set(theme_tufte() + theme(text = element_text(family = "Menlo")))

# Get Data ---- 

# Source: IBM Watson Analytics
raw <- read_csv("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Cleaning ---- 

# remove all rows with NAs 清除具有遗失数据的行
dat <- raw %>% 
  select(-customerID) %>% 
  filter_all(all_vars(!is.na(.))) %>%
  clean_names()

# data pre-processing 数据的预处理
dat <- dat %>% 
  # convert from int to factor
  mutate(senior_citizen = factor(senior_citizen, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  # convert from int to factor (binning)
  mutate(tenure = Hmisc::cut2(tenure, g = 4)) %>% 
  # convert independent variable to factor
  mutate(churn = factor(churn, levels = c("No", "Yes"), labels = c("no", "yes"))) %>% 
  # to prevent singularity (dummy encoding failed)
  # 确保不会产生数据的多重共线性
  # 如果没做这一步，逻辑回归的函数会崩溃
  mutate_if(.predicate = is.character, 
            .funs = ~ ifelse(.x == "No internet service" | .x == "No phone service", "No", .x))

# EDA ---- 

# view correlation plot
# 看看维度之间的关联性
dat %>%
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "color", order = "FPC")

# longer the tenure, lower the churn rate
# 顾客呆的越久，流失的比例越小
dat %>% 
  group_by(tenure) %>% 
  summarise(churn_rate = mean(churn == "yes"))

# longer the contract, lower the churn rate
# 合同签的约久，流失的比例越小
dat %>% 
  group_by(contract) %>% 
  summarise(churn_rate = mean(churn == "yes"))

# Simpson's parodox
# do not be deceived by this
# 不要被这比例给误导了，看下个图
dat %>% 
  ggplot(aes(churn, total_charges, fill = churn)) + 
  geom_boxplot()

# customers who churn usually pay more
# 其实流失客户往往付的金额更多
dat %>% 
  ggplot(aes(x = tenure, y = total_charges, fill = tenure)) + 
  geom_boxplot() + 
  facet_wrap(~ churn)
  
# Prepare for Data Modeling ---- 

set.seed(1212)

# split training and test set 拆分训练、测试集
data_split <- initial_split(dat, strata = "churn")
train <- training(data_split)
test <- testing(data_split)

# 这里我们利用 `recipe`
(rc <- recipe(churn ~ ., data = dat))

rc <- rc %>% 
  # dummify categorical variables 哑变量化
  step_dummy(all_predictors(), -all_numeric()) %>% 
  # shift mean to 0 中心化
  step_center(all_predictors()) %>% 
  # shift sd to 1 比例化
  step_scale(all_predictors())

# we are ready to bake
rc_mod <- prep(rc, training = train)

# process on train/test split
train <- bake(rc_mod, train)
test <- bake(rc_mod, test)

# Modeling ---- 

# a simple logistic model
# 简单的逻辑回归
lg.fit <- glm(formula = churn ~ ., data = train, family = "binomial")
summary(lg.fit)

# add predicted values to original data
# 把预测值附加到训练集上，为了更方便诊断我们的模型
train.aug <- broom::augment(lg.fit, type.predict = "response") %>% 
  mutate(ind = 1:n())

# remember: logistic regression does not assume normal distribution of residuals
# 注意：逻辑回归不对残差的分布进行任何假设，我们只是看看而已
train.aug %>% 
  ggplot(aes(ind, .std.resid, color = churn)) + 
  geom_point()

# McFadden's pseudo R squared
# 伪R方的一种，衡量我们模型的凝合效果
1 - lg.fit$deviance / lg.fit$null.deviance

# check Cook's distance
# 诊断任何数据点对模型的杠杠过高
plot(lg.fit, which = 1:6)

# select important features
# 利用 AIC 对模型进行维度删选
lg.fit.aic <- MASS::stepAIC(lg.fit, direction = "backward")
train.aug.aic <- augment(lg.fit.aic, type.predict = "response")

# Model Accessment ----

train.aug %>% 
  ggplot(aes(.fitted, fill = churn)) + 
  geom_density(alpha = .7)

# confusion matrix  混淆矩阵
train.aug %>% 
  # random cutoff (临界值) at 50% 
  mutate(pred.churn = .fitted > .5) %>% 
  janitor::tabyl(churn, pred.churn)

# ROC curve
train.aug %>% 
  roc_curve(churn, .fitted) %>% 
  # true positive rate  ~ false positive rate
  mutate(tpr = sensitivity, fpr = 1 - specificity) %>% 
  ggplot(aes(x = fpr, y = tpr)) + 
  geom_line(col = "navyblue") + 
  geom_abline(col = "darkgray", lty = 4) + 
  geom_vline(xintercept = .5, col = "darkred")
  
# see https://tidymodels.github.io/yardstick/reference/pr_curve.html#relevant-level
options(yardstick.event_first = FALSE)

# RR curve
train.aug %>% 
  pr_curve(churn, .fitted) %>% 
  # precison ~ recall
  ggplot(aes(x = recall, y =  precision)) + 
  geom_line(col = "navyblue") + 
  geom_vline(xintercept = .5, lty = 3)

# compare 2 models using area under the curve
# 利用 ROC 面积对比两组模型 （stepAIC 前与后）
roc_auc(train.aug, churn, .fitted)
roc_auc(train.aug.aic, churn, .fitted)

pr_auc(train.aug, churn, .fitted)
pr_auc(train.aug.aic, churn, .fitted)

# Optimal Threshold ---- 

# customer lifetime value = 2000
# promotion cost = 1200
# 我们这里假设挽留一个客户的价值是 2000，推广费用为 1200
measure_cf <- train.aug.aic %>% 
  roc_curve(churn, .fitted) %>% 
  mutate(tpr = sensitivity,
         fpr = 1 - specificity,
         gain = tpr * 800,
         loss = fpr * 1200,
         diff  = gain - loss)

# optimal cutoff 最优的临界值
max_cf <- measure_cf %>% 
  filter(diff == max(diff)) %>% 
  pull(.threshold)

# visualize gain minus loss at different cutoff
measure_cf %>% 
  ggplot(aes(.threshold, diff)) + 
  geom_line(col = "navyblue") + 
  geom_hline(yintercept = 0, col = "darkgray", lty = 3) + 
  geom_vline(xintercept = max_cf, col = "darkred") + 
  labs(x = "Cutoff", y = "Gain - Loss")



# Final Validation on Test Set ----

# predict on test set
# 在测试集上验证最后一遍
test <- test %>% 
  mutate(prob = predict(lg.fit.aic, test, type = "response"),
         pred = ifelse(prob > max_cf, "yes", "no"))

# make sure it does not deviate from training data performance
# 确保和训练集上的效果差距不是特别大
roc_auc(test, churn, prob)
pr_auc(test, churn, prob)

# model performance 模型在测试集上的效果
table(predicted = test$pred, true = test$churn)



