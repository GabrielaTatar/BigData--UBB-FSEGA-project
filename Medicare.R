library(tidyverse)
library(modelr)
library(scatterplot3d)
library(dplyr)
library(ggplot2)

Medicare <- read_csv("insurance.csv") 
str( Medicare)

Medicare <- mutate_at(Medicare, vars(sex,smoker,region), as.factor)
str( Medicare)

summary(Medicare)

Medicare %>%
  ggplot(aes(age, charges)) + geom_point() + geom_smooth()

Medicare %>%
  ggplot(aes(sex, charges)) + geom_point() 

Medicare %>%
  ggplot(aes(bmi, charges)) + geom_point() + geom_smooth()

Medicare %>%
  ggplot(aes(children, charges)) + geom_point() 

Medicare %>%
  ggplot(aes(region, charges)) + geom_point() 

Medicare %>%
  ggplot(aes(smoker, charges)) + geom_point() 


mod_charges_age <- lm(data = Medicare, charges ~ age)
summary(mod_charges_age)

grid_age <- Medicare %>%  
  data_grid(age = seq_range(age, 100)) %>%
  add_predictions(mod_charges_age, "charges")

ggplot(Medicare, aes(age, charges)) + 
  geom_point() +  
  geom_line(data = grid_age, color = "red", linewidth = 1) 

confint(mod_charges_age)


mod_charges_sex <- lm(data = Medicare, charges ~ sex)    
summary(mod_charges_sex)

grid_sex <- Medicare %>%  
  data_grid(sex = factor(0:100, levels = levels(Medicare$sex))) %>%
  add_predictions(mod_charges_sex, "charges")

ggplot(Medicare, aes(sex, charges)) + 
  geom_point() +  
  geom_line(data = grid_sex, aes(group = 1), color = "red", linewidth = 1)

confint(mod_charges_sex)


mod_charges_bmi <- lm(data = Medicare, charges ~ bmi)
summary(mod_charges_bmi)

grid_bmi <- Medicare %>%  
  data_grid(bmi = seq_range(bmi, 100)) %>%
  add_predictions(mod_charges_bmi, "charges")

ggplot(Medicare, aes(bmi, charges)) + 
  geom_point() + 
  geom_line(data = grid_bmi, color = "red", linewidth = 1)

confint(mod_charges_bmi)


mod_charges_children <- lm(data = Medicare, charges ~ children)
summary(mod_charges_children)

grid_children <- Medicare %>%  
  data_grid(children = seq_range(children, 100)) %>%
  add_predictions(mod_charges_children, "charges")

ggplot(Medicare, aes(children, charges)) + 
  geom_point() +  
  geom_line(data = grid_children, color = "red", linewidth = 1)

confint(mod_charges_children)


mod_charges_smoker <- lm(data = Medicare, charges ~ smoker)    
summary(mod_charges_smoker)

grid_smoker <- Medicare %>%  
  data_grid(smoker = factor(0:100, levels = levels(Medicare$smoker))) %>%
  add_predictions(mod_charges_smoker, "charges")

ggplot(Medicare, aes(smoker, charges)) + 
  geom_point() +  
  geom_line(data = grid_smoker, aes(group = 1), color = "red", linewidth = 1)

confint(mod_charges_smoker)


mod_charges_region <- lm(data = Medicare, charges ~ region)    
summary(mod_charges_region)

grid_region <- Medicare %>%  
  data_grid(region = factor(0:100, levels = levels(Medicare$region))) %>%
  add_predictions(mod_charges_region, "charges")

ggplot(Medicare, aes(region, charges)) + 
  geom_point() +  
  geom_line(data = grid_region, aes(group = 1), color = "red", linewidth = 1)

confint(mod_charges_region)



mod_charges_all <- lm(data = Medicare, charges ~ age + sex + bmi + children + smoker + region) 
summary(mod_charges_all)

mod_charges_age_bmi_smoker <- lm(data = Medicare, charges ~ age + bmi + smoker) 
summary(mod_charges_age_bmi_smoker)


s3d <- scatterplot3d(Medicare$age, Medicare$bmi, Medicare$smoker, Medicare$charges, type="p") 
s3d$plane3d(mod_charges_age_bmi_smoker, lty.box = "solid")

new_patients <- tibble(
  age = 30,
  bmi = 25,
  smoker = "no"
)

predict(mod_charges_age_bmi_smoker, newdata = new_patients, interval = "confidence")
predict(mod_charges_age_bmi_smoker, newdata = new_patients, interval = "prediction")





# ARBORI DE DECIZIE
install.packages("purr")

library(rsample)
library(tidyverse)
library(dplyr)
library(readr)
library(rpart) 
library(rpart.plot)
library(caret)

Medicare <- read_csv("insurance.csv")
str(Medicare)

Medicare <- mutate_at(Medicare, vars(sex,smoker,region), as.factor)
str(Medicare)

Medicare %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

Medicare %>%
  select_if(is.factor) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

Medicare %>%
  ggplot(aes(charges)) +
  geom_density()

set.seed(123)

medi_split <- initial_split(Medicare, prop = 0.7)
medi_train <- training(medi_split)
medi_test <- testing(medi_split)

m1 <- rpart(
  formula = charges ~ .,
  data = medi_train,
  method = "anova"
)

m1

rpart.plot(m1)

plotcp(m1)

m1$cptable


m2 <- rpart(
  formula = charges ~ .,
  data = medi_train,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

m2
rpart.plot(m2)
plotcp(m2)
abline(v = 8, lty = "dashed")
m2


m3 <- rpart(
  formula = charges ~ .,
  data = medi_train,
  method = "anova",
  control = list(minsplit = 10, maxdepth = 12, xval = 10)
)
m3
plotcp(m3)
rpart.plot(m3)


hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1), 
  maxdepth = seq(8, 15, 1)
)
head(hyper_grid)
models <- list()

set.seed(123) 
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = charges ~. ,
    data = medi_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

head(m2$cptable)

View(models)

get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}

get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

View(models)

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )

mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)


optimal_tree <- rpart(
  formula = charges ~ .,
  data = medi_train,
  method = "anova",
  control = list(minsplit = 6, maxdepth = 10, cp = 0.0100000)
)
optimal_tree

pred <- predict(optimal_tree, newdata = medi_test)
pred

RMSE(pred = pred, obs = medi_test$charges)

optimal_tree



# Bagging
install.packages("ipred")
library(ipred)

set.seed(123)

bagged_m1 <- bagging(
  formula = charges ~ .,
  data = medi_train, 
  coob = TRUE
)
bagged_m1

ntree <- 10:100
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging(  
    formula = charges ~ .,
    data = medi_train,
    coob = TRUE,
    nbagg = ntree[i] 
  )
  rmse[i] = model$err 
}

plot(ntree, rmse, type ="l", lwd=2)

abline(v=30, col = "red", lty="dashed")


#Bagging with CARET
library(caret)

fitControl <- trainControl(
  method = "cv", 
  number = 10    
)

bagged_cv <- train(
  charges ~.,
  data = medi_train,
  method = "treebag",      
  trControl = fitControl,  
  importance = TRUE
)

bagged_cv

plot(varImp(bagged_cv), 20)

pred <- predict(bagged_cv, medi_test)

RMSE(pred, medi_test$charges)

library(tidyverse)
for_plotting <- tibble(
  i = 1:402,  
  pred = pred[], 
  actual = medi_test$charges 
)

ggplot(for_plotting, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +   
  geom_point(aes(y = actual, color = "blue"))  

ggplot(for_plotting, aes(x=i)) + 
  geom_point(aes(y = pred-actual)) 
  
