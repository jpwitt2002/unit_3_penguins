# JPW
# 2023-02-28

# Multiple regression yayy 
# apparently tricky 
# continuous vs categorical variables
# categorical aka factor aka nominal aka discrete
# can use as.factor() to make it categorical 

library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)

head(penguins)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm), 
         !is.na(bill_length_mm), 
         !is.na(species))
dim(penguins_lm_3)

# lets build a model

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)

summary(lm_3)
# intercept is adelie for some silly goose reason
# absolutely NEVER copy numbers from this table, will cause trouble later

coef(lm_3)
# lousy way to do this
anova(lm_3)
# also not great bc we dont have categorical x variables
my_results = broom::tidy(lm_3, conf.int=T, conf.level=0.95) %>%
  mutate_if(is.numeric, round, 2)
my_results$estimate

# time for a cheater visual; enter giraffes??
ggPredict(lm_3, se=T, interactive=T)
# cool for quick stuff, not for specifications tho

# you can do it in base r, ew
lm_3_predictions = predict(lm_3, interval="confidence", level=0.95)
head(lm_3_predictions)
head(penguins_lm_3)

# combine data sets
penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predict)

ggplot(data=penguins_lm_3_predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = NULL, fill=species), alpha = 0.5) + 
  geom_point() +
  geom_line(aes(y=fit)) 
# this only predicts within the boundaries of the data provided 

# generate newdata
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm), 
                             by=0.1)
head(newdata_bill_length_mm)
tail(newdata_bill_length_mm)

newdata = expand.grid(bill_length_mm=newdata_bill_length_mm, 
                      species=unique(penguins_lm_3$species))

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata = newdata, interval="confidence"))
head(newdata_predict_lm_3)

ggplot() +
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(data=newdata_predict_lm_3, aes(ymin=lwr, ymax=upr, x=bill_length_mm, fill=species), 
              alpha=0.5) +
  geom_line(data=newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species))


# tidyverse way
lm_3_predict = lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=T, interval="confidence")
glimpse(lm_3_predict)

ggplot() +
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(data=lm_3_predict, aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), 
              alpha=0.5) +
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))



