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

# generate new data
newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)
lm_3_predict = lm_3 %>%
  broom::augment(newdata=newdata, se_fit=T, interval="confidence")
head(lm_3_predict)

# visualize
ggplot() +
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_ribbon(data=lm_3_predict, aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), 
              alpha=0.5) +
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))

# make an interaction term so slopes can vary
lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm:species, 
          data=penguins_lm_3)
# can also code like this
# lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data=penguins_lm_3)

summary(lm_4)
# interaction terms not significant, adding model complexity not worth it
# can use adjusted r squared

AIC(lm_3, lm_4)
# less AIC, better model

best_model = step(lm_4)
summary(best_model)
# step is best probably

# plot with interaction
lm_4_predict = lm_4 %>%
  broom::augment(interval="confidence")
head(lm_4_predict)


ggplot(data=lm_4_predict) + 
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_line(aes(y=.fitted, x=bill_length_mm, color=species)) + 
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), alpha=0.5)

# depth ~ length + flipper_length
library(car) # vif() to check if variables are multicolinear cause thats bad

gentoo = penguins %>%
  filter(species=="Gentoo")

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)

step(lm_gentoo_3)
vif(lm_gentoo_3) # value of 0 means no colinearity

# visualize, gotta understand how to generate model predictions (spoiler: i dont.)
# for this, show the three relationships separately; no 4d plots allowed
# each is while the other variables are set to their median value


newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=T), 
         body_mass_g = median(gentoo$body_mass_g, na.rm=T))

head(newdata)
 lm_gentoo_3_predict = lm_gentoo_3 %>%
   broom::augment(newdata=newdata, interval="confidence")
head(lm_gentoo_3_predict)

ggplot(data=lm_gentoo_3_predict) + 
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=bill_length_mm)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm), alpha=0.5) + 
  annotate("text", x=57, y=13.5, label=paste0("flipper length = ", 
                                              median(gentoo$flipper_length_mm, na.rm=T), 
                                              " mm")) +
  annotate("text", x=57, y=13.9, label=paste0("body mass = ", 
                                              median(gentoo$body_mass_g, na.rm=T), 
                                              " g"))
# rinse and repeat for the other variables

newdata2 = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=T), 
         body_mass_g = median(gentoo$body_mass_g, na.rm=T))

lm_gentoo_3_predict2 = lm_gentoo_3 %>%
  broom::augment(newdata=newdata2, interval="confidence")

ggplot(data=lm_gentoo_3_predict2) + 
  geom_point(aes(x=flipper_length_mm, y=bill_depth_mm), data=gentoo) +
  geom_line(aes(y=.fitted, x=flipper_length_mm)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=flipper_length_mm), alpha=0.5) + 
  annotate("text", x=225, y=13.5, label=paste0("bill length = ", 
                                              median(gentoo$bill_length_mm, na.rm=T), 
                                              " mm")) +
  annotate("text", x=225, y=13.9, label=paste0("body mass = ", 
                                              median(gentoo$body_mass_g, na.rm=T), 
                                              " g"))

# ANOVA crash course
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
anova(penguin_lm)

penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguin_anova)
TukeyHSD(penguin_anova)
