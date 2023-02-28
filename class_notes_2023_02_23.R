# JPW
# 2023-02-23

# Linear Models 1: Linear Regression

library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

head(penguins)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color=species))

penguins %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

# linear model

# designed to be a failure apparently

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
class(lm_1)
summary(lm_1)
# gave us some horrendous r-squared values so this is bad - was in fact a failure as advertised

# now we are cheating

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")
# you can put the aes in the main ggplot thing if those settings will apply to the whole graph instead of just some pieces of it

plot(lm_1)
# this function generates multiple plots - press return in the console to see them

gentoo = penguins %>%
  filter(species=="Gentoo")

gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)

plot(lm_2)
# the first plot, residuals vs fitted, has 3 points numbered by their row number that may be outliers in some way, but they may not be because it will always do this

ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method="lm")
# you can put the aes in both the geom point and geom smooth, but it's easier to just put it in the ggplot function because it's less repetitive

ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  geom_point(data=penguins %>%
               filter(species=="Adelie"),
             aes(x=bill_length_mm, y=bill_depth_mm), color="red")

# plot each species lm separately

ggplot(data=penguins) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm, color=species), method="lm") +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm), color="black", method="lm") +
  theme_bw()
# created separate regression lines for each species in their corresponding color and a black regression line that is for all of the species
# this is called simpson's paradox - there are times when you group things together that shouldn't be grouped, and you can get the opposite relationship of your variables
# when bill length increases, depth also increases, but our overall regression line shows a negative relationship, which is the paradox

# Exercise 5.1
# linear model predicting Gentoo bill depth as function of flipper length

ggplot(data=gentoo) +
  geom_point(aes(x=flipper_length_mm, y=bill_depth_mm), method = "lm") +
  geom_smooth(aes(x=flipper_length_mm, y=bill_depth_mm), method = "lm") +
  theme_bw()

ggplot(data=gentoo) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), method = "lm") +
  geom_smooth(aes(x=bill_length_mm, y=bill_depth_mm), method = "lm") +
  theme_bw()

lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)
summary(lm_3)

# flipper length seems to do a better job of predicting bill depth because there are more point along the regression line, signifying a stronger relationship between the 2 variables
