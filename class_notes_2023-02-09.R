# JPW
# 2023-02-09

library(tidyverse)
library("palmerpenguins")
head(penguins)
summary(penguins)
glimpse(penguins)
class(penguins)

mean(penguins$bill_depth_mm,na.rm=T)

# find list of verb functions and what they do in dplyr rmd tutorial

head()
# filter by species

gentoo = filter(.data=penguins, species=="Gentoo")
# you can leave out the .data if ur lazy

head(gentoo)
summary(gentoo)


gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
head(gentoo_ladies)
summary(gentoo_ladies)


gentoo_ladies = penguins %>%
  filter(species=="Gentoo") %>%
  filter(sex=="female")

summary(gentoo_ladies)

mean_ladies_mass = penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g = mean(body_mass_g))

#boring way in base r

mean_ladies_mass = mean(penguins$body_mass_g[penguins$sex=="female"], na.rm=T)


species_sex_mass = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g))

write_csv(x=species_sex_mass, file="data/species_sex_mass.csv")


species_count = penguins %>%
  # filter(!is.na(sex)) %>%
  group_by(species) %>%
  summarize(count = n())

penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g*0.0022)
head(penguins_for_america)
glimpse(penguins_for_america)


penguins %>% 
  distinct((island))

for_my_advisor = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

penguins %>%
  arrange(desc(body_mass_g))


mean_bill = penguins %>%
  filter(species=="Adelie", 
         island %in% c("Biscoe", "Dream"), 
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_inches = bill_length_mm * 0.039) %>% # 0.039 in per mm
  summarize(mean_bill_length_inches = mean (bill_length_inches), 
            sd_bill_length_inches = sd(bill_length_inches))
  

  

head(mean_bill)
tail(mean_bill)


