# 2023-02-14
# JPW
# Happy Valentines Day :)

library(tidyverse)
library(palmerpenguins)

head(penguins)
find("filter")
# if you need the stats version of filter that was overwritten 
stats::filter()
# or if you need dplyr select function
dplyr::select()


penguins_without_nas = penguins %>%
  filter(!is.na(flipper_length_mm))
my_plot = ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species, shape=sex))+ 
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g)) +
  xlab("Flipper Length (mm)")+
  ylab("Body Mass (g)") +
  ggtitle("Penguins are cute") +
  theme_bw()

ggsave(my_plot, filename = "figures/flipper_v_mass.png", width = 7, height = 5, units = "in", dpi = 300)


penguins_ts = penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins = n())

ggplot(data = penguins_ts) + 
  geom_line(aes(x=year, y=num_penguins, color=species))


ggplot(data=penguins) +
  geom_histogram(aes(x=flipper_length_mm, fill=species, color=species), 
                 position = "identity", 
                 alpha=0.5) +
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4")) +
  facet_wrap(~species)

ggplot(data = penguins) +
  geom_boxplot(aes(y=flipper_length_mm, x=species)) +
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species), width = 0.2) +
  theme_bw()

ggplot(data=penguins) +
  geom_bar(aes(x=sex, fill=species)) +
  facet_wrap(~species)


ggplot(data=penguins) +
  geom_bar(aes(x=island, fill=species))+
  facet_wrap(~species) +
  coord_flip()


  
  
  
  
