library(raster)
library(spatialEco)
library(dplyr)
library(here)
library(tidyverse)

###load data
dat <- read.csv(here("data", "sampled_data_all_sites.csv")) 

dat = dat %>% 
  mutate(cover_class = as.factor(cover)) %>% 
  mutate(land_class = as.factor(nwi)) %>% 
  mutate(site_class = as.factor(site))

#Graphical analysis of assumptions histogram

ggplot(aes(x = agb), data = dat) +
  geom_histogram(binwidth = 5, color = "cornflowerblue", 
                 fill = "cornflowerblue") +
  labs(x ="", y = "Frequency") + 
  facet_wrap(~site_class) +
  theme_bw() 

### Boxplots
ggplot(aes(x = site_class, y = agb),
       data = dat) +
  geom_boxplot() +
  scale_x_discrete(labels=c("Colville", "Hoh", "Mashel")) +
  labs(x = "", y = "Above Ground Biomass") +
  theme_bw(base_size = 18)

# F-test
# This is for between sites only
anova1 <- aov(agb ~ site_class, data = dat)
summary(anova1)

### will then need t-tests run between sites

###Land class comparison
#Graphical analysis of assumptions histogram

ggplot(aes(x = agb), data = dat) +
  geom_histogram(binwidth = 5, color = "cornflowerblue", 
                 fill = "cornflowerblue") +
  labs(x ="", y = "Frequency") + 
  facet_wrap(~land_class) +
  theme_bw() 

### Boxplots
ggplot(aes(x = land_class, y = agb),
       data = dat) +
  geom_boxplot() +
  scale_x_discrete(labels=c("upland", "wetland")) + ### (Make sure this is right)
  labs(x = "", y = "Above Ground Biomass") +
  theme_bw(base_size = 18)

# F-test
# This is for between wetlands and uplands only
anova2 <- aov(agb ~ land_class, data = dat)
summary(anova2)

### Comparison of canopy cover types 
#Graphical analysis of assumptions histogram

ggplot(aes(x = agb), data = dat) +
  geom_histogram(binwidth = 5, color = "cornflowerblue", 
                 fill = "cornflowerblue") +
  labs(x ="", y = "Frequency") + 
  facet_wrap(~cover_class) +
  theme_bw() 

### Boxplots
ggplot(aes(x = cover_class, y = agb),
       data = dat) +
  geom_boxplot() +
  scale_x_discrete(labels=c("Sparse", "Partial", "Closed")) +
  labs(x = "", y = "Above Ground Biomass") +
  theme_bw(base_size = 18)

# F-test
# This is for between canopy cover only
anova3 <- aov(agb ~ cover_class, data = dat)
summary(anova3)

###Let's compare multiple 

fit <- aov(agb ~ cover_class * site_class * land_class, data = dat)
summary(fit)

### Potential visual for brainstroming
ggplot(aes(x = land_class, y = agb),
       data = dat) +
  geom_boxplot(aes(fill = cover_class)) +
  scale_x_discrete(labels=c("upland", "wetland")) + ### (Make sure this is right)
  labs(x = "", y = "Above Ground Biomass") +
  facet_grid(rows = vars(site_class)) +
  theme_bw(base_size = 18) 



