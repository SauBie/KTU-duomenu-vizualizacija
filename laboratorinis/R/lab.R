library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
data <- read_csv("../data/lab_sodra.csv")

# 1 uzduotis ----
data %>%
  filter(ecoActCode == 453200) %>%
  ggplot(aes(x = avgWage)) +
  geom_histogram(bins = 75, col="darkgray") + theme_light() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 1), 
        axis.ticks = element_line(colour = "black", size = 1))

# 2 uzduotis ----
atrinkti2 <- data %>%
  filter(ecoActCode == 453200) %>% group_by(code) %>% 
  summarise(suma = sum(avgWage)) %>% 
  arrange(desc(suma)) %>% head(5)
merged <- merge(atrinkti2, data, by = "code")

merged %>% 
  ggplot(aes(x = month, y = avgWage, group = name, colour = name)) + geom_line() +
  theme_light() + scale_y_continuous(breaks=seq(0,8000,1000)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 1), axis.text.x = element_blank(),
        axis.ticks.y = element_line(colour = "black", size = 1))
  
# 3 uzduotis ----
merged %>% group_by(name) %>% 
  summarise(maxim = max(numInsured)) %>% 
  ggplot(aes(x = reorder(name, -maxim), y = maxim, fill = name)) +
  geom_col() + ylab("apdraustieji") + xlab("name") +
  theme_light() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(size = 1), 
        axis.ticks = element_line(colour = "black"))
