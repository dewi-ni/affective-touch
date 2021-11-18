library(tidyverse)
library(data.table)
library(dplyr)
library(rmatio)
library(ggplot2)

asd_beta <- read.mat("./data/ASDresult.mat")
td_beta <- read.mat("./data/TDresult.mat")
q_total <- read.xls("./data/score_total.xlsx")

asd1 <- as.data.frame(asd$diff_con) %>% 
  add_column(ID = 1:43,.before = "V1") %>% 
  melt(., id = "ID",variable.name = "channel",value.name = "b_diff") %>% 
  mutate(b_diff = ifelse(b_diff == 0,NA,b_diff)) 
td1 <- as.data.frame(td$diff_con) %>% 
  add_column(ID = 1:31,.before = "V1") %>% 
  melt(., id = "ID",variable.name = "channel",value.name = "b_diff") %>% 
  mutate(b_diff = ifelse(b_diff == 0,NA,b_diff))

ggplot(data = asd1, mapping = aes(x = channel, y = b_diff, color = as.character(ID)))+
  geom_point()
ggplot(data = td2, mapping = aes(x = channel, y = b_diff))+
  geom_boxplot()

sd_asd <- asd1 %>% group_by(channel) %>% summarise(sd= sd(b_diff,na.rm = T))
ggplot(data = sd_asd, mapping = aes(x = channel, y = sd))+
  geom_point()
