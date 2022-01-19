library(tidyverse)
library(data.table)
library(dplyr)
library(rmatio)
library(ggplot2)
library(readxl)

# load data-------
# group = 1 ASD
q_total <- read_xlsx("./data/score_total.xlsx",sheet = 1,col_types = c("numeric","numeric","text","numeric","text",rep("numeric",41)))
nirs_info <- read_xlsx("./data/nirs_information.xlsx",col_types = c("text","numeric","numeric","numeric","date","numeric","text"))
spsrc <- read_xlsx("./data/SPSRC.xlsx",sheet = 2)
# all subjects------
nirs_info <- nirs_info %>% 
  select(id,sub) %>% 
  as.data.table() %>% 
  setkey(id)
q_total <- q_total %>% 
  select(id_new,group,gender,name,age,FSIQ,AQ,SRS,forearm,hand,tactile,SPSRC_total,seeking,underresponse,overresponse) %>% 
  rename(id = id_new) %>% 
  as.data.table() %>% 
  setkey(id)
q_total <- nirs_info[q_total] %>% 
  setkey(sub)
# IQ subgroup---------
# (2) ASD IQ_range: [104,125] N = 23     TD IQ_range: [96,129] N = 23
# (1)  ASD IQ_range: [103,136] N = 33     TD IQ_range: [96,134] N = 33

ggplot(data = q_iq_sub )+
  geom_point(aes(x = SRS, y = SPSRC_total,color = as.factor(group)))+
  theme_bw()
q_iq_sub <- q_total %>% 
  filter((group == 1 & (FSIQ <=143 & FSIQ >= 103)) | (group == 2 &(FSIQ <=138 & FSIQ >= 96))) %>% 
  filter(id_new != 5 & id_new != 2416 &  id_new!= 2 & id_new!= 121 & id_new!= 128 & id_new!= 23 & id_new!= 21 & id_new != 33 & id_new!= 2401 & id_new != 39) %>% 
  # mutate(new_group = ifelse(group == 1 & (FSIQ <=136 & FSIQ >= 103),"ASD",
  #                           ifelse(group == 2 &(FSIQ <=134 & FSIQ >= 96),"TD_low",
  #                                  ifelse(group == 2 & FSIQ >134,"TD_high","other")))) %>% 
  select(id_new,group,gender,name,age,FSIQ,AQ,SRS,forearm,hand,tactile,tactile_z,
         SPSRC_total,SPSRC_total_z,seeking,seeking_z,underresponse,overresponse,stability,
         tactile_over,tactile_under,tactile_seek,GEM_tot,GEM_affective,GEM_cognitive,ToM,tactile_threshold,pain_threshold) %>% 
  rename(id = id_new) %>% 
  as.data.table() %>% 
  setkey(id)
t.test(q_iq_sub$FSIQ[which(q_iq_sub$group==1)],q_iq_sub$FSIQ[which(q_iq_sub$group==2)])
t.test(q_iq_sub$tactile[which(q_iq_sub$group==1)],q_iq_sub$tactile[which(q_iq_sub$group==2)])
t.test(q_iq_sub$age[which(q_iq_sub$group==1)],q_iq_sub$age[which(q_iq_sub$group==2)])
t.test(q_iq_sub$SRS[which(q_iq_sub$group==1)],q_iq_sub$SRS[which(q_iq_sub$group==2)])
t.test(q_iq_sub$SPSRC_total[which(q_iq_sub$group==1)],q_iq_sub$SPSRC_total[which(q_iq_sub$group==2)])
t.test(q_iq_sub$forearm[which(q_iq_sub$group==1)],q_iq_sub$forearm[which(q_iq_sub$group==2)])
t.test(q_iq_sub$hand[which(q_iq_sub$group==1)],q_iq_sub$hand[which(q_iq_sub$group==2)])
mean(q_iq_sub$SPSRC_total[which(q_iq_sub$group == 1)],na.rm = T)
nirs_info <- nirs_info %>% 
  select(id,file_name) %>% 
  rename(sub = file_name) %>% 
  as.data.table() %>% 
  setkey(id)
q_iq_sub <- nirs_info[q_iq_sub] %>% 
  setkey(sub)


#hbo-----------
# subgroups
hbo_tot <- run_hbo[q_iq_sub, on = "sub"] %>% 
  mutate(SPSRC_group = ifelse(SPSRC_total <= 403,"low", "high"),
         SPSRC_z_group = ifelse(SPSRC_total_z<=-2," <= -2 SD",ifelse(SPSRC_total_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         tactile_z_group = ifelse(tactile_z<=-2," <= -2 SD",ifelse(tactile_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         seeking_z_group = ifelse(seeking_z<=-2," <= -2 SD",ifelse(seeking_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         tactile_group = ifelse(tactile <= 87,"low", "high"),
         tac_over_group =ifelse(tactile_over <= 29,"low", "high"),
         tac_under_group =ifelse(tactile_under <= 37,"low", "high"),
         tac_seeking_group = ifelse(tactile_seek <= 19,"low", "high"),
         seeking_group = ifelse(seeking <= 144,"low", "high"),
         over_group = ifelse(overresponse <= 134,"low", "high"),
         under_group = ifelse(underresponse <= 107,"low", "high"),
         stab_group = ifelse(stability <= 24,"low", "high"))


# hbo psts-----
hbo_sts <- hbo_tot %>% 
  group_by(id,group,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
           seeking_group,over_group,under_group,stab_group,channel,time) %>%
  summarise(con_diff = response[which(con == "forearm")] - response[which(con == "hand")]) %>% 
  #filter( channel == 20|channel == 23|channel == 43|channel == 46 | channel == 12| channel == 16 |channel == 39 | channel == 35) %>% 
  filter( channel == 20|channel == 23|channel == 43|channel == 46) %>% 
  mutate(time = time/3.47-2)
save(hbo_sts,file = "./output/hbo_sts_sub.Rdata")
mean_hbo_sts <- Rmisc::summarySE(data = hbo_sts,"con_diff",groupvars = c("time","group"),na.rm = T)
# mean_hbo_sts_ASD <- mean_hbo_sts %>% filter(new_group == "ASD" |new_group == "other")
# mean_hbo_sts_TD <- mean_hbo_sts %>% filter(new_group == "TD_low" |new_group == "TD_high")
ggplot(data = mean_hbo_sts,aes(x = time,y=con_diff,color = as.factor(group)))+
  geom_line()+
  #facet_wrap(~channel,scales = "free_y")+
  geom_ribbon(aes(ymax = con_diff+se,ymin = con_diff-se,fill = as.factor(group)),alpha = 0.25,linetype=0)+
  scale_color_discrete(labels=c("ASD","TD"), aesthetics = c("color","fill"),type = c("#0099CC","#FF6666")) +
  #geom_rect(aes(xmax = 40/3.47-5,xmin = 33/3.47-5,ymax = -0.088,ymin = -0.09),color = "yellow",linetype=0) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw()
ggsave("./output/pSTS_group_diff_all.tiff",units="in", width=5.5, height=3, dpi=500)
ggsave("./output/hbo_pSTS_group_diff_sub.tiff",units="in", width=5.5, height=3, dpi=500)

# hbo somatosensory ------

hbo_sm <- hbo_tot %>% 
  filter(channel == 13|channel == 17|channel == 14|channel ==18|channel == 21) %>% 
  # group_by(id,SPSRC_group,SPSRC_z_group,tactile_z_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
  #          seeking_group,seeking_z_group,over_group,under_group,stab_group,tactile,group,channel,time) %>% 
  # summarise(response = mean(response,na.rm = T)*1e6) %>% 
  mutate(time = time/3.47-2)

mean_hbo_sm <- Rmisc::summarySE(data = hbo_sm,"response",groupvars = c("time","tac_seeking_group"),na.rm = T)
ggplot(data = mean_hbo_sm,aes(x = time,y=response,color = as.factor(tac_seeking_group)))+
  geom_line()+ 
  #facet_wrap(~channel,scales = "free_y")+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = as.factor(tac_seeking_group)),alpha = 0.25,linetype=0)+
  #scale_color_discrete(labels=c("ASD","TD"), aesthetics = c("color","fill"),type = c("#0099CC","#FF6666")) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw()
ggsave("./output/hbo_somatosensory_group_diff_all.tiff",units="in", width=5.5, height=3, dpi=500)
ggsave("./output/hbo_somatosensory_group_diff_sub.tiff",units="in", width=5.5, height=3, dpi=500)
ggsave("./output/hbo_somatosensory_tactile_diff3_sub.tiff",units="in", width=5.5, height=3, dpi=500)


#hbr---------
# subgroups
hbr_tot <- run_hbr[q_iq_sub, on = "sub"] %>% 
  mutate(SPSRC_group = ifelse(SPSRC_total <= 403,"low", "high"),
         SPSRC_z_group = ifelse(SPSRC_total_z<=-2," <= -2 SD",ifelse(SPSRC_total_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         tactile_z_group = ifelse(tactile_z<=-2," <= -2 SD",ifelse(tactile_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         seeking_z_group = ifelse(seeking_z<=-2," <= -2 SD",ifelse(seeking_z <= -1,"-2 to -1 SD", ">= -1 SD")),
         tactile_group = ifelse(tactile <= 87,"low", "high"),
         tac_over_group =ifelse(tactile_over <= 29,"low", "high"),
         tac_under_group =ifelse(tactile_under <= 37,"low", "high"),
         tac_seeking_group = ifelse(tactile_seek <= 19,"low", "high"),
         seeking_group = ifelse(seeking <= 144,"low", "high"),
         over_group = ifelse(overresponse <= 134,"low", "high"),
         under_group = ifelse(underresponse <= 107,"low", "high"),
         stab_group = ifelse(stability <= 24,"low", "high"))


# hbr sts
hbr_sts <- hbo_tot %>% 
  group_by(id,group,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
           seeking_group,over_group,under_group,stab_group,channel,time) %>%
  summarise(con_diff = response[which(con == "forearm")] - response[which(con == "hand")]) %>% 
  filter( channel == 20|channel == 23|channel == 43|channel == 46 | channel == 12| channel == 16 |channel == 39 | channel == 35) %>% 
  #filter( channel == 20|channel == 23|channel == 43|channel == 46) %>% 
  #filter( channel == 20|channel == 23) %>% 
  # group_by(id,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
  #          seeking_group,over_group,under_group,stab_group,group,time) %>% 
  # summarise(con_diff = mean(con_diff,na.rm = T)*1e6)%>% 
  mutate(time = time/3.47-2)
save(hbr_sts,file = "./output/hbo_sts_sub.Rdata")
mean_hbr_sts <- Rmisc::summarySE(data = hbr_sts,"con_diff",groupvars = c("time","group"),na.rm = T)
ggplot(data = mean_hbr_sts,aes(x = time,y=con_diff,color = as.factor(group)))+
  geom_line()+
  geom_ribbon(aes(ymax = con_diff+se,ymin = con_diff-se,fill = as.factor(group)),alpha = 0.25,linetype=0)+
  scale_color_discrete(labels=c("ASD","TD"), aesthetics = c("color","fill"), type = c("#336666","#CCCC33")) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw()
ggsave("./output/hbr_pSTS_group_diff_all.tiff",units="in", width=5.5, height=3, dpi=500)
ggsave("./output/hbr_pSTS_group_diff_sub.tiff",units="in", width=5.5, height=3, dpi=500)

# hbr somatosensory 
hbr_sm <- hbr_tot %>% 
  filter( channel == 13|channel == 17|channel == 14|channel ==18| channel == 21) %>% 
  mutate(time = time/3.47-2)
save(hbr_sm,file = "./output/hbr_sm_sub.Rdata")
mean_hbr_sm <- Rmisc::summarySE(data = hbr_sm,"response",groupvars = c("time","tac_seeking_group"),na.rm = T)
ggplot(data = mean_hbr_sm,aes(x = time,y=response,color = as.factor(tac_seeking_group)))+
  geom_line()+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = as.factor(tac_seeking_group)),alpha = 0.25,linetype=0)+
  scale_color_discrete(aesthetics = c("color","fill"), type = c("#336666","#CCCC33")) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw()
ggsave("./output/hbr_somatosensory_group_diff_all.tiff",units="in", width=5.5, height=3, dpi=500)
ggsave("./output/hbr_somatosensory_group_diff_sub.tiff",units="in", width=5.5, height=3, dpi=500)

# combine hbo & hbr-----
# sts td-asd forearm-hand
hbo_tot1 <- hbo_tot %>% add_column(type = "hbo")
hbr_tot1 <- hbr_tot %>% add_column(type = "hbr")

sts <- rbind(hbo_tot1,hbr_tot1) %>% 
  group_by(id,type,group,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
           seeking_group,over_group,under_group,stab_group,channel,time) %>%
  summarise(con_diff = response[which(con == "forearm")] - response[which(con == "hand")]) %>% 
  group_by(type,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
           seeking_group,over_group,under_group,stab_group,channel,time) %>% 
  summarise(g_diff = con_diff[which(group == 2)] - con_diff[which(group == 1)]) %>% 
  #filter( channel == 20|channel == 23|channel == 43|channel == 46 | channel == 12| channel == 16 |channel == 39 | channel == 35) %>% 
  #filter( channel == 20|channel == 23|channel == 43|channel == 46) %>% 
  filter( channel == 43|channel == 46) %>% 
  # group_by(id,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
  #          seeking_group,over_group,under_group,stab_group,group,time) %>% 
  # summarise(con_diff = mean(con_diff,na.rm = T)*1e6)%>% 
  mutate(time = time/3.47-2)

mean_sts <- Rmisc::summarySE(data = sts,"g_diff",groupvars = c("time","type"),na.rm = T)

ggplot(data = mean_sts,aes(x = time,y=g_diff,color = type))+
  geom_line()+
  #facet_wrap(~channel,scales = "free_y")+
  geom_ribbon(aes(ymax = g_diff+se,ymin = g_diff-se,fill = type),alpha = 0.25,linetype=0)+
  scale_color_discrete( aesthetics = c("color","fill"),type = c("#FF6666","#0099CC")) +
  #geom_rect(aes(xmax = 40/3.47-5,xmin = 33/3.47-5,ymax = -0.088,ymin = -0.09),color = "yellow",linetype=0) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw()

sm <- rbind(hbo_tot1,hbr_tot1) %>% 
  filter(channel == 13|channel == 17|channel == 14|channel ==18| channel == 21) %>% 
  mutate(time = time/3.47-2)

mean_sm <- Rmisc::summarySE(data = sm,"response",groupvars = c("time","type","tac_seeking_group"),na.rm = T)

ggplot(data = mean_sm,aes(x = time,y=response,color = tac_seeking_group,linetype = type))+
  geom_line()+
  #facet_wrap(~channel,scales = "free_y")+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = tac_seeking_group, group = interaction(tac_seeking_group, type)),alpha = 0.25,linetype=0)+
  #scale_color_discrete( aesthetics = c("color","fill"),type = c("#FF6666","#0099CC")) +
  #geom_rect(aes(xmax = 40/3.47-5,xmin = 33/3.47-5,ymax = -0.088,ymin = -0.09),color = "yellow",linetype=0) +
  geom_vline(aes(xintercept = 0))+
  geom_vline(aes(xintercept = 6))+
  theme_bw() +
  facet_grid(type ~ .)
# beta----------

ggplot(data = q_total, aes(x = SPSRC_total, y = FSIQ,color = as.factor(group)))+
  geom_point()+
  geom_smooth(method = "lm")
  
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



hbo_exp <- hbo_tot %>% 
  filter(group == 1) %>% 
  mutate(s_g = ifelse(seeking <= 150,"low","high")) %>% 
  #filter(channel == 13|channel == 17|channel == 14|channel ==18| channel == 15) %>% 
  #filter(channel == 13 | channel == 14) %>% 
  filter( channel == 40|channel == 44) %>% 
  group_by(id,SPSRC_group,tactile_group,tac_over_group,tac_under_group,tac_seeking_group,
           seeking_group,over_group,under_group,stab_group,s_g,time) %>%
  summarise(response = mean(response,na.rm = T)) %>% 
  mutate(time = time/3.47-2)
mean_hbo_sts <- Rmisc::summarySE(data = hbo_exp,"response",groupvars = c("time","s_g"),na.rm = T)
ggplot(data = mean_hbo_sts,aes(x = time,y=response,color = as.factor(s_g)))+
  geom_line()+
  #facet_wrap(~channel,scales = "free_y")+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = as.factor(s_g)),alpha = 0.25,linetype=0)

