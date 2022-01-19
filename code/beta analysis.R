library(tidyverse)
library(data.table)
library(dplyr)
library(rmatio)
library(ggplot2)
library(readxl)
library(afex)
library(emmeans)
library(ggrepel)
# load data-------
asd_beta <- read.mat("./data/ASDresult1218.mat")
td_beta <- read.mat("./data/TDresult1218.mat")
# group = 1 ASD
q_total <- read_xlsx("./data/score_total.xlsx",sheet = 1,col_types = c("numeric","numeric","text","numeric","text",rep("numeric",47)))
nirs_info <- read_xlsx("./data/nirs_information.xlsx",col_types = c("text","numeric","numeric","numeric","date","numeric","text"))
spsrc <- read_xlsx("./data/SPSRC.xlsx",sheet = 2)

q_iq_sub <- q_total %>% 
  #filter((group == 1 & (FSIQ <=126 & FSIQ >= 103)) | (group == 2 &(FSIQ <133 & FSIQ >= 96))) %>% 
  filter((group == 1 & (FSIQ <=143 & FSIQ > 103)) | (group == 2 &(FSIQ <136 & FSIQ >= 96))) %>% 
  filter(id_new != 5 & id_new != 2416 & id_new!= 128 &id_new!= 39 & id_new!= 127
         &id_new!= 2 &id_new!= 8 &id_new!=10) %>% # 23和121? 39 125girl 128 no data in second run
  #filter(id_new!= 21 & id_new != 33 & id_new!= 2401 & id_new != 39) %>% 
  # mutate(new_group = ifelse(group == 1 & (FSIQ <=136 & FSIQ >= 103),"ASD",
  #                           ifelse(group == 2 &(FSIQ <=134 & FSIQ >= 96),"TD_low",
  #                                  ifelse(group == 2 & FSIQ >134,"TD_high","other")))) %>% 
  select(id_new,group,gender,name,age,FSIQ,AQ,SRS,forearm,hand,tactile,tactile_z,
         SPSRC_total,SPSRC_total_z,seeking,seeking_z,underresponse,overresponse,stability,
         tactile_over,tactile_under,tactile_seek,GEM_tot,GEM_affective,GEM_cognitive,ToM,tactile_threshold,pain_threshold) %>% 
  rename(id = id_new) %>% 
  as.data.table() %>% 
  setkey(id)
q_iq_sub <- nirs_info[q_iq_sub] %>% 
  setkey(sub)

asd_sub <- unlist(asd_beta$namelist)
asd_beta1 <-  as.data.frame(asd_beta$con1) %>% 
  add_column(sub = asd_sub,.before = "V1") %>% 
  add_column(con = "forearm",.before = "V1")
asd_beta2 <-  as.data.frame(asd_beta$con2) %>% 
  add_column(sub = asd_sub,.before = "V1") %>% 
  add_column(con = "hand",.before = "V1")

td_sub <- unlist(td_beta$namelist)
td_beta1 <-  as.data.frame(td_beta$con1) %>% 
  add_column(sub = td_sub,.before = "V1") %>% 
  add_column(con = "forearm",.before = "V1")
td_beta2 <-  as.data.frame(td_beta$con2) %>% 
  add_column(sub = td_sub,.before = "V1") %>% 
  add_column(con = "hand",.before = "V1")

beta <- rbind(rbind(asd_beta1,asd_beta2),rbind(td_beta1,td_beta2))
colnames(beta) <- c("sub","con",1:46)
beta <- 
  melt(as.data.table(beta), id = c("sub","con"),variable.name = "channel",value.name = "beta") %>% 
  as.data.table() %>%
  setkey(sub)
beta1 <- beta %>% group_by(sub,channel) %>% summarise(diff = beta[which(con == "forearm")]-beta[which(con == "hand")]) %>% 
  as.data.table() %>% 
  setkey(sub)
beta_tot <- beta[q_iq_sub, on = "sub"]
x <- beta_tot %>% mutate( id = ifelse(id>2000,id-2350,id)) %>% group_by(id,group,channel,FSIQ,age) %>% summarise(diff = beta[which(con == "forearm")] - beta[which(con == "hand")])
ggplot(data = x[which(x$channel==36),],aes(x = age,y = diff))+
  geom_point()+
  geom_text_repel(aes(age,diff,label = id))
x <- x[which(x$channel==36),]
cor.test(x$diff,x$FSIQ)
# anova------------
fit <- beta_tot %>% 
  # group_by(id,group,SRS,AQ,channel) %>%
  # summarise(b_diff = beta[which(con == "forearm")] - beta[which(con == "hand")]) %>% 
  group_by(channel) %>% 
  summarise(p = aov_car(beta ~ AQ + Error(id/con),data =.,observed = "group")$anova_table$`Pr(>F)`[1,1])
fit <- data.frame()
beta_tot1 <- beta_tot %>% 
  group_by(channel) %>% 
  mutate(group = ifelse(group == 1,"ASD","TD")) %>% 
  mutate(aq = scale(AQ),seek = scale(seeking),tactile = scale(tactile),spsrc = scale(SPSRC_total),
         scale_diff = scale(forearm-hand),srs = scale(SRS),iq = scale(FSIQ),tactile_seek = scale(tactile_seek),
         gem_tot = scale(GEM_tot),gem_aff = scale(GEM_affective),gem_cog = scale(GEM_cognitive),
         seeking_group = ifelse(seeking <= 144,"low", "high"))


for(i in 1:46){
  fit[i,1] <- i
  # fit[i,2:6] <- aov_car(beta ~ group+iq+Error(id/con),
  #                       data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = c("group","iq"))$anova_table$`Pr(>F)`
   fit[i,2:4] <- aov_car(beta ~ group+Error(id/con),
                         data =beta_tot1[which(beta_tot1$channel == i),], observed = c("group"))$anova_table$`Pr(>F)`
  # 
  # fit[i,5:7] <- aov_car(beta ~ seeking_group + Error(id/con),
  #                       data =beta_tot1[which(beta_tot1$channel == i),],observed = "seeking_group")$anova_table$`Pr(>F)`
  # fit[i,8:10] <- aov_car(beta ~ seek+iq + Error(id/con),
  #                       data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = c("seek","iq"))$anova_table$`Pr(>F)`
  # fit[i,11:13]<- aov_car(beta ~ aq + Error(id/con),
  #                    data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = "aq")$anova_table$`Pr(>F)`
  # fit[i,14:16]<- aov_car(beta ~ iq + Error(id/con),
  #                        data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = "iq")$anova_table$`Pr(>F)`
  # fit[i,17:19]<- aov_car(beta ~ srs + Error(id/con),
  #                        data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = "srs")$anova_table$`Pr(>F)`
  # fit[i,20:30]<- aov_car(beta ~ group + aq + seek + tactile +iq + Error(id/con),
  #                     data =beta_tot1[which(beta_tot1$channel == i),],factorize = F, observed = c("aq","group","seek","tactile","iq"))$anova_table$`Pr(>F)`
}
colnames(fit) <- c("channel","group","con","group_int")
colnames(fit) <- c("channel","seek","con","seek_con")
colnames(fit) <- c("channel","group","groupp_con","group_int","seeking_group","seeking_group_con","seeking_group_int",
                   "seek","seek_con","seek_int","aq","aq_con","aq_int","iq","iq_con","iq_int","srs","srs_con","srs_int")
x <- fit %>% 
  filter(group<= 0.05)
p.adjust(fit$group,method = "fdr")<0.05
b <- beta_tot1 %>% 
  filter(channel == 4)  # group:2 4 ; con: 1 5 10 11 14 15 18 24 29 31 40;int: 15 34 38
fit0 <- aov_car(beta ~ group + Error(id/con),data = b,  observed = "group")
fit0
pairs(emmeans(fit0,  ~ group))
pairs(emmeans(fit0,  ~ con|group))
pairs(emmeans(fit0,  ~ group|con))
emtrends(fit0, ~ con, var = "gem_cog") %>% pairs()
emtrends(fit0, ~ con, var = "gem_cog") %>% test()
emmip(fit0, con ~gem_aff,cov.reduce = quantile)+
  labs(title = "Channel 22")+
  theme_bw()
beta_group_diff <- beta_tot1 %>% filter(channel == 15| channel == 34| channel == 38| channel == 36 | channel == 40) %>% 
  group_by(channel) %>% 
  filter((beta > mean(beta,na.rm = T) - 3*sd(beta,na.rm = T)) & (beta< mean(beta,na.rm = T) +3*sd(beta,na.rm = T)))
ggplot(beta_group_diff,aes(x = group, y = beta*1e6, fill = con))+
  geom_violin()+
  #geom_jitter(aes(x = group,color = con))+
  facet_wrap(~channel,scales = "free_y")+
  theme_bw()
  
  

b <- beta_tot1 %>% 
  #filter(channel == 6|channel == 9|channel == 12|channel == 13|channel == 16) %>% 
  #filter(channel == 46|channel == 44|channel == 43|channel == 36|channel == 39|channel == 40) %>% 
  filter(channel == 16|channel == 20|channel == 21|channel == 17|channel == 23|channel == 13) %>%
  #filter(channel == 23|channel == 20) %>% 
  filter(id != 8) %>% 
  #filter(channel == 6|channel == 29|channel == 12|channel == 35|channel == 32|channel == 9) %>%
  #filter(channel == 10|channel == 11|channel == 14|channel == 15|channel == 18|channel == 33|channel== 34|channel ==  38|channel ==  30) %>%
  #filter(group == "TD") %>%
  #filter(con == "hand") %>% 
  mutate(gem_tot = scale(gem_tot),gem_aff = scale(gem_aff),gem_cog = scale(gem_cog),
         seek = scale(seeking),tactile = scale(tactile),spsrc = scale(SPSRC_total),tactile_seek = scale(tactile_seek)) %>% 
  group_by(id,group,gem_cog,gem_aff,gem_tot,seek,tactile,spsrc,tactile_seek,con,iq) %>% 
  summarise(beta = mean(beta,na.rm = T))

ggplot(b,aes(x = iq,y = beta, color= group))+
  geom_smooth(method = "lm")+
  geom_point()+
  facet_wrap(~channel,scales = "free_y")
  
fit1 <- aov_car(beta ~  group+Error(id/con),data = b,factorize = F)
fit1 <- lm(beta ~  group,data = b)
fit1 <- aov_car(beta ~  cog_group+Error(id/con),data = b)
fit1
summary(fit1)
cor.test(b$beta,b$seek)
y <- cor(b[,c("beta","seeking","tactile","tactile_seek","GEM_cognitive","GEM_tot","GEM_affective")])
testRes = cor.mtest(b[,c("beta","seeking","tactile","tactile_seek","GEM_cognitive","GEM_tot","GEM_affective")], conf.level = 0.95)
corrplot(y,p.mat = testRes$p,method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)
pairs(emmeans(fit1,  ~group))
emtrends(fit1, ~ con, var = "gem_cog") %>% pairs()
plot(sim_slopes(lm(beta ~  gem_aff*con,data = b),pred = gem_aff,modx = con))
interact_plot(lm(beta ~  gem_cog*con,data = b),pred = gem_cog,modx = con)
emtrends(fit1, ~con|seek, at = list(scale_diff_c = c(-1,0,1))) %>% pairs()
emmip(fit1,con|group ~seek,cov.reduce = quantile)
b1 <- b %>% group_by(gem_cog,aq,group,id) %>% 
  summarise(con_diff = beta[which(con == "forearm")]-beta[which(con == "hand")])
ggplot(beta_tot1,aes(x = gem_cog,y = beta,color = con))+
  geom_smooth(method = "lm",se = F)+
  facet_wrap(~channel,scales = "free_y")

ggplot(data = beta_tot1[which(beta_tot1$channel==40),],aes(x = aq,y = beta, color = con))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~channel, scales = "free_y")

ggplot(data = b1,aes(x = gem_cog,y = con_diff, color = group))+
  geom_point()+
  geom_smooth(method = "lm")

b<- beta_tot1 %>% 
  filter(con == "forearm",!is.na(forearm),channel == 40) %>% 
  mutate(forearm = scale(forearm), seek = scale(seek))

palm <- beta_tot1 %>% 
  filter(con == "hand"&group == "ASD" ) %>%  
  filter(!is.na(tactile_under),!is.na(beta)) %>% 
  # group_by(channel,id,tactile_seek) %>% 
  # summarise(diff = beta[which(con == "forearm")]-beta[which(con == "hand")]) %>% 
  group_by(channel) %>% 
  summarise(cor = cor(beta,tactile_under),p =  cor.test(beta,tactile_under)$p.value)
palm$p < 0.05
x <- beta_tot1 %>% 
  filter(channel == 36|channel == 40| channel == 41| channel == 44) %>% 
  #filter(!is.na(AQ),!is.na(beta)) %>% 
  group_by(id,channel,GEM_tot) %>% 
  #summarise(beta = mean(beta)*1e6)
  summarise(diff = 1e6*(beta[which(con == "forearm")]-beta[which(con == "hand")]))
ggplot(x,aes(GEM_tot,diff))+
  geom_point()+
  geom_smooth(method = "lm",se = F)+facet_wrap(~channel,scales = "free_y")+theme_bw()+labs(y = "Arm-Palm")
ggplot(beta_tot1[which(beta_tot1$channel == 36),],aes(GEM_tot,beta*1e6,color = con))+
  geom_point()+
  geom_smooth(method = "lm",se = F)+
  labs(title = "channel 36")+
  theme_bw()

# all sub---------
q_iq_total <- q_total %>% 
  # filter((group == 1 & (FSIQ <=143 & FSIQ >= 103)) | (group == 2 &(FSIQ <=138 & FSIQ >= 96))) %>% 
  # filter(id_new != 5 & id_new != 2416 &  id_new!= 2 & id_new!= 121 & id_new!= 128 & id_new!= 23 & id_new!= 21 & id_new != 33 & id_new!= 2401 & id_new != 39) %>% 
  select(id_new,group,gender,name,age,FSIQ,AQ,SRS,forearm,hand,tactile,tactile_z,
         SPSRC_total,SPSRC_total_z,seeking,seeking_z,underresponse,overresponse,stability,
         tactile_over,tactile_under,tactile_seek,GEM_tot,GEM_affective,GEM_cognitive,ToM,tactile_threshold,pain_threshold) %>% 
  rename(id = id_new) %>% 
  as.data.table() %>% 
  setkey(id)
q_iq_total <- nirs_info[q_iq_total] %>% 
  setkey(sub)
beta_tot_tot <- beta[q_iq_total, on = "sub"]
beta_tot1 <- beta_tot_tot %>% 
  group_by(channel) %>% 
  mutate(group = ifelse(group == 1,"ASD","TD")) %>% 
  mutate(aq = scale(AQ),seek = scale(seeking),tactile = scale(tactile),spsrc = scale(SPSRC_total),
         scale_diff = scale(forearm-hand),srs = scale(SRS),iq = scale(FSIQ),
         gem_tot = scale(GEM_tot),gem_aff = scale(GEM_affective),gem_cog = scale(GEM_cognitive),
         seeking_group = ifelse(seeking <= 144,"low", "high"))
y <- beta_tot1 %>% 
  filter(channel == 46|channel == 44|channel == 43|channel == 40|channel == 36) %>%
  filter(group == "TD") %>% 
  group_by(id,con,gem_cog,gem_aff,gem_tot) %>% 
  summarise(beta = mean(beta,na.rm = T)) %>% mutate(cog_group = ifelse(gem_cog <= -1,"low",ifelse(gem_cog <= 0,"mid","high")))
fit1 <- aov_car(beta ~  gem_cog+Error(id/con),data = y,factorize = F)
fit1
fit1 <- aov_car(beta ~ new_group + Error(id/con),data = y,  observed = "new_group")
emmeans(fit1, ~new_group|con) %>% pairs()
emmip(fit1, ~con|new_group)

y <- y %>% select(diff,iq,seek,spsrc,scale_diff,srs,tactile)
y <- y[,2:8]
M <- cor(y)
corrplot(M)
testRes = cor.mtest(y, conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
          diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))
