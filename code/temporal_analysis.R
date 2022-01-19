library(tidyverse)
library(data.table)
library(dplyr)
library(rmatio)
library(ggplot2)
library(stringr)

# ASD data
path <-
  "./data/ASDBlockAverageFiles"
folderNames <- dir(path)
setwd(path)
run_tot <- data.frame()
for (j in 1:length(folderNames)) {
  folder <- folderNames[j]
  path <- paste(".",folder,sep = "/")
  setwd(path)
  subNames <- dir(getwd())
  for (file in subNames) {
    sub <- str_sub(file, 1, 14)
    nirs <- read.mat(file)
    hbr <- nirs[[1]]
    hbo <- nirs[[2]]
    hbt <- nirs[[4]]
    marker <- as.data.frame(nirs[[3]])
    forearm_marker <- which(marker$V1 == 1)
    hand_marker <- which(marker$V2 == 1)
    run <- data.frame()
    for (i in 1:length(forearm_marker)) {
      tmp <-
        hbo[(forearm_marker[i] - round(2 * 3.47)):(forearm_marker[i] + round(18 * 3.47)), ]%>%  #hbo/hbr/hbt
        as.data.table() %>% 
        add_column(block = i+(j-1)*5, .before = "V1")
      tmp <- tmp %>% 
        add_column(time = 1:nrow(tmp), .before = "V1")
      run <- rbind(run,tmp)
    }
    colnames(run) <- c("block", "time", 1:46)
    run <-
      melt(
        run,
        measure = 3:48,
        id = c("block","time"),
        variable.name = "channel",
        value.name = 'response'
      ) %>%
      group_by(block,channel) %>%
      mutate(response = response - response[which(time == 7)]) %>%
      mutate(diff = response - lag(response)) %>% 
      mutate(block_valid = ifelse(abs(diff) > 1/3.47*(1e-06*1/0.2),FALSE,TRUE)) %>% 
      # filter(block_valid == T) %>% 
      # filter(length(time) == 69) %>% 
      #mutate(response = response - mean(response,na.rm = T)) %>%
      #mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      #mutate(response_ave = unlist(ksmooth(time,response,bandwidth = 1,n.points = 70)[2])) %>%
      ungroup() 
    run <-
      run %>%  add_column(con = "forearm",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
    
    run <- data.frame()
    for (i in 1:length(hand_marker)) {
      tmp <-
        hbo[(hand_marker[i] - round(2 * 3.47)):(hand_marker[i] + round(18 * 3.47)), ] %>%  #hbo/hbr/hbt
        as.data.table() %>% 
        add_column(block = i+(j-1)*5, .before = "V1")
      tmp <- tmp %>% 
        add_column(time = 1:nrow(tmp), .before = "V1")
      run <- rbind(run,tmp)
    }
    colnames(run) <- c("block", "time", 1:46)
    run <-
      melt(
        run,
        measure = 3:48,
        id = c("block","time"),
        variable.name = "channel",
        value.name = 'response'
      ) %>%
      group_by(block,channel) %>%
      mutate(response = response - response[which(time == 7)]) %>%
      mutate(diff = response - lag(response)) %>% 
      mutate(block_valid = ifelse(abs(diff) > 1/3.47*(1e-06*1/0.2),FALSE,TRUE)) %>% 
      # filter(block_valid == T) %>% 
      # filter(length(time) == 69) %>% 
      #mutate(response = response - mean(response,na.rm = T)) %>%
      #mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      #mutate(response_ave = unlist(ksmooth(time,response,bandwidth = 1,n.points = 70)[2])) %>%
      ungroup() 
    run <-
      run %>%  add_column(con = "hand",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
  }
  setwd("..")
}
run_tot1 <- run_tot %>% 
  group_by(sub,con,channel,time) %>% 
  summarise(response = mean(response_ave,na.rm = T))
run_ASD <- run_tot1

# TD data
setwd("../..")
path <-
  "./data/TDBlockAverageFiles"
folderNames <- dir(path)
setwd(path)
run_tot <- data.frame()
for (j in 1:length(folderNames)) {
  folder <- folderNames[j]
  path <- paste(".",folder,sep = "/")
  setwd(path)
  subNames <- dir(getwd())
  for (file in subNames) {
    sub <- str_sub(file, 1, 14)
    nirs <- read.mat(file)
    hbr <- nirs[[1]]
    hbo <- nirs[[2]]
    hbt <- nirs[[4]]
    marker <- as.data.frame(nirs[[3]])
    forearm_marker <- which(marker$V1 == 1)
    hand_marker <- which(marker$V2 == 1)
    run <- data.frame()
    for (i in 1:length(forearm_marker)) {
      tmp <-
        hbo[(forearm_marker[i] - round(2 * 3.47)):(forearm_marker[i] + round(18 * 3.47)), ]%>%  #hbo/hbr/hbt
        as.data.table() %>% 
        add_column(block = i+(j-1)*5, .before = "V1")
      tmp <- tmp %>% 
        add_column(time = 1:nrow(tmp), .before = "V1")
      run <- rbind(run,tmp)
    }
    colnames(run) <- c("block", "time", 1:46)
    run <-
      melt(
        run,
        measure = 3:48,
        id = c("block","time"),
        variable.name = "channel",
        value.name = 'response'
      ) %>%
      group_by(block,channel) %>%
      mutate(response = response - response[which(time == 7)]) %>%
      mutate(diff = response - lag(response)) %>% 
      mutate(block_valid = ifelse(abs(diff) > 1/3.47*(1e-06*1/0.2),FALSE,TRUE)) %>% 
      #mutate(response = response - mean(response,na.rm = T)) %>%
      #mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      #mutate(response_ave = unlist(ksmooth(time,response,bandwidth = 1,n.points = 70)[2])) %>%
      ungroup() 
    run <-
      run %>%  add_column(con = "forearm",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
    
    run <- data.frame()
    for (i in 1:length(hand_marker)) {
      tmp <-
        hbo[(hand_marker[i] - round(2 * 3.47)):(hand_marker[i] + round(18 * 3.47)), ] %>%  #hbo/hbr/hbt
        as.data.table() %>% 
        add_column(block = i+(j-1)*5, .before = "V1")
      tmp <- tmp %>% 
        add_column(time = 1:nrow(tmp), .before = "V1")
      run <- rbind(run,tmp)
    }
    colnames(run) <- c("block", "time", 1:46)
    run <-
      melt(
        run,
        measure = 3:48,
        id = c("block","time"),
        variable.name = "channel",
        value.name = 'response'
      ) %>%
      group_by(block,channel) %>%
      mutate(response = response - response[which(time == 7)]) %>%
      mutate(diff = response - lag(response)) %>% 
      mutate(block_valid = ifelse(abs(diff) > 1/3.47*(1e-06*1/0.2),FALSE,TRUE)) %>% 
      #mutate(response = response - mean(response,na.rm = T)) %>%
      #mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      #mutate(response_ave = unlist(ksmooth(time,response,bandwidth = 1,n.points = 70)[2])) %>%
      ungroup() 
    run <-
      run %>%  add_column(con = "hand",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
  }
  setwd("..")
}

x <- run_tot %>% group_by(channel,con,time)  %>% summarise(response = mean(response,na.rm = T))
x1 <- Rmisc::summarySE(data = run_tot,"response",groupvars = c("time","con","channel"),na.rm = T)
ggplot(x1, aes(x = time , y = response,color = con))+
  geom_line()+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = as.factor(con)),alpha = 0.25,linetype=0)+
  facet_wrap(~channel,scales = "free_y")
z <- run_tot %>% filter(channel == 40) %>% group_by(sub,con,time) %>% summarise(response = mean(response,na.rm = T))
ggplot(z, aes(x = time , y = response,color = con))+geom_line()+facet_wrap(~sub,scales = "free_y")
z1 <- run_tot %>% 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)
z1 <- z1[q_iq_sub, on = "sub"]
z2 <- z1 %>%  filter(channel == 2) %>% group_by(sub,con,time) %>% summarise(response = mean(response,na.rm = T))
ggplot(z2, aes(x = time , y = response,color = con))+geom_line()+
  facet_wrap(~sub,scales = "free_y")

run_tot1 <- run_tot %>% 
  group_by(sub,con,channel,time) %>% 
  summarise(response = mean(response_ave,na.rm = T))
run_tot1 <- y %>% 
  group_by(con,channel,time) %>% 
  summarise(response = mean(response,na.rm = T))
ggplot(run_tot1, aes(x = time , y = response,color = con))+geom_line()+facet_wrap(~channel,scales = "free_y")
run_TD <- run_tot1


run_hbo <- rbind(run_ASD,run_TD) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)
run_hbr <- rbind(run_ASD,run_TD) %>% # change the code "hbo" from row 118 & row 145 to "hbr" 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)


save(run_hbo,file = "../../output/run_hbo.Rdata")
save(run_hbr,file = "../../output/run_hbr.Rdata")

x <- run_tot %>% filter(block_valid == F)
x <- x %>% select(sub,block,channel,con) 
x1<-x[!duplicated(x, fromLast=TRUE), ]
x2 <- x1 %>% 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)
x2 <- x2[q_iq_sub, on = "sub"] %>% filter(!is.na(block))
x2 <- x2 %>% 
  group_by(sub,id,con,block,channel) %>% 
  summarise(block_invalid1 = ifelse(id%%2 == 1,ifelse(con == "forearm",block*2-1,block*2),
                                 ifelse(con == "forearm",block*2,block*2-1))) %>% 
  ungroup() 
table(run_ASD_valid_block$sub)
