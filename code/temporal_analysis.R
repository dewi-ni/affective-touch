quality <- read.mat("../TDqualityReport_basedNaN.mat")

quality1 <- as.data.frame(do.call(rbind,quality[[1]]))
td_good <- run_tot %>%  filter(response == NaN)
td_good <- td_good %>% mutate(time = time/3.47-5) %>% group_by(time,channel,con) %>% summarise(response = mean(response,na.rm = T))


path <-
  "./data/ASDBlockAverageFiles"
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
        hbr[(forearm_marker[i] - round(2 * 3.47)):(forearm_marker[i] + round(16 * 3.47)), ]%>%  #hbo/hbr/hbt
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
      mutate(response = response - mean(response,na.rm = T)) %>%
      mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      ungroup()
    run <-
      run %>%  add_column(con = "forearm",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
    
    run <- data.frame()
    for (i in 1:length(hand_marker)) {
      tmp <-
        hbr[(hand_marker[i] - round(2 * 3.47)):(hand_marker[i] + round(16 * 3.47)), ] %>%  #hbo/hbr/hbt
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
      mutate(response = response - mean(response,na.rm = T)) %>%
      mutate(response_ave = c(rep(NA,2), RcppRoll::roll_mean(response, n = 5, by = 1),rep(NA,2))) %>%
      ungroup()
    run <-
      run %>%  add_column(con = "hand",.before = "time") %>% add_column(sub = sub, .before = "block") %>% add_column(run = j, .before = "sub")
    run_tot <- rbind(run_tot,run)
  }
  setwd("..")
}

# td40 <- run_tot %>% filter(sub == "2021-10-23_008")
# td_40_mean <- td40 %>% group_by(con,time,channel) %>% summarise(response = mean(response_ave,na.rm = T)) %>% mutate(time = time/3.47-5)
# ggplot(td_40_mean,aes(x = time,y = response,color = con)) + geom_line()+facet_wrap(~channel,scales = "free_y")+
#   geom_vline(aes(xintercept = 0))+
#   geom_vline(aes(xintercept = 6))+
#   theme_bw()


run_tot1 <- run_tot %>% 
  group_by(sub,con,channel,time) %>% 
  summarise(response = mean(response,na.rm = T))
run_ASD <- run_tot1
run_TD <- run_tot1
run_hbo <- rbind(run_ASD,run_TD) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)
run_hbr <- rbind(run_ASD,run_TD) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  setkey(sub)


save(run_hbo,file = "./output/run_hbo.Rdata")
save(run_hbr,file = "./output/run_hbo.Rdata")
