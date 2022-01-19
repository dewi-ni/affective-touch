
hbo_sts_wide <- spread(hbo_sts[,c(1,9,10)],time,con_diff)
hbo_sts_wide <- hbo_sts_wide %>% 
  ungroup() %>% mutate(id = as.character(id))


max_cluster_t <- function(df){
  sig <- df$p < .05
  sign <- df$t >= 0
  type <- 2 * sig + sign
  runLength <- rle(type) #0: -t non-sig，1: t non-sig，2: -t sig，3: t sig
  cluster <- rep(seq_along(runLength$values), runLength$lengths)
  cluster_t <- data.frame(t = df$t, sig, cluster) %>%
    filter(sig) %>%
    group_by(cluster) %>%
    summarise_at("t", list(cluster_statistic = ~ sum(t))) %>%
    pull(cluster_statistic)
  t <- cluster_t[which.max(abs(cluster_t))]
  ifelse(length(t) == 0, 0, t)
}
generate_t <- function(x){
  tmp_df <- hbo_sts_wide %>% 
    mutate(new_id = sample(id)) 
  tmp_df <-tmp_df[order(tmp_df$new_id),]
  tmp_df1 <-data.frame(tmp_df[1:23,],group = 1)    
  tmp_df2 <-data.frame(tmp_df[24:46,],group = 2)
  tmp_df <- rbind(tmp_df1,tmp_df2) %>% 
    as.data.frame() %>% 
    reshape2::melt(id.vars = c('new_id','group'),
                   measure.vars = 2:243,
                   variable.name='time',
                   value.name='response')
  df <- tmp_df %>% group_by(time) %>% 
    summarise(t = t.test(response~group)$statistic,
              p = t.test(response~group)$p.value)
  max_cluster_t(df)
  
}
set.seed(123)
perm_t_dist <- parallel::mclapply(1:1000,generate_t)
perm_t_dist <- lapply(1:1000,generate_t)
perm_t_dist <- unlist(perm_t_dist)
hist(perm_t_dist,breaks = 50)


# group
t <- hbo_sts %>% 
  group_by(id,group,time) %>% 
  summarise(con_diff = mean(con_diff)) %>% 
  group_by(time) %>% 
  summarise(t = t.test(con_diff~group)$statistic,
            p = t.test(con_diff~group)$p.value) %>% 
  ungroup()
sig <- t$p < .05
sign <- t$t >= 0
type <- 2 * sig + sign
runLength <- rle(type)
cluster <- rep(seq_along(runLength$values), runLength$lengths)
cluster_t <- data.frame(t = t$t, sig, cluster) %>%
  filter(sig) %>%
  group_by(cluster) %>%
  summarise_at("t", list(cluster_statistic = ~ sum(t))) %>%
  pull(cluster_statistic)
cluster_t
t.test(perm_t_dist,mu = cluster_t) #33-40
table(cluster)


# tb data----------
setwd("..")
np <- read_csv("./Pupil_for_permutation_p_2014(1).csv")[,c(3,43:247)]
colnames(np) <- c("id",2:206)
np <- np %>% mutate(id = as.character(id))

p <- read_csv("./Pupil_for_permutation_p_2014(1).csv")[,c(3,43:247)]
colnames(p) <- c("id",2:206)
p <- p %>% mutate(id = as.character(id))

max_cluster_t <- function(df){
  sig <- df$p < .05
  sign <- df$t >= 0
  type <- 2 * sig + sign
  runLength <- rle(type) #0: -t non-sig，1: t non-sig，2: -t sig，3: t sig
  cluster <- rep(seq_along(runLength$values), runLength$lengths)
  cluster_t <- data.frame(t = df$t, sig, cluster) %>%
    filter(sig) %>%
    group_by(cluster) %>%
    summarise_at("t", list(cluster_statistic = ~ sum(t))) %>%
    pull(cluster_statistic)
  t <- cluster_t[which.max(abs(cluster_t))]
  ifelse(length(t) == 0, 0, t)
}
generate_t <- function(x){
  tmp_df <- p %>% 
    mutate(new_id = sample(id)) 
  tmp_df <-tmp_df[order(tmp_df$new_id),]
  tmp_df1 <-data.frame(tmp_df[1:23,],group = "1")    
  tmp_df2 <-data.frame(tmp_df[24:46,],group = "2")
  tmp_df <- rbind(tmp_df1,tmp_df2) %>% 
    as.data.frame() %>% 
    reshape2::melt(id.vars = c('new_id','group'),
                   measure.vars = 2:206,
                   variable.name='time',
                   value.name='response')
  df <- tmp_df %>% filter(!is.na(response)) %>% 
    group_by(time) %>% 
    summarise(t = t.test(response~group)$statistic,
              p = t.test(response~group)$p.value)
  max_cluster_t(df)
  
}
set.seed(123)
perm_t_dist <- parallel::mclapply(1:1000,generate_t,mc.cores = 6)
perm_t_dist <- unlist(perm_t_dist)
hist(perm_t_dist,breaks = 50)


# group
np1 <- read_csv("./Pupil_for_permutation_np_2014.csv")[,c(1,3,43:247)]
colnames(np1) <- c("group","id",3:207)
np1 <- np1 %>% mutate(group = as.character(group),id = as.character(id))
np2 <- np1 %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = c('id','group'),
                 measure.vars = 3:207,
                 variable.name='time',
                 value.name='response')
t <- np2 %>% 
  group_by(time) %>% 
  summarise(t = t.test(response~group)$statistic,
            p = t.test(response~group)$p.value) %>% 
  ungroup()

p1 <- read_csv("./Pupil_for_permutation_p_mm2.csv")[,c(1,3,43:247)]
colnames(p1) <- c("group","id",3:207)
p1 <- p1 %>% mutate(group = as.character(group),id = as.character(id))
p2 <- p1 %>% 
  as.data.frame() %>% 
  reshape2::melt(id.vars = c('id','group'),
                 measure.vars = 3:207,
                 variable.name='time',
                 value.name='response')
t <- p2 %>% 
  group_by(time) %>% 
  summarise(t = t.test(response~group)$statistic,
            p = t.test(response~group)$p.value) %>% 
  ungroup()
sig <- t$p < .05
sign <- t$t >= 0
type <- 2 * sig + sign
runLength <- rle(type)
cluster <- rep(seq_along(runLength$values), runLength$lengths)
cluster_t <- data.frame(t = t$t, sig, cluster) %>%
  filter(sig) %>%
  group_by(cluster) %>%
  summarise_at("t", list(cluster_statistic = ~ sum(t))) %>%
  pull(cluster_statistic)
cluster_t
t.test(perm_t_dist,mu = cluster_t[17]) #1 3 5 6 7 9 10 11 15 16 17
table(cluster)

p2 <- p2 %>% 
  mutate(time = as.numeric(time))
draw_p <- Rmisc::summarySE(data = p2,"response",groupvars = c("time","group"),na.rm = T)
ggplot(draw_p , aes(x = time, y = response,color = group))+
  geom_line()+
  geom_ribbon(aes(ymax = response+se,ymin = response-se,fill = group),alpha = 0.25,linetype=0)
