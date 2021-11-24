
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
  tmp_df1 <-data.frame(tmp_df[1:33,],group = 1)    
  tmp_df2 <-data.frame(tmp_df[34:66,],group = 2)
  tmp_df <- rbind(tmp_df1,tmp_df2) %>% 
    as.data.frame() %>% 
    reshape2::melt(id.vars = c('new_id','group'),
                   measure.vars = 2:71,
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
