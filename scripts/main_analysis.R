#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Main analysis script
#+  -- This script first screens each of the five meta-analyses originally
#+  -- reported by Chua et al. (2021). Then we fit robust Bayesian meta-
#+  -- analyses to each meta-analysis including all studies and with
#+  -- outliers removed.
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#| SCRIPT SETUP ----

source("scripts/libraries.R")

dat <- readr::read_csv(file = "data/12-07_ef_meta_data.csv")

##| separate the original meta-analyses by Chua et al.
ret <- filter(dat, dat$`Type_of_Meta-Analysis` == "Retention")
tra <- filter(dat, dat$`Type_of_Meta-Analysis` == "Transfer")
per <- filter(dat, dat$`Type_of_Meta-Analysis` == "Performance")
emg <- filter(dat, dat$`Type_of_Meta-Analysis` == "EMG")
dist <- filter(dat, dat$`Type_of_Meta-Analysis` == "Distance effect")


#| RETENTION META-ANALYSIS ----
res <- rma(yi = ret$Hedges_g,
           sei = ret$Standard_Error,
           data = ret)
res

##| check for influential cases and remove any potential outliers
inf <- influence(res)
plot(inf)

retc <- ret[-c(5, 34),]

##| fit random effects model with outliers removed
res2 <- rma(yi = retc$Hedges_g,
            sei = retc$Standard_Error,
            data = retc)
res2

##| fit RoBMA model with all cases then save posterior for histogram
d1 <- ret$Hedges_g
se1 <- ret$Standard_Error
d2 <- retc$Hedges_g
se2 <- retc$Standard_Error
rob1 <- RoBMA(d = d1,
              se = se1,
              model_type = "PSMA",
              seed = 1,
              parallel = TRUE)

ret_post <- as.vector((rob1[[9]][[3]][[1]]))
ret_post <- as_tibble(ret_post)

# write_csv(ret_post, file = "data/figs_ret_post.csv")

###| make summary objects
rob1_models <- summary(rob1, type = "models")
rob1_sum <- summary(rob1)

###| save outputs
# saveRDS(rob1_models, file = "data/ret1_models.rds")
# saveRDS(rob1_sum, file = "data/ret1_sum.rds")

###| read outputs
readRDS(file = "data/ret1_models.rds")
readRDS(file = "data/ret1_sum.rds")

##| fit RoBMA model with outliers removed then save posterior for histogram
rob2 <- RoBMA(d = d2,
              se = se2,
              model_type = "PSMA",
              set.seed(1),
              parallel = TRUE)

ret_post_nol <- as.vector((rob2[[9]][[3]][[1]]))
ret_post_nol <- as_tibble(ret_post_nol)

# write_csv(ret_post_nol, file = "data/figs_ret_post_nol.csv")

###| make summary objects
rob2_models <- summary(rob2, type = "models")
rob2_sum <- summary(rob2)

###| save outputs
# saveRDS(rob2_models, file = "data/ret2_models.rds")
# saveRDS(rob2_sum, file = "data/ret2_sum.rds")

###| read outputs
readRDS(file = "data/ret2_models.rds")
readRDS(file = "data/ret2_sum.rds")


#| PERFORMANCE META-ANALYSIS ----
per_res <- rma(yi = per$Hedges_g,
               sei = per$Standard_Error,
               data = per)
per_res

##| check for influential cases and remove any potential outliers
per_inf <- influence(per_res)
plot(per_inf)

perc <- per[-c(51, 58, 69, 83),]

##| fit random effects model with outliers removed
per_res2 <- rma(yi = perc$Hedges_g,
                sei = perc$Standard_Error,
                data = perc)
per_res2

##| fit RoBMA model with all cases then save posterior for histogram
dp <- per$Hedges_g
sep <- per$Standard_Error
dp2 <- perc$Hedges_g
sep2 <- perc$Standard_Error
robp <- RoBMA(d = dp,
              se = sep,
              model_type = "PSMA",
              seed = 1,
              parallel = TRUE)

per_post <- as.vector((robp[[9]][[3]][[1]]))
per_post <- as_tibble(per_post)

# write_csv(per_post, file = "data/figs_per_post.csv")

###| make summary objects
robp_models <- summary(robp, type = "models")
robp_sum <- summary(robp)

###| save outputs
# saveRDS(robp_models, file = "data/per_models.rds")
# saveRDS(robp_sum, file = "data/per_sum.rds")

###| read outputs
readRDS(file = "data/per_models.rds")
readRDS(file = "data/per_sum.rds")

##| fit RoBMA model with outliers removed then save posterior for histogram
robp2 <- RoBMA(d = dp2,
               se = sep2,
               model_type = "PSMA",
               seed = 1,
               parallel = TRUE)

per_post_nol <- as.vector((robp2[[9]][[3]][[1]]))
per_post_nol <- as_tibble(per_post_nol)

# write_csv(per_post_nol, file = "data/figs_per_post_nol.csv")

###| make summary objects
robp2_models <- summary(robp2, type = "models")
robp2_sum <- summary(robp2)

###| save outputs
# saveRDS(robp2_models, file = "data/per2_models.rds")
# saveRDS(robp2_sum, file = "data/per2_sum.rds")

###| read outputs
readRDS(file = "data/per2_models.rds")
readRDS(file = "data/per2_sum.rds")


#| TRANSFER META-ANALYSIS ----
tra_res <- rma(yi = tra$Hedges_g,
               sei = tra$Standard_Error,
               data = tra)
tra_res
##> Note: these results do not quite match the analysis reported
##> by Chua and colleagues in their meta-analysis

##| check for influential cases and remove any potential outliers
tra_inf <- influence(tra_res)
plot(tra_inf)

trac <- tra[-c(19),]

##| fit random effects model with outliers removed
tra_res2 <- rma(yi = trac$Hedges_g,
                sei = trac$Standard_Error,
                data = trac)
tra_res2

##| fit RoBMA model with all cases then save posterior for histogram
dt <- tra$Hedges_g
set <- tra$Standard_Error
dt2 <- trac$Hedges_g
set2 <- trac$Standard_Error
robt <- RoBMA(d = dt,
              se = set,
              model_type = "PSMA",
              seed = 1,
              parallel = TRUE)

tra_post <- as.vector((robt[[9]][[3]][[1]]))
tra_post <- as_tibble(tra_post)

# write_csv(tra_post, file = "data/figs_tra_post.csv")

###| make summary objects
robt_models <- summary(robt, type = "models")
robt_sum <- summary(robt)

###| save outputs
# saveRDS(robt_models, file = "data/tra_models.rds")
# saveRDS(robt_sum, file = "data/tra_sum.rds")

###| read outputs
readRDS(file = "data/tra_models.rds")
readRDS(file = "data/tra_sum.rds")

##| fit RoBMA model with outliers removed then save posterior for histogram
robt2 <- RoBMA(d = dt2,
               se = set2,
               model_type = "PSMA",
               seed = 1,
               parallel = TRUE)

tra_post_nol <- as.vector((robt2[[9]][[3]][[1]]))
tra_post_nol <- as_tibble(tra_post_nol)

# write_csv(tra_post_nol, file = "data/figs_tra_post_nol.csv")

###| make summary objects
robt2_models <- summary(robt2, type = "models")
robt2_sum <- summary(robt2)

###| save outputs
# saveRDS(robt2_models, file = "data/tra2_models.rds")
# saveRDS(robt2_sum, file = "data/tra2_sum.rds")

###| read outputs
readRDS(file = "data/tra2_models.rds")
readRDS(file = "data/tra2_sum.rds")


#| DISTANCE EFFECT META-ANALYSIS ----
dist_res <- rma(yi = dist$Hedges_g,
                sei = dist$Standard_Error,
                data = dist)
dist_res

##| check for influential cases and remove any potential outliers
dist_inf <- influence(dist_res)
plot(dist_inf)

distc <- dist[-c(8),]

##| fit random effects model with outliers removed
dist_res2 <- rma(yi = distc$Hedges_g,
                 sei = distc$Standard_Error,
                 data = distc)
dist_res2

##| fit RoBMA model with all cases then save posterior for histogram
dd <- dist$Hedges_g
sed <- dist$Standard_Error
dd2 <- distc$Hedges_g
sed2 <- distc$Standard_Error
robd <- RoBMA(d = dd,
              se = sed,
              model_type = "PSMA",
              seed = 1,
              parallel = TRUE)

dist_post <- as.vector((robd[[9]][[3]][[1]]))
dist_post <- as_tibble(dist_post)

# write_csv(dist_post, file = "data/figs_dist_post.csv")

###| make summary objects
robd_models <- summary(robd, type = "models")
robd_sum <- summary(robd)

###| save outputs
# saveRDS(robd_models, file = "data/dis_models.rds")
# saveRDS(robd_sum, file = "data/dis_sum.rds")

###| read outputs
readRDS(file = "data/dis_models.rds")
readRDS(file = "data/dis_sum.rds")

##| fit RoBMA model with outliers removed then save posterior for histogram
robd2 <- RoBMA(d = dd2,
               se = sed2,
               model_type = "PSMA",
               seed = 1,
               parallel = TRUE)

dist_post_nol <- as.vector((robd2[[9]][[3]][[1]]))
dist_post_nol <- as_tibble(dist_post_nol)

# write_csv(dist_post_nol, file = "data/figs_dist_post_nol.csv")

###| make summary objects
robd2_models <- summary(robd2, type = "models")
robd2_sum <- summary(robd2)

###| save outputs
# saveRDS(robd2_models, file = "data/dis2_models.rds")
# saveRDS(robd2_sum, file = "data/dis2_sum.rds")

###| read outputs
readRDS(file = "data/dis2_models.rds")
readRDS(file = "data/dis2_sum.rds")


#| ELECTROMYOGRAPHY (EMG) META-ANALYSIS ----
emg_res <- rma(yi = emg$Hedges_g,
               sei = emg$Standard_Error,
               data = emg)
emg_res

##| check for influential cases and remove any potential outliers
emg_inf <- influence(emg_res)
plot(emg_inf) ##> none were identified

##| fit RoBMA model with all cases then save posterior for histogram
dem <- emg$Hedges_g
seem <- emg$Standard_Error
robem <- RoBMA(d = dem,
               se = seem,
               model_type = "PSMA",
               seed = 1,
               parallel = TRUE)

emg_post <- as.vector((robem[[9]][[3]][[1]]))
emg_post <- as_tibble(emg_post)

# write_csv(emg_post, file = "data/figs_emg_post.csv")

###| make summary objects
robem_models <- summary(robem, type = "models")
robem_sum <- summary(robem)

###| save outputs
# saveRDS(robem_models, file = "data/emg_models.rds")
# saveRDS(robem_sum, file = "data/emg_sum.rds")

###| read outputs
readRDS(file = "data/emg_models.rds")
readRDS(file = "data/emg_sum.rds")
