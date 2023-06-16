#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Summary of results to programmatically read into manuscript.
#+
#+ Author(s):
#+  -- Brad McKay
#+  -- Mike Carter
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

##| Load RoBMA results (all studies)
ret1_models <- readRDS(file = "../../data/ret1_models.rds")
ret1_sum <- readRDS(file = "../../data/ret1_sum.rds")

per1_models <- readRDS(file = "../../data/per_models.rds")
per1_sum <- readRDS(file = "../../data/per_sum.rds")

tra1_models <- readRDS(file = "../../data/tra_models.rds")
tra1_sum <- readRDS(file = "../../data/tra_sum.rds")

dis1_models <- readRDS(file = "../../data/dis_models.rds")
dis1_sum <- readRDS(file = "../../data/dis_sum.rds")

emg_models <- readRDS(file = "../../data/emg_models.rds")
emg_sum <- readRDS(file = "../../data/emg_sum.rds")

##| Load RoBMA results (outliers removed). EMG did not have any outliers.
ret2_models <- readRDS(file = "../../data/ret2_models.rds")
ret2_sum <- readRDS(file = "../../data/ret2_sum.rds")

per2_models <- readRDS(file = "../../data/per2_models.rds")
per2_sum <- readRDS(file = "../../data/per2_sum.rds")

tra2_models <- readRDS(file = "../../data/tra2_models.rds")
tra2_sum <- readRDS(file = "../../data/tra2_sum.rds")

dis2_models <- readRDS(file = "../../data/dis2_models.rds")
dis2_sum <- readRDS(file = "../../data/dis2_sum.rds")

##| Load moderator analysis script
moderator_res <- readRDS(file = "../../data/moderator_res.rds")
moderator_est <- readRDS(file = "../../data/moderator_est.rds")


#| RETENTION META-ANALYSIS ----

##| hedges' g
###| all studies
retention_mean <- round(ret1_sum$estimates$Mean[1], digits = 2)
retention_lb <- round(ret1_sum$estimates$`0.025`[1], digits = 2)
retention_ub <- round(ret1_sum$estimates$`0.975`[1], digits = 2)

###| outliers removed
retention2_mean <- round(ret2_sum$estimates$Mean[1], digits = 2)
retention2_lb <- round(ret2_sum$estimates$`0.025`[1], digits = 2)
retention2_ub <- round(ret2_sum$estimates$`0.975`[1], digits = 2)

##| tau estimate
###| all studies
retention_tau <- round(ret1_sum$estimates$Mean[2], digits = 2)
retention_tlb <- round(ret1_sum$estimates$`0.025`[2], digits = 2)
retention_tub <- round(ret1_sum$estimates$`0.975`[2], digits = 2)

###| outliers removed
retention2_tau <- round(ret2_sum$estimates$Mean[2], digits = 2)
retention2_tlb <- round(ret2_sum$estimates$`0.025`[2], digits = 2)
retention2_tub <- round(ret2_sum$estimates$`0.975`[2], digits = 2)

##| bayes factor
###| all studies
retention_BF10 <- round(ret1_sum$components$inclusion_BF[[1]], digits = 2)
retention_BFrf <- round(ret1_sum$components$inclusion_BF[[2]], digits = 2)
retention_BFpb <- round(ret1_sum$components$inclusion_BF[[3]], digits = 2)

###| outliers removed
retention2_BF10 <- round(ret2_sum$components$inclusion_BF[[1]], digits = 2)
retention2_BFrf <- round(ret2_sum$components$inclusion_BF[[2]], digits = 2)
retention2_BFpb <- round(ret2_sum$components$inclusion_BF[[3]], digits = 2)


#| PERFORMANCE META-ANALYSIS ----

##| hedges' g
###| all studies
performance_mean <- round(per1_sum$estimates$Mean[1], digits = 2)
performance_lb <- round(per1_sum$estimates$`0.025`[1], digits = 2)
performance_ub <- round(per1_sum$estimates$`0.975`[1], digits = 2)

###| outliers removed
performance2_mean <- round(per2_sum$estimates$Mean[1], digits = 2)
performance2_lb <- round(per2_sum$estimates$`0.025`[1], digits = 2)
performance2_ub <- round(per2_sum$estimates$`0.975`[1], digits = 2)

##| tau estimate
###| all studies
performance_tau <- round(per1_sum$estimates$Mean[2], digits = 2)
performance_tlb <- round(per1_sum$estimates$`0.025`[2], digits = 2)
performance_tub <- round(per1_sum$estimates$`0.975`[2], digits = 2)

###| outliers removed
performance2_tau <- round(per2_sum$estimates$Mean[2], digits = 2)
performance2_tlb <- round(per2_sum$estimates$`0.025`[2], digits = 2)
performance2_tub <- round(per2_sum$estimates$`0.975`[2], digits = 2)

##| bayes factor
###| all studies
performance_BF10 <- round(per1_sum$components$inclusion_BF[[1]], digits = 2)
performance_BFrf <- round(per1_sum$components$inclusion_BF[[2]], digits = 2)
performance_BFpb <- round(per1_sum$components$inclusion_BF[[3]], digits = 2)

###| outliers removed
performance2_BF10 <- round(per2_sum$components$inclusion_BF[[1]], digits = 2)
performance2_BFrf <- round(per2_sum$components$inclusion_BF[[2]], digits = 2)
performance2_BFpb <- round(per2_sum$components$inclusion_BF[[3]], digits = 2)


#| TRANSFER META-ANALYSIS ----

##| hedges' g
###| all studies
transfer_mean <- round(tra1_sum$estimates$Mean[1], digits = 2)
transfer_lb <- round(tra1_sum$estimates$`0.025`[1], digits = 2)
transfer_ub <- round(tra1_sum$estimates$`0.975`[1], digits = 2)

###| outliers removed
transfer2_mean <- round(tra2_sum$estimates$Mean[1], digits = 2)
transfer2_lb <- round(tra2_sum$estimates$`0.025`[1], digits = 2)
transfer2_ub <- round(tra2_sum$estimates$`0.975`[1], digits = 2)

##| tau estimate
###| all studies
transfer_tau <- round(tra1_sum$estimates$Mean[2], digits = 2)
transfer_tlb <- round(tra1_sum$estimates$`0.025`[2], digits = 2)
transfer_tub <- round(tra1_sum$estimates$`0.975`[2], digits = 2)

###| outliers removed
transfer2_tau <- round(tra2_sum$estimates$Mean[2], digits = 2)
transfer2_tlb <- round(tra2_sum$estimates$`0.025`[2], digits = 2)
transfer2_tub <- round(tra2_sum$estimates$`0.975`[2], digits = 2)

##| bayes factor
###| all studies
transfer_BF10 <- round(tra1_sum$components$inclusion_BF[[1]], digits = 2)
transfer_BFrf <- round(tra1_sum$components$inclusion_BF[[2]], digits = 2)
transfer_BFpb <- round(tra1_sum$components$inclusion_BF[[3]], digits = 2)

###| outliers removed
transfer2_BF10 <- round(tra2_sum$components$inclusion_BF[[1]], digits = 2)
transfer2_BFrf <- round(tra2_sum$components$inclusion_BF[[2]], digits = 2)
transfer2_BFpb <- round(tra2_sum$components$inclusion_BF[[3]], digits = 2)


#| DISTANCE META-ANALYSIS ----

##| hedges' g
###| all studies
distance_mean <- round(dis1_sum$estimates$Mean[1], digits = 2)
distance_lb <- round(dis1_sum$estimates$`0.025`[1], digits = 2)
distance_ub <- round(dis1_sum$estimates$`0.975`[1], digits = 2)

###| outliers removed
distance2_mean <- round(dis2_sum$estimates$Mean[1], digits = 2)
distance2_lb <- round(dis2_sum$estimates$`0.025`[1], digits = 2)
distance2_ub <- round(dis2_sum$estimates$`0.975`[1], digits = 2)

##| tau estimate
###| all studies
distance_tau <- round(dis1_sum$estimates$Mean[2], digits = 2)
distance_tlb <- round(dis1_sum$estimates$`0.025`[2], digits = 2)
distance_tub <- round(dis1_sum$estimates$`0.975`[2], digits = 2)

###| outliers removed
distance2_tau <- round(dis2_sum$estimates$Mean[2], digits = 2)
distance2_tlb <- round(dis2_sum$estimates$`0.025`[2], digits = 2)
distance2_tub <- round(dis2_sum$estimates$`0.975`[2], digits = 2)

##| bayes factor
###| all studies
distance_BF10 <- round(dis1_sum$components$inclusion_BF[[1]], digits = 2)
distance_BFrf <- round(dis1_sum$components$inclusion_BF[[2]], digits = 2)
distance_BFpb <- round(dis1_sum$components$inclusion_BF[[3]], digits = 2)

###| outliers removed
distance2_BF10 <- round(dis2_sum$components$inclusion_BF[[1]], digits = 2)
distance2_BFrf <- round(dis2_sum$components$inclusion_BF[[2]], digits = 2)
distance2_BFpb <- round(dis2_sum$components$inclusion_BF[[3]], digits = 2)


#| EMG META-ANALYSIS ----

##| hedges' g
###| all studies
emg_mean <- round(emg_sum$estimates$Mean[1], digits = 2)
emg_lb <- round(emg_sum$estimates$`0.025`[1], digits = 2)
emg_ub <- round(emg_sum$estimates$`0.975`[1], digits = 2)

##| tau estimate
###| all studies
emg_tau <- round(emg_sum$estimates$Mean[2], digits = 2)
emg_tlb <- round(emg_sum$estimates$`0.025`[2], digits = 2)
emg_tub <- round(emg_sum$estimates$`0.975`[2], digits = 2)

##| bayes factor
###| all studies
emg_BF10 <- round(emg_sum$components$inclusion_BF[[1]], digits = 2)
emg_BFrf <- round(emg_sum$components$inclusion_BF[[2]], digits = 2)
emg_BFpb <- round(emg_sum$components$inclusion_BF[[3]], digits = 2)


#| MODERATOR ANALYSIS RESULTS ----

##| Test of selection moderator
q <- round(moderator_res$QM, digits = 2)
qdf1 <- moderator_res$QMdf[1]
qdf2 <- moderator_res$QMdf[2]
q_p <- round(moderator_res$QMp, digits = 3)

##| Estimated effect among non-selected effects
not_selected_mean <- round(moderator_est$beta[1], digits = 2)
not_selected_lb <- round(moderator_est$ci.lb[1], digits = 2)
not_selected_ub <- round(moderator_est$ci.ub[1], digits = 2)

##| Estimated effect among selected effects
selected_mean <- round(moderator_est$beta[2], digits = 2)
selected_lb <- round(moderator_est$ci.lb[2], digits = 2)
selected_ub <- round(moderator_est$ci.ub[2], digits = 2)
