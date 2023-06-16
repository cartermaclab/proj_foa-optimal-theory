#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Moderator analysis comparing outcomes that were not selected for analysis
#+ by Chua et al.
#+  -- i.e., post-hoc selection as a moderator
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#| SCRIPT SETUP ----

source("scripts/libraries.R")

##| load hand coded data that has an "included" moderator
dat <- read_csv("data/dat_long_05_09_hand_coded.csv")


#| DATA WRANGLING ----

##| create a vector with each study labelled as author_year then
##| transform standard error to variance
dat$study <- paste0(dat$author, dat$year)
dat$v <- dat$sei^2

##| include only cases with complete information and reorder to make forest
##| plots easier to interpret
dat <- dat %>% drop_na(measure)
dat <- dat %>% drop_na(effect_size)
dat <- dat[order(dat$effect_size),]
dat <- dat[order(dat$selected),]


##| influential case screening
# inf <- rma.mv(effect_size, v, random = ~1 | study/measure, data = dat)
# plot(cooks.distance(inf, cluster = study))
# plot(residuals.rma(inf, type = "rstudent", cluster = study))


#| MODEL ----

##| fit multilevel model with outcome nested in study
test_of_selected_moderator <- rma.mv(effect_size,
                                     v,
                                     mods = ~factor(selected),
                                     random = ~1 | study/measure,
                                     data = dat)

##| calculate estimates with cluster robust standard errors
moderator_res <- robust(test_of_selected_moderator,
                        cluster = dat$study)

##| drop the intercept to get estimates for selected and not selected
##| outcomes
estimates_of_selected_moderator <- rma.mv(effect_size,
                                          v,
                                          mods = ~factor(selected)-1,
                                          random = ~1 | study/measure,
                                          data = dat)

##| get estimates with cluster robust standard errors
moderator_est <- robust(estimates_of_selected_moderator,
                        cluster = dat$study)

##| get 95% prediction intervals that incorporate heterogeneity
predict(moderator_est)
