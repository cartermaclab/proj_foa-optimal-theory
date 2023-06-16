#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Data extraction diffs
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

source("scripts/libraries.R")


#| EXTRACTION DIFFS

dat_ac <- read.csv(file = "extractions/daff/dat_ac.csv")
dat_js <- read.csv(file = "extractions/daff/dat_js.csv")

accuracy_diff <- daff::diff_data(dat_ac, dat_js)
daff::render_diff(accuracy_diff)


dat_cf <- read.csv(file = "extractions/daff/dat_cf.csv")
dat_ka <- read.csv(file = "extractions/daff/dat_ka.csv")

accuracy_diff <- daff::diff_data(dat_cf, dat_ka)
daff::render_diff(accuracy_diff)


dat_hh <- read.csv(file = "extractions/daff/dat_hh.csv")
dat_fa <- read.csv(file = "extractions/daff/dat_fa.csv")

accuracy_diff <- daff::diff_data(dat_hh, dat_fa)
daff::render_diff(accuracy_diff)


pi_dat_cf <- read.csv(file = "extractions/daff/pi_dat_cf.csv")
pi_dat_fa <- read.csv(file = "extractions/daff/pi_dat_fa.csv")

accuracy_diff <- daff::diff_data(pi_dat_cf, pi_dat_fa)
daff::render_diff(accuracy_diff)
