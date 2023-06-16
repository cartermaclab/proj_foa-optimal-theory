#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Functions
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

source("scripts/libraries.R")


#| FUNCTIONS ----

##| Combine means, standard deviations, and convert standard
##| error to standard deviation
##| Example:
##| -- combine_means(m1 = 5, m2 = 4, n1 = 10, n2 = 10)
##| -- combine_sds(m1 = 5, m2 = 6, n1 = 1, n2 = 10, sd1 = .5, sd2 = .6)
##| -- se_to_sd(se1 = .8, n1 = 10, se2 = .5, n2 = 10)

combine_means <- function(m1, m2, n1, n2){
  m_comb <- (((n1 * m1) + (n2 * m2)) / (n1 + n2))
  return(m_comb)
}

combine_sds <- function(m1, m2, n1, n2, sd1, sd2){
  sd_comb <- sqrt(
    (((n1 - 1) * (sd1^2)) + ((n2 - 1) * (sd2 ^ 2)) + ((n1 * n2) / (n1 + n2)) *
    ((m1^2 + m2^2) - (2 * m1 * m2))) / (n1 + n2 - 1)
  )
  return(sd_comb)
}

se_to_sd <- function(se1, n1, se2, n2){
  sd_gr_1 <- se1 * sqrt(n1)
  sd_gr_2 <- se2 * sqrt(n2)
  sds <- cbind(sd_gr_1, sd_gr_2)
  return(sds)
}
