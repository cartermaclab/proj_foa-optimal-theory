#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Collapse groups in articles
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

source("scripts/functions.R")


#| BECKER AND SMITH (2013) ----
##| Age, task complexity, and sex as potential moderators of attentional
##| focus effects. Perceptual & Motor Skills: Learning & Memory, 117(1),
##| 1172–1186. https://doi.org/10.2466/23.25.PMS.117x14z3

##| Adults
###| internal, complex
combine_means(9.97, 10.75, 6, 6)
###> 10.36
combine_sds(9.97, 10.75, 6, 6, 3.44, 3.35)
###> 3.262817

###| internal, simple
combine_means(4.67, 5.29, 6, 6)
###> 4.98
combine_sds(4.67, 5.29, 6, 6, 1.56, .98)
###> 1.283575

###| external, complex
combine_means(6.83, 12.02, 6, 6)
###> 9.425
combine_sds(6.83, 12.02, 6, 6, 1.55, 2.55)
###> 3.375491

###| external, simple
combine_means(4.13, 4.93, 6 ,6)
###> 4.53
combine_sds(4.13, 4.93, 6 ,6, .61, .71)
###> 0.7568475


###| internal collapsed adults
combine_means(10.36, 4.98, 12, 12)
###> 7.67
combine_sds(10.36, 4.98, 12, 12, 3.262817, 1.283575 )
###> 3.664728

###| external collapsed adults
combine_means(9.425, 4.53, 12 ,12 )
###> 6.9775
combine_sds(9.425, 4.53, 12 ,12, 3.375491, 0.7568475 )
###> 3.460337


###| males, internal
combine_means(9.97, 4.67, 6, 6)
###> 7.32
combine_sds(9.97, 4.67, 6, 6, 3.44, 1.56)
###> 3.761117

###| males, external
combine_means(6.83, 4.13, 6, 6)
###> 5.48
combine_sds(6.83, 4.13, 6, 6, 1.55, .61)
###> 1.802599

###| females, internal
combine_means(10.75, 5.29, 6, 6)
###> 8.02
combine_sds(10.75, 5.29, 6, 6, 3.35, .98)
###> 3.697042

###| females, external
combine_means(12.02, 4.93, 6, 6)
###> 8.475
combine_sds(12.02, 4.93, 6, 6, 2.55, .71)
###> 4.110268


###| overall effect size
compute.es::mes(7.67, 6.9775, 3.664728, 3.460337, 24, 24)

###| complex effect size
compute.es::mes(10.36, 9.425, 3.262817, 3.375491, 12, 12)

###| simple effect size
compute.es::mes(4.98, 4.53, 1.283575, 0.7568475, 12, 12)

###| males effect size
compute.es::mes(7.32, 5.48, 3.761117, 1.802599, 12, 12)

###| females effect size
compute.es::mes(8.02, 8.475, 3.697042, 4.110268, 12, 12)


##| Children
###| complex, internal
combine_means(12.51, 8.13, 6, 6)
###> 10.32
combine_sds(12.51, 8.13, 6, 6, 7.16, 3.23)
###> 5.768612

###| complex, external
combine_means(8.18, 10.21, 6, 6)
###> 9.195
combine_sds(8.18, 10.21, 6, 6, 2.56, 4.89)
###> 3.869357

###| simple, internal
combine_means(4.35, 5.03, 6, 6)
###> 4.69
combine_sds(4.35, 5.03, 6, 6, .63, .61)
###> 0.6896771


###| simple, external
combine_means(4.91, 5.68, 6, 6)
###> 5.295
combine_sds(4.91, 5.68, 6, 6, .76, 1.27)
###> 1.075817


###| internal collapsed children
combine_means(4.69, 10.32, 12, 12)
###> 7.505
combine_sds(4.69, 10.32, 12, 12, 0.6896771, 5.768612)
###> 4.940777

###| external collapsed children
combine_means(5.295, 9.195, 12, 12)
###> 7.245
combine_sds(5.295, 9.195, 12, 12, 1.075817, 3.869357)
###> 3.417871


###| complex effect size
compute.es::mes(10.32, 9.195, 5.768612, 3.869357, 12, 12)


##| Use mini-meta approach
###| adults
####| males, complex
compute.es::mes(9.97, 6.83, 3.44, 1.55, 6, 6)
####> g = 1.09, v = .33

####| females, complex
compute.es::mes(10.75, 12.02, 3.35, 2.55, 6, 6)
####> g = -.39, v = .29

####| create dataframe from above values and meta-analyze
g <- c(1.09, -.39)
v <- c(.33, .29)

complex <- as.data.frame(cbind(g, v))

metafor::rma(g,v, complex)


####| males, simple
compute.es::mes(4.67, 4.13, 1.56, .61, 6, 6)
####> g = .42, v = .29

####| females, simple
compute.es::mes(5.29, 4.93, .98, .71, 6, 6)
####> .39, v = .29

####| create dataframe from above values and meta-analyze
g <- c(.42, .39)
v <- c(.29, .29)

simple <- as.data.frame(cbind(g,v))

metafor::rma(g,v, simple)

####| simple and complex combined for adults and meta-analyze
g <- c(1.09, -.39, .42, .39)
v <- c(.33, .29, .29, .29)

adults <- as.data.frame(cbind(g,v))

metafor::rma(g,v,adults)

metafor::rma(g,v, method = "FE", data = adults)


#| JACKSON AND HOLMES (2011) ----
##| The effects of focus of attention and task objective consistency on
##| learning a balancing task. Research Quarterly for Exercise and Sport,
##| 82(3), 574–579. https://doi.org/10.1080/02701367.2011.10599791

##| first comparison, favours external
compute.es::fes(.82, 9, 9)

g <- .41
v <- .21

df1 <- data.frame(g,v)

##| second comparison, favours internal
compute.es::fes(.68, 9, 9)

g <- -.37
v <- .21

df2 <- data.frame(g,v)

##| combine created dataframes from above and meta-analyze
df3 <- rbind(df1, df2)

metafor::rma(g,v, data = df3, method = "FE")
##> g = .02, se = .324


#| WULF ET AL (2010) ----
##| Frequent external-focus feedback enhances motor learning. Frontiers in
##| Psychology, 1, 190. https://doi.org/10.3389/fpsyg.2010.00190

##| combine external focus groups
se_to_sd(0.231130555, 12, 0.175138153, 12)
##> ef_100_sd = 0.8006597
##> ef_33_sd = 0.6066964

combine_means(5.701744186, 5.417908128, 12, 12)
combine_sds(5.701744186, 5.417908128, 12, 12, 0.8006597, 0.6066964)

##| combine internal focus groups
se_to_sd(0.125339627, 12, 0.170904329, 12)
##> if_100_sd = 0.4341892
##> if_33_sd = 0.59203

combine_means(5.261011973, 5.44367085, 12, 12)
combine_sds(5.261011973, 5.44367085, 12, 12, 0.4341892, 0.59203)
