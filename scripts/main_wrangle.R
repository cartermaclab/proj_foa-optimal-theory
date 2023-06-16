#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Data wrangling
#+  -- These data are the newly extracted outcomes from articles included
#+  -- in meta-analysis of retention results by Chua et al. 2021
#+
#+ Author(s):
#+  -- Brad McKay
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

source("scripts/libraries.R")

##| load data including newly extracted outcomes
dat <- read_csv("data/dat_ext_05_09.csv")


#| WRANGLING ----

dat_long <- dat %>%
  pivot_longer(
    cols = !c(author,
              year,
              type,
              age,
              health,
              skill,
              N,
              task,
              outcome,
              g,
              se,
              rob),
    names_to = c(".value", "num"),
    names_sep = "_")

dat_long$id <- seq(1:nrow(dat_long))

dat_long$id <- seq(1:nrow(dat_long))

##| calculate standard deviations from reported standard error
dat_long$csdef <- dat_long$seef*sqrt(dat_long$nef)
dat_long$csdif <- dat_long$seif*sqrt(dat_long$nif)


##| calculate effect sizes from means and standard deviations if available
mes <- mes(m.1 = mef,
           m.2 = mif,
           sd.1 = sdef,
           sd.2 = sdif,
           n.1 = nef,
           n.2 = nif,
           dig = 9,
           id = id,
           data = dat_long)

g1 <- dplyr::select(mes, g)
v1 <- dplyr::select(mes, var.g)

##| calculate effect sizes from means and calculated standard deviations
mes2 <- mes(m.1 = mef,
            m.2 = mif,
            sd.1 = csdef,
            sd.2 = csdif,
            n.1 = nef,
            n.2 = nif,
            dig = 9,
            id = id,
            data = dat_long)

g2 <- dplyr::select(mes2, g)
v2 <- dplyr::select(mes2, var.g)

##| calculate effect sizes from t values
tes <- tes(t = t,
           n.1 = nef,
           n.2 = nif,
           dig = 9,
           id = id,
           data = dat_long)

g3 <- dplyr::select(tes, g)
v3 <- dplyr::select(tes, var.g)

##| calculate effect sizes from f statistics
fes <- fes(f = f,
           n.1 = nef,
           n.2 = nif,
           dig = 9,
           id = id,
           data = dat_long)

g4 <- dplyr::select(fes, g)
v4 <- dplyr::select(fes, var.g)

##| combine the effect sizes and variances calculated with
##| the above 4 approaches
v <- coalesce(v1, v2, v3, v4)
dat_long$vi <- round(v$var.g, digits = 3)
g <- coalesce(g1, g2, g3, g4)
dat_long$rawg <- round(g$g, digits = 3)

##| ensure positive effects reflect benefits for an external focus
dat_long$yi <- ifelse(dat_long$favours == "external",
                      abs(dat_long$rawg),
                      -1*abs(dat_long$rawg))

dat_long$standard_error <- round(sqrt(dat_long$vi), digits = 3)

##| compare effect sizes calculated by our extraction to the original
##| to facilitate coding
dat_long$match <- ifelse(dat_long$g == dat_long$yi, "yes", "no")

##| save data for hand coding
# write_csv(dat_long, file = "data/dat_long_05_09.csv")
