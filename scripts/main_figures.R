#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ ROBUST BAYESIAN META-ANALYSIS OF FOCUS OF ATTENTION LITERATURE
#+  -- McKay, Corson, Seedu, De Faveri, Hasan, Arnold, Adams, and Carter
#+
#+ Figures
#+
#+ Author(s):
#+  -- Brad McKay
#+  -- Mike Carter
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#| SCRIPT SETUP ----

source("scripts/libraries.R")
source("scripts/moderator_analysis.R")
extrafont::loadfonts(device = "win")

##| load files for each meta-analysis (all included)
ret_post <- read_csv("data/figs_ret_post.csv")
per_post <- read_csv("data/figs_per_post.csv")
tra_post <- read_csv("data/figs_tra_post.csv")
dist_post <- read_csv("data/figs_dist_post.csv")
emg_post <- read_csv("data/figs_emg_post.csv")

##| load files for each meta-analysis (no outliers)
ret_post_nol <- read_csv("data/figs_ret_post_nol.csv")
per_post_nol <- read_csv("data/figs_per_post_nol.csv")
tra_post_nol <- read_csv("data/figs_tra_post_nol.csv")
dist_post_nol <- read_csv("data/figs_dist_post_nol.csv")

##| load performance models
#per_models <- readRDS(file = "data/per_models.rds")

##| load all models
ret1_models <- readRDS(file = 'data/ret1_models.rds')
ret2_models <- readRDS(file = "data/ret2_models.rds")
per1_models <- readRDS(file = "data/per_models.rds")
per2_models <- readRDS(file = "data/per2_models.rds")
tra1_models <- readRDS(file = "data/tra_models.rds")
tra2_models <- readRDS(file = "data/tra2_models.rds")
dis1_models <- readRDS(file = "data/dis_models.rds")
dis2_models <- readRDS(file = "data/dis2_models.rds")
emg_models <- readRDS(file = "data/emg_models.rds")

##| define figure theme
theme_set(theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  text = element_text(
                    family = "Roboto Condensed",
                    size = 12)))


#| CREATE FIGURES ----
#|
#| Figure 1 in paper
#| Some editing was completed using inkscape after creating the base image
#| using the code below. In inkscape, the following was done:
#|    - added Y and X axis labels and titles
#|    - added blue region indicating increased belief in the null hypothesis
#|    - added the line at 50% prior probability of the spike null hypothesis
#|    - added the text to the bottom panel indicating increased and decreased
#|      belief in the null.
#|
##| Retention meta-analysis
ret_plot <- ggplot(ret_post, aes(x = value)) +
  geom_histogram(fill = "#2a788e",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
ret_plot

ret_plot_nol <- ggplot(ret_post_nol, aes(x = value)) +
  geom_histogram(fill = "#2a788e",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  scale_x_continuous(breaks = c(-.5, .5)) +
  theme(axis.text.x = element_text(size = 12)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
ret_plot_nol


##| Performance meta-analysis
per_plot <- ggplot(per_post, aes(x = value)) +
  geom_histogram(fill = "#414487",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
per_plot

per_plot_nol <- ggplot(per_post_nol, aes(x = value)) +
  geom_histogram(fill = "#414487",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  scale_x_continuous(breaks = c(-.5, .5)) +
  theme(axis.text.x = element_text(size = 12)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
per_plot_nol


##| Transfer meta-analysis
tra_plot <- ggplot(tra_post, aes(x = value)) +
  geom_histogram(fill = "#22a884",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
tra_plot

tra_plot_nol <- ggplot(tra_post_nol, aes(x = value)) +
  geom_histogram(fill = "#22a884",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  scale_x_continuous(breaks = c(-.5, .5)) +
  theme(axis.text.x = element_text(size = 12)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
tra_plot_nol


##| Distance effect meta-analysis
dist_plot <- ggplot(dist_post, aes(x = value)) +
  geom_histogram(fill = "#fde725",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0,10000)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
dist_plot

dist_plot_nol <- ggplot(dist_post_nol, aes(x = value)) +
  geom_histogram(fill = "#fde725",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  scale_x_continuous(breaks = c(-.5, .5)) +
  theme(axis.text.x = element_text(size = 12)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
dist_plot_nol


##| Electromyography activity meta-analysis
emg_plot <- ggplot(emg_post, aes(x = value)) +
  geom_histogram(fill = "#7ad151",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
emg_plot


##| Spike and slab prior distribution and plot
spike <- rep(0, 5000)
slab <-  rnorm(5000)
prior <- dplyr::as_tibble(c(spike, slab))

prior_plot <- ggplot(prior, aes(x = value)) +
  geom_histogram(fill = "#440154",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  ylab("Count") +
  xlab("Mu") +
  theme(axis.line.y = element_line()) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
prior_plot

prior_plot2 <- ggplot(prior, aes(x = value)) +
  geom_histogram(fill = "#440154",
                 color = "black",
                 binwidth = .15) +
  coord_cartesian(expand = FALSE,
                  xlim = c(-1.09, 1.09),
                  ylim = c(0, 10000)) +
  ylab("Count") +
  xlab("Mu") +
  theme(axis.line.y = element_line()) +
  scale_x_continuous(breaks = c(-.5, .5)) +
  theme(axis.text.x = element_text(size = 12)) +
  geom_vline(xintercept = c(-.5, .5),
             linetype = "dashed",
             size = .1)
prior_plot2

##| Setup multiplot layout
design <- "
ACEGIJ
BDFH#K
"

fig1 <- wrap_plots(prior_plot,
                   prior_plot2,
                   per_plot,
                   per_plot_nol,
                   ret_plot,
                   ret_plot_nol,
                   tra_plot,
                   tra_plot_nol,
                   emg_plot,
                   dist_plot,
                   dist_plot_nol,
                   design = design)
fig1



#| Figure 2 in paper
#|
##| Define color variables
colp <- "#440154"
colg <-  "#21918c"

##| Forest plot for post hoc selection moderator analysis
fig_forest <- metafor::forest(moderator_est ,
                    header = FALSE,
                    cex = .55,
                    cex.lab = .8,
                    addfit = FALSE,
                    slab = paste0(dat$author, "," ,dat$year),
                    xlim = c(-5, 5.5),
                    xlab = expression(bold("Hedges' ")*bolditalic("g")),
                    at = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
                    rows = c(57:91, 5:53),
                    fonts = "Roboto Condensed")

addpoly.default(x = .5977,
                ci.lb = .2654,
                ci.ub = 0.9299,
                pi.lb = -1.0678,
                pi.ub = 2.2631,
                row = 55,
                cex = .7,
                font = 2,
                efac = 1,
                mlab = "Not Selected",
                col = colp,
                border = colp,
                addpred = TRUE,
                fonts = "Roboto Condensed")

addpoly.default(x = .7386,
                ci.lb = 0.4867,
                ci.ub = 0.9905,
                pi.lb = -0.9127,
                pi.ub = 2.3899,
                row = 3,
                cex = .7,
                font = 2,
                efac = 1,
                mlab = "Selected",
                col = colg,
                border = colg,
                addpred = TRUE,
                fonts = "Roboto Condensed")

text(fig_forest$xlim[2],
     pos = 2,
     -1,
     "Favours External Focus",
     cex = .7,
     font = 2,
     fonts = "Roboto Condensed")

text(fig_forest$xlim[1],
     pos = 4,
     -1,
     "Favours Internal Focus",
     cex = .7,
     font = 2,
     fonts = "Roboto Condensed")

text(-4.3,
     93,
     "Author and Year",
     cex = .8,
     font = 2,
     fonts = "Roboto Condensed")

text(4.95,
     93,
     "Mean [95%CI]",
     cex = .8,
     font = 2,
     fonts = "Roboto Condensed")


#| Figure 3 in paper
#| Some editing was completed using inkscape after creating the base image
#| using the code below. In inkscape, the following was done:
#|    - added the sankey diagrams (created below) to the top of the figure
#|    - added a circle that matches the size of the corresponding points of
#|      the lollipop plot
#|
##| Model Bayes factors
ret1_sum <- ret1_models$summary
ret2_sum <- ret2_models$summary
per1_sum <- per1_models$summary
per2_sum <- per2_models$summary
tra1_sum <- tra1_models$summary
tra2_sum <- tra2_models$summary
dis1_sum <- dis1_models$summary
dis2_sum <- dis2_models$summary
emg_sum <- emg_models$summary

# fig_models <- ggplot(per1_sum, aes(x = Model, y = BF)) +
#   geom_count()
# fig_models

model_BFs <- as.tibble(cbind(ret1_sum$Model,
                             ret1_sum$inclusion_BF,
                             ret2_sum$inclusion_BF,
                             per1_sum$inclusion_BF,
                             per1_sum$inclusion_BF,
                             tra1_sum$inclusion_BF,
                             tra2_sum$inclusion_BF,
                             emg_sum$inclusion_BF,
                             dis1_sum$inclusion_BF,
                             dis2_sum$inclusion_BF))

model_BFs <- model_BFs |>
  mutate(BF_tot = V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10)

model_BFs$effect <- rep(1:2, each = 18)
model_BFs$rf <- rep(1:2, each = 9, times = 2)
model_BFs$pb <- rep(c(1, rep(2, 8)), 4)

model_BFs$prior_prob <- c(.125, rep(.01, 6), .031, .031, .125,
                          rep(.01, 6), .031,.031,.125, rep(.01, 6),
                          .031,.031,.125, rep(.01, 6), .031,.031)


model_post_probs <- as.tibble(cbind(ret1_sum$Model,
                                    ret1_sum$post_prob,
                                    ret2_sum$post_prob,
                                    per1_sum$post_prob,
                                    per1_sum$post_prob,
                                    tra1_sum$post_prob,
                                    tra2_sum$post_prob,
                                    emg_sum$post_prob,
                                    dis1_sum$post_prob,
                                    dis2_sum$post_prob))

model_post_probs <- model_post_probs |>
  mutate(post_prob_tot = V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10)

model_post_probs$prior_prob <- c(.125, rep(.01, 6), .031, .031, .125,
                                 rep(.01, 6), .031,.031,.125, rep(.01, 6),
                                 .031, .031, .125, rep(.01, 6), .031,.031)

##| Update default theme layout
theme_set(theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  #axis.line = element_blank(),
                  #axis.text = element_blank(),
                  #axis.title = element_blank(),
                  text = element_text(
                    family = "Roboto Condensed",
                    size = 18)))

bf_1 <- model_BFs[1:18,]
bf_2 <- model_BFs[19:36,]

bf_flipped <- as.tibble(rbind(bf_2, bf_1))
bf_flipped$model <- seq(1:36)

fig_bfs <- ggplot(bf_flipped, x = model, y = BF_tot) +
  annotate("rect",
           xmin = 0.5, xmax = 9.5,
           ymin = 0, ymax = 500,
           fill = "lightgrey",
           alpha = 1) +
  annotate("rect",
           xmin = 18.5, xmax = 27.5,
           ymin = 0, ymax = 500,
           fill = "lightgrey",
           alpha = 1) +
  geom_segment(aes(x = model, xend = model,
                   y = 0, yend = BF_tot)) +
  geom_point(aes(x = model, y = BF_tot,
                 size = prior_prob,
                 color = factor(pb))) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  scale_color_manual(values = c("#21918c", "#440154"),
                     labels = c("No Publication Bias",
                                "Publication Bias")) +
  scale_size_continuous(range = c(2,9)) +
  coord_cartesian(ylim = c(0,420),
                  clip = "off") +
  ylab("Sum of Inclusion Bayes Factors (Log Scale)") +
  xlab("Model") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(size = "none") +
  annotate("text",
           x = 9.5,
           y = 650,
           label = "Effect is Present",
           size = 6) +
  annotate("text",
           x = 27.5,
           y = 650,
           label = "Effect is Absent",
           size = 6) +
  annotate("rect",
           xmin = 0.5,
           xmax = 18.5,
           ymin = 0,
           ymax = 500,
           fill = "#0d0887",
           alpha = 0.09) +
  annotate("text",
           x = 4.75,
           y = 550,
           label = "No Heterogeneity") +
  annotate("text",
           x = 14.25,
           y = 550,
           label = "Heterogeneity") +
  annotate("text",
           x = 23.25,
           y = 550,
           label = "No Heterogeneity") +
  annotate("text",
           x = 32.25,
           y = 550,
           label = "Heterogeneity") +
  theme(legend.title = element_blank(),
        plot.margin = margin(t = 20, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5)))
fig_bfs


##| Figures illustrating the different selection models
##|
###| Model 1: basic (two-tailed sig selection)
model1 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Sig. (p < .05)",
              "Null (p > .05)",
              "\nReported",
              "\nNot Reported"),
    color = c("#21918c", "#440154", "#21918c","#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1),
    target = c(2,2,2,3),
    value =  c(10,0,2,8)
  )
)
model1 <- model1 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model1


###| Model 2: trends (two-tailed with trends)
model2 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Sig. (p < .05)",
              "Trend (p < .10)",
              "\nNull (p > .10)",
              "Reported",
              "Not Reported"),
    color = c("#21918c", "#440154", "#440154", "#21918c", "#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model2 <- model2 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model2


###| Model 3: tpred (trending in predicted direction selection)
model3 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Trend (p < .05)",
              "Null (p > .05)",
              "\nReported",
              "\nNot Reported"),
    color = c("#21918c", "#440154","#21918c", "#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1),
    target = c(2,3,2,3),
    value =  c(10,0,2,8)
  )
)
model3 <- model3 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model3


###| Model 4: trendspred (trends and sig one-tail)
model4 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Sig. (p < .025)",
              " Trend (p < .05)",
              "\nNull (p > .05)",
              "Reported",
              "Not Reported"),
    color = c("#21918c", "#440154", "#440154","#21918c", "#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model4 <- model4 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model4


###| Model 5: trenddir (trends with direction)
model5 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Trend (p < .05)",
              "Direction (p < .5)",
              "\nOpposite(p > .5)",
              "Reported",
              "Not Reported"),
    color = c("#21918c", "#440154", "#440154","#21918c", "#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model5 <- model5 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model5


###| Model6: fullsel (full selection)
model6 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Sig. (p < .025)",
              "Trend (p < .05)",
              "Direction (p < .5)",
              "\nOpposite (p > .5)",
              "Reported\n",
              "\nNot Reported"),
    color = c("#21918c", "#440154",
              "#440154", "#440154",
              "#21918c","#440154"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2,3,3),
    target = c(4,5,4,5,4,5,4,5),
    value =  c(10,0,5,5,2,8,1,9)
  )
)
model6 <- model6 %>% layout(
  font = list(
    size = 45,
    color = "black"
  )
)
model6


###| Model 7: pet model
dat <- as_tibble(
  faux::rnorm_multi(n = 200,
                    mu = .6,
                    sd = .2,
                    r = .7,
                    varnames = c("standard_error",
                                 "effect_size")))

theme_set(theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  text = element_text(
                    family = "Roboto Condensed",
                    size = 12)))

pet <- dat %>% ggplot(aes(x = standard_error, y = effect_size)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, se = FALSE, color = "#440154", size = 2) +
  theme_minimal() +
  labs(x = "Standard Error", y = "Effect Size") +
  theme(axis.title.x = element_text(size = 43)) +
  theme(axis.title.y = element_text(size = 43)) +
  theme(rect = element_rect(fill = "transparent")) +
  theme(axis.text = element_text(size = 24)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))
pet

ggsave(p, filename = "figs/pet.png",  bg = "transparent")


###| Model 8: peese model
dat <- as_tibble(
  faux::rnorm_multi(n = 200,
                    mu = .6,
                    sd = .2,
                    r = .7,
                    varnames = c("standard_error",
                                 "effect_size")))
dat$effect_size <- dat$effect_size^2

peese <- dat %>% ggplot(aes(x = standard_error, y = effect_size)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, formula = y ~ x + I(x^2),
              se = FALSE, colour = "#440154", size = 2) +
  theme_minimal() +
  labs(x = "Standard Error", y = "Effect Size") +
  theme(axis.title.x = element_text(size = 43)) +
  theme(axis.title.y = element_text(size = 43)) +
  theme(rect = element_rect(fill = "transparent")) +
  theme(axis.text = element_text(size = 24)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line()) +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))
peese

ggsave(peese, filename = "figs/peese.png",  bg = "transparent")
