# This script loads the simulation results produced by "simulation.r" and
# produces the figures shown in the article.

rm(list = ls())

library("plyr")
library("reshape2")
library("ggplot2")
library("ggtern")
library("ggalt")

source("./util.r")

extension = "tiff" # "png" or "tiff"

################################################################################
################################################################################
################################################################################
# Determining the range of values of G explored in the simulation.
# This is used to fill in Table A.1. from the Supplementary Information.

# (G is the granularity of the typical panel ranking, aka tiering).
d <- expand.grid(gamma = c(3, 5, 10), R = c(3, 5, 7))
for (i in 1:nrow(d)) d$G[i] <- d$gamma[i] * d$R[i] - (d$R[i] - 1)

# So, the actual values of G that we explored are:
d$G[order(d$G)]


################################################################################
################################################################################
################################################################################
# Loading and preparing simulation results
load("./output/results.RData")

# recoding, reshaping and relabeling:
r$type[r$type == "type 0"] <- "Type 0"
r$type[r$type == "type 1"] <- "Type 1"
r$type[r$type == "type 2, 10% pool"] <- "Type 2, S=10"
r$type[r$type == "type 2, 50% pool"] <- "Type 2, S=50"
r$type[r$type == "type 2, 90% pool"] <- "Type 2, S=90"
r$type[r$type == "type 3"] <- "Type 3"
r$type[r$type == "type 4"] <- "Type 4"

# reverse coding these two variables - so that high values mean 'fairer'
r$bias <- 1 - abs(r$bias)
r$inequality <- 1 - r$inequality

# This format of "r" is convenient for generating the ternary plot later. So we
# put aside a copy of it.
rtri <- r

# And we format "r" to make it easier to generate the traditional violins and
# boxplots.
r <- reshape2::melt(
  data = r,
  id.vars = names(r)[! names(r) %in% c("merit", "bias", "inequality")]
)

r$variable <- plyr::revalue(
  r$variable,
  c(
    "merit" = "epistemic correctness",
    "bias" = "unbiased fairness",
    "inequality" = "distributive fairness"
  )
)

# Adding a color variable so we can color the boxplots by Type.
r$color <- 2
r$color[r$type == "Type 0"] <- 0
r$color[r$type == "Type 1"] <- 1
r$color[r$type == "Type 3"] <- 3
r$color[r$type == "Type 4"] <- 4
r$color <- as.factor(r$color)

# Defining the plotting function:
plotResults <- function (rr) {
  ggplot(data = rr, mapping = aes(x = value, y = type, fill = color)) +
    geom_violin(scale = "width", color = "gray80", fill = "gray80") +
    geom_boxplot(width = 0.5, alpha = 1, color = "#00000080") +
    geom_blank( # this is to set x axis limits consistently for all facets
      data = data.frame(
        type = rep("Type 0", times = 6),
        color = rep(NA, times = 6),
        variable = as.factor(c(
          "epistemic correctness", "epistemic correctness",
          "unbiased fairness", "unbiased fairness",
          "distributive fairness", "distributive fairness"
        )),
        value = c( # min and max of each variable/facet
          min(r$value[r$variable == "epistemic correctness"]), 1, # merit
          0, 1,                                            # unbiased
          min(r$value[r$variable == "distributive fairness"]), 1 # distributive
        )
      )
    ) +
    facet_wrap(nrow = 1, facets = "variable", scales = "free_x") +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.92) +
    xlab("low \U2194 high") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray97"),
      panel.grid = element_line(color = "gray93", size = 0.2),
      strip.text.x = element_text(size = 12),
      axis.line = element_line(color = "black", size = 1),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 10, hjust = 0.5, vjust = -0.5),
      strip.background = element_blank(),
      legend.position = "NA"
    )
}
#plotResults(rr)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Plotting


# Baseline _____________________________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2 # c(0.2, 0.5)
)

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_1.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()



# stronger competition _________________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.25 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.75 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2
)

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_2.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()


# weaker competition ___________________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.75 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.25 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2
)

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_4.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()


# higher panel granularity _____________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 10 & # c(3, 5, 10)
    r$nReviewers == 7 & # c(3, 5, 7)
    r$panelBias == 0.2
)

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_5.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()


# stronger bias ________________________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.5 # c(0.2, 0.5)
)

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_6.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()


# lower panel granularity ______________________________________________________
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 3 & # c(3, 5, 10)
    r$nReviewers == 3 & # c(3, 5, 7)
    r$panelBias == 0.2
)


do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_7.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
)
plotResults(rr)
dev.off()


# Ternary plot _________________________________________________________________
#
# Selecting benchmark runs (same as Figure 1):
rrtri <- subset(
  rtri,
  rtri$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    rtri$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    rtri$scaleGranularity == 5 & # c(3, 5, 10)
    rtri$nReviewers == 5 & # c(3, 5, 7)
    rtri$panelBias == 0.2 # c(0.2, 0.5)
)

# Transforming performance scores into orderings.
rrtri$EpistemicCorrectnessRank <- rank(
  normalize(rrtri$merit), # mapping "merit" to positive range via normalization
  ties.method = "average"
)
rrtri$unbiasedFairnessRank <- rank(rrtri$bias, ties.method = "average")
rrtri$distributiveFairnessRank <- rank(
  rrtri$inequality,
  ties.method = "average"
)

# Adding a color variable so we can color the boxplots by Type.
rrtri$color <- 2
rrtri$color[rrtri$type == "Type 0"] <- 0
rrtri$color[rrtri$type == "Type 1"] <- 1
rrtri$color[rrtri$type == "Type 3"] <- 3
rrtri$color[rrtri$type == "Type 4"] <- 4
rrtri$color <- factor(
  x = rrtri$color,
  levels = 0:4,
  labels = c(paste("Type", 0:4))
)

# Producing the ternary plot
do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/ternary_plot.", extension),
    width = 2300, height = 2000, unit = "px", res = 300, bg = "transparent")
)
ggtern::ggtern(
  data = rrtri,
  aes(
    x = EpistemicCorrectnessRank, 
    y = unbiasedFairnessRank, 
    z = distributiveFairnessRank,
    fill = color,
    color = color#type
  )
) + 
  labs(
    x = "more\nepistemic\ncorrectness",# xarrow = "epistemic correctness",
    y = "more\nunbiased fairness",# yarrow = "unbiased fairness",
    z = "more\ndistributive\nfairness"#, zarrow = "distributive fairness"
  ) +
  ggalt::geom_encircle( # adds the convex hulls
    alpha = 0.2,
    expand = 0.05#,
    #show.legend = FALSE
  ) +
  geom_point(shape = 16, alpha = 0.6) +
  scale_color_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "gray97"),
    panel.grid = element_line(color = "gray80"),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.5)
  ) + 
  theme_nolabels() + 
  theme_hideprimary()
dev.off()


################################################################################
# convex-hull plots

rrtri <- subset(
  rtri,
  rtri$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    rtri$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    rtri$scaleGranularity == 5 & # c(3, 5, 10)
    rtri$nReviewers == 5 & # c(3, 5, 7)
    rtri$panelBias == 0.2 # c(0.2, 0.5)
)

# Transforming performance scores into orderings.
rrtri$EpistemicCorrectnessRank <- rank(
  normalize(rrtri$merit), # mapping "merit" to positive range via normalization
  ties.method = "average"
)
rrtri$unbiasedFairnessRank <- rank(rrtri$bias, ties.method = "average")
rrtri$distributiveFairnessRank <- rank(
  rrtri$inequality,
  ties.method = "average"
)

# Adding a color variable so we can color the boxplots by Type.
rrtri$color <- 2
rrtri$color[rrtri$type == "Type 0"] <- 0
rrtri$color[rrtri$type == "Type 1"] <- 1
rrtri$color[rrtri$type == "Type 3"] <- 3
rrtri$color[rrtri$type == "Type 4"] <- 4
rrtri$color <- factor(
  x = rrtri$color,
  levels = 0:4,
  labels = c(paste("Type", 0:4))
)


themebiplot <- theme (
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "gray97"),
  panel.grid = element_blank(),
  axis.line = element_line(color = "black", size = 1),
  axis.ticks = element_blank(),
  axis.title.y = element_text(size = 12, hjust = 0.5, vjust = -5),
  axis.title.x = element_text(size = 12, hjust = 0.5, vjust = 5),
  legend.title = element_blank(),
  legend.justification = "top"
)
#arrow <- grDevices::italic("lower \U2194 higher")

a <- ggplot(
  data = rrtri,
  aes(
    x = EpistemicCorrectnessRank, 
    y = unbiasedFairnessRank, 
    color = color, fill = color
  )
) +
  ggalt::geom_encircle(alpha = 0.2, expand = 0.05) + # adds the convex hulls
  geom_point(shape = 16, alpha = 0.6) +
  scale_x_continuous(
    expand = c(0.01, 0.01),
    breaks = range(rrtri$EpistemicCorrectnessRank),
    labels = c("worst        ", "best")
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    breaks = max(rrtri$unbiasedFairnessRank),#range(rrtri$unbiasedFairnessRank),
    labels = "best"#c("worst", "best")
  ) +
  scale_color_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  labs(x = "epistemic correctness", y = "unbiased fairness") +
  themebiplot

b <- ggplot(
  data = rrtri,
  aes(
    x = EpistemicCorrectnessRank, 
    y = distributiveFairnessRank, 
    color = color, fill = color
  )
) +
  ggalt::geom_encircle(alpha = 0.2, expand = 0.05) + # adds the convex hulls
  geom_point(shape = 16, alpha = 0.6) +
  scale_x_continuous(
    expand = c(0.01, 0.01),
    breaks = range(rrtri$EpistemicCorrectnessRank),
    labels = c("worst        ", "best")
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    breaks = max(rrtri$distributiveFairnessRank),
    labels = "best"#c("worst", "best")
  ) +
  scale_color_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  scale_fill_viridis_d(option = "B", begin = 0.25, end = 0.92) +
  labs(x = "epistemic correctness", y = "distributive fairness") +
  themebiplot

do.call(
  extension,
  args = list(
    filename = paste0("./outputGraphics/fig_3.", extension),
    width = 2300, height = 1000, unit = "px", res = 300, bg = "white")
)
ggpubr::ggarrange(
  a, b,
  align = "hv",
  common.legend = TRUE,
  legend = "right"
)
dev.off()
