rm(list = ls())

library("reshape2")
library("ggplot2")


publications <- as.data.frame(rbind(
  c(2022, 2),
  c(2021, 7),
  c(2020, 9),
  c(2019, 6),
  c(2018, 1),
  c(2017, 1),
  c(2016, 2),
  c(2015, 1),
  c(2014, 1),
  c(2013, 2),
  c(2012, 0),
  c(2011, 1),
  c(2010, 0),
  c(2009, 1),
  c(2008, 1),
  c(2007, 2),
  c(2006, 0),
  c(2005, 0),
  c(2004, 0),
  c(2003, 0),
  c(2002, 0),
  c(2001, 0),
  c(2000, 0),
  c(1999, 0),
  c(1998, 2)
))
names(publications) <- c("year", "count")

png(
  filename = "./outputGraphics/fig_0.png",
  width = 2000, height = 1000, unit = "px", res = 300, bg = "transparent")
ggplot(data = publications, aes(x = year, y = count)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = 1998:2022) +
  scale_y_continuous(breaks = 0:12) +
  xlab("publication year") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    #axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
dev.off()


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

load("./output/results.RData")




r <- reshape2::melt(
  data = r,
  id.vars = names(r)[! names(r) %in% c("merit", "bias", "inequality")]
)
#r <- r[r$variable != "inequality",]

# Renaming variable labels
#r$variable <- as.character(r$variable)
#r$variable[r$variable == "merit"] <- "merit\n(correlation with\nreference evaluation)"
#r$variable[r$variable == "bias"] <- "bias\n(correlation with\nunimportant attribute)"
#r$variable[r$variable == "inequality"] <- "inequality\n(Gini index)\n"

# Adding a color variable
r$color <- 2
r$color[r$type == "type 0"] <- 0
r$color[r$type == "type 1"] <- 1
r$color[r$type == "type 3"] <- 3
r$color[r$type == "type 4"] <- 4
r$color <- as.factor(r$color)

# Defining the plotting function:
plotResults <- function (rr) {
  ggplot(data = rr, mapping = aes(x = value, y = type, fill = color)) +
    geom_violin(scale = "width", color = "gray80", fill = "gray80") +
    geom_boxplot(width = 0.5, alpha = 1) +
    facet_wrap(nrow = 1, facets = "variable", scales = "free_x") +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_d(option = "B") +
    xlab(paste0(
      "correlation with reference evaluation", "                ",
      "correlation with unimportant attribute", "                             ",
      "   Gini index"
    )) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "gray97"),
      panel.grid = element_line(color = "gray93", size = 0.2),
      strip.text.x = element_text(size = 12),
      axis.line = element_line(color = "black", size = 1),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 8, hjust = 0.1), ############
      strip.background = element_blank(),
      legend.position = "NA"
    )# |> print()
}




# Baseline
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2 # c(0.2, 0.5)
)

png(
  filename = "./outputGraphics/fig_1.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()

# Make pairwise comparison between types to learn which differences are 
# significant:
meritAOV <- aov(value ~ as.factor(type), data = rr[rr$variable == "merit",])
plot(meritAOV); TukeyHSD(meritAOV)
biasAOV <- aov(value ~ as.factor(type), data = rr[rr$variable == "bias",])
plot(biasAOV); TukeyHSD(biasAOV)
biasAOV <- aov(value ~ as.factor(type), data = rr[rr$variable == "inequality",])
plot(biasAOV); TukeyHSD(biasAOV)





# stronger competition
rr <- subset(
  r,
  r$targetFundingRate == 0.25 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.75 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2
)

png(
  filename = "./outputGraphics/fig_2_stronger competition.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()


# weaker competition
rr <- subset(
  r,
  r$targetFundingRate == 0.75 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.25 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.2
)
png(
  filename = "./outputGraphics/fig_3_weaker_competition.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()


# stronger bias
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 5 & # c(3, 5, 10)
    r$nReviewers == 5 & # c(3, 5, 7)
    r$panelBias == 0.5 # c(0.2, 0.5)
)
png(
  filename = "./outputGraphics/fig_6_stronger bias.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()


# higher panel granularity 
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 10 & # c(3, 5, 10)
    r$nReviewers == 7 & # c(3, 5, 7)
    r$panelBias == 0.2
)
png(
  filename = "./outputGraphics/fig_4_higher granularity.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()


# lower panel granularity 
rr <- subset(
  r,
  r$targetFundingRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$sufficientMerit == 0.5 & # c(0.25, 0.5, 0.75)
    #r$lotteryChoiceRate == 0.5 & # c(0.25, 0.5, 0.75)
    r$scaleGranularity == 3 & # c(3, 5, 10)
    r$nReviewers == 3 & # c(3, 5, 7)
    r$panelBias == 0.2
)
png(
  filename = "./outputGraphics/fig_5_lower granularity.png",
  width = 2300, height = 1000, unit = "px", res = 300, bg = "transparent")
plotResults(rr)
dev.off()