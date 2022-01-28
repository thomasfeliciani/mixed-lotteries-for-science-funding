# Runs on R 4.1.2

rm(list = ls())
source("util.r")

if (!dir.exists("./output/")) dir.create("./output/")






nProposals = 10
fundingRate = 0.5
lotteryChoice = 0.3
#lotterySize = 0.5
scaleGranularity = 5
nReviewers = 3
panelChoice = fundingRate - lotteryChoice # default
panelGranularity = scaleGranularity * nReviewers - (nReviewers - 1)







if (fundingRate != panelChoice + lotteryChoice) warning(
  "panelChoice and lotteryChoice don't add up to the fundingRate."
)


p <- data.frame(
  won = rep(0, times = nProposals), # tally of awards
  gradeContinuous = rbeta(n = nProposals, shape1 = 10, shape2 = 3)
)

# We convert the raw, continuous evaluation "gradeContinuous" into an ordinal
# variable, with as many equivalence classes as there are possible unique
# evaluation by the panel (panelGranularity).
p$gradeOrd <- findInterval(
  x = p$gradeContinuous,
  vec = 0:panelGranularity / panelGranularity
)


# We calculate the resulting ranking position (highest grade means 1st position)
p$panelRank <- nProposals + 1 - rank(p$panelGrade, ties.method = "min")


nBestFundable <- round((panelChoice + lotterySize) * nProposals)
p$fundable <- p$panelRank <= nBestFundable

nBestExcellent <- round(panelChoice * nProposals)
p$excellent <- p$panelRank <= nBestExcellent

#$fundable <- p$panelGrade >= thFundable
#p$excellent <- p$panelGrade >= thExcellent
p$lotteryPool <- p$fundable & !p$excellent
















































































