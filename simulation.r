# Runs on R 4.1.2

rm(list = ls())
source("util.r")

if (!dir.exists("./output/")) dir.create("./output/")






nProposals = 10
targetFundingRate = 0.5
sufficientMerit = 0.6

lotteryChoiceRate = 0.3
#lotterySize = 0.5
scaleGranularity = 5
nReviewers = 2
#panelChoice = fundingRate - lotteryChoice # default
panelGranularity = scaleGranularity * nReviewers - (nReviewers - 1)







if (targetFundingRate != panelChoice + lotteryChoice) warning(
  "panelChoice and lotteryChoice don't add up to the fundingRate."
)


p <- data.frame(
  won = rep(0, times = nProposals), # tally of awards
  gradeCont = rbeta(n = nProposals, shape1 = 10, shape2 = 3)
)

# We convert the raw, continuous evaluation "gradeContinuous" into an ordinal
# variable, with as many equivalence classes as there are possible unique
# evaluations by the panel (panelGranularity).
p$gradeOrd <- findInterval(
  x = p$gradeCont,
  vec = 1:(panelGranularity-1) / panelGranularity
)


# We identify which proposals received a sufficiently high grade for it to be
# considered for funding. When sufficientMerit is set to 0, it means that all
# proposals will be eligible: this models a Type 4 lottery, where a panel's 
# opinion does not matter at all, and all (formally eligibile) proposals make it
# to the lottery pool.
p$sufficMeritorious <- p$gradeOrd >= ((panelGranularity - 1) * sufficientMerit) 


# We calculate the resulting ranking position (highest grade means 1st
# position). Note that this is a weak ordering, meaning that proposals
# with the same grade will be in a tie. For downstream efficiency, here we
# calculate two alternative ways of coding ties: "floor" and "ceiling".
p$panelRank_floor <- nProposals - rank(p$gradeOrd, ties.method = "max") + 1
p$panelRank_ceil <- nProposals - rank(p$gradeOrd, ties.method = "min") + 1


# And we initialize some other variables:
p$panelChoice <- FALSE
p$lotteryPool <- FALSE
p$lotteryChoice <- FALSE
p$funded <- FALSE


# Next we calculate how many proposals are to be funded.
NtoBeFunded <- round(nProposals * targetFundingRate)
NtoBeDrawn <- round(NtoBeFunded * lotteryChoiceRate)

# And run the lottery.
p0 <- runType0(p, NtoBeFunded)
p1 <- runType1(p, NtoBeFunded, weights = NA)
p2 <- runType2(p, NtoBeFunded, NtoBeDrawn, weights = NA)
p3 <- runType3(p, NtoBeFunded, weights = NA)
p4 <- runType4(p, NtoBeFunded, weights = NA)










