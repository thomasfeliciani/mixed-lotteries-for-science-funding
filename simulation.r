

rm(list = ls())

if (!dir.exists("./output/")) dir.create("./output/")



gini = function (x) {# adapted from ineq::Gini 
  if (any(is.na(x)))
    warning("NAs were ignored in the calculation of the Gini coefficient.")
  x <- sort(as.numeric(na.omit(x)))
  n <- length(x)
  G <- sum(x * 1L:n)
  G <- 2 * G/sum(x) - (n + 1L)
  return(G/(n - 1L))
}



nProposals = 10
fundingRate = 5
panelChoice = 0.5
lotteryChoice = 1 - panelChoice



won <- rep(0, times = nProposals) # tally of awards
subjMerit <- rbeta(n = nProposals, shape1 = 10, shape2 = 3)



thFundable <- sort(subjMerit, decreasing = TRUE)[fundingRate]
thExcellent <- 
  sort(subjMerit, decreasing = TRUE)[round(fundingRate * panelChoice)]

fundable <- subjMerit >= thFundable
excellent <- subjMerit >= thExcellent
lotteryPool <- fundable & !excellent


proposals <- data.frame(
  subjMerit = subjMerit,
  fundable = fundable,
  excellent = excellent,
  lotteryPool = lotteryPool
)













































































