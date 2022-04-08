# Runs on R 4.1.2

rm(list = ls())
source("util.r")
library("faux")
library("compiler")


################################################################################
##  Simulation  ################################################################
################################################################################

run <- function(
  nProposals = 100,
  targetFundingRate = 0.5,
  sufficientMerit = 0.6,
  lotteryChoiceRate = 0.3,
  scaleGranularity = 5,
  nReviewers = 2,
  panelBias = 0.3, # influence of unrelated attribute on panel evaluation
  repeatLottery = 100,
  seed = NULL
) {
  
  # For debugging:
  # 
  
  
  if(is.null(seed)) seed <- sample(-999999999:999999999, size = 1)
  set.seed(seed)
  
  
  
  p <- data.frame(
    #won = rep(0, times = nProposals), # tally of awards
    refEval = rbeta(n = nProposals, shape1 = 10, shape2 = 3),
    attrUncorr = runif(n = nProposals, min = 0, max = 1)
  )
  
  p$attrCorrPos <- faux::rnorm_pre(x = p$refEval, r = 0.5, empirical = TRUE)
  #p$attrCorrNeg <- faux::rnorm_pre(x = p$refEval, r = 0.5, empirical = TRUE)
  #p$attrCorrNeg <- abs(p$attrCorrNeg - max(p$attrCorrNeg))
  
  # Panel evaluation approximates the evaluation of the reference panel (r=0.5)
  # but is also undesirably affected by an unrelated proposals attribute. The
  # size of this influence is determined by "panelBias".
  p$panEval <- faux::rnorm_pre(
    x = p[,c("refEval", "attrUncorr")],
    r = c(0.5, panelBias)
  )
  
  # We convert the raw, continuous evaluation "gradeContinuous" into an ordinal
  # variable, with as many equivalence classes as there are possible unique
  # evaluations by the panel (panelGranularity).
  panelGranularity = scaleGranularity * nReviewers - (nReviewers - 1)
  p$panelGrade <- findInterval(
    x = p$panEval,
    vec = 1:(panelGranularity-1) / panelGranularity
  )
  
  #plot_ly(x=p$attrUncorr, y=p$attrCorrPos, z=p$refEval, color=p$panelGrade)
  
  # We identify which proposals received a sufficiently high grade for it to be
  # considered for funding. When sufficientMerit is set to 0, it means that all
  # proposals will be eligible: this models a Type 4 lottery, where a panel's 
  # opinion does not matter at all, and all (formally eligibile) proposals make
  # it to the lottery pool.
  p$sufficMeritorious <- 
    p$panelGrade >= ((panelGranularity - 1) * sufficientMerit) 
  
  
  # We calculate the resulting ranking position (highest grade means 1st
  # position). Note that this is a weak ordering, meaning that proposals
  # with the same grade will be in a tie. For downstream efficiency, here we
  # calculate two alternative ways of coding ties: "floor" and "ceiling".
  p$panelRank_floor <- nProposals - rank(p$panelGrade, ties.method = "max") + 1
  p$panelRank_ceil <- nProposals - rank(p$panelGrade, ties.method = "min") + 1
  p$won <- 0
  
  # And we initialize some other variables:
  p$panelChoice <- FALSE
  p$lotteryPool <- FALSE
  p$lotteryChoice <- FALSE
  p$funded <- FALSE
  
  
  # Next we calculate how many proposals are to be funded.
  NtoBeFunded <- round(nProposals * targetFundingRate)
  NtoBeDrawn <- round(NtoBeFunded * lotteryChoiceRate)
  
  # Inizialiting vectors where we'll store the outcomes of each proposal:
  p0won <- p1won <- p2won <- p3won <- p4won <- rep(0, times = nProposals)
  #merit0 <- merit1 <- merit2 <- merit3 <- merit4 <- c()
  #bias0 <- bias1 <- bias2 <- bias3 <- bias4 <- c()
  
  # And run the lottery, as many times as specified by "repeatLottery":
  for (t in 1:repeatLottery){
    p0won <- p0won + runType0(p, NtoBeFunded)$funded
    p1won <- p1won + runType1(p, NtoBeFunded, w = NULL)$funded
    p2won <- p2won + runType2(p, NtoBeFunded, NtoBeDrawn, w = NULL)$funded
    p3won <- p3won + runType3(p, NtoBeFunded, w = NULL)$funded
    p4won <- p4won + runType4(p, NtoBeFunded, w = NULL)$funded
  }
  
  results <- data.frame(
    type = c("type 0", "type 1", "type 2", "type 3", "type 4"),
    
    # Parameters:
    seed = rep(seed, times = 5),
    nProposals = rep(nProposals, times = 5),
    targetFundingRate = rep(targetFundingRate, times = 5),
    sufficientMerit = rep(sufficientMerit, times = 5),
    lotteryChoiceRate = rep(lotteryChoiceRate, times = 5),
    scaleGranularity = rep(scaleGranularity, times = 5),
    nReviewers = rep(nReviewers, times = 5),
    panelBias = rep(panelBias, times = 5),
    repeatLottery = rep(repeatLottery, times = 5),
    
    # Outcome variables: 
    merit = rep(NA, times = 5),
    bias = rep(NA, times = 5),
    gini = rep(NA, times = 5)
  )
  
  
  # Lastly we calculate performance:
  for (type in 0:4) { # foreach lottery type...
    if(length(unique(eval(parse(text = paste0("p", type, "won"))))) > 1) {
      results$merit[type + 1] <- cor(
        x = p$refEval,
        y = eval(parse(text = paste0("p", type, "won"))), # wins
        method = "spearman"
      )
      
      results$bias[type + 1] <- cor(
        x = p$attrUncorr,
        y = eval(parse(text = paste0("p", type, "won"))), # wins
        method = "spearman"
      )
    }

    results$gini[type + 1] <- gini(
      eval(parse(text = paste0("p", type, "won")))
    )
  }
  
  return(results)
}


################################################################################
##  Usage  #####################################################################
################################################################################
if (FALSE) {
  run(
    nProposals = 100,
    targetFundingRate = 0.5,
    sufficientMerit = 0.6,
    lotteryChoiceRate = 0.3,
    scaleGranularity = 5,
    nReviewers = 2,
    panelBias = 0.3, # influence of unrelated attribute on panel evaluation
    repeatLottery = 100,
    seed = NULL
  )
  
  # For debugging:
  #
  nProposals = 100
  targetFundingRate = 0.75
  sufficientMerit = 0.25
  lotteryChoiceRate = 0.5
  scaleGranularity = 10
  nReviewers = 5
  panelBias = 0.3
  repeatLottery = 100
  seed = -366345978
}




################################################################################
##  Battery  ###################################################################
################################################################################

if (!dir.exists("./output/")) dir.create("./output/")

nRepetitions = 100 ##############

parameterSpace <- expand.grid(
  targetFundingRate = c(0.25, 0.5, 0.75),
  sufficientMerit = c(0.25, 0.5, 0.75),
  lotteryChoiceRate = c(0.25, 0.5, 0.75),
  scaleGranularity = c(3, 5, 10),
  nReviewers = c(3, 5, 7),
  repeatLottery = 100
)

pb <- txtProgressBar(min = 0, max = nRepetitions, style = 3)

enableJIT(1)
for (rep in 1:nRepetitions) { # For N repetitions...
  
  for (bat in 1:nrow(parameterSpace)){ # For each parameter configuration...
    
    seed <- sample(-999999999:999999999, size = 1)
    #print(cbind(parameterSpace, rep(seed, times = nrow(parameterSpace)))[bat,])
    
    results <- run( # ... run the simulation ...
      targetFundingRate = parameterSpace$targetFundingRate[bat],
      sufficientMerit = parameterSpace$sufficientMerit[bat],
      lotteryChoiceRate = parameterSpace$lotteryChoiceRate[bat],
      scaleGranularity = parameterSpace$scaleGranularity[bat],
      nReviewers = parameterSpace$nReviewers[bat],
      repeatLottery = parameterSpace$repeatLottery[bat],
      seed = seed
    )
    
    # ... and append the results to a dataframe:
    ifelse(
      rep == 1 & bat == 1,
      r <- results,
      r <- rbind(r, results)
    )
  }
  
  
  setTxtProgressBar(pb, rep)
}
close(pb)
enableJIT(0)

save(r, file = "./output/results.RData")