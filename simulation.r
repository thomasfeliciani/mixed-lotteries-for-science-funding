# The first part of this script defines the function to run a Monte Carlo
# simulation experiment.
# The second part runs the simulation experiment exploring a specified parameter
# space.

# Runs on R 4.2.2.

################################################################################
# Cleaning environment and loading resources
rm(list = ls())
source("util.r") # This script contains some of the core functions we need
library("compiler")

debug = FALSE

################################################################################
##  Simulation  ################################################################
################################################################################
# Here we define the function "run" which executes one model simulation under
# the specified conditions (model parameters are passed to "run" as function
# arguments).
run <- function(
  nProposals = 100,        # parameter N
  targetFundingRate = 0.5, # parameter T
  sufficientMerit = 0.6,   # parameter θ ("sufficiency bar")
  scaleGranularity = 5,    # parameter γ (number of available grades)
  nReviewers = 2,          # parameter R (number of reviewers)
  panelError = 0.1,        # parameter σ_ε (size of evaluation errors)
  panelBias = 0.1,         # parameter λ (size of evaluation bias)
  repeatLottery = 100,     # how many repetitions over the same set of proposals
  seed = NULL
) {
  
  # Setting random seed
  if(is.null(seed)) seed <- sample(-999999999:999999999, size = 1)
  set.seed(seed)
  
  # Initializing proposals.
  # Note that we draw the reference evaluation
  p <- data.frame(
    # initializing the variable where we'll store the tally of awards:
    won = rep(0, times = nProposals),
    
    # reference evaluation:
    refEval = rbeta(n = nProposals, shape1 = 10, shape2 = 3),
    
    # a random error term that we'll use to simulate biased evaluations by the 
    # typical panel (denoted "b" in the article).
    attrUncorr = runif(n = nProposals, min = 0, max = 1) # random term "b"
  )
  #p$attrCorrPos <- faux::rnorm_pre(x = p$refEval, r = 0.5, empirical = TRUE)
  
  # Inizialiting vectors where we'll store the outcomes of each proposal:
  p0won <- p1won <- p2_10won <- p2_50won <- p2_90won <- p3won <- p4won <- 
    rep(0, times = nProposals)
  
  
  # And run the panel selection with the appropriate lottery where needed:
  for (t in 1:repeatLottery){
    if (debug) print(t)
    # Panel evaluation approximates the evaluation of the reference panel 
    # with some error, "panelError".
    # But is also undesirably affected by an unrelated proposals attribute. The
    # size of this influence is determined by "panelBias". So:
    p$panEval <- p$refEval + # Reference evaluation by the panel
      rnorm(n = nProposals, mean = 0, sd = panelError) + # plus random error
      (p$attrUncorr * panelBias) # plus bias
      #faux::rnorm_pre(x = p$attrUncorr, r = panelBias, empirical = TRUE)
      #sapply(p$attrUncorr, \(x) rnorm(n = 1, mean = x, sd = panelBias)) # +bias
    #plot(p$attrUncorr, panEval)
    #print(paste0("panelBias=",panelBias,"; corr=",cor(p$attrUncorr,panEval)))
    #p$panEval <- faux::rnorm_pre(
    #  x = p[,c("refEval", "attrUncorr")],
    #  r = c(panelError, panelBias)
    #)
    
    # Truncating the panel evaluation so it doesn't exceed the range [0,1]:
    p$panEval <- truncate(p$panEval, min = 0, max = 1)
    
    if (var(p$panEval) == 0) {
      warning ("No variance in panel evaluation.")
      break()
    }
    
    # We convert the raw, continuous evaluation "panelGrade" into an ordinal
    # variable, with as many equivalence classes as there are possible unique
    # evaluations by the panel (panelGranularity).
    panelGranularity = scaleGranularity * nReviewers - (nReviewers - 1)
    p$panelGrade <- findInterval(
      x = p$panEval,
      vec = 1:(panelGranularity-1) / panelGranularity
    )
    
    #plotly::plot_ly(
    #  x=p$attrUncorr, y=p$attrCorrPos, z=p$refEval, color=p$panelGrade)
    
    # We identify which proposals received a sufficiently high grade for it to be
    # considered for funding. When sufficientMerit is set to 0, it means that all
    # proposals will be eligible: this models a Type 4 lottery, where a panel's 
    # opinion does not matter at all, and all (formally eligible) proposals make
    # it into the lottery pool.
    p$sufficMeritorious <- 
      p$panelGrade >= ((panelGranularity - 1) * sufficientMerit) 
    
    
    # We calculate the resulting ranking position (highest grade means 1st
    # position). Note that this is a weak ordering, meaning that proposals
    # with the same grade will be in a tie. For downstream efficiency, here we
    # calculate two alternative ways of coding ties: "floor" and "ceiling".
    p$panelRank_floor <- 
      nProposals - rank(p$panelGrade, ties.method = "max") + 1
    p$panelRank_ceil <- 
      nProposals - rank(p$panelGrade, ties.method = "min") + 1
    p$panelRank_strict <- 
      nProposals - rank(p$panelGrade, ties.method = "first") + 1
    
    
    # And we initialize some other variables:
    p$panelChoice <- FALSE
    p$lotteryPool <- FALSE
    p$lotteryChoice <- FALSE
    p$funded <- FALSE
    
    
    # Next we calculate how many proposals are to be funded.
    NtoBeFunded <- round(nProposals * targetFundingRate)
    #NtoBeDrawn <- round(NtoBeFunded * lotteryChoiceRate)
    
    
    # Now running the actually selection procedures.
    # Note that we run p2_* (i.e. Type 2) three times (i.e. p2_10won, p2_50won, 
    # p2_90won). This hardcodes the parameter *S* (the "lottery choice rate"),
    # which only pertains to Type 2 partial lotteries with bypass.
    p0won <- p0won + runType0(p, NtoBeFunded, panelError, panelBias)$funded
    p1won <- p1won + runType1(p, NtoBeFunded, w = NULL)$funded
    p2_10won <- p2_10won + runType2(
      p, NtoBeFunded, NtoBeDrawn = round(NtoBeFunded * 0.1), w = NULL)$funded
    p2_50won <- p2_50won + runType2(
      p, NtoBeFunded, NtoBeDrawn = round(NtoBeFunded * 0.5), w = NULL)$funded
    p2_90won <- p2_90won + runType2(
      p, NtoBeFunded, NtoBeDrawn = round(NtoBeFunded * 0.9), w = NULL)$funded
    p3won <- p3won + runType3(p, NtoBeFunded, w = NULL)$funded
    p4won <- p4won + runType4(p, NtoBeFunded, w = NULL)$funded
  }
  
  results <- data.frame(
    type = c(
      "type 0",
      "type 1", 
      "type 2, 10% pool", 
      "type 2, 50% pool", 
      "type 2, 90% pool", 
      "type 3", 
      "type 4"
    ),
    
    # Parameters:
    seed = rep(seed, times = 7),
    nProposals = rep(nProposals, times = 7),
    targetFundingRate = rep(targetFundingRate, times = 7),
    sufficientMerit = rep(sufficientMerit, times = 7),
    #lotteryChoiceRate = rep(lotteryChoiceRate, times = 5),
    scaleGranularity = rep(scaleGranularity, times = 7),
    nReviewers = rep(nReviewers, times = 7),
    panelBias = rep(panelBias, times = 7),
    repeatLottery = rep(repeatLottery, times = 7),
    
    # Outcome variables: 
    merit = rep(NA, times = 7),
    bias = rep(NA, times = 7),
    inequality = rep(NA, times = 7)
  )
  
  
  # Lastly we calculate performance. Foreach lottery type...
  types <- c("0", "1", "2_10", "2_50", "2_90", "3", "4")
  for (typ in 1:length(types)) {
    if(length(unique(eval(parse(text = paste0("p", types[typ], "won"))))) > 1) {
      
      # This is merit fairness:
      results$merit[typ] <- cor(
        x = p$refEval,
        y = eval(parse(text = paste0("p", types[typ], "won"))), # wins
        method = "spearman"
      )
      
      # This is for calculating unbiased fairness.
      # Later we'll calculate 1 - abs(results$bias).
      results$bias[typ] <- cor(
        x = p$attrUncorr,
        y = eval(parse(text = paste0("p", types[typ], "won"))), # wins
        method = "spearman"
      )
    }
    
    # This is used for calculating distributive fairness.
    # Later we'll take 1 - results$inequality.
    results$inequality[typ] <- gini(
      eval(parse(text = paste0("p", types[typ], "won")))
    )
  }
  
  return(results)
}


################################################################################
##  Usage  #####################################################################
################################################################################
# This part of the script illustrates how to run a simulation.
# We put it all inside a "if(FALSE)" statement. This prevents this block of code
# from being executed when sourcing this script.

if (FALSE) { #
  x <- run(
    nProposals = 100,
    targetFundingRate = 0.5,
    sufficientMerit = 0.6,
    scaleGranularity = 5,
    nReviewers = 2,
    panelBias = 0.3,
    repeatLottery = 100,
    seed = NULL
  )
}




################################################################################
##  Battery  ###################################################################
################################################################################
# This part of the script replicates the simulation experiment by running the
# model 100 times for each unique parameter configuration.

# Ensuring that we have a ./output/ directory where to store the results:
if (!dir.exists("./output/")) dir.create("./output/")

nRepetitions = 100 # how many runs per condition (default is 100)

# Creating a full factorial design:
parameterSpace <- expand.grid(
  targetFundingRate = c(0.25, 0.5, 0.75), # parameter T
  sufficientMerit = c(0.25, 0.5, 0.75), # parameter θ
  scaleGranularity = c(3, 5, 10), # parameter γ
  nReviewers = c(3, 5, 7), # parameter R
  panelBias = c(0.2, 0.5), # parameter λ
  repeatLottery = 100
)

######################################################################
# And now we run the simulation battery. This may take several hours.

# printing a progress bar in console:
pb <- txtProgressBar(min = 0, max = nRepetitions, style = 3)

# Compiling functions to speed up things a bit.
# We could use parallelization to improve performance dramatically. I didn't 
# because CPU time was still manageable without parallelization.
enableJIT(1)
for (rep in 1:nRepetitions) { # For N repetitions...
  
  for (bat in 1:nrow(parameterSpace)){ # For each parameter configuration...

    # ... we define the parameter configuration ...
    parameters <- list(
      nProposals = 100,
      targetFundingRate = parameterSpace$targetFundingRate[bat], # parameter T
      sufficientMerit = parameterSpace$sufficientMerit[bat], # parameter θ
      scaleGranularity = parameterSpace$scaleGranularity[bat], # parameter γ
      nReviewers = parameterSpace$nReviewers[bat], # parameter R
      panelBias = parameterSpace$panelBias[bat], # parameter λ
      repeatLottery = parameterSpace$repeatLottery[bat],
      seed = sample(-999999999:999999999, size = 1)
    )
    if(debug) print(parameters)
    
    # ... run the simulation with the right parameter configuration ...
    results <- do.call(run, parameters)
    
    # ... and append the results to a dataframe:
    ifelse(
      rep == 1 & bat == 1,
      r <- results,
      r <- rbind(r, results)
    )
  }
  
  # Updating the progress bar at the end of each repetition
  setTxtProgressBar(pb, rep)
}
close(pb)
enableJIT(0)

# Saving the output.
save(r, file = "./output/results.RData")