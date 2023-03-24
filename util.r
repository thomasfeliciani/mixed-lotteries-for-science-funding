# This script contains all support functions for the simulation.

################################################################################
################################################################################
# Types of selection procedures



# Type 0 _______________________________________________________________________
# Type 0 lotteries actually involve no lottery at all: funding is entirely
# determined by the panel ranking. If there are ties for the last funded
# position, then the panel re-examines the proposals in the tie at the funding
# line, and breaks the tie(s) based on their re-examination.
runType0 <- function(p, NtoBeFunded, panelError, panelBias, ...) {
  winners <- p$panelRank_floor <= NtoBeFunded
  
  # If there are ties, then some potential winners need to be discarded.
  # To do that, we first try to break that tie by examining the tieing proposals
  # closer.
  if (sum(winners) > NtoBeFunded) {
    
    # Proposals with a ranking position above the funding line and thus not in 
    # the tie:
    abovetie <- which(p$panelRank_floor < max(p$panelRank_floor[winners]))
    # Proposals in the ranking that tie for the funding line:
    tie <- which(p$panelRank_floor == max(p$panelRank_floor[winners]))
    
    # Re-examining those proposals.
    reevaluation <- p$refEval[tie] + # Reference evaluation by the panel
      rnorm(n = length(tie), mean = 0, sd = panelError) + # plus random error
      (p$attrUncorr[tie] * panelBias)# plus bias (b)
    
    # Updating the winners:
    reevaluatedWinners <- rep(FALSE, times = nrow(p))
    
    # The winners in a Type 0 systems are the proposals that...
    reevaluatedWinners[c(
      # ... have received an evaluation above the tie ...
      abovetie,
      #... and, those in the tie that were reevaluated more favorably:
      tie[order(reevaluation, decreasing=T)[1:(NtoBeFunded - length(abovetie))]]
    )] <- TRUE
    
    winners <- reevaluatedWinners
  }
  
  # Updating wins records in the proposals dataframe "p":
  p$won <- p$won + winners # adding to the winners' tally of total wins
  p$panelChoice <- winners
  p$funded <- winners
  
  return(p)
}


# Type 1 _______________________________________________________________________
# Type 1 means that a lottery is used to break any tie for the last fundable
# position(s) in the panel ranking.
runType1 <- function(p, NtoBeFunded, w = NULL, ...) {
  
  # Using the "ceiling" ranking allows us to exclude from the panel choice 
  # any tie that might arise for the last fundable position(s).
  p$panelChoice <- p$panelRank_ceil <= NtoBeFunded
  
  
  # Are there ties we need to break with a lottery?
  NtoBeDrawn <- NtoBeFunded - sum(p$panelChoice)
  if (NtoBeDrawn > 0) {
    # Identifying proposals in the tie
    pool <- which(
      !p$panelChoice &
        p$panelRank_floor <= NtoBeFunded
    )
    p$lotteryPool[pool] <- TRUE
    
    # Drawing winners from the pool.
    # We declare probability weights so that, unless specified otherwise, the
    # sampling will be from a uniform distribution.
    if (is.null(w)) w <- rep(1, times = length(pool))
    
    # And then we determine the tie-breaking lottery winners:
    lotteryWinners <- sample(
      x = pool,
      size = NtoBeDrawn,
      replace = FALSE,
      prob = w
    )
    p$lotteryChoice[lotteryWinners] <- TRUE
  }
  
  
  # Funded proposals are those chosen by the panel, plus those that won the
  # lottery:
  p$funded[p$panelChoice | p$lotteryChoice] <- TRUE
  p$won <- p$won + p$funded # (tally of wins)
  
  return(p)
}


# Type 2 _______________________________________________________________________
# Type 2 is a mixed lottery with bypass: the panel can choose some proposals,
# (we assume they'll choose those they liked the most), but some winners will
# have to be chosen by lot from among the ones deemed to be "fundable".
#
runType2 <- function(p, NtoBeFunded, NtoBeDrawn, w = NULL, ...) {
  
  # Using the "ceiling" ranking allows us to exclude from the panel choice 
  # any tie that might arise for the last fundable position(s).
  # Note that there might be ties that make it difficult for the panel to choose
  # their quota. We assume here that the panel will make an arbitrary decision
  # as to how to break that tie.
  p$panelChoice <- p$panelRank_strict <= NtoBeFunded - NtoBeDrawn
  
  # Then we define the lottery pool. It should include:
  pool <- which(
    !p$panelChoice & # - proposals that have not already been chosen;
      p$sufficMeritorious # - proposals found to be sufficiently meritorious.
  )
  p$lotteryPool[pool] <- TRUE
  
  # Drawing winners from the pool.
  # We declare probability weights so that, unless specified otherwise, the
  # sampling will be from a uniform distribution.
  if (length(pool) > 0) { # If there are eligible proposals in the pool:
    
    if (length(pool) == 1) {
      lotteryWinners <- pool
    } else {
      if (is.null(w)) w <- rep(1, times = length(pool))
      
      lotteryWinners <- sample(
        x = pool,
        size = min(NtoBeDrawn, length(pool)),
        replace = FALSE,
        prob = w
      )
    }
    
    p$lotteryChoice[lotteryWinners] <- TRUE
  } else { # Else, if there aren't ...
    p$lotteryChoice <- FALSE
  }
  
  
  
  # Funded proposals are those chosen by the panel, plus those that won the
  # lottery:
  p$funded[p$panelChoice | p$lotteryChoice] <- TRUE
  p$won <- p$won + p$funded # (tally of wins)
  
  return(p)
}


# Type 3 _______________________________________________________________________
# These are lotteries of all sufficiently meritorious proposals.
#
runType3 <- function(p, NtoBeFunded, w = NULL, ...) {
  
  # The lottery pool includes all proposals that received a sufficiently
  # high grade, i.e. the ones that are sufficiently meritorious:
  pool <- which(p$sufficMeritorious)
  p$lotteryPool[pool] <- TRUE
  
  # Drawing winners from the pool.
  # We declare probability weights so that, unless specified otherwise, the
  # sampling will be from a uniform distribution.
  if (is.null(w)) w <- rep(1, times = length(pool))
  lotteryWinners <- sample(
    x = pool,
    size = min(NtoBeFunded, length(pool)),
    replace = FALSE,
    prob = w
  )
  p$lotteryChoice[lotteryWinners] <- TRUE
  p$funded[lotteryWinners] <- TRUE
  p$won <- p$won + p$funded # (tally of wins)
  
  return(p)
}


# Type 4 _______________________________________________________________________
# A lottery of this type is a lottery of all proposals -- no evaluation by the
# panel comes into play, not even in determining the lottery pool.
#
runType4 <- function(p, NtoBeFunded, w = NULL, ...) {
  
  # All proposals are in the lottery pool:
  p$lotteryPool <- TRUE
  
  # Drawing winners.
  # We declare probability weights so that, unless specified otherwise, the
  # sampling will be from a uniform distribution.
  if (is.null(w)) w <- rep(1, times = nrow(p))
  lotteryWinners <- sample(
    x = 1:nrow(p),
    size = NtoBeFunded, ##
    replace = FALSE,
    prob = w
  )
  p$lotteryChoice[lotteryWinners] <- TRUE
  p$funded[lotteryWinners] <- TRUE
  p$won <- p$won + p$funded # (tally of wins)
  
  return(p)
}




################################################################################
################################################################################
# Miscellanea

gini = function (x) { # adapted from ineq::Gini 
  if (any(is.na(x))) {
    x <- na.omit(x)
    warning("NAs are ignored in the calculation of the Gini coefficient.")
  }
  x <- x |> as.numeric() |> sort()
  if (length(x) <= 1) return(NA) ###
  if (all(x == 0)) return(0) ###
  n <- length(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  return(G / (n - 1L))
}


truncate <- function(x, min = 0, max = 1){
  if (length(x) > 1) return(sapply(x, truncate, min = min, max = max))
  ifelse(
    x <= min,
    return(min),
    ifelse(
      x >= max,
      return(max),
      return(x)
    )
  )
}