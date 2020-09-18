library("HyRiM")

## moseg
#
# Create a multi-objective zero-sum segment game from a single-objective 
# zero-sum distribution-valued game (of the mosg class).
#
# sosg:            Single-objective game of the mosg class.
# partitionPoints: Vector of partition points, in increasing order, including
#    the right and left endpoints of the support.
#
# returns: A game of the moseg class (multi-objective segment game).
moseg <- function(sosg, partitionPoints) { 
    moseg <- NULL
    if (sosg$dim != 1) {
        stop("only single-objective games can be turned into a multi-objective segment game.")
    }
    # Convert the loss-distribution payoffs into expectation vectors of the segments
    moseg$losses <- mapply(
        function(l) segmentedLossDistribution(l, partitionPoints),
        sosg$losses)
    moseg$partitionPoints <- partitionPoints

    # Copy other properties from the original game
    moseg$nDefenses <- sosg$nDefenses
    moseg$nAttacks <- sosg$nAttacks
    moseg$defensesDescriptions <- sosg$defensesDescriptions
    moseg$attacksDescriptions <- sosg$attacksDescriptions
    class(moseg) <- "moseg"
    moseg
}

## moseg.paretoNashEquilibrium
#
# Compute a Pareto-Nash-Equilibrium of a multi-objective segmented game based
# on a given weighting of the objectives.
#
# moseg:   Multi-objective segment game (of the moseg class).
# weights: Numeric vector of weights, one weight for each segment.
#
# returns: A Pareto-Nash equilibrium of the game with given weights (of the mgss class).
moseg.paretoNashEquilibrium <- function(moseg, weights) {
    if (length(weights) != length(moseg$partitionPoints) - 1) {
        stop(sprintf("there are %d segments, but %d weights were specified.",
            length(moseg$partitionPoints) - 1, length(weights)))
    }
    # Scalarize the game with the given weights
    scalarLosses <- rep(0, length(moseg$losses))
    for (i in 1:length(scalarLosses)) {
        scalarLosses[i] <- moseg$losses[i]$expectedValues %*% weights
    }
    scalarGame <- mosg(n=moseg$nDefenses, m=moseg$nAttacks, goals=1, losses=scalarLosses)
    # Compute an equilibrium of the scalar game using HyRiM's built-in algorithm
    mgss(scalarGame)
}

## segmentedLossDistribution
#
# Given a loss distribution and a partition of its support, compute the vector
# of its expected values in the segments given by the partition points.
#
# lossDistribution:   Distribution of the lossDistribution class.
# partitionPoints:    Vector of partition points, including the right and left
#     endpoints of the support.
#
# returns: A Pareto-Nash equilibrium of the game with given weights (of the mgss class).
segmentedLossDistribution <- function(lossDistribution, partitionPoints) {
    segmentedDistribution <- NULL
    expectedValues <- rep(0, length(partitionPoints) - 1)

    # Check that there are at least two partition points
    if (length(partitionPoints) <= 1) {
        stop("at least two partition points required.")
    }
    # Check that the partition points are in ascending order 
    for (i in 1:(length(partitionPoints) - 1)) {
        if (partitionPoints[i+1] <= partitionPoints[i]) {
            stop("partition points must be in ascending order.")
        }
    }

    # Compute expected values in the discrete, finitely-supported case 
    if (lossDistribution$is.discrete) {
        supp <- lossDistribution$supp
        pdf <- lossDistribution$dpdf
        i <- 1 # Index of the current support point
        j <- 2 # Index of the right end of the current partition

        # Skip support points left of the first partition
        while (supp[i] < partitionPoints[1]) {
            i <- i + 1
        } 

        while (i <= length(supp) && j <= length(partitionPoints)) {
            # All intervals except the last one are half-open, the last one is closed.
            if (supp[i] < partitionPoints[j] || (supp[i] == partitionPoints[j] && j == length(partitionPoints))) {
                expectedValues[j - 1] <- expectedValues[j - 1] + supp[i] * pdf[i]
                i <- i + 1 # Go to the next support point
            } else {
                j <- j + 1 # Go to the next partition
            }
        }
    } 
    # Compute expected values in the absolutely-continuous case
    else {
        density <- lossDistribution$lossdistr
        # Go over all partition intervals
        for (i in 1:(length(partitionPoints) - 1)) {
            # Numerically calculate the integral. The normalizationFactor is calculated by HyRiM since the kernel-estimated densities often do not integrate to 1 on the specified support.
            integral <- integrate(density, partitionPoints[i], partitionPoints[i+1])
            expectedValues[i] <- integral$value * lossDistribution$normalizationFactor
        }
    }
    segmentedDistribution$expectedValues <- expectedValues
    class(segmentedDistribution) <- "segmentedLossDistribution"
    return(segmentedDistribution)
}