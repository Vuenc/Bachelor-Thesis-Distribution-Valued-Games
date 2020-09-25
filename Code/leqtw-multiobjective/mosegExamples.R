source("multiobjectiveSegmentGame.R")
library("HyRiM")

## computeParetoNashAndMgss
#
# Base function for both examples: Segments a mosg and computes its Pareto-Nash equilibrium wrt. weights,
# computes the HyRiM mgss solution for comparison.
#
# distributionValuedGame: The mosg instance to be converted.
# partitionPoints:        The points for the segmentation.
# weights:                Weights used for computing the Pareto-Nash equilibrium.
#
# returns: Both the Pareto-Nash equilibrium of the segmented game and the mgss solution HyRiM computes for the original game
computeParetoNashAndMgss <- function(distributionValuedGame, partitionPoints, weights) {
    output <- NULL

    # Creating a segmented game from the distribution-valued game:
    output$segmentedGame <- moseg(sosg=distributionValuedGame, partitionPoints=partitionPoints)

    # Computing a Pareto-Nash equilibrium:
    output$segmentedGameEquilibrium <- moseg.paretoNashEquilibrium(moseg=output$segmentedGame, weights=weights)
    
    # For comparison, compute the mgss HyRiM computes for the original distribution-valued game.
    output$originalMgss <- mgss(G=distributionValuedGame)

    output
}

## exampleDiscrete
#
# Example of computing a Pareto-Nash equilibrium of a segmented 2x2 game, obtained from a game
# that has discrete distributions supported on {1, 2, ..., 10} as payoffs.
#
# returns: Both the Pareto-Nash equilibrium of the segmented game and the mgss solution HyRiM computes for the original game.
exampleDiscrete <- function() {
    # A distribution-valued game supported on the numbers from 1 to 10
    support <- c(1,2,3,4,5,6,7,8,9,10)
    distributionValuedGame <- mosg(n=2, m=2, goals=1, losses = list(
        lossDistribution(supp=support, discrete = TRUE, dataType = "pdf", dat=c(0.05, 0.55, 0.071, 0.30, 0.005, 0.01, 0.002, 0.003, 0.001, 0.008)),
        lossDistribution(supp=support, discrete = TRUE, dataType = "pdf", dat=c(0.03, 0.48, 0.04, 0.01, 0.04, 0.07, 0.03, 0.1, 0.05, 0.15)),
        lossDistribution(supp=support, discrete = TRUE, dataType = "pdf", dat=c(0.095, 0.069, 0.033, 0.017, 0.16, 0.15, 0.20, 0.13, 0.01, 0.136)),
        lossDistribution(supp=support, discrete = TRUE, dataType = "pdf", dat=c(0.005, 0.413, 0.034, 0.1, 0.1, 0.12, 0.047, 0.13, 0.013, 0.038))))
    
    # Segment the game, compute Pareto-Nash equilibrium and mgss solution.
    partitionPoints <- c(1, 4, 7, 10)
    weights <- c(0.1, 0.2, 0.7)
    computeParetoNashAndMgss(distributionValuedGame, partitionPoints, weights)
}

## exampleAC
#
# Example of computing a Pareto-Nash equilibrium of a segmented 2x2 game, obtained from a game
# that has absolutely continuous distributions as payoffs.
#
# returns: Both the Pareto-Nash equilibrium of the segmented game and the mgss solution HyRiM computes for the original game.
exampleAC <- function() {
    # A distribution-valued game on the numbers from 1 to 10
    distributionValuedGame <- mosg(n=2, m=2, goals=1, losses = list(
        # Samples from normal distribution (mu=6, sigma=2),
        lossDistribution(discrete = FALSE, dat=c(7.683, 8.589, 6.487, 5.160, 8.326, 5.693, 4.098, 7.295, 7.878, 2.448)),
        # Samples from two normal distribution (mu=3, sigma=0.3 / mu=8, sigma=3),
        lossDistribution(discrete = FALSE, dat=c(2.821, 2.943, 3.314, 3.069, 2.862, 2.225, 4.253, 11.967, 11.786, 8.033, 3.427, 13.422)),
        # Samples representing uniform distribution on [4, 5],
        lossDistribution(discrete = FALSE, dat=c(4.00, 4.05, 4.10, 4.15, 4.20, 4.25, 4.30, 4.35, 4.40, 4.45, 4.50, 4.55, 4.60, 4.65, 4.70, 4.75, 4.80, 4.85, 4.90, 4.95, 5.00)),
        # Samples representing distribution centered around 2, with an outlier at 15
        lossDistribution(discrete = FALSE, dat=c(1.8, 1.84, 1.88, 1.92, 1.96, 2.0, 2.04, 2.08, 2.12, 2.16, 2.20, 15))))
    
    partitionPoints <- c(1, 4, 5, 10, 25)
    weights <- c(0.1, 0.2, 0.2, 0.5)
    computeParetoNashAndMgss(distributionValuedGame, partitionPoints, weights)
}