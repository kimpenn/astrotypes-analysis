###########################################################################
## Author: Youtao Lu <luyoutao@sas.upenn.edu>
## 
## Copyright (c) 2021-2022, Youtao Lu and Junhyong Kim, Department of Biology, University of Pennsylvania
## Copyright (c) 2021-2022, Jai-Yoon Sul, Department of Systems Pharmacology and Translational Therapeutics, University of Pennsylvania
## 
## This Source Code is distributed under Creative Commons Attribution License 4.0 (CC BY).
###########################################################################
if (!exists("GSEAJava") || is.environment(GSEAJava)) GSEAJava <- new.env(parent = emptyenv())
local({
    .VERSION = "1.5"

    getRunSum <- function(geneList, geneset, exponent = 1, minGSSize = 10, maxGSSize = 500) {
        geneset <- unique(geneset)
        g <- names(geneList)
        n <- length(geneList)
        P <- rep(NA, n)
        names(P) <- g
        geneset <- geneset[geneset %in% g] # get rid of non-existing genes
        m <- length(geneset)
        if (m < minGSSize || m > maxGSSize) {
            return(P) 
        }
        h <- g %in% geneset
        P[h] <- abs(geneList[h]) ^ exponent
        P[h] <- P[h] / sum(P[h])
        P[!h] <- -1/(n - m)
        P <- cumsum(P)
    }

    getRunSums <- function(geneList, genesets, exponent = 1, minGSSize = 10, maxGSSize = 500) {
        sets <- names(genesets)
        res <- lapply(genesets, function(geneset) {
            getRunSum(geneList, geneset, exponent = 1, minGSSize = minGSSize, maxGSSize = maxGSSize)
        })
        res <- do.call(cbind, res)
        colnames(res) <- sets
        res
    }

    for (obj in ls()) {
        assign(obj, get(obj), envir = GSEAJava)
    }
})
