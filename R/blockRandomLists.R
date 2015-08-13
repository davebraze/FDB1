##' @title Block randomize a set of trials.
##'
##' @description Generate a specified number of uniquely block randomized copies of a stimulus list.
##'
##' @details
##' TODO: this function assumes an equal number of items per condition. extend it to work with a
##' specified number of items per condition.
##'
##' @param nLists Number of randomized lists to create (default=4).
##' @param repBlock Number of repetitions/condition within each block (default=2).
##' @param nCond Number of conditions in data.
##' @param data A data.frame containing specification of all conditions/items, sorted by condition.
##' @return A list of data.frames
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
blockRandomLists <- function(nLists=4, repBlock=2, nCond, data) {
    LIST <- 1:nLists
    nItem <- dim(data)[1]
    nBlock <- nItem/(nCond*repBlock)
    blockLen <- nItem/nBlock

    retval <- list()
    for (ll in LIST) {
        block <- as.vector(sapply(1:nCond, function(x) sample(rep(1:nBlock, each=repBlock))))
        D0 <- data.frame(data, block)
        D0 <- D0[order(D0$block),]
        itemOrder <- as.vector(sapply(1:nBlock, function(x) sample(1:blockLen)))
        D0 <- data.frame(D0, itemOrder)
        D0 <- D0[order(D0$block, D0$itemOrder),]
        retval[[ll]] <- D0
    }
    retval
}

if(FALSE) {
    nCond <- 4
    nRep <- 6
    nItem <- nCond*nRep
    cond <- factor(rep(LETTERS[1:nCond], each=nRep))
    itemID <- 1:length(cond)
    D <- data.frame(itemID, cond)
    blockRandomLists(nCond=4, data=D)
}
