##' @title Block randomize a set of trials.
##'
##' @description Generate a specified number of uniquely block randomized copies of a stimulus list.
##'
##' @details The purpose of this function is to create a set of randomized copies of a given
##'     stimulus list. At present, the function does not check to ensure that all lists are
##'     unique. Counterbalancing of lists must be done separately.
##'
##' @param nLists Number of randomized lists to create (default=4).
##' @param repBlock Number of repetitions/condition within each randomization block (default=2). How
##'     many items in each condition in each block?
##' @param nCond Number of conditions in data. It may actually be better to require a column for
##'     condition label in the input DF and then (2) count the number of conditions. So if we have a
##'     2x2 design (Freq, Cons) with levels (HF,LF) and (HC,LC), then a Cond variable would just
##'     contain labels derived via interaction (Freq, Cons).
##' @param data A data.frame containing specification of all experimental trials (items), blocked
##'     so that each block contains the appropriate number of trials in each condition.
##' @return A list of data.frames. Each data.frame will contain a (probably unique) block randomized
##'     list of items the same length as the input DF.
##' @examples
##'     \dontrun{
##'     ## use case 1
##'     ## first build a master data.frame to work from
##'     library(reshape)
##'     D <- expand.grid(freq=c("HF", "LF"), cons=c("HC", "LC"))
##'     cond <- interaction(D$freq, D$cons)
##'     D <- data.frame(D, cond)
##'     D <- expand.grid.df(D,data.frame(blockID=1:6))
##'     nCond <- length(unique(D$cond))
##'     nRep <- 6
##'     itemID <- 1:(nCond*nRep)
##'     D <- data.frame(D, itemID)
##'     blockRandomLists(nLists=3, nCond=4, data=D)
##'     }
##' @author David Braze \email{davebraze@@gmail.com}
##' @export

blockRandomLists <- function(nLists=4, repBlock=2, nCond, data) {
    LIST <- 1:nLists
    nItem <- dim(data)[1]
    nBlock <- nItem/(nCond*repBlock)
    blockLen <- nItem/nBlock

    retval <- list()
    for (ll in LIST) {
        blockOrd <- as.vector(sapply(1:nCond, function(x) sample(rep(1:nBlock, each=repBlock))))
        D0 <- data.frame(data, blockOrd)
        D0 <- D0[order(D0$blockOrd),]
        itemOrder <- as.vector(sapply(1:nBlock, function(x) sample(1:blockLen)))
        D0 <- data.frame(D0, itemOrder)
        D0 <- D0[order(D0$blockOrd, D0$itemOrd),]
        retval[[ll]] <- D0
    }
    retval
}

if(FALSE) {
    library(reshape)
    D <- expand.grid(freq=c("HF", "LF"), cons=c("HC", "LC"))
    cond <- interaction(D$freq, D$cons)
    D <- data.frame(D, cond)
    nCond <- length(unique(D$cond))
    nRep <- 6
    D <- expand.grid.df(D,data.frame(blockID=1:6))
    itemID <- 1:(nCond*nRep)
    D <- data.frame(D, itemID)
    blockRandomLists(nCond=4, data=D)
}
