##' @title Function for exporting data.frame to matlab-suitable tab delimited text file.
##'
##' @details
##' matlabDelim generates 2 ascii files. The first is a tab delimited ascii data file that contains
##' only numerical values, or the missing value code 'NaN' (factors in DF are converted to integer
##' vectors). The second file is a 'key' file that lists all of the variables included (columnwise)
##' in the data file. For those variables/columns that should be interpreted as factors, gives a
##' list of the 'levels/labels' that the numerical values in the data file correspond to.
##'
##' @param DF A dataframe to be exported.
##' @param name A basename to be used in constructing filenames for key and data files. Defaults to
##' name of dataframe.
##' @return None. Used for its side effect.
##' @author David Braze \email{davebraze@@gmail.com}
##' @export
matlabDelim <-
function(DF, name=deparse(substitute(DF))) {
  if (length(DF)<1) return(NULL)

# write a key to file and convert factors to integer vectors
  key.file <- file(description=paste(name, "_key.txt", sep=""), open="w")
  for (ii in 1:length(DF)) {
    cat(sprintf("%d. %s\n", ii, names(DF[ii])), file=key.file)
    if(is.factor(DF[[ii]])){
      lab <- as.character(unique(DF[[ii]]))
      num <- unique(as.integer(DF[[ii]]))
      na.idx <- which(is.na(num))
      if (length(na.idx)){
        num <- num[-na.idx]
        lab <- lab[-na.idx]
      }
      s.idx <- sort(num, index.return=TRUE)$ix
      for (jj in 1:length(lab)){
        cat(sprintf("  %d\t%s\n", num[s.idx[jj]], lab[s.idx[jj]]), file=key.file)
      }
      DF[[ii]] <- as.integer(DF[[ii]])      # convert the factor to an integer
    }
  }
  close(key.file)

  # write the data
  write.table(DF, file=paste(name, "_data.txt", sep=""), na="NaN", quote=FALSE,
              sep="\t", col.names=FALSE, row.names=FALSE)
  invisible(NULL)
}
