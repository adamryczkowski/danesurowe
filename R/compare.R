#' This function compares two datasets
comparedf<-function(df1, df2)
{
  if (!'data.frame' %in% class(df1))
  {
    ans<-list(message='First argument is not a data.frame!', result=FALSE)
    return(ans)
  }
  if (!'data.frame' %in% class(df2))
  {
    ans<-list(message='Second argument is not a data.frame!', result=FALSE)
    return(ans)
  }

  if (nrow(df1) != nrow(df2))
  {
    ans<-list(message=paste0('Data frames have different number of cases: ', nrow(df1), ' vs ', nrow(df2)), result=FALSE)
    rownams(df1)
  }

  names1<-colnames(df1)
  names2<-colnames(df2)

  commonnames<-intersect(names1,names2)
  if (length(commonnames)!=max(length(names1),length(names2)))
  {
    dif1 <- sort(setdiff(names1, names2))
    dif2 <- sort(setdiff(names2, names1))
    if (max(10, length(commonnames)) < max(dif1, dif2)) #10 is chosen arbitrarily: if there are only few variables, lets report all of them separately anyway.
    {
      ans<-list(message=paste0('Most variables names do not match. Although the data frames have equal length, they seem to be very different. '), result=FALSE)
      return(ans)
    }
    if (length(names1)==length(names2))
    {
      message<-'Both arguments have the same number of variables. '
    } else {
      if (length(names1)>length(names2)) {
        message<-paste0("Argument 2 has ", length(names2)-length(names1), " more variables than argument 1. ")
      } else {
        message<-paste0("Argument 1 has ", length(names1)-length(names2), " more variables than argument 2. ")
      }
    }
    sample1<-head(dif1, 10)
    sample2<-head(dif2, 10)
    ans<-list(message=paste0(ans$message,
                            "In total there are ", length(dif1), " variable names in set 1, that have no counterpart in set 2, ",
                            "and there are ", length(dif2), " variable names in set 2, that have no counterpart in set 1. ",
                            ifelse(length(sample1)<length(dif1) || length(sample2)<length(dif2), "E. g. ", ''),
                            paste0(sample1, collapse=', '),
                            ifelse(length(sample1)<length(dif1), ", ...", ''),
                            ' from the first set and ',
                            paste0(sample2, collapse=', '),
                            ifelse(length(sample2)<length(dif2),", ...",''),
                            ' from the second set.'),
              result=FALSE,
              difnames1=dif1,
              difnames2=dif2)
  } else {
    ans<-list(message="Variable names match between the sets. ", result=TRUE, difnames1=character(0), difnames2=character(0))
  }

  makeHash<-function(var, dosort)
  {
    if (dosort)
    {
      if(is.character(var))
      {
        return(digest::digest(stringi::stri_encode(sort(as.vector(var)))))
      } else {
        return(digest::digest(sort(as.vector(var))))
      }
    } else {
      if(is.character(var))
      {
        return(digest::digest(stringi::stri_encode(as.vector(var))))
      } else {
        return(digest::digest(as.vector(var)))
      }
    }
  }

  varHashes1<-plyr::laply(as.list(df1)[commonnames], makeHash, dosort=TRUE)
  names(varHashes1)<-commonnames
  varHashes2<-plyr::laply(as.list(df2)[commonnames], makeHash, dosort=TRUE)
  names(varHashes2)<-commonnames

  commonvars<-intersect(varHashes1, varHashes2)
  npos<-plyr::aaply(commonvars, 1, function(h) sum(h==varHashes1))
  pos<-plyr::aaply(commonvars, 1, function(h) data.table::first(which(h==varHashes1)))
  names(commonvars)<-commonnames[pos]

  if (length(commonvars)!=length(commonnames))
  {
    dif1 <- sort(setdiff(varHashes1, varHashes2))
    pos<-plyr::aaply(dif1, 1, function(h) which(h==varHashes1))
    names(dif1)<-commonnames[pos]

    if (max(10, length(commonvars)) < length(dif1))
    {
      ans$message<-paste0(ans$message, 'Among the variables that have matching names most of their contents seem to be very different. ')
      return(ans)
    }
    sample1<-head(dif1, 10)
    ans$message<-paste0(ans$message,
                        "In total there are ", length(dif1), " variables with not matching contents",
                        ifelse(length(sample1)<length(dif1), ", e. g. ",": "),
                        paste0(names(sample1), collapse=', '),
                        ifelse(length(sample1)<length(dif1), ", ...", ''),
                        ". ")
    ans$difcontents<-dif1
  } else {
    ans$message<-paste0(ans$message,
                        "Variable contents match between the sets. ")
    ans$difcontents<-character(0)
  }

  browser()
  if (length(ans$difcontents)>0)
  {
    for(i in seq_along(ans$difcontents))
    {
      browser()
      ansv<-comparevar(df1[[names(ans$difcontents)[[i]] ]], df2[[names(ans$difcontents)[[i]] ]])
    }
  }



  dfcopy1<-dplyr::arrange_(df1, names(commonvars))
  dfcopy2<-dplyr::arrange_(df2, names(commonvars))

  varHashesB1<-plyr::laply(as.list(dfcopy1), makeHash, dosort=FALSE)
  names(varHashesB1)<-colnames(dfcopy1)
  varHashesB2<-plyr::laply(as.list(dfcopy2), makeHash, dosort=FALSE)
  names(varHashesB2)<-colnames(dfcopy2)

  commonvarsB<-intersect(varHashesB1, varHashesB2)
  pos<-plyr::aaply(colnames(dfcopy1), 1, function(h) which(h==varHashesB1))
  names(commonvarsB)<-colnames(dfcopy1)[pos]

  browser()


  #Check if there is a problem with record ordering (variables as a set of values are guaranteed to be the same,
  #but we need to check if they are the same if we include given order of records)
  if (length(commonvarsB)<length(commonvars))
  {
    for (i in seq_along(ans$difcontents))
    {
      browser()
      ansv<-comparevar(dfcopy1[[i]], dfcopy2[[i]])
    }
  }

}

comparevar<-function(var1, var2)
{
  browser()
  missings<-is.na(var1)
  missingok<-all(missings==is.na(var2))
  if (missingok)
  {
    return(all(as.vector(var1)[!missings]==as.vector(var2)[!missings]))
  } else {
    return(FALSE)
  }
}
