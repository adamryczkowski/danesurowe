# Reads in the datasheet as a data.table and applies the variable labels. It doesn't skip all the empty variables, because that would introduce
# problems with reading other sheets, where variable references are position-based. But it outputs list of all NA columns.
readDataSheet<-function(file)
{
  rngName<-getOption('rng_DataOrigin')
  address<-danesurowe::getNamedRange(file, rngName)
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  rowcnt <- nrow(rng)-2
  colcnt <- ncol(rng) - which.min(rev(is.na(rng[2,1:ncol(rng)])))

  my.colnames<-as.character(rng[2,2:(colcnt+1)])
  na.colnames<-which(is.na(rng[2,2:(colcnt+1)]))

  collabels<-as.character(rng[1,2:(colcnt+1)])

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=2, colcnt=colcnt+2)


  my.rownames<-rng[[2]]
  rng[,(1):=NULL] #Drop first column
  rng[,(1):=NULL] #Drop column LP
  rng<-rng[!is.na(my.rownames),]

  my.rownames<-my.rownames[!is.na(my.rownames)]

  flagIgnoreRownames=FALSE
  if (is.numeric(my.rownames))
  {
    tmpdiff<-diff(my.rownames)
    mtmpdiff<-max(tmpdiff)
    if (mtmpdiff==min(tmpdiff) && (mtmpdiff==1))
    {
      #All rownames are consecutive integers, so ignore them
      flagIgnoreRownames=TRUE
    }
  }
  if(!flagIgnoreRownames)
  {
    row.names(rng)<-my.rownames
  }

#  rng<-rng[,2:ncol(rng)]

  colnames(rng)<-c(my.colnames)

  for(i in 1:colcnt)
  {
    #    cat(paste0("i=",i,'\n'))
    if (!is.na(collabels[[i]]))
    {
      setattr(rng[[i]],'label',collabels[[i]])
    }
  }

#  rng[,(1):=NULL] #Remove the first 'lp' column
  if (length(na.colnames)>0)
  {
    na.colnames<-na.colnames-1 #Because we deleted the first column with rownames
  }
  return(list(dt=rng, na.colnames=na.colnames))
}

