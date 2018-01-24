readLabelSheet3<-function(file, dt, colcnt)
{
  rngName<-getOption('rng_LabelOrigin')
  address<-danesurowe::getNamedRange(file, rngName)

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=0, colcnt=colcnt+4)

  rng[,(1):=NULL] #Drop first column
  rng[,(1):=NULL] #Drop column
  rng[,(1):=NULL] #for Lp.
  rng<-rng[3:nrow(rng),]

  rowcnt <- nrow(rng)

  setLabels<-function(varnr, dt)
  {
    rngLevels<-rng[[(varnr)*2+1]]
    if (!all(is.na(rngLevels))){
      # if (varnr==178)
      # {
      #   browser()
      # }
    }

    if (!all(is.na(rngLevels)))
    {
      #      if (varnr==178)
      #      {
      #                browser()
      #      }
      maxNA<-match(TRUE, is.na(rngLevels))
      if (is.na(maxNA))
      {
        maxNA<-rowcnt+1
      }
      if(varnr==355)
      {
     #   browser()
      }
      rngLevels<-rngLevels[1:(maxNA-1)]
      rngLabels<-rng[[(varnr)*2+2]]
#      cat(paste(varnr,'\n'))

      if (identical(sort(suppressWarnings(as.integer(rngLevels))),seq_along(rngLevels)))
      {
        #Standard factor
        ord<-order(as.integer(rngLevels))
        rngLabels<-rngLabels[ord]
        label<-attr(dt[[varnr]],'label', exact = TRUE)
        var<-as.integer(dt[[varnr]])
        setattr(var,'levels',rngLabels)
        setattr(var,'class','factor')
        setattr(var,'label',label)
        dt[,(varnr):=var]
      } else {
        #Labelled variable
        numLevels<-suppressWarnings(as.numeric(rngLevels))
        if (sum(is.na(numLevels))==0)
        {
          rngLevels<-numLevels
          if (min(abs(c(rngLevels%%1, rngLevels%%1-1))) < 0.0000000000005)
          {
            rngLevels<-as.integer(rngLevels)
            label<-attr(dt[[varnr]],'label', exact = TRUE)
            dt[, varnr:=as.integer(dt[[varnr]]), with=FALSE]
            setattr(dt[[varnr]],'label',label)
          }
          names(rngLevels)<-rngLabels
          setattr(dt[[varnr]], 'class', 'labelled')
          setattr(dt[[varnr]], 'labels', rngLevels)
        }
      }
    }
  }
  for (i in 1:(ncol(dt)))
  {
    #cat(paste('i=',i,'\n'))
    # if(i==80)
    # {
    #   browser()
    # }
    setLabels(i, dt)
  }
}

# Function reads in variables and forms two table of dictionaries: variablenr->(value->label) for numeric labels, and the same for non-numeric labels.
readLabelSheet4<-function(file, varcnt, na.colnames)
{
  colcnt<-varcnt*2+2
  rngName<-getOption('rng_LabelOrigin')
  address<-danesurowe::getNamedRange(file, rngName)

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=0, colcnt=colcnt+1)
  rng[,(1):=NULL] #Drop first column
  rng[,(1):=NULL] #Drop LP pair of columns
  rng[,(1):=NULL] #..
  rng<-rng[3:nrow(rng),]

  rowcnt <- nrow(rng)

  label_list<-list()

  for(varnr in seq(varcnt))
  {
    rngLevels<-rng[[(varnr-1)*2+1]]
    if (!all(is.na(rngLevels)))
    {
      maxNA<-match(TRUE, is.na(rngLevels))
      if (is.na(maxNA))
      {
        maxNA<-rowcnt+1
      }
      rngLevels<-rngLevels[1:(maxNA-1)]
      rngLabels<-rng[[(varnr-1)*2+2]]
      rngLabels<-rngLabels[1:(maxNA-1)]

      label_list[[as.character(varnr)]]<-list(levels=rngLevels,labels=rngLabels)
    }

  }
  names(label_list)<-as.numeric(names(label_list))
  return(label_list)
}

readLabelSheetNA<-function(file, varcnt)
{
  colcnt<-varcnt*2+2
  rngName<-getOption('rng_LabelOrigin')
  address<-danesurowe::getNamedRange(file, rngName)

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=2, colcnt=colcnt)

  rng[,(1):=NULL] #Drop first
  rng[,(1):=NULL] #two columns

  rowcnt <- nrow(rng)

  na_list<-list()
  label_list<-list()

  for(varnr in seq(varcnt))
  {
    rngLevels<-rng[[(varnr-1)*2+1]]
    if (!all(is.na(rngLevels)))
    {
      maxNA<-match(TRUE, is.na(rngLevels))
      if (is.na(maxNA))
      {
        maxNA<-rowcnt+1
      }
      rngLevels<-rngLevels[1:(maxNA-1)]
      rngLabels<-rng[[(varnr-1)*2+2]]
      rngLabels<-rngLabels[1:(maxNA-1)]

      which_na<-is.na(suppressWarnings(as.numeric(rngLevels)))
      if (sum(which_na)>0)
      {
        na_list[[as.character(varnr)]]<-list(levels=rngLevels[which_na],labels=rngLabels[which_na])
      }
      if (sum(!which_na)>0)
      {
        label_list[[as.character(varnr)]]<-list(levels=as.numeric(rngLevels[!which_na]),labels=rngLabels[!which_na])
      }
    }

  }

  return(list(label_list=label_list, na_list=na_list))
}

#suggestedTaggedMissings is a named vector that maps description to the na_tag. Any tagged na encountered that matches this dictionary
#will be converted to this specific tagged_na
assign_nas<-function(NAlabels, suggestedTaggedMissings=character())
  #suggestedTaggedMissings : label -> tag
{
  # 2. Now we have to learn about the na's encoded as string levels
  NAtags<-rep(NA, length(NAlabels))

  # 3. Now we have to make sure our two lists conform to the suggestedTaggedMissings

  for(i in seq_along(NAlabels))
  {
    l <- NAlabels[[i]]
    if (l != '')
    {
      if ((l %in% NAlabels[1:i-1]) && i>1)
      {
        j <- which.max(NAlabels==l)
        NAtags[[i]]<-NAtags[[j]]
      } else	{
        if (l %in% names(suggestedTaggedMissings))
        {
          newtag <- suggestedTaggedMissings[[l]]
          NAtags[i] <- newtag
        } else {
          newtag <- new_tag(unique(c(NAtags,suggestedTaggedMissings)))
          NAtags[i] <- newtag
          suggestedTaggedMissings<-c(suggestedTaggedMissings, setNames(newtag, l))
        }
      }
    }
  }

  return(list(suggestedTaggedMissings = suggestedTaggedMissings, NAtags=NAtags))
}

new_tag<-function(existingTags)
{
  a<-strsplit("abcdefghijklmnopqrstuvwxyz",'')[[1]]
  a[which.max(! (a %in% existingTags))]
}


apply1<-function(dt, varnr, myNAlabels, myNAlevels, mylevels, mylabels, vartype)
{
  if (vartype=='D')
  {
    varDate<-tryCatch(
      as.Date(origvar) #We assume Excel for Windows format
      , error=function(e){return(NULL)})
    if (!is.null(varDate))
    {
      return(list(var=varDate, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
    }
  }
}
