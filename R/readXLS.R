#' This function reads the database in file,
#' and puts returns its contents.
#'
#' @export
readDaneSurowe <- function(file) {
  dt<-readDataSheet(file)
  readLabelSheet(file,dt, ncol(dt)*2+2)
  readMeasureAndUnits(file,dt)
  readVariableGroups(file, df)

  address<-danesurowe::getNamedRange(file, getOption('rng_DBLongName'))
  globalname<-readSingleCellAtAddress(address)
  setattr(dt,'label',globalname)

  address<-danesurowe::getNamedRange(file, getOption('rng_CustomerFolder'))
  customerfolder<-readSingleCellAtAddress(address)

  address<-danesurowe::getNamedRange(file, getOption('rng_SubcustomerFolder'))
  subcustomerfolder<-readSingleCellAtAddress(address)

  paths<-c(getOption('globalPathPrefix'),customerfolder, subcustomerfolder)

  setattr(dt,'paths',paths)
  dt[,1:=NULL,with=FALSE]
}


#' Returns c(<sheet_name>,<row>,<column>) from the namedRange
getNamedRange <- function(file, namedRange, rowcount=1, colcount=1)
{
  if (!file.exists(file))
  {
    stop(paste0("The danesurowe file ",file, " doesn't exist."))
  }

  wewsheetname<-getOption('wew_sheet_name')
  wewsheetrow<-getOption('wew_pointer_row')

  sheets<-readxl::excel_sheets(file)
  if (! wewsheetname %in% sheets)
  {
    stop(paste0("Wrong format of the ", file, ". Cannot find the ", wewsheetname , " sheet"))
  }
  rng<-readxl::read_excel(path=file, sheet=wewsheetname, skip=wewsheetrow-2)
  if (!namedRange %in% rng[[1]])
  {
    stop(paste0("Cannot find the named range ", namedRange, " in sheet ", wewsheetname, " in file ", file))
  }
  rownr<-which(rng[[1]]==namedRange)
  sheetname<-rng[[5]][[rownr]]
  colnr<-rng[[7]][[rownr]]
  rownr<-rng[[6]][[rownr]]
  return(list(file=file, sheetname=sheetname, rownr=rownr, colnr=colnr, rowcount=rowcount, colcount=colcount))
}

readSingleCellAtAddress<-function(address)
{
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  return(rng[[address$colnr]][[address$rownr]])
}

readCellAtAddress<-function(address)
{
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  return(rng[[address$colnr]][[address$rownr]])
}

readColumnAtAddress<-function(address)
{
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  mycol<-rng[[address$colnr]]
  mycol2<-mycol[(address$rownr-1):(length(mycol)-address$rownr+1)]
  return(mycol2)
}

readRowAtAddress<-function(address)
{
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  rng[address$rownr, address$colnr:(ncol(rng) - address$colnr+1)]
  return(rng)
}

readSheet<-function(path, sheet, skip = 0, colcnt=NA)
{
  if (is.na(colcnt))
  {
    rng<-readxl::read_excel(path=path, sheet=sheet, col_names = FALSE, skip=skip)
    colcnt <- ncol(rng) - which.min(rev(is.na(rng[2,1:ncol(rng)])))
  }

  rng<-as.data.table(readxl::read_excel(path=path, sheet=sheet, col_names = FALSE, skip = skip))

  new.colids<-paste0('X',seq(0,colcnt))
  new.colids.idx<-new.colids[! (new.colids %in% colnames(rng))]
  delete.colids.idx<-colnames(rng)[!(colnames(rng) %in% new.colids )]

  for (i in delete.colids.idx)
  {
    rng[,(i):=NULL]
  }

  for (i in new.colids.idx)
  {
    rng[,(i):=NA]
  }

  setcolorder(rng, new.colids)
  return(rng)
}

readDataSheet<-function(file)
{
  rngName<-getOption('rng_DataOrigin')
  address<-danesurowe::getNamedRange(file, rngName)
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  rowcnt <- nrow(rng)-2
  colcnt <- ncol(rng) - which.min(rev(is.na(rng[2,1:ncol(rng)])))

  my.colnames<-as.character(rng[2,1:colcnt])
  collabels<-as.character(rng[1,1:colcnt])

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=2, colcnt=colcnt)



  my.rownames<-rng[[1]]
  rng<-rng[!is.na(my.rownames),]
  rng[,(1):=NULL] #Remove the first 'lp' column
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


  colnames(rng)<-my.colnames

  for(i in 1:colcnt)
  {
#    cat(paste0("i=",i,'\n'))
    setattr(rng[[i]],'label',collabels[[i]])
  }

  return(rng)
}

readLabelSheet<-function(file, dt, colcnt)
{
  rngName<-getOption('rng_LabelOrigin')
  address<-danesurowe::getNamedRange(file, rngName)

  rng<-readSheet(path=address$file, sheet=address$sheetname, skip=2, colcnt=colcnt)

  rng[,(1):=NULL] #Drop first
  rng[,(1):=NULL] #two columns

  rowcnt <- nrow(rng)

  setLabels<-function(varnr, dt)
  {
    rngLevels<-rng[[(varnr-1)*2+1]]
    if (!all(is.na(rngLevels))){
      if (varnr==178)
      {
        browser()
      }
    }

    if (!all(is.na(rngLevels)))
    {
      if (varnr==178)
      {
                browser()
      }
      maxNA<-match(TRUE, is.na(rngLevels))
      if (is.na(maxNA))
      {
        maxNA<-rowcnt
      }
      rngLevels<-rngLevels[1:(maxNA-1)]
      rngLabels<-rng[[(varnr-1)*2+2]]

      if (identical(sort(as.integer(rngLevels)),seq_along(rngLevels)))
      {
        #Standard factor
        ord<-order(as.integer(rngLevels))
        rngLabels<-rngLabels[ord]
        label<-attr(dt[[varnr]],'label')
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
            label<-attr(dt[[varnr]],'label')
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
#    cat(paste('i=',i,'\n'))
#    if(i==178)
 #     browser()
    setLabels(i, dt)
  }
}

readMeasureAndUnits<-function(file, dt)
{

  address<-danesurowe::getNamedRange(file, getOption('rng_Measure'))

  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)
  measures<-rng[[address$colnr]][(address$rownr):(address$rownr-1+ncol(dt))]

  address<-danesurowe::getNamedRange(file, getOption('rng_Units'))
  units<-rng[[address$colnr]][(address$rownr):(address$rownr-1+ncol(dt))]


  for (varnr in seq_along(measures))
  {
    if (!is.na(units[[varnr]]))
    {
      setattr(dt[[varnr]], 'units', units[[varnr]])
    }
    setattr(dt[[varnr]],'f.o.b',switch(measures[[varnr]],N=1,O=2,D=3,0))
    if (measures[[varnr]]=='O' && is.factor(dt[[varnr]]))
    {
      setattr(dt[[varnr]], 'class', c(attr(dt[[varnr]],'class'), 'ordered'))
    }
  }

}

readVariableGroups<-function(file, dt)
{
  address<-danesurowe::getNamedRange(file, getOption('rng_VariableGroupNames'))
  VariableGroupNames<-readColumnAtAddress(address)
  whichNA<-match(TRUE, is.na(VariableGroupNames))
  if (is.na(whichNA))
  {
    whichNA<-nrow(VariableGroupNames)
  }
  VariableGroupNames<-VariableGroupNames[seq(2,whichNA-1)]


  address<-danesurowe::getNamedRange(file, getOption('rng_VariableGroupsColumn'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)

  groups<-list()
  cols<-rng[2:nrow(rng),address$colnr:ncol(rng)]
  for(i in seq_along(VariableGroupNames))
  {
    colnrs<-plyr::llply(cols,function(col){which(col==i)})
    unique_colnrs<-Reduce(union, colnrs, integer(0))
    groups[[VariableGroupNames[[i]]]]<-names(dt)[unique_colnrs]
  }
  setattr(dt, 'variablegroups', groups)
  return(groups)
}
