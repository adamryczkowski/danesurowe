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

  rng<-data.table::as.data.table(suppressWarnings(readxl::read_excel(path=path, sheet=sheet, col_names = FALSE, skip = skip)))

  new.colids<-paste0('X__',seq(0,colcnt-1))
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
