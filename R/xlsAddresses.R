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
    wb<-xlsx::loadWorkbook(file)
    nr<-xlsx::getRanges(wb)
    nr_names<-purrr::map_chr(nr, ~.$getNameName())
    if(!namedRange %in% nr_names) {
      if(namedRange=='VariableGroupNames') {
        xlsrange<-"grupy!F4"
        sheetname<-"grupy"
#        browser()
      } else if (namedRange=='VariableGroupsColumn') {
        xlsrange<-"variables!AI2"
        sheetname<-'variables'
      } else if (namedRange=='DBLongName') {
        xlsrange<-"ster!C3"
        sheetname<-'ster'
      } else if (namedRange=='CustomerFolder') {
        xlsrange<-"ster!B3"
        sheetname<-'ster'
      } else if (namedRange=='SubcustomerFolder') {
        xlsrange<-"ster!B5"
        sheetname<-'ster'
      } else {
        browser()
        stop(paste0("Wrong format of the ", file, ". Cannot find the ", wewsheetname , " sheet or named range ", namedRange, '.'))
      }

    } else {

      pos<-which(nr_names %in% namedRange)
      nr<-nr[[pos]]
      sheetname<-nr$getSheetName()
      xlsrange<-nr$getRefersToFormula()
    }
    ans<-stringr::str_match(xlsrange, stringr::regex(paste0("^",sheetname,
                                                            "!\\$?([A-Za-z]{0,2})\\$?([0-9]{0,5}):?\\$?([A-Za-z]{0,2})\\$?([0-9]{0,5})$")))
    if(ans[[4]]=="") {
      rng1<-cellranger::as.cell_addr(paste0("$", ans[[2]], "$", ans[[3]]))
      rng2<-rng1
    } else {
      rng1<-cellranger::as.cell_addr(paste0("$", ans[[2]], "$", ans[[3]]))
      rng2<-cellranger::as.cell_addr(paste0("$", ans[[4]], "$", ans[[5]]))
    }
    colnr<-rng1$col
    rownr<-rng1$row
    rowcount<-rng2$row - rng1$row + 1
    colcount<-rng2$col - rng1$col + 1

  } else {
    rng<-readxl::read_excel(path=file, sheet=wewsheetname, skip=wewsheetrow-2)
    if (!namedRange %in% rng[[1]])
    {
      stop(paste0("Cannot find the named range ", namedRange, " in sheet ", wewsheetname, " in file ", file))
    }
    rownr<-which(rng[[1]]==namedRange)
    sheetname<-rng[[5]][[rownr]]
    colnr<-rng[[7]][[rownr]]
    rownr<-rng[[6]][[rownr]]

  }
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
    whichNA<-length(VariableGroupNames)
  }
  VariableGroupNames<-VariableGroupNames[seq(2,whichNA-1)]


  address<-danesurowe::getNamedRange(file, getOption('rng_VariableGroupsColumn'))
  rng<-readxl::read_excel(path=address$file, sheet=address$sheetname, col_names = FALSE)

  groups<-list()
  if(ncol(rng)>=address$colnr) {
  	cols<-rng[2:nrow(rng),address$colnr:ncol(rng)]
  	for(i in seq_along(VariableGroupNames))
  	{
  		colnrs<-plyr::llply(cols,function(col){which(col==i)})
  		unique_colnrs<-Reduce(union, colnrs, integer(0))
  		groups[[VariableGroupNames[[i]]]]<-names(dt)[unique_colnrs]
  	}
  }
  setattr(dt, 'variablegroups', groups)
  return(groups)
}
