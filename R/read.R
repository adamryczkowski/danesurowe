#' This function reads the database in file,
#' and puts returns its contents.
#'
#' @export

getVersion<-function(file) {
  addr<-danesurowe::getNamedRange(file, getOption('rng_Version'))
  val<-readSingleCellAtAddress(addr)
  return(as.numeric(val))

}

readDaneSurowe<-function(file) {
  ver<-getVersion(file)
  if (identical(ver, 3))
  {
    return(readDaneSurowe3(file))
  }
  if (identical(ver, 4))
  {
    return(readDaneSurowe4(file))
  }
}

readDaneSurowe3 <- function(file) {
  ans<-readDataSheet(file)
  ans$dt[,(1):=NULL] #Remove the first 'lp' column
#  if (length(na.colnames)>0)
#  {
#    na.colnames<-na.colnames-1 #Because we deleted the first column with rownames
#  }

  dt<-ans$dt

  readLabelSheet3(file,dt, ncol(dt)*2+2)
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
  #  browser()
  for(i in rev(ans$na.colnames))
  {
    dt[,(i):=NULL]
  }
  return(dt)
}


readDaneSurowe4 <- function(file, flag_keep_tagged_na = FALSE) {
  ans<-readDataSheet(file)
  txt<-colnames(ans$dt)
  na.colnames<-c(ans$na.colnames,which(stringi::stri_count(txt, regex='^\\..*$')==1))
  labels<-readLabelSheet4(file, ncol(ans$dt))

  types<-readTypes(file, ncol(ans$dt))

  ans<-set_apply_labels(dt = ans$dt, labels=labels, vartypes = types, flag_keep_tagged_na=flag_keep_tagged_na)
  dt<-ans$dt

  errors<-ans$errors
  mywarnings<-ans$warnings

  #Ustaw min i max teoretyczny i zwracaj load error, gdy zauważymy przypadki, które ich nie spełniają
  ans<-readMinMax(file, ncol(dt))
  errors<-join_messages(errors, ans$errors, dt)

  ans<-set_TheoreticalMinMax(dt, ans$mins, ans$maxs)
  errors<-join_messages(errors, ans$errors, dt)
  dt<-ans$dt

  #TODO: Gdy zmienna jest dyskretna zwracaj load error, gdy wartość jest niecałkowita
  forceIntegers<-readForceInteger(file, ncol(dt))
  ans<-set_ForcedIntegers(dt, forceIntegers)
  errors<-join_messages(errors, ans$errors, dt)
  dt<-ans$dt

  #TODO: Zwracaj load error, gdy typ się nie zgadza z oczekiwanym
  errors<-join_messages(errors, ValidateTypes(dt), dt)

  #TODO: Zwracaj error dla zmiennych required z brakami
  required<-readRequired(file, ncol(dt))
  ans<-set_Required(dt, required)
  errors<-join_messages(errors, ans$errors, dt)

  #TODO: Zwracaj error dla zmiennych słownikowych, dla których są wartości spoza słownika i ma ich nie być
 # browser()
  ints<-readLimitedToLabels(file, ncol(ans$dt))
  ans<-set_LimitToLabels(dt, ints)
  errors<-join_messages(errors, ans$errors, dt)



  #TODO: Zwracaj error dla zmiennych, dla których nie udała się walidacja
  errors<-join_messages(errors, ValidateCustom(dt), dt)

#  browser()

  vars_to_keep <- which(types == '0')
  for(varnr in rev(vars_to_keep))
  {
    dt[,(varnr):=NULL]
  }

  if(length(as.list(errors)))
  {
#    browser()
    positions<-match(names(errors), colnames(dt))
    positions<-unclass(na.omit(positions))
    warning(paste0('Loading failed with the following reasons: \n',
                  paste0(seq_along(errors), '. Ad ', colnames(dt)[positions], ": ", as.list(errors)[colnames(dt)[positions] ], collapse ='\n') ))
  }
  return(list(dt=dt, errors=as.list(errors), warnings=as.list(mywarnings)))
}


