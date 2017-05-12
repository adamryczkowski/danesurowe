#' danesurowe: Functions to parse and read the danesurowe format directly from within R
#'
#'It uses readr to validate and read the file and to convert it into the canonical data_frame structure.
#'
#'
#' @docType package
#' @name denesurowe
#' @import readxl
NULL


# nocov start
.onLoad	<-	function(libname,	pkgname)	{
  op	<-	options()
  op.danesurowe	<-	list(
    wew_sheet_name	=	'pointers',
    wew_pointer_column = 1,
    wew_pointer_row = 2,
    rng_DataOrigin = 'DataOrigin',
    rng_LabelOrigin = 'LabelOrigin',
    rng_Measure = 'Measure',
    rng_Units = 'Units',
    rng_VariableGroupsColumn = 'VariableGroupsColumn',
    rng_VariableGroupNames = 'VariableGroupNames',
    rng_CustomerFolder = 'CustomerFolder',
    rng_SubcustomerFolder = 'SubcustomerFolder',
    rng_DBLongName = 'DBLongName',
    rng_RObjName = 'RObjName',
    rng_VariableClass = 'VariableClass', #
    rng_IntendedVariableClass = 'IntendedVariableClass', #
    rng_OnlyLabelledValues = 'OnlyLabelledValues', #
    rng_TheoreticalMin = 'TheoreticalMin',
    rng_TheoreticalMax = 'TheoreticalMax',
    rng_ForceInteger = 'ForceInteger',
    rng_Required = 'Required', #
    rng_Version = 'Version', #
    globalPathPrefix = '/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy'
  )
  toset	<-	!(names(op.danesurowe)	%in%	names(op))
  if(any(toset))	options(op.danesurowe[toset])
  invisible()
}
# nocov end


#Applies labels together with measures to the data.table dt. It modifies the data.table in place.
set_apply_labels<-function(dt, labels, vartypes, flagUseTaggedNA=TRUE, in_varnames=NULL, flag_keep_tagged_na=FALSE)
{
#  browser()
  if (is.null(in_varnames))
  {
    varnames=colnames(dt)
  } else
  {
    varnames=in_varnames
  }

  suggestedTaggedMissings <- character(0)
  errors<-new.env()
  mywarnings<-new.env()

  fn_baseclass<-function(varnr)
  {
#    if (varnr==132) browser();
#    if (colnames(dt)[[varnr]]=='q_88e') browser()
#    cat(paste(varnr,'\n'))
    #First we find missings and convert them into proper tagged NA
    myALLlevels<-labels[[as.character(varnr)]]$levels
    myALLlabels<-labels[[as.character(varnr)]]$labels
    charLevels<-is.na(suppressWarnings(as.numeric(myALLlevels)))
    myNAlevels<-myALLlevels[charLevels]
    myNAlabels<-myALLlabels[charLevels]

    info<-assign_nas(myNAlabels, suggestedTaggedMissings=suggestedTaggedMissings)
    suggestedTaggedMissings<<-info$suggestedTaggedMissings
    myNAtags<-info$NAtags
    myNAlabels<-myNAlabels[!is.na(myNAtags)]
    myNAlevels<-myNAlevels[!is.na(myNAtags)]
    myNAtags<-myNAtags[!is.na(myNAtags)]

    mylevels<-as.numeric(myALLlevels[!charLevels])
    mylabels<-myALLlabels[!charLevels]

    origvar <- dt[[varnr]]
    var <- origvar #copy

    if (length(myNAlevels)>0){
      var[var %in% myNAlevels] <- NA
    }

    # Now we have properly replaced all missings with adequate tagged missings.
    # Now we should decide what base class the variable should have. Choices are:
    # date			 if we are forced to do this via the 'class' parameter. If we fail to do that continue:
    if (vartypes[[varnr]]=='D')
    {
      varDate<-tryCatch(
        as.Date(origvar,origin="1899-12-30") #We assume Excel for Windows format
        , error=function(e){return(NULL)})
      if (!is.null(varDate))
      {
        return(list(var=varDate, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
      }
    }

    # Now we check, whether the variable is a valid number

    numvar<-tryCatch(
      as.numeric(var),
      warning=function(w){NULL}
    )

    # character  if there is at least one character
    if (length(numvar)==0)
    {
      if (vartypes[[varnr]]=='D')
      {
        add_msg(varname = colnames(dt)[[varnr]], message =
                  paste0("Cannot convert variable ",
                         nice_varname(dt, varnr),
                         " to Date."),
                msg_list = errors )
        return(list(var=var, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
      }

      #Variable is a character string
      if (length(myNAlabels)>0)
      {
        if(flag_keep_tagged_na)
        {
          msg<-paste0("Warning: Character vector variable \"", varnames[[varnr]], "\" doesn't support tagged NA. Keeping all tagged NA as non-missing text labels. ")
          add_msg(colnames(dt)[[varnr]], msg, mywarnings)
          var <- origvar
        } else {
          msg<-paste0("Warning: Character vector variable \"", varnames[[varnr]], "\" doesn't support tagged NA. Removing all tagged NA into plain NA. Consider replacing the vector into numeric")
          add_msg(colnames(dt)[[varnr]], msg, mywarnings)
          warning(msg)
        }
      }
      return(list(var=var, NAlabels=character(0), NAlevels=character(0), levels=mylevels, labels=mylabels ))
    }

    if (vartypes[[varnr]]=='D')
    {
      if (length(myNAlabels)>0)
      {
        msg<-paste0('Warning: Date variable ',
                    nice_varname(dt, varnr),
                    " doesn't support tagged NA. Removing all tagged NA into plain NA.")
        add_msg(varname = colnames(dt)[[varnr]],
                message = msg,
                msg_list = mywarnings)
        warning(msg)
      }
      varDate<-tryCatch(
        as.Date(numvar,origin="1899-12-30") #We assume Excel for Windows format
        , error=function(e){
          add_msg(varname = colnames(dt)[[varnr]],
                  message = paste0("Cannot convert the ",
                                   nice_varname(dt, varnr),
                                   " variable to date"),
                  msg_list = errors );
          return(NULL)
          }
        )
      if (!is.null(varDate))
      {
        return(list(var=NA, NAlabels=myNAlabels, NAlevels=myNAlevels, levels=mylevels, labels=mylabels ))
#        return(list(var=varDate, NAlabels=myNAlabels, NAlevels=myNAtags, levels=mylevels, labels=mylabels ))
      }
    }

    #We can put all NAs in place
    if (flagUseTaggedNA)
    {
      for (i in seq_along(myNAlevels))
      {
        n=myNAlevels[[i]]
        t=myNAtags[[i]]
        numvar[origvar==n] <- haven::tagged_na(t)
      }
    }

    #Now we know, we have a proper numeric number.
    if ((length(myNAlevels)>0) && vartypes[[varnr]] %in% c('I','F') && flagUseTaggedNA){
      msg<-paste0('Integer variable ',
                   nice_varname(dt, varnr),
                   ' don\'t support tagged NAs. Promoting integer into the numeric. ')
      warning(msg)
      add_msg(varname = colnames(dt)[[varnr]],
              message = msg,
              msg_list = mywarnings)
      forceNumeric=TRUE
    } else {
      forceNumeric=FALSE
    }

    #Now we need to decide whether to keep the numeric

    if (((length(myNAlevels)==0) || flagUseTaggedNA) && !forceNumeric && vartypes[[varnr]] %in% c('I','F'))
    {
      numvar <- as.integer(numvar)
      return(list(var=numvar, NAlabels=list(), NAlevels=list(), levels=mylevels, labels=mylabels ))

    }
    if (length(levels)==0 && length(NAlevels)>0)
    {
      add_msg(varname = colnames(dt)[[varnr]],
              message = paste0("Numerical type doesn't support named missings. Discarding the names for the missings (but leaving the attributes and tagged na)"),
              msg_list = errors );
    }

    return(list(var=numvar, NAlabels=myNAlabels, NAlevels=myNAtags, levels=mylevels, labels=mylabels ))
  }

  fn_labelled<-function(var, NAlabels, NAlevels, labels, levels)
  {

    if (length(NAlabels)>0)
    {
      if (length(levels)>0)
      {
        varret <- labelled::labelled(var, c(setNames(levels, labels), setNames(haven::tagged_na(NAlevels),NAlabels)))
      } else {
#        browser()
        setattr(var, "labels", setNames(haven::tagged_na(NAlevels),NAlabels))
        varret <- var
      }
      return(varret)
    }

    if (length(labels)>0)
    {
      if (is.integer(var))
      {
        if (all(sort(unique(var)) %in% levels))
        {
          return(factor(var, levels=levels, labels=labels))
        }
      }
      varret <- labelled::labelled(var, setNames(levels, labels))
      return(varret)
    }
    return(var)
  }

  fn<-function(varnr)
  {
  #  if (varnr==7) browser();

#    cat(paste0(varnr,'\n'))

    info<-fn_baseclass(varnr)
    var<-do.call(fn_labelled, info)
    #browser()
    if (!is.null(in_varnames))
    {
      if (varnr==1)
      {
        if (varnames[[varnr]]=='var1')
        {
          setnames(dt,'var2')
          n <- 'var2'
        } else { n <- 'var1'}
      }
      if (varnames[[varnr]] %in% names(dt))
      {
        newname <- make.un-ique(c(names(dt), varnames[[varnr]]))[[length(dt)+1]]
        add_msg(varname = colnames(dt)[[varnr]],
                message = paste0('Duplicate variable name detected "',
                                 varnames[[varnr]],
                                 '" for variable nr ',
                                 varnr,
                                 ". Renaming it into '",
                                 newname,
                                 "'"),
                msg_list = errors )

      } else {
        newname <- varnames[[varnr]]
      }
      dt[, (newname):=var]
      if (varnr==1)
      {
        dt[,(n):=NULL]
      }
    } else {
      dt[[varnr]]<-var
    }

    if (length(vartypes)>0 && length(vartypes[[varnr]]>0)) {
      setattr(dt[[varnr]],'measure_type', vartypes[[varnr]])
      if (vartypes[[varnr]]=='O' && is.factor(dt[[varnr]]))
      {
        setattr(dt[[varnr]],'class', c(attr(dt[[varnr]])),"ordered")
      }
    }
    return(var)
  }


  dts<-plyr::alply(seq_along(varnames),1, fn)
  newdt=as.data.table(dts)
  rownames(newdt) <- rownames(dt)
  names(newdt) <- names(dt)

#  browser()
  plyr::l_ply(seq_along(dt), function(varnr) setattr(newdt[[varnr]], 'label', Hmisc::label(dt[[varnr]])) )

  return(list(errors=errors, warnings=mywarnings, dt=newdt))
}

fn_validate_ForceIntegers<-function(varnr, dt)
{
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type')
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return("")
  }
  is_num<-switch(vartype, 'F'=1, 'L'=1, 'I'=1, 'N'=1, 'D'=1,0)
  force_integers<-attr(var, 'force_integers')
  if (!is.null(force_integers) && is_num)
  {
    # if (varnr==13)
    # {
    #   browser()
    # }
    var <- as.numeric(var)
    int1 <- var%%1 < 1E-10
    int2 <- 1 - var%%1 < 1E-10
    which_int = int1 | int2

    if(sum(!which_int, na.rm=TRUE)>0)
    {
      cases=row.names(dt)[!which_int]

      return(paste0(
               'Variable ',
               nice_varname(dt, varnr),
               ' contains ',
               ifelse(length(cases)>1,
                      paste0(length(cases),
                             " cases"),
                      paste0(" a case")),
               " with value that is not integer: ",
               format_case_value_list(cases, var[!which_int]),
               ". "))
    }

  }

  return('')
}

fn_validate_TheoreticalMin<-function(varnr, dt)
{
#  cat(paste0(varnr,'\n'))
  #if(varnr==132) browser();
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type')
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return('')
  }
  is_num<-switch(vartype, 'F'=1, 'L'=1, 'I'=1, 'N'=1, 'D'=1,0)
  mins<-attr(var, 'theoretical_min')
  error <- ""
  if (!is.null(mins) && is_num)
  {
    which_min=var<mins
    which_min[is.na(which_min)]<-FALSE
    if(sum(which_min)>0)
    {
      cases=row.names(dt)[which_min]
      #browser()
      error <- paste0(
                      'Variable ',
                      nice_varname(dt, varnr),
                      ' contains ',
                      ifelse(length(cases)>1,
                             paste0(length(cases),
                                    " cases"),
                             paste0("a case")),
                      " with value smaller than theoretical min: ",
                      format_case_value_list(case_names = cases,
                                             values = dt[[varnr]][which_min]
                                             ))
    }

  }
  maxs<-attr(var, 'theoretical_max')
  if (!is.null(maxs) && is_num)
  {
    which_max=var>maxs
    which_max[is.na(which_max)]<-FALSE
    if(sum(which_max)>0)
    {
      cases=row.names(dt)[which_max]

      if (nchar(error)>0)
      {
        error <- paste0(error,
                        ', and ',
                        ifelse(length(cases)>1,
                               paste0(length(cases),
                                      " cases"),
                               paste0("a case")),
                        " with value greater than theoretical max: ",
                        format_case_value_list(case_names = cases,
                                               values = dt[[varnr]][which_max]
                                               ))
      } else {
        error <- paste0(
                      'Variable ',
                      nice_varname(dt, varnr),
                      ' contains ',
                      ifelse(length(cases)>1,
                             paste0(length(cases),
                                    " cases"),
                             paste0(" a case")),
                      " with value greater than theoretical max: ",
                      format_case_list(cases, FALSE),
                      ". ")
      }
    } else {
      if (nchar(error)>0)
      {
        error <- paste0(error, ". ")
      }
    }
    return(error)
  }
  return('')
}

fn_validate_Required<-function(varnr, dt)
{
  var<-dt[[varnr]]
  required<-attr(var, 'required')
  if (!is.null(required) )
  {
    which_required=is.na(var)
    if(sum(which_required)>0)
    {
      cases=row.names(dt)[which_required]

      return(paste0(
                      'Variable ',
                      nice_varname(dt, varnr),
                      ' contains ',
                      ifelse(length(cases)>1,
                             paste0(length(cases),
                                    " missing data in the following cases: "),
                             paste0("a missing data in the following case: ")),
                      format_case_list(cases, FALSE),
                      '. ')
      )
    }

  }
  return('')
}
fn_validate_Type<-function(varnr, dt)
{
#  cat(paste0(varnr,'\n'))
#  if (varnr==151)  browser()
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type')
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return("")
    #browser()
  }
  varname <- names(dt)[[varnr]]

  if (stringr::str_detect(varname,pattern="^\\..*$"))
  {
    return('')
  }

  if(vartype=='0' )
  {
    if (sum(!is.na(var))>0)
    {
      return(paste0(
        'Variable ',
        nice_varname(dt, varnr),
        ' should be empty, but it contains ',
        ifelse(sum(!is.na(var))>1,
               paste0(sum(!is.na(var)),
                      " non-missing cases. "),
               paste0("1 non-missing case. "))
      ))
    }
  } else if (vartype=='F' || vartype=='L' || vartype == 'I' || vartype == 'N' || vartype == 'D' || vartype == 'S')
  {
    intended_class <- vartype2class(vartype)
    if (intended_class=='##')
    {
      browser()
    }
    if (!(intended_class %in% class(var)))
    {
      if (intended_class %in% c('integer', 'numeric') && any(class(var) %in% c('integer', 'numeric') ))
      {
        return('') #Don't bother about problems with conversion between integer and numeric
      }
      msg <- paste0(
        'Variable ',
        nice_varname(dt, varnr),
        " should be ",
        intended_class,
        " but in fact is of type(s) ",
        paste0(class(var), collapse = ' and '),
        ". ")
      if ('character' %in% class(var) && is_vartype_numeric(vartype))
      {
        numNAs <- suppressWarnings(as.numeric(var))
        NAs <- is.na(var)
        non_numeric <- is.na(numNAs) & !NAs
        if (sum(non_numeric)>0)
        {
          msg<-paste0(msg,
                      "The following ",
                      ifelse(sum(non_numeric)>1,
                             paste0(sum(non_numeric),
                                    " cases are"),
                             paste0("case is")),
                      " non-numeric: ",
                      format_case_value_list(case_names = rownames(dt)[non_numeric], values=var[non_numeric]),
                      ". ")
        }
      }
      return(msg)
      #todo: Jeśli zmienna aktualna jest character, a wpisana jest, że ma być inna, to wypisz przypadki tekstowe do raportu błędu
    }
  } else
  {
    return(paste0(
      'Variable ',
      nice_varname(dt, varnr),
      " has unknown type(s) ",
      class(vartype),
      '. (It belongs to the following classes: ',
      paste0(class(var), collapse = ' and '),
      "). "
    ))
  }
  return("")
}

fn_validate_LimitToLabels<-function(varnr, dt)
{
#  if(varnr==12)
#    browser()
#  cat(paste0(varnr,'\n'))
#  if (varnr==12) browser();
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type')
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return('')
    browser()
  }

  should_be_labelled <- attr(var, 'limit_to_labels')

  if (is.null(should_be_labelled))
  {
    return('')
  }

  if (!should_be_labelled)
  {
    return('')
  }

  if(class(var) == "labelled")
  {
    l<-labelled::val_labels(var)
    values<-as.numeric(l)

    vars<-var
    vars[vars %in% values]<-NA

    filter<-!(vars %in% c(NA, values))
    cases<-row.names(dt)[filter]

    vars<-var[filter]

    if (length(vars)>0)
    {
      msg<-paste0("Variable ",
                  nice_varname(dt, varnr),
                  " contains ",
                  ifelse(length(vars)>1,
                         paste0(length(cases),
                                " values that are"),
                         paste0(" a value that is")),
                  " not labelled: ",
                  format_case_value_list(case_names = cases, values = as.numeric(vars)))
      return(msg)
    }
  }
  return('')
}

set_TheoreticalMinMax<-function(dt, mins, maxs)
{
  fn_set<-function(varnr)
  {
    if(!is.na(mins[[varnr]]))
    {
      setattr(dt[[varnr]],'theoretical_min', mins[[varnr]])
    }

    if(!is.na(maxs[[varnr]]))
    {
      setattr(dt[[varnr]],'theoretical_max', maxs[[varnr]])
    }
  }
  #browser()

  plyr::a_ply(seq_along(dt), 1, fn_set)

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_TheoreticalMin, dt=dt)
  return(list(dt=dt, errors=errors))
}

set_ForcedIntegers<-function(dt, forcedIntegers)
{
  fn_set<-function(varnr)
  {
    if(!is.na(forcedIntegers[[varnr]]))
    {
      setattr(dt[[varnr]],'force_integers', forcedIntegers[[varnr]])
    }

  }
  #browser()

  plyr::a_ply(seq_along(dt), 1, fn_set)

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_ForceIntegers, dt=dt)
  return(list(dt=dt, errors=errors))
}

set_Required<-function(dt, required)
{
  fn_set<-function(varnr)
  {
    if(!is.na(required[[varnr]]))
    {
      setattr(dt[[varnr]],'required', required[[varnr]])
    }
  }
  #browser()

  plyr::a_ply(seq_along(dt), 1, fn_set)

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_Required, dt=dt)
  return(list(dt=dt, errors=errors))
}


set_LimitToLabels<-function(dt, limitToLabels)
{
  fn_set<-function(varnr)
  {
    if(!is.na(limitToLabels[[varnr]]))
    {
      setattr(dt[[varnr]],'limit_to_labels', limitToLabels[[varnr]])
    }
  }
  #browser()

  plyr::a_ply(seq_along(dt), 1, fn_set)

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_LimitToLabels, dt=dt)
  return(list(dt=dt, errors=errors))
}

ValidateCustom<-function(dt)
{
#  browser()
  varnr <- 1
  basevarnr <- 1
  errors <- new.env()
  while(varnr <= length(dt))
  {
#    cat(paste0(varnr,'\n'))
#    if(varnr==196) browser()
    varname<-colnames(dt)[[varnr]]
    if(stringr::str_detect(varname,pattern="^\\..*$"))
    {
#      browser()
      basevarNotNA <- !is.na(dt[[basevarnr]])
      validatevar <- dt[[varnr]]==0
      checkvar <- validatevar * basevarNotNA != 0
      if (sum(checkvar, na.rm=TRUE)>0)
      {
        pos <- which(checkvar)
        cases <- row.names(dt)[checkvar]
        vartype <- attr(var,'measure_type')

        msg <- paste0(
                       "In the following ",
                       ifelse(sum(checkvar)>1,
                              paste0(sum(checkvar),
                                     " cases"),
                              paste0(" case")),
                       " Variable ",
                       nice_varname(dt, basevarnr),
                       " ",
                       Hmisc::label(dt[[varnr]]),
                       ": ",
                       format_case_value_list(case_names = cases, values = dt[[basevarnr]][checkvar]
                       )
            )
        add_msg(varname = varname, message = msg, msg_list = errors)
      }
    } else {
      basevarnr <- varnr
    }
    varnr <- varnr + 1
  }
  #browser()

  return(errors=errors)
}

ValidateTypes<-function(dt)
{
  #browser()

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_Type, dt=dt)
  return(errors)
}

