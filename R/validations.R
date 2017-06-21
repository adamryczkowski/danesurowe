fn_validate_ForceIntegers<-function(varnr, dt)
{
  # if(varnr==328) browser()
  #  warning(varnr)
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
    var <- suppressWarnings(as.numeric(var))
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

is_symbol_recursive<-function(symbol, dt, forbidden_symbols) {
  if (!symbol %in% colnames(dt)) {
    browser() #Makes no sense to ask for a proprty of a symbol that's not there
  }

  local_symbols <- GetRFormulaSymbols(var)
  if (any(forbidden_symbols %in% local_symbols)){
    return(list(ans=TRUE, path=symbol))
  }

  for(s in local_symbols) {
    ans <- is_symbol_recursive(s, dt, c(forbidden_symbols, symbol))
    if (ans$ans) {
      return(list(ans=TRUE, path=c(ans$path, symbol)))
    }
  }
  return(list(ans=FALSE))
}

fn_validate_Formulas<-function(varnr, dt)
{
  #  if(varnr==12)
  #    browser()
  #  cat(paste0(varnr,'\n'))
  #  if (varnr==12) browser();
  var<-dt[[varnr]]

  rformula="var1+abs(var1-var2)*fkcja(sin(var3)-var2^var3)"
  rformula <- GetRFormula(var)

  if (rformula=='') {
    return('')
  }

  eformula <- tryCatch(parse(text=str), error=function(e) e)
  if ('error' %in% class(eformula)) {
    msg <- paste0("There is a syntax error in formula attached to the variable ",
                  nice_varname(dt, varnr),
                  ": ",
                  eformula$message)
    return(msg)
  }

  if(!is.expression(eformula)) {
    browser()
    #unknown error. Result of parse should always be of type expression
  }

  symbole <- Vectorize(exists)(all.names(parse(text = eformula)))
  symbole <- unique(names(symbole[!symbole]))

  existances <- symbole %in% c(paste0('var',1:10))
  existances <- symbole %in% colnames(dt)

  missings <- symbole[!existances]
  if (length(missings)>0){
    msg <- paste0("The following symbols are not defined in the formula of ",
                  nice_varname(dt, varnr),
                  ": ",
                  format_case_list(missings, flag_quote = TRUE),
                  ". Without them, cannot evaluate the formula")
    return(msg)
  }

  #Upewniamy się, że drugi typ formuły też jest wpisany
  xlsformula <- GetExcelFormula(var)
  if (nchar(xlsformula)==0) {
    msg <- paste0("There is no Excel counterpart for R formula ",
                  deparse(eformula), " for variable ", nice_varname(dt, varnr), ". ")
  } else {
    if (stringr::str_sub(xlsformula, 1,1)!='='){
      msg <- paste0('Excel formula "', xlsformula, '" for variable ', nice_varname(dt, varnr),
                    'does not start with "=".')
    }
  }

  #Teraz sprawdzamy, czy nie ma rekurencji w wywołaniach symboli
  ans<-is_symbol_recursive(colnames(dt)[[varnr]])
  if (ans$ans) {
    if (length(ans)==1) {
      msg <- paste0(msg, 'Formula for variable ', nice_varname(dt, varnr), ' "', deparse(eformula), '" references itself.')
    } else if(length(ans)==2)  {
      msg <- paste0(msg, 'Circular references in formula "', deparse(eformula), '" for variable ',
                    nice_varname(dt, varnr), ". ",
                    "The definition of ", ans$path[[1]], " depends on ", colnames(dt)[[varnr]], ". ")
    } else {
      msg <- paste0(msg, 'Circular references in formula for variables ', nice_varname(dt, varnr), ': ',
                    paste0(ans$path, collapse = '⇒')
                    )
    }
    return(msg)
  }


  #Teraz liczymy wektor formuły i upewniamy się, że jego zawartość zgadza się z tym, co w zmiennej
  var2 <- tryCatch(
    lazyeval::lazy_eval(eformula, data = dt),
    error=function(e){e}
  )
  if ('error' %in% class(ans)){
    msg <- paste0(msg, 'There was an error in evaluating formula "', deparse(eformula), ' for variable ', nice_varname(dt, varnr),
                  ': ', error$message)
    return(msg)
  }

  if(class2vartype(var2)=='') {
    msg <- paste0(msg, 'Effect of evaluation of "', deparse(eformula),
                  '" in variable ', nice_varname(dt,varnr),
                  ' is of uncompatible class ', paste0(class(var2), collapse = ", "), ". Cannot validate further. ")
    return(msg)
  }

  if (length(var2)!=length(var) && length(var2)!=1) {
    msg <- paste0('Formula "', deparse(eformula),
                  '" for variable ', nice_varname(dt,varnr),
                  ' produces vector of size ', length(var2), " where size ", length(var), " was expected. " )
    return(msg)
  }

  diffs <- compare_two_variables(var2, var)$compvar
  if (sum(diffs)>0) {
    if (sum(diffs) == length(var)) {
      msg<- paste0(msg, 'All values computed with the formula "', deparse(eformula), '" differ form existing values for variable ', nice_varname(dt,varnr))
    } else if (sum(diffs)*2 > length(var)){
      msg<- paste0(msg, 'Most values computed with the formula "', deparse(eformula), '" differ form existing values for variable ', nice_varname(dt,varnr))
    } else {
      msg<- paste0(msg, sum(diffs), ' values computed with the formula "', deparse(eformula), '" differ form existing values for variable ', nice_varname(dt,varnr))
    }
    return(msg)
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

set_Formulas<-function(dt, xlsformulas, rformulas)
{
  for (varnr in seq_along(xlsformulas)){
    var <- dt[[varnr]]
    if (!is.na(GetExcelFormula(var))) {
      setattr(dt[[varnr]], 'xls_formula',  xlsformulas[[varnr]] )
    }
    if (!is.na(GetRFormula(var))) {
      setattr(dt[[varnr]], 'r_formula',  rformulas[[varnr]] )
    }
  }

  errors<-plyr::aaply(seq_along(dt),1,fn_validate_Formulas, dt=dt)
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

