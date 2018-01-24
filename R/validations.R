fn_validate_ForceIntegers<-function(varnr, dt)
{
  # if(varnr==328) browser()
  #  warning(varnr)
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type', exact = TRUE)
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return()
  }
  is_num<-switch(vartype, 'F'=1, 'L'=1, 'I'=1, 'N'=1, 'D'=1,0)
  force_integers<-attr(var, 'force_integers', exact = TRUE)
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

      add_msg(dt = dt, varname = colnames(dt)[[varnr]],
              message = paste0(
                'contains ',
                ifelse(length(cases)>1,
                       paste0(length(cases),
                              " cases"),
                       paste0(" a case")),
                " with value that is not integer: ",
                format_case_value_list(cases, var[!which_int])),
              flag_show_type=TRUE)
    }

  }
}

fn_validate_TheoreticalMin<-function(varnr, dt)
{
 #   cat(paste0(varnr,'\n'))
#  if(varnr==6) browser();
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type', exact = TRUE)
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return('')
  }
  is_num<-switch(vartype, 'F'=1, 'L'=1, 'I'=1, 'N'=1, 'D'=1,0)
  mins<-attr(var, 'theoretical_min', exact = TRUE)
  if (!is.null(mins) && is_num)
  {
    which_min=var<mins
    which_min[is.na(which_min)]<-FALSE
    if(sum(which_min)>0)
    {
      cases=row.names(dt)[which_min]
      #browser()
      error <- paste0(
        'contains ',
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
  maxs<-attr(var, 'theoretical_max', exact = TRUE)
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
          'contains ',
          ifelse(length(cases)>1,
                 paste0(length(cases),
                        " cases"),
                 paste0(" a case")),
          " with value greater than theoretical max: ",
          format_case_list(cases, FALSE))
      }
      add_msg(dt=dt, varname = colnames(dt)[[varnr]],
      				message = error)
    }
  }
}

fn_validate_Required<-function(varnr, dt)
{
  var<-dt[[varnr]]
  required<-attr(var, 'required', exact = TRUE)
  if (!is.null(required) )
  {
    which_required=is.na(var)
    if(sum(which_required)>0)
    {
      cases=row.names(dt)[which_required]
      msg<-paste0(
        'contains ',
        ifelse(length(cases)>1,
               paste0(length(cases),
                      " missing data in the following cases: "),
               paste0("a missing data in the following case: ")),
        format_case_list(cases, FALSE))
      add_msg(dt=dt, varname = colnames(dt)[[varnr]],
              message = msg)
    }
  }
  return('')
}
fn_validate_Type<-function(varnr, dt)
{
  #  cat(paste0(varnr,'\n'))
  #  if (varnr==151)  browser()
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type', exact = TRUE)
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
      add_msg(dt=dt, varname=varname,
              message=
                paste0(
                  'should be empty, but it contains ',
                  ifelse(sum(!is.na(var))>1,
                         paste0(sum(!is.na(var)),
                                " non-missing cases. "),
                         paste0("1 non-missing case. "))
                ))
      return()
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
        "should be ",
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
                      "contains ",
                      ifelse(sum(non_numeric)>1,
                             'cases that are ',
                             'case that is'),
                      " non-numeric: ",
                      format_case_value_list(case_names = rownames(dt)[non_numeric], values=var[non_numeric])
          )
        }
      }
      add_msg(dt=dt, varname = varname, message = msg)
      return()
      #todo: Jeśli zmienna aktualna jest character, a wpisana jest, że ma być inna, to wypisz przypadki tekstowe do raportu błędu
    }
  } else
  {
    add_msg(dt=dt, varname = varname,
            paste0(
              "has unknown type(s) ",
              class(vartype),
              '. (It belongs to the following classes: ',
              paste0(class(var), collapse = ' and '),
              "). "
            ))
  }
  return()
}

fn_validate_LimitToLabels<-function(varnr, dt)
{
  #  if(varnr==12)
  #    browser()
  #  cat(paste0(varnr,'\n'))
  #  if (varnr==12) browser();
  var<-dt[[varnr]]
  vartype <- attr(var,'measure_type', exact = TRUE)
  if (is.null(vartype))
  {
    #Error! Nie dodaliśmy typu do zmiennej!...
    return('')
    browser()
  }

  should_be_labelled <- attr(var, 'limit_to_labels', exact = TRUE)

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
      add_msg(dt=dt, varname = colnames(dt)[[varnr]],
              message =  paste0(
                "contains ",
                ifelse(length(vars)>1,
                       paste0(length(cases),
                              " values that are"),
                       paste0(" a value that is")),
                " not labelled: ",
                format_case_value_list(case_names = cases, values = as.numeric(vars))))
      return()
    }
  }
  return()
}

is_symbol_recursive<-function(symbol, dt, forbidden_symbols=NULL) {
  if (!symbol %in% colnames(dt)) {
    browser() #Makes no sense to ask for a proprty of a symbol that's not there
  }

  local_symbols <- GetRFormulaSymbols(symbol)
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


locateSymbol<-function(symbol, startenv=globalenv(), nest_level=1) {
  if(environmentName(startenv)=='R_EmptyEnv') {
    return(list(nest_level=nest_level, env_name = NA))
  }
  if(exists(symbol, envir=startenv, inherits = FALSE)) {
    return(list(nest_level=nest_level,env_name = environmentName(startenv)))
  } else {
    return(locateSymbol(symbol, parent.env(startenv), nest_level = nest_level +1))
  }
}

fn_validate_Formulas<-function(varnr, dt)
{
  #  if(varnr==12)
  #    browser()
#  cat(paste0(varnr,'\n'))
#  if (varnr==19) browser();
  #  if (varnr==4) browser();
  var<-dt[[varnr]]

  rformula <- GetRFormula(var)
  if(length(rformula)>1) browser()

  if (is.na(rformula)) {
    return('')
  }

  eformula <- tryCatch(parse(text=rformula), error=function(e) e)
  if ('error' %in% class(eformula)) {
    add_msg(dt=dt, varname = colnames(dt)[[varnr]],
            message = paste0("has the following syntax error in its formula: *",
                             eformula$message, "*. "), flag_show_formula=TRUE)
    return()
  }

  if(!is.expression(eformula)) {
    browser()
    #unknown error. Result of parse should always be of type expression
  }

  symbole <- unique(all.names(eformula))

  #We filter out symbols, that are known globally, like pi
  if(length(symbole)>0) {
    global_symbols <- symbole[!purrr::map_lgl(purrr::map_chr(purrr::map(symbole, locateSymbol), 'env_name'), is.na)]
    symbols_to_remove <- setdiff(global_symbols, colnames(dt))
    symbole <- setdiff(symbole, symbols_to_remove)

    existances <- symbole %in% colnames(dt)

    missings <- symbole[!existances]
    if (length(missings)>0){
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message= paste0("has a formula that refers to the following udefined symbols: ",
                              format_case_list(missings, flag_quote = TRUE)), flag_show_formula=TRUE)
      return()
    }
  }


  #Upewniamy się, że drugi typ formuły też jest wpisany
  xlsformula <- GetExcelFormula(var)
  if (is.na(xlsformula)) {
    add_msg(dt=dt, varname=colnames(dt)[[varnr]],
            message = paste0("has no Excel counterpart for the R formula"))
  } else {
    if (stringr::str_sub(xlsformula, 1,1)!='='){
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0("has the Excel formula `", xlsformula, "` that does not start with the `=`"), flag_show_formula=TRUE)
    }
  }

  #Teraz sprawdzamy, czy nie ma rekurencji w wywołaniach symboli
  #
  if(length(symbole)>0) {
    ans<- purrr::map(symbole, function(smb) is_symbol_recursive(symbol = smb, dt = dt))
    which_ans <- purrr::map_lgl(ans, 'ans')
    ans <- purrr::map_chr(ans[which_ans], 'path')

    if (length(ans)>0){
      for(path in ans) {
        if (length(path)==1) {
          add_msg(dt=dt, varname=colnames(dt)[[varnr]],
                  message = paste0('has a formula that references itself'), flag_show_formula=TRUE)
        } else if(length(path)==2)  {
          add_msg(dt=dt, varname=colnames(dt)[[varnr]],
                  message = paste0('has circular references in its formula: "',
                                   "the definition of `", path[[1]],
                                   "` depends on the definition of `", colnames(dt)[[varnr]], '`'),
                  flag_show_formula=TRUE)
        } else {
          add_msg(dt=dt, varname=colnames(dt)[[varnr]],
                  message = paste0('has circular references in its formula; "',
                                   paste0('`', path, '`', collapse = '⇒')),
                  flag_show_formula = TRUE)
        }
      }
      return()
    }
  }


  #Teraz liczymy wektor formuły i upewniamy się, że jego zawartość zgadza się z tym, co w zmiennej
  var2 <- tryCatch(
    eval(eformula,envir = dt),
    error=function(e){e}
  )
  if ('error' %in% class(var2)){
    add_msg(dt=dt, varname=colnames(dt)[[varnr]],
            message = paste0('has a formula that evaluates with the following error: *', var2$message, '*'),
            flag_show_formula = TRUE)

    return()
  }

  if(class2vartype(var2)=='') {
    add_msg(dt=dt, varname=colnames(dt)[[varnr]],
            message = paste0('has a formula that evaluates to value of uncompatible class ',
                             paste0('`', class(var2), '`', collapse = ", ")),
            flag_show_formula = TRUE)

    return()
  }

  if (length(var2)!=length(var) && length(var2)!=1 ) {
    add_msg(dt=dt, varname=colnames(dt)[[varnr]],
            message = paste0('has a formula that produces vector of size ',
                             length(var2), " where size ", length(var), " was expected" ),
            flag_show_formula = TRUE)
    return()
  }

  if(IsVariableValidation(dt = dt, varnr=varnr))  {
    if(!class(var2) %in% c('numeric', 'logical', 'integer' ))
    {
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0('evaluates to type `', class(var2),
                               '`, where type `logical` or numeric was expected' ),
              flag_show_formula = TRUE)
      return()
    }
  } else {
    if (is.list(var2) || length(intersect(class(var), class(var2)))==0) {
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0('has a formula that produces incompatible object of type `', class(var2),
                               '`, where type `', class(var), "` was expected" ),
              flag_show_formula = TRUE)
      return()
    }
  }



  diffs <- compare_two_variables(var2, var)$compvar
  if (sum(diffs)>0) {
    if (sum(diffs) == length(var)) {
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0('has a formula that evaluates to a vector with all values different from existing values'),
              flag_show_formula = TRUE)

    } else if (sum(diffs)*2 > length(var)){
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0('has a formula that evaluates to a vector with most (', sum(diffs),
                               ') values different form existing values'),
              flag_show_formula = TRUE)
    } else {
      add_msg(dt=dt, varname=colnames(dt)[[varnr]],
              message = paste0('has a formula that evaluates to a vector with', sum(diffs),
                               ' values different from existing values',
                               nice_varname(dt,varnr)),
              flag_show_formula = TRUE)
    }
    return()
  }

  return()

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

  purrr::walk(seq_along(dt), fn_set)
  purrr::walk(seq_along(dt), fn_validate_TheoreticalMin, dt=dt)
 # browser()

  return(dt)
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

  purrr::walk(seq_along(dt), fn_set)
  purrr::walk(seq_along(dt), fn_validate_ForceIntegers, dt=dt)

  return(dt)
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

  purrr::walk(seq_along(dt), fn_set)
  purrr::walk(seq_along(dt), fn_validate_Required, dt=dt)

  return(dt)
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

  purrr::walk(seq_along(dt), fn_set)
  purrr::walk(seq_along(dt), fn_validate_LimitToLabels, dt=dt)

  return(dt)
}

set_Formulas<-function(dt, xlsformulas, rformulas)
{
  for (varnr in seq_along(xlsformulas)){
    var <- dt[[varnr]]
    if (!is.na(xlsformulas[[varnr]])) {
      setattr(dt[[varnr]], 'xls_formula',  xlsformulas[[varnr]] )
    }
    if (!is.na(rformulas[[varnr]])) {
      setattr(dt[[varnr]], 'r_formula',  rformulas[[varnr]] )
    }
  }


  purrr::walk(seq_along(dt), fn_validate_Formulas, dt=dt)

  return(dt)
}

fn_validateCustom<-function(varnr, dt)
{
#  if(varnr==18) browser()
#  if(varnr==19) browser()
  #  browser()
  validations <- GetValidations(dt[[varnr]])
  if(IsVariableValidation(dt, varnr)) {
    if(!is.null(validations)) {
      add_msg(dt=dt, varname = colnames(dt)[[varnr]],
              message = paste0("contains its own validation rules"))
    }
    return()
  }

  basevarname <- colnames(dt)[[varnr]]

  for(validation in validations) {
    varname <- validation$varname
    #      browser()
    basevarNotNA <- !is.na(dt[[basevarname]])
    validatevar <- dt[[varname]]==0
    checkvar <- validatevar * basevarNotNA != 0
    if (sum(checkvar, na.rm=TRUE)>0)
    {
      pos <- which(checkvar)
      cases <- row.names(dt)[checkvar]
      vartype <- attr(var,'measure_type', exact = TRUE)

      msg <- paste0(
        Hmisc::label(dt[[varname]]),
        " in the following ",
        ifelse(sum(checkvar)>1,
               paste0(sum(checkvar),
                      " cases"),
               paste0(" case")),
        ": ",
        format_case_value_list(case_names = cases, values = dt[[varnr]][checkvar]
        )
      )
      add_msg(dt=dt, varname = basevarname,
              message = msg)
    }
  }
  return()
}

ValidateCustom<-function(dt)
{
  base_varname <- NA
  for (varnr in seq_along(dt)){
    var <- dt[[varnr]]
    varname <- colnames(dt)[[varnr]]
    if(IsVariableValidation(dt, varnr)) {
      if(is.na(base_varname)) {
        add_msg(dt=dt, varname = varname,
                message = paste0("has a name that starts with the `.validate`, yet there is no predecessor variable to validate"))
      } else {
        validations <- attr(dt[[base_varname]], 'validations', exact = TRUE)
        setattr(dt[[base_varname]], 'validations', c(validations, list(list(varname=varname))))
      }
    } else {
      base_varname <- colnames(dt)[[varnr]]
    }
  }

  #browser()

  purrr::walk(seq_along(dt), fn_validateCustom, dt=dt)
  return(dt)
}


ValidateTypes<-function(dt)
{
  #browser()

  purrr::walk(seq_along(dt), fn_validate_Type, dt=dt)
  return(dt)
}

