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
    rng_TheoreticalMin = 'TheoreticalMin', #
    rng_TheoreticalMax = 'TheoreticalMax', #
    rng_ForceInteger = 'ForceInteger', #
    rng_Required = 'Required', #
    rng_Version = 'Version', #
    rng_XLSFormulas = 'XLSFormulas',
    rng_RFormulas = 'RFormulas',
    globalPathPrefix = '/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy'
  )
  toset	<-	!(names(op.danesurowe)	%in%	names(op))
  if(any(toset))	options(op.danesurowe[toset])
  invisible()
}
# nocov end





IsLimitedToLabels<-function(var)
{
  if('factor' %in% class(var))
  {
    return(TRUE)
  }
  flag<-attr(var2,'limit_to_labels')
  if (is.null(flag)){
    return(FALSE)
  } else {
    return(flag)
  }
}

GetDBName<-function(db, default=NULL){
  label <- attr(db, 'label')
  if (is.null(label)) {
    if(is.null(default)) {
      label<-deparse(substitute(db))
    } else {
      label <- default
    }
  }
  return(label)
}


IsRequired_1<-function(var)
{
  flag<-attr(var,'required')
  if (is.null(flag)){
    return(FALSE)
  } else {
    return(flag)
  }
}

IsRequired<-Vectorize(IsRequired_1)

GetFOB<-function(var, flag_recalculate_uniques=FALSE) {
  if(is.null(var)) {
    browser()
  }
  if(flag_recalculate_uniques) {
    count<-length(unique(var))
    if(count==2) {
      data.table::setattr(var, 'f.o.b', 3)
      return(3)
    } else if (count<=1) {
      browser()
      data.table::setattr(var, 'f.o.b', -1)
      return(-1)
    }
  } else {
    count<-NA
  }
  if(is.null(attr(var, 'f.o.b'))) {
    typ <- class2vartype(var)
    if(typ %in% c('F', 'L')) {
      labs <- GetLabels(var)
      if(min(length(labs),count, na.rm=TRUE)==2) {
        data.table::setattr(var, 'f.o.b', 3)
      } else {
        if (IsLimitedToLabels_1(var)) {
          data.table::setattr(var, 'f.o.b', 1)
        } else {
          data.table::setattr(var, 'f.o.b', 2)
        }
      }
    } else if (typ %in% c('I', 'N', 'D') ) {
      data.table::setattr(var, 'f.o.b', 0)
    } else if (typ == '0') {
      data.table::setattr(var, 'f.o.b', 3)
    } else if (typ == 'S') {
      u <- unique(var)
      if (length(u)>2) {
        data.table::setattr(var, 'f.o.b', 1)
      } else {
        data.table::setattr(var, 'f.o.b', 3)
      }
    } else {
      browser()
    }
  }

  return(attr(var, 'f.o.b'))
}

IsLimitedToLabels_1<-function(var) {
  val<-attr(var,'limit_to_labels')
  typ <- class2vartype(var)
  if (typ == 'F') {
    return(TRUE)
  } else {
    if(is.null(val)){
      return(FALSE)
    } else {
      return(val)
    }
  }
}

IsLimitedToLabels<-Vectorize(IsLimitedToLabels_1)

GetLabelsString_1<-function(var) {
  levels<-GetLevels(var)
  if(length(levels)>0) {
    levels<-levels[order(levels)]
    levels_str <- paste0(format_values(levels), '="', names(levels),'"', collapse = ';')
  } else {
    levels_str<-character(0)
  }

  NAlevels<-GetNALevels(var)
  if(length(NAlevels)>0) {
    NAlevels<-NAlevels[order(names(NAlevels))]
    levelsNA_str <- paste0('NA(', NAlevels, ')="', names(levels),'"', collapse = ';')
  } else{
    levelsNA_str<-character(0)
  }



  if(length(levels_str)>0 && length(levelsNA_str)>0) {
    znak<-';'
  } else {
    znak<-''
  }
 return(paste0('', levels_str, znak, levelsNA_str))
}

GetLabelsString<-Vectorize(GetLabelsString_1)

GetLevels<-function(var)
{
  if('factor' %in% class(var))
  {
    return(setNames(seq(levels(var)), levels(var)))
  } else if ('labelled' %in% class(var))
  {
    l<-labelled::val_labels(var)
    return(l[!is.na(l)])
  } else {
    return(var[FALSE]) #Returns zero-length vector of the correct class
  }
}

GetLabels<-function(var)
{
  if('factor' %in% class(var))
  {
    return(levels(var))
  } else if ('labelled' %in% class(var))
  {
    return(names(na.omit(labelled::val_labels(var))))
  } else {
    return(character(0))
  }
}

#Returns named vector that maps labels associated with tagged_na to letter of the na
GetNALevels<-function(var)
{
  if ('labelled' %in% class(var))
  {
    levs<-labelled::val_labels(var)
    levs<-levs[is.na(levs)]
    levs[seq_along(levs)]<-haven::na_tag(levs)
    return(levs)
  } else {
    return(var[FALSE]) #Returns zero-length vector of the correct class
  }
}

GetNALabels<-function(var)
{
  if ('labelled' %in% class(var))
  {
    levs<-labelled::val_labels(var)
    levs<-levs[is.na(levs)]
    return(names(levs))
  } else {
    return(character(0)) #Returns zero-length vector of the correct class
  }
}

GetVarLabel<-function(dt, varname, quote_varname='', quote_varlabel='') {
  var<-dt[[varname]]
  mylabel<-attr(var, 'label')
  if(is.null(mylabel)) {
    return(paste0(quote_varname, varname, quote_varname))
  } else {
    return(paste0(quote_varlabel, mylabel, quote_varlabel))
  }
}

# Returns label corresponding to the value in variable var, or returns character(0) if not found.
# If possible, matches tagged_na.
# For simple NA returns just the same, NA
GetLabelToValue<-function(var, value)
{
  if ('labelled' %in% class(var))
  {
    if (is.na(value))
    {
      if(haven::is_tagged_na(value))
      {
        tag<-haven::na_tag(value)
        if (tag %in% GetNALabels(var))
        {
          return(GetNALevels(var)[match(tag, GetNALabels(var))])
        } else {
          return(character(0))
        }
      } else {
        return(NA)
      }
    } else {
      pos <- which(Vectorize(all.equal)
                   (value ,labelled::val_labels(var)  ) )
      if (length(pos)>0)
      {
          value_out <- labelled::val_labels(var)[[pos]]
      } else {
        return(character(0))
      }
    }
  } else if ('factor' %in% class(var))
  {
    if (is.na(value))
    {
      return(NA)
    } else {
      if (val %in% seq_along(labels(var)))
      {
        return(labels(var)[[val]])
      } else {
        return(character(0))
      }
    }
  } else {
    if (is.na(value))
    {
      return(NA)
    } else {
      return(character(0))
    }
  }
}

GetExcelFormula_1<-function(var) {
  frm <- attr(var, 'xls_formula')
  if(is.null(frm)){
    return(NA)
  } else {
    return(frm)
  }
}

GetExcelFormula<-Vectorize(GetExcelFormula_1)

GetRFormula_1<-function(var) {
  frm <- attr(var, 'r_formula')
  if(is.null(frm)){
    return(NA_character_)
  } else {
    return(frm)
  }
}

GetRFormula<-Vectorize(GetRFormula_1)

#Returns list of symbols mentioned by the formula
GetRFormulaSymbols<-function(var) {
  rformula <- GetRFormula(var)

  symbole <- Vectorize(exists)(all.names(parse(text = rformula)))
  symbole <- unique(names(symbole[!symbole]))
  return(symbole)
}

GetTheoreticalMin_1<-function(var) {
  val <- attr(var, 'theoretical_min')
  if(is.null(val)){
    if(is.integer(var)) {
      return(NA_integer_)
    } else {
      return(NA_real_)
    }
  } else {
    return(val)
  }
}

GetTheoreticalMin<-Vectorize(GetTheoreticalMin_1)

AreIntegersForced_1<-function(var){
  val <- attr(var, 'force_integers')
  if(is.null(val)){
    return(as.logical(NA))
  } else {
    return(val)
  }
}

AreIntegersForced<-Vectorize(AreIntegersForced_1)

GetTheoreticalMax_1<-function(var) {
  val <- attr(var, 'theoretical_max')
  if(is.null(val)){
    if(is.integer(var)) {
      return(NA_integer_)
    } else {
      return(NA_real_)
    }
  } else {
    return(val)
  }
}

GetTheoreticalMax<-Vectorize(GetTheoreticalMax_1)

IsVariableValidation<-function(dt, varnr) {
  varname <- colnames(dt)[[varnr]]
  return(stringr::str_sub(varname,end=nchar('.valid')) == '.valid')

}

GetValidations<-function(var) {
  val <- attr(var, 'validations')
  return(val)
}

GetProblems<-function(dt) {
  contexts <- purrr::map_lgl(dt, function(v) is.null(attr(v,'warnings_context') ))
  warnings <- purrr::map(dt, function(v) attr(v,'warnings') )
  varnames <- names(contexts)[!contexts]
  warnings <- warnings[!contexts]
  long_varnames <- map_chr(varnames, function(varname) getProblemContextForVariable(dt, varname))
  entries <- map_chr(warnings, function(w) {
    if(length(w)==1) {
      return(w)
    } else {
      return(pander::pandoc.list.return(paste0(w,'\n\n'),style = 'bullet' ,add.line.breaks=TRUE,  add.end.of.list = FALSE, loose=TRUE))
    }})
  df <- data.table("Variable description"= long_varnames, Problems=entries)
  caption = paste0("Problems with the dataframe ", attr(dt, 'label'))
  tab<-pander::pandoc.table.return(df, caption=caption, justify = 'll', use.hyphening = TRUE, style='grid', split.tables = Inf , split.cells = 80, keep.line.breaks = TRUE  )
  myrap<-pander::Pandoc$new()
  myrap$format <- 'docx'
  myrap$title <- "Variable validation report"
  myrap$author <- "Adam Ryczkowski"

  myrap$add.paragraph(tab)
  myrap$export('tmp.docx')
  return(tab)
}

getProblemContextForVariable<-function(dt, varname) {
  var <- dt[[varname]]
  if( does_show_type(var) ) {
    mytype=class2vartype(var)
    mytype=switch(mytype,
                  'F' = 'factor',
                  L = 'labelled',
                  I = 'integer',
                  D = 'Date',
                  S = 'character',
                  '0' = 'logical',
                  N = 'numeric',
                  NA)
    if(is.na(mytype)) {
      #unknown type
      browser()
    }
    msg<-paste0(mytype, ' variable ')
  } else {
    msg<-''
  }
  msg <- paste0(msg,
                Hmisc::label(var)
  )
  if(does_show_varnr(var)) {
    msg <- paste0(msg,
                  ' (variable number  ', which.max(colnames(dt) == varname ),
                  ', `', varname, '`) '
    )
  } else {
    msg <- paste0(msg,
                  ' (`', varname, '`) '
    )
  }

  if(does_show_formula(var)) {
    msg <- paste0(msg,
                  ' with formula `', GetRFormula(var), '` '
    )
  }
  return(msg)
}

GetMessagesForVariable<-function(dt, varname, flag_give_context = TRUE) {

  if(flag_give_context) {
    msg <- getProblemContextForVariable(dt, varname)
  } else {
    msg <- ''
  }

  a<-attr(var, 'warnings')

  if(!is.null(a)) {
    msg <- paste0(msg, paste0(ifelse(stringr::str_sub(a, -2)=='. ',stringr::str_sub(a, 1, -3), a), collapse = ', '), ". ")
  }


  return(msg)
}

