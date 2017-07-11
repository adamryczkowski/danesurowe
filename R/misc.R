format_case_list<-function(case_names, flag_quote=TRUE)
{
  if(length(case_names)==0)
  {
    return('')
  }
  if (flag_quote)
  {
    case_names <- paste0("`", case_names, "`")
  }
  if(length(case_names)==1)
  {
    return(case_names)
  }
  if(length(case_names)<=10)
  {

    return(paste0(
      paste0(head(case_names,length(case_names)-1), collapse = ', '),
      " and ",
      tail(case_names,1)
    ))
  }
  return(paste0(
    paste0(head(case_names, 7), collapse=', '),
    ", ...",
    paste0(tail(case_names,3), collapse=", ")
  ))
}

format_values<-function(values)
{
  UseMethod("format_values", values)
}

format_values.character<-function(values)
{
  return(paste0('`', values, '`'))
}

format_values.integer<-function(values)
{
  return(as.character(values))
}

format_values.numeric<-function(values)
{
  return(trimws(as.character(haven::format_tagged_na(values))))
}

format_values.Date<-function(values)
{
  old_locale <- Sys.getlocale(category="LC_TIME")
  Sys.setlocale(category="LC_TIME", locale ="en_US.UTF-8")
  ans<-as.character(values, format='%e %b %Y')
  Sys.setlocale(category="LC_TIME", locale = old_locale)
  return(ans)
}

format_values.factor<-function(values)
{
  values<-as.integer(values)
  labels<-as.character(values)
  return(paste0(values, ' (', labels, ')'))
}

format_values.labelled<-function(values)
{
#  browser()
  out <- as.character(values)

  if(sum(haven::is_tagged_na(values))>0)
  {
    taged_nas<-haven::is_tagged_na(values)
    tags<-haven::na_tag(values)
    NAvalues<-haven::na_tag(labelled::val_labels(values))
    NAlabels<-names(labelled::val_labels(values))[!is.na(NAvalues)]
    NAvalues<-na.omit(NAvalues)
    for(i in seq_along(NAvalues))
    {
      NAvalue <- NAvalues[[i]]
      out[tags==NAvalue]<-paste0("<", NAlabels[[i]], ">")
    }
  } else {
    taged_nas<-rep(FALSE, length(values))
  }
  if(sum(is.na(out))>0)
  {
    out[is.na(out)]<-'NA'
  }

  if(sum(!is.na(values))>0)
  {
    not_nas<-!is.na(values)
    vals<-values[not_nas]
    Lvalues<-labelled::val_labels(values)
    Llabels<-names(labelled::val_labels(values))[!is.na(Lvalues)]
    Lvalues<-na.omit(Lvalues)
    for(i in seq_along(Llabels))
    {
      Llabel<-Llabels[[i]]
      Lvalue<-Lvalues[[i]]
      out[values==Lvalue]<-paste0(Lvalue, '(', Llabel, ')')
    }
  }
  return(out)
}

format_case_value_list<-function(case_names, values, flag_quote=FALSE)
{
  if(length(case_names)==0)
  {
    return('')
  }
  if (length(case_names)!=length(values) )
  {
    stop("You need case_names to have the same length as values")
  }
  if (flag_quote)
  {
    case_names <- paste0("'", case_names, "'")
  }

  value_names<-format_values(values)

  case_names <- paste0(case_names, ": ", values)

  if(length(case_names)==1)
  {
    return(case_names)
  }
  if(length(case_names)<=10)
  {

    return(paste0(
      paste0(head(case_names,length(case_names)-1), collapse = ', '),
      " and ",
      tail(case_names,1)
    ))
  }
  return(paste0(
    paste0(head(case_names, 7), collapse=', '),
    ", ...",
    paste0(tail(case_names,3), collapse=", ")
  ))
}

format_case_value_diff_list<-function(case_names, values1, values2, flag_quote=FALSE, flag_skip_nothing=FALSE)
{
  if(length(case_names)==0)
  {
    return('')
  }
  if (length(case_names)!=length(values1) )
  {
    stop("You need case_names to have the same length as values")
  }
  if (length(case_names)!=length(values2) )
  {
    stop("You need case_names to have the same length as values")
  }
  if (flag_quote)
  {
    case_names <- paste0("'", case_names, "'")
  }

  value_names1<-format_values(values1)
  value_names2<-format_values(values2)

  case_names <- paste0(case_names, ": ", value_names1, " -> ", value_names2)

  if(length(case_names)==1)
  {
    return(case_names)
  }
  if(length(case_names)<=10 || flag_skip_nothing)
  {

    return(paste0(
      paste0(head(case_names,length(case_names)-1), collapse = ', '),
      " and ",
      tail(case_names,1)
    ))
  }
  return(paste0(
    paste0(head(case_names, 7), collapse=', '),
    ", ...",
    paste0(tail(case_names,3), collapse=", ")
  ))
}

format_variable_name <- function(colname, var) {
  if(is.null(attr(var, 'label')))
  {
    return(colname)
  } else {
    return(paste0(attr(var, 'label'), '(', colname, ')'))
  }
}

join_messages<-function(strvec1, strvec2, dt)
{
  if(class(strvec1)=='character')
  {
    v1<-new.env()
    for(i in seq_along(strvec1))
    {
      if(strvec1[[i]]!='')
      {
        name<-colnames(dt)[[i]]
        v1[[name]]<-strvec1[[i]]
      }
    }
  } else {
    v1 <- strvec1
  }

  if(class(strvec2)=='character')
  {
    v2<-new.env()
    for(i in seq_along(strvec2))
    {
      if(strvec2[[i]]!='')
      {
        name<-colnames(dt)[[i]]
        v2[[name]]<-strvec2[[i]]
      }
    }
  } else {
    v2 <- strvec2
  }

  if(length(v1)>length(v2))
  {
    v <- v1
    w <- v2
  } else {
    v <- v2
    w <- v1
  }
  #We are going to add w to v

  for (name_w in names(w))
  {
    if(name_w %in% names(v))
    {
      v[[name_w]] <- paste0(v[[name_w]], w[[name_w]])
    } else {
      v[[name_w]] <- w[[name_w]]
    }
  }

  return(v)
}

nice_varname<-function(dt, varnr)
{
  return(paste0(" \"",
         Hmisc::label(dt[[varnr]]),
         "\" (",
         names(dt)[[varnr]],
         ")"))
}

vartype2class <- function(vartype)
{
  return( switch(vartype,
                           'F'='factor',
                           'L'='labelled',
                           'I'='integer',
                           'N'='numeric',
                           'D'='Date',
                           'S'='character',
                           '##'
  )     )
}

#Returns a letter that encodes data type.
class2vartype<-function(var)
{
  classes_sorted <- paste0(sort(class(var)), collapse=',')

  if(classes_sorted %in%  c('factor', 'factor,ordered'))
  {
    ret<-'F'
  } else if(classes_sorted == 'labelled')
  {
    ret<-'L'
  } else if(classes_sorted == 'integer')
  {
    ret<-'I'
  } else if(classes_sorted == 'numeric')
  {
    ret<-'N'
  } else if(classes_sorted %in% c('Date', "POSIXct,POSIXt"))
  {
    ret<-'D'
  } else if(classes_sorted == 'character')
  {
    ret<-'S'
  } else if(classes_sorted == 'logical')
  {
    ret<-'0'
  } else {
    browser()
    return('')
#    stop(paste0("Unkown class: ", classes_sorted))
#    browser()
  }
}

is_vartype_numeric <- function(vartype)
{
  return( switch(vartype,
                 'F'=TRUE,
                 'L'=TRUE,
                 'I'=TRUE,
                 'N'=TRUE,
                 'D'=TRUE,
                 'S'=FALSE,
                 '##'
  )     )
}

#Function compares two vectors for equality
compareNA <- function(v1,v2) {
  if(class(v1)=='numeric')
  {
#    browser()
    diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! (abs(v1 - v2) < 1E-6 & (is.na(v1) == is.na(v2)) )))
  } else {
    diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((v1 == v2) & (is.na(v1) == is.na(v2)) )))
  }
  return(diff)
}

errors <- new.env()
warnings <- new.env()

pack_flags<-function(flag_show_formula=FALSE, flag_show_type=FALSE, flag_show_varnr=FALSE) {
  flags<-as.integer(0)

  if(flag_show_type){
    flags<-bitwOr(flags,1)
  }
  if(flag_show_varnr){
    flags<-bitwOr(flags,2)
  }
  if(flag_show_formula){
    flags<-bitwOr(flags,4)
  }

  return(flags)
}

does_show_factory<-function(pos) {
  return(function(var) {
    flags<-attr(var, 'warnings_context')
    if(is.null(flags)) {
      return(FALSE)
    } else {
      return(as.logical(bitwAnd(as.integer(flags),bitwShiftL(1,pos-1))))
    }
  })
}

does_show_formula<-does_show_factory(3)
does_show_type<-does_show_factory(1)
does_show_varnr<-does_show_factory(2)


add_msg<-function(dt, varname, message, flag_error, flag_warning=TRUE, ...) {
  msg <- attr(dt[[varname]], 'warnings');
  setattr(dt[[varname]], 'warnings', c(msg, message))
  ctx <- attr(dt[[varname]], 'warnings_context')
  if(is.null(ctx)) {
    ctx <- 0
  }
  ctx <- bitwOr(ctx, pack_flags(...))
  setattr(dt[[varname]],'warnings_context',  ctx)
}

add_msg_var<-function(var, message, flag_error, flag_warning=TRUE, ...) {
  msg <- attr(var, 'warnings');
  setattr(var, 'warnings', c(msg, message))
  ctx <- attr(var, 'warnings_context')
  if(is.null(ctx)) {
    ctx <- 0
  }
  ctx <- bitwOr(ctx, pack_flags(...))
  setattr(var,'warnings_context',  ctx)
  return(var)
}

copy_dt_attributes<-function(dt_source, dt_dest, which_colnames='') {
  copy_attributes<-function(dt_source, dt_dest, colname) {
    a <- attributes(dt_source[[colname]])
    attrnames<-setdiff(names(a),c('class','dim', 'dimnames','names', 'levels', 'labels'))
    for(aname in attrnames) {
      setattr(dt_dest[[colname]], aname, a[[aname]])
    }
  }
  if(which_colnames==''){
    which_colnames<- intersect(colnames(dt_dest), colnames(dt_source))
  }
  for (varname in which_colnames) {
    copy_attributes(dt_source, dt_dest, varname)
  }
}

copy_var_attributes<-function(var_source, var_dest) {
  a <- attributes(var_source)
  attrnames<-setdiff(names(a),c('class','dim', 'dimnames','names', 'levels', 'labels'))
  for(aname in attrnames) {
    setattr(var_dest, aname, a[[aname]])
  }
}
