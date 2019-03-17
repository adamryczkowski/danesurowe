# name_of_datasets='datasets', name_of_variables='questions', name_of_variable='question',
# name_of_cases='patients', name_of_case='patient', flag_include_rownumbers = TRUE )
format_case_list<-function(case_names, flag_quote='`', all_cases=NULL, name_of_cases='cases', language='EN')
{

  if(length(case_names)==0)
  {
    return('')
  }
  if(is.null(all_cases)) {
    complementary_case_names <- NULL
  } else {
    complementary_case_names <- setdiff(all_cases, case_names)
  }

	if(is.logical(flag_quote)){
		if(flag_quote) {
			quotes<-'`'
		} else {
			quotes<-''
		}
	} else {
		quotes <- flag_quote
	}

  case_names <- paste0(quotes, case_names, quotes)
  if(length(case_names)==1)
  {
    return(case_names)
  }
  if(length(case_names)<=10)
  {
    if(language=='EN') {
      oraz=' and '
    } else if (language=='PL') {
      oraz=' i '
    } else {
      stop(paste0("Unsupported language ", language))
    }
    return(paste0(
      paste0(head(case_names,length(case_names)-1), collapse = ', '),
      oraz,
      tail(case_names,1)
    ))
  }

  if(!is.null(complementary_case_names)) {
    if(length(complementary_case_names)==0) {
      return(paste0('all ', length(case_names)+length(complementary_case_names), ' ', name_of_cases))
    }
    if(length(case_names)/(length(case_names)+length(complementary_case_names)) > 0.8) {
      msg<-format_case_list(case_names=complementary_case_names, all_cases=all_cases,
                            flag_quote=flag_quote)
      msg<-paste0('all ', length(case_names)+length(complementary_case_names),
                  ' ', name_of_cases, ' except for ', msg)
      return(msg)
    }
  }

  return(paste0(length(case_names), ' ', name_of_cases, ': ',
    paste0(head(case_names, 7), collapse=', '),
    ", ...",
    paste0(tail(case_names,3), collapse=", ")
  ))
}

format_var_name_1<-function(colname, longcolname,
                          flag_main_is_short_name=TRUE, flag_include_secondary_name=FALSE,
                          flag_quote_shortname='', flag_quote_longname='', units="") {
  if(longcolname=='') {
    flag_main_is_short_name <- TRUE
    flag_include_secondary_name <- FALSE
  }

  if(flag_main_is_short_name) {
    msg<-paste0(flag_quote_shortname, colname, flag_quote_shortname)
  } else {
    msg<-paste0(flag_quote_longname, longcolname, flag_quote_longname)
  }
  if(flag_include_secondary_name) {
    msg<-paste0(msg, ' (' )
    if(flag_main_is_short_name) {
      msg<-paste0(msg, flag_quote_longname, longcolname, flag_quote_longname)
    } else {
      msg<-paste0(msg, flag_quote_shortname, colname, flag_quote_shortname)
    }
    msg<-paste0(msg, ')')
  }
  if(units!="") {
    msg<-paste0(msg, " [", units, "]")
  }
	return(msg)
}

df <- tibble(name = c("Tata", "Mama", "Mikołaj", "Zosia", paste0("Dziecko ",3:10)),
             lata = c(38, 34, 9, 1.5, runif(8)*10),
             height = c(170, 160, 120, 67, rnorm(8, 130,10)))
setattr(df$lata, 'infix_equal', ": ")
setattr(df$height, 'label', 'Wzrost')

format_var_name<-Vectorize(format_var_name_1, vectorize.args = c('colname', 'longcolname' ,'units'))


#Zwraca wektor character z ładnymi nazwami i wartościami atrybutów, np.
#   Imię: Adam (wiek: 13)
#df musi mieć tylko jeden rekord
format_attr_list<-function(df, nrow=1, quote_bare_name='`', quote_label='', infix='=') {
  mycolnames <- format_colnames(df, quote_bare_name=quote_bare_name, quote_label=quote_label)

  infixes <- purrr::map_chr(df,  function(v) {
    x=attr(v, 'infix_equal', exact = TRUE)
    if(is.null(x)){
      infix
    } else {
      x
    }})

  colnames_to_remove<-stringr::str_detect(colnames(df), pattern = stringr::regex("^\\."))
  mycolnames[colnames_to_remove]<-""
  infixes[colnames_to_remove]<-""

  ans <- paste0(mycolnames, infixes, purrr::map_chr(df[nrow,], as.character))
  return(ans)
}

format_colnames<-function(df, i, quote_bare_name='`', quote_label='') {
  fn_getlabel<-function(i) {
    myname <- attr(df[[i]], 'label', exact = TRUE)
    if (is.null(myname)) {
      myname <- paste0(quote_bare_name, colnames(df)[[i]], quote_bare_name)
    } else {
      myname <- paste0(quote_label, myname, quote_label)
    }
    return(myname)
  }
  ans <- purrr::map_chr(seq(ncol(df)), fn_getlabel)
  return(ans)
}

format_var_list<-function(colnames, longcolnames=NULL, dt=NULL,
                          flag_main_is_short_name=FALSE, flag_include_secondary_name=FALSE,
                          flag_quote_shortname='', flag_quote_longname='', name_of_variables='variables', units="")
{
  checkmate::assert_character(colnames)
  if(is.null(dt)) {
    complementary_colnames <- NULL
  } else {
    complementary_colnames <- setdiff(base::colnames(dt), colnames)
  }

  if(is.null(longcolnames)) {
    if(is.null(dt)) {
      flag_main_is_short_name<-TRUE
      flag_include_secondary_name<-FALSE
    } else {
      longcolnames<-Hmisc::label(data.frame(dt)[colnames])
      longcolnames[longcolnames==colnames]<-''
    }
  } else {
    if(length(colnames) != length(longcolnames)) {
      stop("Lengths of colnames and long colnames don't match.")
      browser()
    }
  }

  if(length(units)>1){
    if(length(units)!=length(colnames)){
      stop("Length of units must be the same as length of colnames")
    }
  }

  if(length(colnames)==0)
  {
    return('')
  }

  mynames<-format_var_name(colnames, longcolnames, flag_main_is_short_name=flag_main_is_short_name,
                           flag_include_secondary_name=flag_include_secondary_name,
                           flag_quote_shortname=flag_quote_shortname,
                           flag_quote_longname=flag_quote_longname, units=units)

  if(length(colnames)==1)
  {
    return(mynames)
  }

  if(length(colnames)<=10)
  {
    return(paste0(
      paste0(head(mynames,length(mynames)-1), collapse = ', '),
      " and ",
      tail(mynames,1)
    ))
  }
  if(!is.null(complementary_colnames)) {
    if(length(complementary_colnames)==0) {
      return(paste0('all ', length(colnames)+length(complementary_colnames), ' ', name_of_variables))
    }
    if(length(colnames)/(length(colnames)+length(complementary_colnames)) > 0.8) {
      msg<-format_var_list(complementary_colnames, longcolnames=NULL, dt=dt,
                           flag_main_is_short_name=flag_main_is_short_name,
                           flag_include_secondary_name=flag_include_secondary_name,
                           flag_quote_shortname=flag_quote_shortname,
                           flag_quote_longname=flag_quote_longname)
      msg<-paste0('all ', length(colnames)+length(complementary_colnames),
                  ' ', name_of_variables, ' except for ', msg)
      return(msg)
    }
  }
  return(paste0(length(colnames), ' ', name_of_variables, ': ',
                paste0(head(mynames, 7), collapse=', '),
                ", ...",
                paste0(tail(mynames,3), collapse=", ")
  ))
}

format_values_as_one_string<-function(values, complementary_values=NULL, plural_form = 'items',
                                      extra_prop=NULL,extra_prop_name=NULL) {

  if(!is.null(extra_prop)){
    if(length(extra_prop)!=length(values)){
      stop("extra_prop must have the same length as values")
    }
    if(is.null(extra_prop_name)) {
      extra_prop_name<-''
    }
    formatted_values<-paste0(format_values(values), ' (',
                             c(extra_prop_name,rep('', length(extra_pop)-1)), extra_prop, ')')

  } else {
    formatted_values<-format_values(values)
  }

  if(length(formatted_values)==1)
  {
    return(formatted_values)
  }

  if(length(formatted_values)<=10)
  {
    return(paste0(
      paste0(head(formatted_values,length(formatted_values)-1), collapse = ', '),
      " and ",
      tail(formatted_values,1)
    ))
  }
  if(!is.null(complementary_values)) {
    if(length(complementary_values)==0) {
      return(paste0('all ', length(complementary_values)+length(values), ' ', plural_form))
    }
    if(length(values)/(length(values)+length(complementary_values)) > 0.8) {
      msg<-format_values_as_one_string(complementary_values, plural_form=plural_form)
      msg<-paste0('all ', length(values)+length(complementary_values),
                  ' ', plural_form, ' except for ', msg)
      return(msg)
    }
  }
  return(paste0(length(formatted_values), ' ', plural_form,': ',
                paste0(head(formatted_values, 7), collapse=', '),
                ", ...",
                paste0(tail(formatted_values,3), collapse=", ")
  ))
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
  if(is.null(attr(var, 'label', exact = TRUE)))
  {
    return(colname)
  } else {
    return(paste0(attr(var, 'label', exact = TRUE), '(', colname, ')'))
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
                 'B'='logical',
                 'D'='Date',
                 'F'='factor',
                 'I'='integer',
                 'L'='labelled',
                 'N'='numeric',
                 'S'='character',
                 'T'='POSIXct',
                 '##'
  )     )
}

VariableHasLabels<-function(var) {
  if(is.factor(var)) {
    return(TRUE)
  }
  if(!is.null(labelled::val_labels(var))) {
    return(TRUE)
  }
  if('numeric' %in% class(var)) {
    return(!is.null(attr(var, 'labels')))
  }
  return(FALSE)
}

allvartypes<-c('F','I', 'N', 'D', 'T', 'S', '0', 'B', 'L')

#Returns a letter that encodes data type.
class2vartype<-function(var)
{
  if('data.frame' %in% class(var)) {
    return(purrr::map_chr(as.list(var), class2vartype))
  } else {
    has_labels<-VariableHasLabels(var)
    class2vartype_str(class(var), all(is.na(var)), has_labels=has_labels)
  }
}

#Returns a letter that encodes data type.
class2vartype_str<-function(classes, all_is_na, has_labels=NA)
{
  is_labelled<-'labelled' %in% classes
  if(is_labelled) {
    if(!is.na(has_labels)) {
      if(!has_labels)  {
        is_labelled<-FALSE
      }
    }
  }
  classes <- setdiff(classes, 'labelled')
  classes_sorted <- paste0(sort(classes), collapse=',')

  if(classes_sorted %in% c('factor', 'factor,ordered'))
  {
    return('F')
  } else if(classes_sorted == 'integer')
  {
    return('I')
  } else if(classes_sorted == 'numeric')
  {
    if(is.na(has_labels) || !has_labels) {
      return('N')
    } else {
      return('L')
    }
  } else if(classes_sorted %in% c('Date'))
  {
    return('D')
  } else if(classes_sorted %in% c("POSIXct,POSIXt","POSIXlt,POSIXt"))
  {
    return('T')
  } else if(classes_sorted %in% c('character','character,labelled'))
  {
    return('S')
  } else if(classes_sorted %in% c('logical', 'labelled,logical'))
  {
    if(all_is_na) {
      return('0')
    } else {
      return('B')
    }
  } else if(is_labelled)
  {
    return('L')
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
                 'B'=TRUE,
                 'F'=TRUE,
                 'L'=TRUE,
                 'I'=TRUE,
                 'N'=TRUE,
                 'D'=TRUE,
                 'T'=TRUE,
                 'S'=FALSE,
                 '##'
  )     )
}

#Function compares two vectors for equality
compareNA <- function(v1,...) {
  UseMethod("compareNA", v1)
}

compareNA.numeric <- function(v1, ..., v2) {
  diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! (abs(v1 - v2) < 1E-6 & (is.na(v1) == is.na(v2)) )))
  return(diff)
}

compareNA.default <- function(v1, ..., v2) {
  diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((v1 == v2) & (is.na(v1) == is.na(v2)) )))
  return(diff)
}

compareNA.character <- function(v1, ..., v2) {
	diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((v1 == as.character(v2)) & (is.na(v1) == is.na(v2)) )))
	return(diff)
}

compareNA.factor <- function(v1, ..., v2) {
	diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((as.character(v1) == as.character(v2)) & (is.na(v1) == is.na(v2)) )))
	return(diff)
}

compareNA.Date <- function(v1, ..., v2) {
	diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((v1 == as.Date(v2, origin="1899-12-30")) & (is.na(v1) == is.na(v2)) )))
	return(diff)
}

compareNA.POSIXct <- function(v1, ..., v2) {
	diff <- as.integer(ifelse(is.na(v1) & is.na(v2), FALSE, ! ((as.Date(v1) == as.Date(v2, origin="1899-12-30")) & (is.na(v1) == is.na(v2)) )))
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
    flags<-attr(var, 'warnings_context', exact = TRUE)
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
  msg <- attr(dt[[varname]], 'warnings', exact = TRUE);
  setattr(dt[[varname]], 'warnings', c(msg, message))
  ctx <- attr(dt[[varname]], 'warnings_context', exact = TRUE)
  if(is.null(ctx)) {
    ctx <- 0
  }
  ctx <- bitwOr(ctx, pack_flags(...))
  setattr(dt[[varname]],'warnings_context',  ctx)
}

add_msg_var<-function(var, message, flag_error, flag_warning=TRUE, ...) {
  msg <- attr(var, 'warnings', exact = TRUE);
  setattr(var, 'warnings', c(msg, message))
  ctx <- attr(var, 'warnings_context', exact = TRUE)
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
  dt_dest
}

copy_var_attributes<-function(var_source, var_dest_name, dt_dest, force_include_cols=character(0)) {
  a <- attributes(var_source)
  attrnames<-setdiff(names(a),setdiff(c('class','dim', 'dimnames','names', 'levels', 'labels'), force_include_cols))
  for(aname in attrnames) {
    setattr(dt_dest[[var_dest_name]], aname, a[[aname]])
  }
  return(dt_dest)
}

copy_obj_attributes<-function(obj_source, obj_dest, force_include_cols=character(0)) {
  a <- attributes(obj_source)
  attrnames<-setdiff(names(a),setdiff(c('class','dim', 'dimnames','names', 'levels', 'labels'), force_include_cols))
  for(aname in attrnames) {
    setattr(obj_dest, aname, a[[aname]])
  }
  obj_dest
}

nice_class_names_1<-function(var, language=c('EN','PL')) {
  if(length(language)!=1) {
    stop("You must specify language")
  }
  known_classes<-c('numeric','integer','character','logical','factor',
                   'ordered','labelled','Date','POSIXlt','POSIXt', 'POSIXct', 'logical')
  if(language=='EN') {
    dic<-c('real number'='numeric',
           'integer'='integer',
           'text'='character',
           'binary'='logical',
           'nominal'='factor',
           'ordered nominal'='factor,ordered',
           #           'labelled'='labelled',
           'date'='Date',
           'time & date'='POSIXlt,POSIXt',
           'time & date UTC'='POSIXct,POSIXt',
           'logical'='logical'
    )
    prefix_unknown <- " (which is also "
    sufffix_unknown <- ")"
  } else if (language=='PL') {
    dic<-c('liczba rzeczywista'='numeric',
           'liczba całkowita'='integer',
           'tekst'='character',
           'zmienna dwupoziomowa'='logical',
           'zmienna nominalna'='factor',
           'zmienna porządkowa'='factor,ordered',
#           'zmienna z etykietami wartości'='labelled',
           'data'='Date',
           'czas ze strefą czasową'='POSIXlt,POSIXt',
           'czas UTC'='POSIXct,POSIXt'
#           'tak/nie'='logical'

    )
    prefix_unknown <- " (która dodatkowo jest "
    sufffix_unknown <- ")"
  } else if (language=='') {
    dic<-c('N'='numeric',
           'I'='integer',
           'S'='character',
           'L'='logical',
           'F'='factor',
           'O'='factor,ordered',
           #           'zmienna z etykietami wartości'='labelled',
           'D'='Date',
           'T'='POSIXlt,POSIXt',
           'T'='POSIXct,POSIXt'
#           'B'='logical'
    )
    prefix_unknown <- " ("
    sufffix_unknown <- ")"
  }
  myclasses<-class(var)
  which_known_myclasses<-which(myclasses %in% known_classes)
  known_myclasses<-myclasses[which_known_myclasses]
  class_string<-paste0(sort(myclasses), collapse = ',')
  unknown_myclasses<-myclasses[setdiff(seq_along(myclasses), which_known_myclasses)]

  fob<-GetFOB(var)
  if(fob==3) {
    out <- names(dic)[dic=='logical']
  } else {
    if(class_string %in% dic){
      out <- names(dic)[dic==class_string]
    } else if('labelled' %in% class(var)) {
      if(fob==2) {
        out <- names(dic)[dic=='factor,ordered']
      } else if (fob==1) {
        out <- names(dic)[dic=='factor']
      } else {
        out <- class_string
      }
    } else {
      out <- class_string
    }
  }

  if(length(unknown_myclasses)>0) {
    out <- paste0(out, prefix_unknown, paste0(unknown_myclasses, collapse=', '), suffix_unknown)
  }
  #cat(paste0("out: ", out))
  return(out)
}

nice_class_names<-Vectorize(nice_class_names_1, vectorize.args = 'var')

FindVariables<-function(dt, text) {
  vec1<-colnames(dt)
  vec2<-danesurowe::GetVarLabel(dt, vec1)
  vec3<-purrr::map(dt, ~danesurowe::GetLabels(.,flag_recalculate = FALSE))
  if(!'pattern' %in% class(text) ){
    text<-stringr::fixed(text)
  }
  hits1<-stringr::str_detect(string = vec1, pattern = text)
  hits2<-stringr::str_detect(string = vec2, pattern = text)
  hits3<-purrr::map_lgl(vec3, ~sum(stringr::str_detect(string = ., pattern = text))>0)

  vars<-unique(c(which(hits1), which(hits2), which(hits3)))
  setNames(vec2[vars], vec1[vars])
  #danesurowe::format_var_list(colnames = vec1[vars], longcolnames = vec2[vars])
}
