#Funkcja, która dokleja rekordy z df1 do df2. Zwraca bazę konfliktów (jeśli istnieją) i wspólną bazę danych.
df_merge<-function(df1, df2, df1_key=NULL, df2_key=NULL, flag_remove_non_existing_cases=FALSE,
                   columns_to_ignore=NULL, flag_check_consistency=TRUE) {

  diff_db <- gen_difference_df(df1=df1, df2=df2, df1_key = df1_key, df2_key = df2_key,
                               columns_to_ignore=columns_to_ignore,
                               flag_include_new_rows = TRUE,
                               flag_include_new_cols = TRUE,
                               flag_include_deleted_rows = flag_remove_non_existing_cases,
                               flag_include_deleted_cols = TRUE,
                               name_of_cases = name_of_cases
  )
  browser()

  valid_db <- dplyr::filter(diff_db$diffdb, status==0)

  dest_db <- copy(df2)

  if(nrow(valid_db)>0) {
    for(i in seq(1, nrow(valid_db))) {
    	mycolname<-valid_db$colname[[i]]

      browser()
    }
  }
  unvalid_db <-  dplyr::filter(diff_db$diffdb, status!=0)
  return(list(db = dest_db, unprocessed_rows = unvalid_db))
}

#Function, that tries to convert the variable from orig to dest as faithfully, as possible
convert_value<-function(value, orig_var, dest_var) {
  orig_type<-class2vartype(var = orig_var)
  dest_type<-class2vartype(var = dest_var)

}

#Returns the differences between factors.
best_common_type_for_factors<-function(var1, var2, flag_one_sided=FALSE) {
  #If factors are coded in the same way, it returns "F", which mean that they are binary-compatible.
  #Otherwise returns "S" that indicate, that the factor must be converted to string
  varl1<-GetLabels(var1, flag_recalculate = TRUE)
  varl2<-GetLabels(var2, flag_recalculate = TRUE)
  if(flag_one_sided) {
    if(all(varl1==varl2)) {
      return('F')
    } else {
      return('S')
    }
  } else {
    #var1 => var2

    if(varl1 %in% varl2) {
      if(all(varl1==varl2[1:length(varl1)])) {
        return('F')
      }
    }
    return('S')
  }
}

#Returns the differences between factors.
best_common_type_for_FL<-function(var1, var2, flag_one_sided=FALSE) {
  if(is.factor(var1) && is.factor(var2)) {
    return(best_common_type_for_factors(var1,var2, flag_one_sided=flag_one_sided))
  }
  #For factors to be compatible, they must encode the same levels with the same value.
  #If there is a conflict between encodings, returns "S".
  #If there are some levels missing, but with no conflicts, it returns "L"
  varl1<-GetLevels(var1, flag_recalculate = TRUE)
  varl2<-GetLevels(var2, flag_recalculate = TRUE)

  cvarl<-intersect(varl1, varl2)

  for(i in cvarl) {
    if(!flag_one_sided) {
      if(!all(sort(names(varl1)[varl1==i]) == sort(names(varl2)[varl2==i]) )) {
        return('S')
      }
    } else {
      if(!all(sort(names(varl1)[varl1==i]) %in% sort(names(varl2)[varl2==i]) ))  {
        return('S')
      }
    }
  }
  return('L')
}

#Function finds best common type to represent values of the two variables.
#If flag_one_sided, assumes the var1 will be converted to var2, not vice versa
best_common_type<-function(var1, var2, flag_one_sided=FALSE) {
  var1t<-class2vartype(var1)
  var2t<-class2vartype(var2)
  classes_sorted <- paste0(sort(
    c(
      var1t,
      var2t
    )),collapse='|')
  if(var1t=='0') {
    return(var2t)
  }
  if(var2t=='0') {
    return(var1t)
  }

  #If types match, then return any, but only if not Factor. For factor, the level must also match.
  if(var1t==var2t){
    if(var1t %in% c('F','L')) {
      return(best_common_type_for_FL(var1 = var1, var2 = var2, flag_one_sided = flag_one_sided))
    }
    return(var1t)
  }

  #'B'='logical',
  #'D'='Date',
  #'F'='factor',
  #'I'='integer',
  #'L'='labelled',
  #'N'='numeric',
  #'S'='character',
  #'T'='POSIXct',

  #Dates
  if(classes_sorted %in% c('B|D', 'D|F', 'D|L', 'D|S',
                           'B|T', 'F|T', 'L|T', 'S|T'))  {
    return('S')
  } else if (classes_sorted == 'D|I') {
    return('D')
  } else if (classes_sorted %in% c('D|N', 'D|T', 'N|T')) {
    return('T')
  }

  #Factors and labels
  if(classes_sorted %in% c('B|F', 'F|N', 'F|S', 'B|L', 'L|S')) {
    return('S')
  } else if (classes_sorted == 'F|L') {
    return(best_common_type_for_FL(var1 = var1, var2 = var2, flag_one_sided = flag_one_sided))
  } else if (classes_sorted == 'F|I') {
    if(flag_one_sided) {
      if(var1t=='I') {
        uv<-unique(var1)
        lv<-GetLevels(var2, flag_recalculate = TRUE)
        if(uv %in% lv) {
          return('F')
        } else {
          return('S')
        }
      } else {
        return('I')
      }
    } else {
      if(var1t=='I'){
        uv<-unique(var1)
        lv<-GetLevels(var2, flag_recalculate = TRUE)
      } else {
        uv<-unique(var2)
        lv<-GetLevels(var1, flag_recalculate = TRUE)
      }
      if(all(sort(uv)==sort(lv))) {
        return('F')
      } else {
        return('S')
      }
    }
  } else if (classes_sorted %in% c('L|I', 'L|N')) {
    if(flag_one_sided) {
      if(var1t=='L') {
        return('N')
      } else {
        uv<-unique(var1)
        lv<-GetLevels(var2, flag_recalculate = TRUE)
        if(uv %in% lv) {
          return('L')
        } else {
          return('N')
        }
      }
    } else {
      if(var1t=='L'){
        uv<-unique(var2)
        lv<-GetLevels(var1, flag_recalculate = TRUE)
      } else {
        uv<-unique(var1)
        lv<-GetLevels(var2, flag_recalculate = TRUE)
      }
      if(all(sort(uv)==sort(lv))) {
        return('L')
      } else {
        return('N')
      }
    }
  }

  #Booleans
  if(classes_sorted == 'B|I') {
    return('I')
  } else if (classes_sorted == 'B|N') {
    return('N')
  } else if (classes_sorted == 'B|S') {
    return('S')
  }

  #Integer
  if(classes_sorted == 'I|N') {
		return('N')
  } else if (classes_sorted %in% c('I|S', 'N|S')) {
  	return('S')
  }
}

#Converts value(s) from the variable var into type type.
#If we need to convert into factor, then we may pass the already-existing factor varaible in the dest_factor_stencil argument
convert_type<-function(type, var, value=NULL, dest_factor_stencil=NULL) {

	vartype <- class2vartype(var)

	if(is.null(value)) {
		value<-var
	}

	#'B'='logical',
	#'D'='Date',
	#'F'='factor',
	#'I'='integer',
	#'L'='labelled',
	#'N'='numeric',
	#'S'='character',
	#'T'='POSIXct',

	#Konwertujemy na stringi
	if(type=='S') {
		return(as.character(value))
	}

	#Konwertujemy na numeric
	if(type=='N') {
		if(vartype %in% c('B', 'F', 'L', 'I', 'N')) {
			return(as.numeric(value))
		} else if (vartype %in% c('D')) {
			return(as.numeric(value)+25569)
		} else if (vartype %in% c('T')) {
		  return(as.numeric(value)/24/3600 + 25569)
		} else if (vartype == 'S') {
		  return(suppressWarnings(as.numeric(value)))
		}
	}

	#Konwertujemy na integer
	if(type=='I') {
	  if(vartype %in% c('B', 'F', 'I', 'N')) {
	    return(as.integer(value))
	  } else if (vartype %in% c('D')) {
	    return(as.integer(value)+25569)
	  } else if (vartype %in% c('T')) {
	    return(as.integer(as.numeric(value)/24/3600 + 25569))
	  } else if (vartype %in% c('S', 'L')) {
	    return(suppressWarnings(as.integer(value)))
	  }
	}

	#Konwertujemy na boolean
	if(type=='B') {
	  return(suppressWarnings(as.logical(value)))
	}

	#Konwertujemy na Date
	if(type=='D') {
		if(vartype %in% c('B')) {
			return(rep(NA, length(value)))
		} else if (vartype %in% c('D', 'T')) {
			return(as.Date(value))
		} else if (vartype %in% c('I', 'N')) {
			return(as.Date(value, origin="1899-12-30"))
		} else if (vartype %in% c('S','F','L')) {
			return(lubridate::as_date(as.character(value)))
		}
	}
	if(type=='T') {
		if(vartype %in% c('B')) {
			return(rep(NA, length(value)))
		} else if (vartype %in% c('D', 'T')) {
			return(as.POSIXct(value))
		} else if (vartype %in% c('I', 'N')) {
			return(as.POSIXct(value * (60*60*24), origin="1899-12-30", tz="GMT"))
		} else if (vartype %in% c('S','F','L')) {
			return(lubridate::as_date(as.character(value)))
		}
	}

	#Konwersję na factor dzielimy na 2 rodzaje: albo jako przez string, albo przez value.
	if(type== 'F') {
		if(vartype %in% c('S', 'B', 'D', 'T', 'F', 'L') || (vartype == 'N' && !all(value == as.integer(value))) ){
			if(is.null(dest_factor_stencil)) {
				return(as.factor(as.character(value)))
			}
			mylevels<-GetLevels(dest_factor_stencil)
			new_values<-sort(unique(as.character(value)))
			values_to_add<-setdiff(new_values, names(mylevels))
			values_to_add<-setNames(seq_along(values_to_add)+length(mylevels),values_to_add)
			mylevels<-c(mylevels, values_to_add)
			return(factor(x=as.character(value), levels=names(mylevels)))
		} else if (vartype %in% c('I', 'N')) {
			if(is.null(dest_factor_stencil)) {
				return(as.factor(as.character(value)))
			}
			mylevels<-GetLevels(dest_factor_stencil)
			new_values<-sort(unique(as.integer(value)))
			values_to_add<-setdiff(new_values, mylevels)
			values_to_add<-setNames(seq_along(values_to_add)+length(mylevels),as.character(values_to_add))
			mylevels<-c(mylevels, values_to_add)
			return(factor(x=as.character(value), levels=names(mylevels)))
		}
	}

	#Labelled wkładamy value albo przez string.
	if(type== 'L') {
		if(vartype %in% c('S', 'B', 'D', 'T') ){
			if(is.null(dest_factor_stencil)) {
				return(haven::labelled(value,  as.character(value)))
			}
			mylevels<-GetLevels(dest_factor_stencil)
			new_values<-sort(unique(as.character(value)))
			values_to_add<-setdiff(new_values, names(mylevels))
			values_to_add<-setNames(seq_along(values_to_add)+length(mylevels),values_to_add)
			mylevels<-c(mylevels, values_to_add)
			return(factor(x=as.character(value), levels=names(mylevels)))
		} else if (vartype %in% c('I', 'N')) {
			if(is.null(dest_factor_stencil)) {
				return(as.factor(as.character(value)))
			}
			mylevels<-GetLevels(dest_factor_stencil)
			new_values<-sort(unique(as.integer(value)))
			values_to_add<-setdiff(new_values, mylevels)
			values_to_add<-setNames(seq_along(values_to_add)+length(mylevels),as.character(values_to_add))
			mylevels<-c(mylevels, values_to_add)
			return(factor(x=as.character(value), levels=names(mylevels)))
		}
	}

}
