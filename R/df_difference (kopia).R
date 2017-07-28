#lab_var<-labelled::labelled(sample(c(1:10, haven::tagged_na('a'), haven::tagged_na('x'))), setNames(c(1:10, haven::tagged_na('a'),haven::tagged_na('x')) ,c(LETTERS[1:10],"NA","NX")))
#

#This function is focused on edits on the dataframe.
#It concerns only the columns that are the same on both datasets, but reports columns that have different names.
#It tries to group edits by cases; it encodes creates "changed" vs "not-changed" boolean matrix n_row×n_cols. It sorts it by rows and columns.
#It tries to produce report that is the most concise using either row or column approach.

#df1_key<-'q_0'
#df2_key<-NULL
#df2_all<-danesurowe::readDaneSurowe4('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/data/dane surowe2d.xlsm', flag_keep_tagged_na = TRUE)
#df2<-df2_all$dt
#df1<-danesurowe::readDaneSurowe3('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/data_review/corrections/als (2)_Lisbon reviewed_marta_1march.xlsm')
#columns_to_ignore=NULL
#
#Function that returns diff between the two databases.
#The diff contains all the informations, that are needed to transform one database into the other.
#
#The function assumes matching column names and row names (keys). If you don't have a key column - make one.
#
#The output is a list:
#  - diff_cells_dt - a data.table, where each row reporesents one mismatched cell in both tables. It includes missing columns and missing rows on both datasets.
#  - diff_metadata_dt - a data.table, where each element is a database' property that mismatches, such as data type or label.
#  - diff_levels - a list, with an entry for each mismatched labels information. Entry consists of a diff_db.
df_difference<-function(df1, df2, df1_key=NULL, df2_key=NULL, columns_to_ignore=NULL)
{
  if (!'data.frame' %in% class(df1))  {
    ans<-list(message='First argument is not a data.frame!', result=FALSE)
    return(ans)
  }

  if (!'data.frame' %in% class(df2))  {
    ans<-list(message='Second argument is not a data.frame!', result=FALSE)
    return(ans)
  }

  # Algorithm:
  # 1. Get common set of column names and assume that column that match by name must be equivalent

  df1_names<-colnames(df1)
  df2_names<-colnames(df2)

  common_colnames<-intersect(df1_names,df2_names)
  df1specific_colnames <- diff(df1_names, common_colnames)
  df2specific_colnames <- diff(df2_names, common_colnames)


  if(length(common_colnames)==0) {
  	stop("No common columns (variables) between datasets")
  } else if (length(common_colnames)<5 && ncol(df1) >5 && ncol(df2) > 5 ) {
  	warning(paste0("Only ", length(common_colnames), " common col names between sets"))
  }


  if(!is.null(columns_to_ignore))  {
    common_colnames <- setdiff(common_colnames,columns_to_ignore)
  }

  # 2. Get common set of rownames

  if(!is.null(df1_key))  {
    if (df1_key %in% colnames(df1))    {
      df1_rownames <- df1[[df1_key]]
    } else {
      stop(paste0("There is no column '", df1_key, "' in the first data frame"))
    }
  } else {
    if(all(rownames(df1) == seq_along(rownames(df1))))    {
      if (nrow(df1) != nrow(df2))      {
        stop("Number of rows in datasets differ, and there is no key variable specified for the first data frame.")
      } else {
        df1_rownames<-seq(nrow(df1))
      }
    } else {
      df1_rownames <- rownames(df1)
    }
  }


  if(!is.null(df2_key))  {
    if (df2_key  %in% colnames(df2) )    {
      df2_rownames <- df2[[df2_key]]
    } else {
      stop(paste0("There is no column '", df2_key, "' in the first data frame"))
    }
  } else {
    if( all(rownames(df2) == seq_along(rownames(df2))))    {
      if (nrow(df1) != nrow(df2))      {
        stop("Number of rows in datasets differ, and there is no key variable specified for the second data frame")
      } else {
        df2_rownames<-seq(nrow(df2))
      }
    } else {
      df2_rownames <- rownames(df2)
    }
  }

  dupl<-duplicated(na.omit(df1_rownames))
  if(sum(dupl)>0)  {
    stop(paste0("First database has ", sum(dupl), " non-unique row names: ", format_case_list(na.omit(df1_rownames)[dupl])))
  }

  dupl<-duplicated(na.omit(df2_rownames))
  if(sum(dupl)>0)  {
    stop(paste0("Second database has ", sum(dupl), " non-unique row names: ", format_case_list(na.omit(df2_rownames)[dupl])))
  }

  common_rownames <- sort(intersect(df1_rownames, df2_rownames))
  if(length(common_rownames)==0) {
  	stop("No common rows (cases) between data sets")
  } else if (length(common_rownames)<5 && nrow(df1) >5 && nrow(df2) > 5 ) {
  	warning(paste0("Only ", length(common_rownames), " common row between data sets"))
  }
  df1specific_rownames <- diff(df1_rownames, common_rownames)
  df2specific_rownames <- diff(df2_rownames, common_rownames)

  df1_selected_rows <- match(common_rownames, df1_rownames)
  df2_selected_rows <- match(common_rownames, df2_rownames)

  df1_sorted <- df1[df1_selected_rows]
  df2_sorted <- df2[df2_selected_rows]

  # 3. Do matching among rows using either keys or rowname matching.
  diff_matrix<-DiffMatrix(df1_sorted, df2_sorted, common_colnames)

  diffdb<-create_diffdb(diff_matrix = diff_matrix,
                        diff_rownames = common_rownames,
                        diff_colnames = common_colnames,
                        df1_rownames = df1_rownames,
                        df2_rownames = df2_rownames,
  											df1=df1,
  											df2=df2
                        )


  diffdb <- update_diffdt(diffdb = diffdb, df1 = df1, df2 = df2)

  return(list(diffdb=diffdb, df1=df1, df2=df2, df1keys=df1_rownames, df2keys=df2_rownames,
              df1specific_colnames=df1specific_colnames, df2specific_colnames=df2specific_colnames,
              df1specific_rownames=df1specific_rownames, df2specific_rownames=df2specific_rownames
              ))
}


# We assume that df1_sorted and df2_sorted are already sorted according to the same key,
# and their rows correspond to each other (1:1)
DiffMatrix<-function(df1_sorted, df2_sorted, col_names){
  # 3. Do matching among rows using either keys or rowname matching.

  diffdb<-numeric(0)
  for(colname in col_names)  {
    #    cat(paste0(colname,'\n'))
    #if(colname=='q_110c') browser()
    var1<-df1_sorted[[colname]]
    var2<-df2_sorted[[colname]]
    ans <- compare_two_variables(var1, var2)
    compvar <- ans$compvar
    var1 <- ans$var1
    var2 <- ans$var2

    if(sum(is.na(compvar)>0))
      browser()
    if(length(diffdb)==0)  {
      diffdb <- data.table(var=compvar)
      data.table::setnames(diffdb, 'var', colname)
      dt_out1 <- data.table(var=var1)
      data.table::setnames(dt_out1, 'var', colname)
      dt_out2 <- data.table(var=var2)
      data.table::setnames(dt_out2, 'var', colname)
    } else {
    	set(diffdb, i=NULL, j=colname, value=compvar)
      set(dt_out1, i=NULL, j=colname, value=var1)
      set(dt_out2, i=NULL, j=colname, value=var2)
    }

  }

  diff_matrix=data.matrix(diffdb)
  return(diff_matrix)
}


compare_two_variables<-function(var1, var2)
{
  if (length(setdiff(class(var1), class(var2)))>0)  {
    classes_sorted <- paste0(sort(
      c(
        paste0(sort(class(var1)), collapse=','),
        paste0(sort(class(var2)), collapse=',')
      )),collapse='|')
    if(classes_sorted == 'factor|labelled')  {
      if (class(var1)=='factor')        {
        var1 <- labelled::to_labelled(var1)
      } else {
        var2 <- labelled::to_labelled(var2)
      }
    } else if (classes_sorted %in% c('Date|numeric', 'numeric|POSIXct,POSIXt'))   {
      if(class(var1)=='Date' || 'POSIXct' %in% class(var1)) {
        var1 <- as.numeric(as.Date(var1) - as.Date(0, origin="1899-12-30", tz='UTC'))
      } else {
        var2 <- as.numeric(as.Date(var2) - as.Date(0, origin="1899-12-30", tz='UTC'))
      }
    } else if (classes_sorted=='character|numeric') {
      if(class(var1)=='character') {
        var1 <- suppressWarnings(as.numeric(var1))
      } else {
        var2 <- suppressWarnings(as.numeric(var2))
      }
    } else if (classes_sorted=='integer|numeric')   {
      if(class(var1)=='integer') {
        var1 <- suppressWarnings(as.numeric(var1))
      } else {
        var2 <- suppressWarnings(as.numeric(var2))
      }
    } else if (classes_sorted=='integer|logical' )  {
    	if(class(var1)=='logical') {
    		if(sum(!is.na(var1))==0)  {
    			browser()
    		} else {
    			var1<-suppressWarnings(as.integer(var1))
    		}
    	} else {
    		if(sum(!is.na(var2))==0)
    		{
    			browser()
    		} else {
    			var2<-suppressWarnings(as.integer(var2))
    		}
    	}
    } else if (classes_sorted=='factor|logical' )  {
    	if(class(var1)=='logical') {
    		if(sum(!is.na(var1))==0)  {
    			browser()
    		}
    	} else {
    		if(sum(!is.na(var2))==0)
    		{
    			browser()
    		}
    	}
    } else if (classes_sorted=='labelled|numeric')  {
      if(class(var1)=='numeric') {
        labels1 <- na.omit(unique(var1))
        if(length(labels1)==0)  {
          labels1=c('0')
        }
        names(labels1) <- labels1
        var1 <- labelled::labelled(var1, labels1)
      } else {
        labels2 <- na.omit(unique(var2))
        if(length(labels2)==0)  {
          labels2=c('0')
        }
        names(labels2) <- labels2
        var2 <- labelled::labelled(var2, labels2)
      }
    } else if (classes_sorted=='factor|numeric')  {
      if(class(var1)=='factor') {
        var1<-suppressWarnings(as.numeric(var1))
      } else {
        var2<-suppressWarnings(as.numeric(var2))
      }
    } else if (classes_sorted=='character|logical')  {
      if(class(var1)=='logical') {
        if(sum(!is.na(var1))==0)  {
          var1<-as.character(var1)
        } else {
          browser()
        }
      } else {
        var2<-as.character(var2)
      }
    } else if (classes_sorted=='logical|numeric')  {
    	if(class(var1)=='logical') {
    		if(sum(!is.na(var1))==0)  {
    			browser()
    		} else {
    			var1<-suppressWarnings(as.numeric(var1))
    		}
    	} else {
    		if(sum(!is.na(var2))==0)
    		{
    			browser()
    		} else {
    			var2<-suppressWarnings(as.numeric(var2))
    		}
    	}
    } else if (classes_sorted=='character|factor')  {
    	if(class(var2)=='factor') {
  			var2<-suppressWarnings(as.character(var2))
    	} else {
  			var1<-suppressWarnings(as.character(var1))
    	}
    } else {
      browser()
      stop(paste0("I don't know how to compare between classes ", classes_sorted))
    }
  } else {
  	if('factor' %in% class(var1)) {
  		if (length(danesurowe::GetLevels(var1))==length(danesurowe::GetLevels(var2))) {
  			if (!all(danesurowe::GetLevels(var1)==danesurowe::GetLevels(var2)) || all(danesurowe::GetLabels(var1)==danesurowe::GetLabels(var2))) {
  				var1 <- as.character(var1)
  				var2 <- as.character(var2)
  			}
  		} else  {
  			var1 <- as.character(var1)
  			var2 <- as.character(var2)
  		}
  	}
  }
   #if(colname=='q_10b') browser()

  return(list(compvar=danesurowe:::compareNA(var1,var2),
              var1=var1,
              var2=var2))
}

#Converts a value var[[row]] into string. All NAs gets translated into NA. Returns list (value, error_code)

convert_to_string<-function(var, row)
{
	if(is.null(var)){
		browser()
	}
  vartype<-class2vartype(var)
  val<-var[[row]]
  if (vartype == 'L') {
    if (is.na(val))  {
      val_out <- NA
    } else {
      val_out <- GetLabels(var)[match(val, GetLevels(var))]
    }
  } else if (vartype %in% c('I', 'N', 'D', 'S', 'F', '0'))  {
    val_out <- as.character(val)
  } else  {
    stop(paste0("Unknown type ", vartype))
  }
  return(val_out)
}

#Converts NA (tagged) value var[[row]] into string.
#It returns a list(value, warning)
#If not NA, returns NULL, otherwise string or NA if it is real NA.
#For numeric, the tagged NA is returned as a tag string,
#For labelled, the label associated with the tag is returned instead, or NA if string is not found.
convert_NA_to_string.statuses<-list(OK=0, NOT_NA=1, TAGGED_NA_IN_NUMERIC=2, UNNAMED_TAGGED_NA=3 )
convert_NA_to_string<-function(var, row)
{
  vartype <- class2vartype(var)
  val <- var[[row]]

  if (!is.na(val))  {
    return(list(
      value=NULL,
      status=convert_NA_to_string.statuses$NOT_NA
      ))
  }

  if (vartype == 'N')  {
    if (haven::is_tagged_na(val))  {
      status <- convert_NA_to_string.statuses$TAGGED_NA_IN_NUMERIC
    } else {
      status <- convert_NA_to_string.statuses$OK
    }
    val_out <- NA
  } else if (vartype == 'L')  {
    if (haven::is_tagged_na(val))
    {
      tag<-haven::na_tag(val)
      val_out <-GetLabelsNA(var)[match(tag, GetLabelsNA(var))]
      if(is.na(val_out))  {
        status <- convert_NA_to_string.statuses$UNNAMED_TAGGED_NA
        val_out <- tag
      } else {
        status <- convert_NA_to_string.statuses$OK
      }
    } else {
      val_out <- NA
      status <- convert_NA_to_string.statuses$OK
    }
  } else {
    val_out <- NA
    status <- convert_NA_to_string.statuses$OK
  }
  return(list(
    value=val_out,
    status=status
  ))
}

#Function that converts string into variable's class, so it can be easily pasted.
#
#Returns list:
#list(value, status)
convert_str_to_var.statuses<-list(OK=0, LEVEL_NOT_EXISTING=1, STRING_IS_NOT_A_NUMBER=2, NOT_WHOLE_NUMBER=3, NOT_A_DATE=4)
convert_str_to_var<-function(strval, destvar)
{
  destvartype <- class2vartype(destvar)
  if (destvartype %in% c('F', 'L'))  {
    labels<-GetLabels(destvar)
    pos<-match(strval, labels)
    if (is.na(pos)) {
      status <- convert_str_to_var.statuses$LEVEL_NOT_EXISTING
      value <- NULL
    } else {
      status <- convert_str_to_var.statuses$OK
      value <- GetLevels(destvar)[[pos]]
    }
  } else if (destvartype == 'S') {
    status <- convert_str_to_var.statuses$OK
    value <- strval
  } else if (destvartype %in% c('I', 'N'))  {
    val<-suppressWarnings(as.numeric(strval))

    if(is.na(val))  {
      status <- convert_str_to_var.statuses$STRING_IS_NOT_A_NUMBER
      value <- NULL
    } else {
      if (destvartype == 'N')  {
        status <- convert_str_to_var.statuses$OK
        value <- val
      } else {
        value <- as.integer(val)
        if (all.equal(val, value))  {
          status <- convert_str_to_var.statuses$OK
        } else {
          status <- convert_str_to_var.statuses$NOT_WHOLE_NUMBER
        }
      }
    }
  } else if (destvartype == 'D') {
    value <- try(as.Date(strval), silent = TRUE)
    if (class(value) == 'try-error') {
      status <- convert_str_to_var.statuses$NOT_A_DATE
      value <- NULL
    } else {
      status <- convert_str_to_var.statuses$OK
    }
  } else if (destvartype == 'S') {
  	status <- convert_str_to_var.statuses$OK
  	value <- strval
  } else if (destvartype == '0') {
  	if(all(is.na(destvar))) {
  		status <- convert_str_to_var.statuses$OK
  		value <- strval
  	} else {
  		browser()
  	}
  } else {
    stop(paste0('Unexpected variable type ', destvartype))
  }
  return(list(value=value, status=status))
}

convert_num_to_var.statuses<-list(OK=0, LEVEL_NOT_EXISTING=1, STRING_IS_NOT_A_NUMBER=2, NOT_WHOLE_NUMBER=3, NOT_A_DATE=4)
convert_num_to_var<-function(numval, destvar)
{
  destvartype <- class2vartype(destvar)
  if (destvartype %in% c('F', 'L'))  {
    levels<-GetLevels(destvar)
    pos<-which.max(Vectorize(all.equal)(levels, numval)==TRUE)
#    pos<-match(strval, labels)
    if (is.na(pos)) {
      status <- convert_var_to_var.status$LEVEL_NOT_EXISTING
      value <- NULL
    } else {
      status <- convert_var_to_var.status$OK
      if (destvartype == 'F') {
        value = as.integer(numval)
      } else {
        value = numval
      }
    }
  } else if (destvartype == 'S') {
    status <- convert_var_to_var.status$OK
    value <- as.character(numval)
  } else if (destvartype == 'N')  {
    value<-numval
    status <- convert_var_to_var.status$OK
  } else if (destvartype == 'I')  {
    if (all.equal(numval, as.integer(numval))==TRUE)
    {
      value <- as.integer(numval)
      status <- convert_var_to_var.status$OK
    } else {
      value <- NA
      status <- convert_var_to_var.status$NOT_WHOLE_NUMBER
    }
  } else if (destvartype=='D') {
    ans<-try(as.Date(numval, origin = "1899-12-30"), silent = TRUE)
    if (class(ans)=='try-error')  {
      value<-NULL
      status<-convert_var_to_var.status$NOT_A_DATE
    } else {
      value<-ans
      status<-convert_var_to_var.status$OK
    }
  } else {
    stop(paste0('Unexpected variable type ', destvartype))
  }
  return(list(value=value, status=status))
}

convert_var_to_var.status=list(OK=0,
                               LEVEL_NOT_EXISTING=1,
                               STRING_IS_NOT_A_NUMBER=2,
                               NOT_WHOLE_NUMBER=3,
                               NOT_A_DATE=4,
                               LOST_TAGGED_NA=5,
                               TAGGED_NA_ON_NUMERIC=6
                               )
convert_var_to_var<-function(srcvar, srcrow, destvar){
#  if (srcrow==318) browser()
  srcval <- srcvar[[srcrow]]
  srctype <- class2vartype(srcvar)
  desttype <- class2vartype(destvar)
  srcdesttype <- paste0(srctype, desttype)
  if (!is.na(srcval))  {
    if(srcdesttype %in% c('ND', 'ID'))  {
      ans<-convert_num_to_var(numval = srcval, destvar = destvar)
      value<-ans$value
      status<-ans$status
    } else if (srcdesttype %in% c('DI','DN') ) {
      value <- as.numeric(as.Date(srcval) -as.Date(0, origin="1899-12-30", tz='UTC'))
      if (desttype == 'I') {
        value <- as.integer(value)
      }
      status<-convert_var_to_var.status$OK
    } else if (srctype == desttype && ! desttype %in% c('F', 'L') ) {
      #input type == outpput type, and they are not factors
      value <- srcval
      status = convert_var_to_var.status$OK
    } else if (srctype %in% c('I', 'N')) {
      ans <-convert_num_to_var(numval=srcval, destvar = destvar)
      value<-ans$value
      status<-ans$status
    }
    else if (srcdesttype %in% c('N', 'I') ){
      value <- as.integer(srcval)
      if (all.equal(srcval, value))  {
        status <- convert_var_to_var.status$OK
      } else {
        status <- convert_var_to_var.status$NOT_WHOLE_NUMBER
      }
    }  else {
      #All the other cases are first converted to string, and then to the target var.
      str_srcval <- convert_to_string(srcvar, row=srcrow)
      outval <- convert_str_to_var(strval = str_srcval, destvar = destvar)
      value = outval$value
      status = outval$status
    }
  } else {
    if (srctype %in% c('F', 'I', 'D', 'S'))  {
      value <- NA
      status <- convert_var_to_var.status$OK
    } else {
      if (!haven::is_tagged_na(srcval))   {
        value <- NA
        status <- convert_var_to_var.status$OK
      } else {
        #We have source with tagged NA.

        if (desttype %in% c('F', 'I', 'S', 'D'))  {
          value <- NA
          status <- convert_var_to_var.status$LOST_TAGGED_NA
        } else {
          #We also have destination that supports tagged NA. We have 4 cases:
          srctag<-haven::na_tag(srcval)
          if (srcdesttype == 'NN')  {
            value <- srcval
            status <- convert_var_to_var.status$OK
          } else if (srcdesttype == 'LN')  {
            statis <- convert_var_to_var.status$TAGGED_NA_ON_NUMERIC
            value <- srcval
          } else if (srcdesttype == 'LL')   {
            browser()
            #TODO:
            # 1. Znajdź etykietę tagu w zmiennej źródłowej
            # 2. Znajdź tą samą etykietę tagu w zmiennej docelowej
            # 3. Zwróć level tego tagu dla zmiennej docelowej
            strLabels <- GetNALevels(srcvar)
            if (srctag %in% strLabels)  {
              pos <- match(srctag, strLabels)
              value <- NA #TDO
            }
          } else if (srcdesttype == 'NL')  {
            browser()
            #TODO
            # 2. Znajdź tag wśród wartości zmiennej docelowej -
            #    jeśli znalazłeś - ok,
            #    jeśli nie znalazłeś, to zwróć ostrzeżenie
          }
        }
        if (desttype %in% c('L', 'N')) {
          value <- NA
          status <- convert_var_to_var.status$TAGGED_NA_IN_NUMERIC
        } else {
          browser()
        }
      }
    }
  }
  return(list(value=value, status=status))
}

update_diffdt<-function(diffdb, df1, df2)
{
  if(!'value_int' %in% colnames(diffdb))
  {
  	set(diffdb, i=NULL, j='value_int', value=integer(0))
  	set(diffdb, i=NULL, j='value_num', value=numeric(0))
  	set(diffdb, i=NULL, j='value_char', value=character(0))
  	set(diffdb, i=NULL, j='status', value=integer(0))
#  	diffdb[,value_int:=integer(0)]
#    diffdb[,value_num:=numeric(0)]
#    diffdb[,value_char:=character(0)]
#    diffdb[,status:=integer(0)]
  }

	if(nrow(diffdb)>0) {
		for (i in seq(nrow(diffdb)))
		{
			status <- diffdb[['status']][[i]]
			if (is.na(status))
			{
				update_diffdt_row(diffrow = i, df1 = df1, df2 = df2, diffdb = diffdb)
			}
		}
	}
  return(diffdb)
}

comment_diffs<-function(diff_list, flag_comment_replace_NA=FALSE)
{
	diffdb<-diff_list$diffdb
	df1<-diff_list$df1
	df2<-diff_list$df2
	df1_key<-diff_list$df1keys
	df2_key<-diff_list$df2keys
	df1specific_colnames <- diff_list$df1specific_colnames
	df2specific_colnames <- diff_list$df2specific_colnames
	df1specific_rownames <- diff_list$df1specific_rownames
	df2specific_rownames <- diff_list$df2specific_rownames

	if(length(df1specific_colnames)>0 && length(df2specific_colnames)>0 ) {
		msg <- "Warning: both datasets have unmatched columns."
	} else {
		msg <- "All column names are matched between the data sets."
	}
	if(length(df1specific_colnames)>0 ) {
		msg[[length(msg)+1]] <- paste0("Conversion will add the following ",
																	 length(df1specific_colnames),
																	 " columns: ",
																	 format_var_list(colnames = df1specific_colnames, dt = df1,
																	 								flag_include_secondary_name = TRUE), '. ')
	}

	if(length(df2specific_colnames)>0 ) {
		msg[[length(msg)+1]] <- paste0("The following ", length(df2specific_colnames),
																	 " columns were ignored: ",
																	 format_var_list(colnames = df2specific_colnames, dt = df2,
																	 								flag_include_secondary_name = TRUE), '. ')
	}



	if(length(df1specific_rownames)>0 && length(df2specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("Warning: both datasets have unmatched cases.")
	} else {
		msg[[length(msg)+1]] <- paste0("All cases are matched between the data sets. ")
	}
	if(length(df1specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("Conversion will add the following ",
																	 length(df1specific_colnames),
																	 " cases: ",
																	 format_case_list(case_names = df1specific_rownames), '. ')
	}

	if(length(df2specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("The following ", length(df2specific_rownames),
																	 " cases were ignored: ",
																	 format_case_list(case_names = df2specific_rownames), '. ')
	}


	if(flag_comment_replace_NA) {
		db <- diffdb %>% filter(!(is.na(df2_value) & is.na(df2_label))) %>% data.table()
	} else {
		db <- copy(diffdb) %>% data.table()
	}
	out <- list()



	while(nrow(db)) {
		rownames<-rev(sort(table(db$rowname)))
		colnames<-rev(sort(table(db$colname)))

		if(rownames[[1]]>colnames[[1]]) {
			rowname_in <- names(rownames)[[1]]
			tmpdb <- db[rowname==rowname_in, ]
			setkey(tmpdb, colname)

			outvec <- rep('', nrow(tmpdb))
			for (i in seq(nrow(tmpdb)))
			{
				msg <- comment_one_diff_grp(i, diffdb = tmpdb, df1 = df1, df1keys = df1keys, df2 = df2, df2keys = df2keys,  flag_say_colname = TRUE)
				outvec[[i]] <- msg
			}
			df1_rownr <- match(rowname_in, df1keys)
			df2_rownr <- match(rowname_in, df2keys)
			outrec <- list(msg = paste0(
				'Changes to the database record ', rowname_in,
				' (that corresponds to source row nr ', df1_rownr, ' and destination row nr ', df2_rownr, '): \n',
				paste0(outvec, collapse = ',\n')))
			db <- db[rowname != rowname_in,]

		} else {
			colname_in <- names(colnames)[[1]]
			tmpdb <- db[colname==colname_in, ]
			setkey(tmpdb, rowname)

			if (is.null(attr(df2[[colname_in]], 'label'))){
				varlabel <- colname_in
			} else {
				varlabel <- paste0(attr(df2[[colname_in]], 'label'), ' (', colname_in, ')')
			}

			outvec <- rep('', nrow(tmpdb))
			for (i in seq(nrow(tmpdb)))
			{
				msg <- comment_one_diff_grp(i, diffdb = tmpdb, df1 = df1, df1keys = df1keys, df2 = df2, df2keys = df2keys, flag_say_rowname =  TRUE)
				outvec[[i]] <- msg
			}
			outrec <- list(msg = paste0(
				'Changes to the variable  ', varlabel, ': \n',
				paste0(outvec,  collapse = ',\n')))
			db <- db[colname != colname_in,]
		}
		out <- list(out, outrec)
	}
	return(unlist(c(msg,out)))
}

comment_one_diff_row<-function(diffdb, df1, df1keys, df2, df2keys, rownames,
															 flag_say_colname = FALSE, flag_say_rowname = FALSE)
{
	rowname_in <- names(rownames)[[1]]
	tmpdb <- db[rowname==rowname_in, ]
	setkey(tmpdb, colname)

	outvec <- rep('', nrow(tmpdb))
	for (i in seq(nrow(tmpdb)))
	{
		msg <- comment_one_diff(rownr=1, diffdb=tmpdb,
														df1=df1, df1keys=df1keys, df2=df2, df2keys=df2keys)
		outvec[[i]] <- msg
	}

	browser()
	msgs_counts<-data.frame(table(outvec))




	return(msg)
}


decorate_comment_msg<-function(msg, df1, df2, colname=NULL,
															 rowname=NULL, df1rownr=NULL, df2rownr=NULL,
															 decorate_options = list(flag_include_secondary_name=TRUE)) {
	if (!is.null(colname)) {
		varlabel <- do.call(format_var_list, c(decorate_options,
																					 flag_include_secondary_name=TRUE,
																					 dt=df1))
		msgA_ <- paste0(varlabel, ' ')
		msg_A <- paste0(' ', varlabel)
		msg_B <- paste0(' for ', varlabel)
	} else {
		msgA_ <- ''
		msg_A <- ''
		msg_B <- ''
	}
	msg <- stringr::str_replace(msg, stringr::fixed("#A_#"), msgA_)
	msg <- stringr::str_replace(msg, stringr::fixed("#_A#"), msg_A)
	msg <- stringr::str_replace(msg, stringr::fixed("#_B#"), msg_B)

	if (!is.null(rowname)) {
		rowlabel<-paste0(rowname, ' (that corresponds to source row nr ',
										 df1rownr, ' and destination row nr ', df2rownr, ')')
#		rowlabel<-paste0(rowname, ' (that corresponds to source row nr ',
#										 match(rowname, df1keys), ' and destination row nr ',
#										 match(rowname, df2keys), ')')

		msg_2 <- paste0(' for ', rowlabel)

	} else {
		msg_2 <- ''
	}
	msg <- stringr::str_replace(msg, stringr::fixed("#_2#"), msg_2)

	return(msg)
}

comment_one_diff<-function(rownr, diffdb, df1, df1keys, df2, df2keys)
{
  var2_type <- diffdb[rownr, df2_data_type]
  value_var <- switch(var2_type,
                      F='value_int',
                      L='value_num',
                      I='value_int',
                      N='value_num',
                      D='value_num',
                      S='value_char',
                      'NULL'
  )
  newvar2_value <- diffdb[[value_var]][[rownr]]
  if(var2_type == 'D')
  {
    newvar2_value <- as.Date(newvar2_value, origin="1899-12-30", tz='UTC')
  }
  colname <- diffdb[rownr, colname]
  collabel <- attr(df2[[colname]], 'label')
  rowname <- diffdb[rownr, rowname]
  df1_value <-convert_to_string(var = df1[[colname]], row = match(rowname, df1keys))
  df2_value <-convert_to_string(var = df2[[colname]], row = match(rowname, df2keys))

  if (is.null(collabel)) {
    collabel <- NA
  }

  status <- diffdb[rownr, status]
  if(status==1)
  { # LEVEL_NOT_EXISTING
    msg <- paste0(ifelse(var2_type=='F', 'factor', 'labelled'), " variable #A_#", "doesn't have defined level ", df1_value, "#_2#")
  } else if (status==2) { # STRING_IS_NOT_A_NUMBER
    msg <- paste0("cannot convert value ", df1_value, " to number#_B##_2#")
  } else if (status==3) { # NOT_WHOLE_NUMBER
    msg <- paste0("cannot convert value ", df1_value, " to integer#_B##_2#")
  } else if (status==4) { # NOT_A_DATE
    msg <- paste0("cannot convert value ", df1_value, " to proper date#_B##_2#")
  } else if (status==5) { # LOST_TAGGED_NA
    msg <- paste0("#A_#doesn't allow for tagged NA ", format_values(newvar2_value), '#_2#')
  } else if (status==6) { # TAGGED_NA_ON_NUMERIC
    msg <- paste0("tagged NA ", format_values(newvar2_value), " inserted into numeric variable#_A##_2#")
  } else if (status==0) { #OKis.na(df2[[colname]][[match(rowname, df2keys)]])
#    browser()
    if(is.na(df2[[colname]][[match(rowname, df2keys)]]) ) {
  		msg <- paste0(format_values(newvar2_value), " added#_A##_2#")
    } else {
      msg <- paste0(format_values(newvar2_value), " replaces value ", df2_value, '#_A##_2#')
    }
  }

  return(msg)
}

update_diffdt_row<-function(diffrow, df1, df2, diffdb)
{
  full_row<-diffdb[diffrow,]
  varname<-full_row$colname
#  if(varname=='q_65b') browser()
  var1<-df1[[ varname ]]
  var2<-df2[[ varname ]]
  val1<-var1[[ full_row$rownr1 ]]
  val2<-var2[[ full_row$rownr2 ]]
  var2_type<-diffdb[diffrow, 'df2_data_type'][[1]]

#  cat(paste0(diffrow,'\n'))
#  if(diffrow==873) browser()
  ans<-convert_var_to_var(srcvar = var1, srcrow = full_row$rownr1,
                          destvar = var2 )
  if (!is.null(ans$value)) {
    value_var <- switch(var2_type,
                        F='value_int',
                        L='value_num',
                        I='value_int',
                        N='value_num',
                        D='value_num',
                        S='value_char',
                        'NULL'
    )
    if(is.null(value_var)) browser()
    if (var2_type != 'D' || is.na(ans$value)) {
    	set(diffdb, i=diffrow, j=value_var, value=ans$value)
    } else {
      if (class(ans$value) != 'Date') {
      	if('POSIXct' %in% class(ans$value)) {
      		ans$value <- as.Date(ans$value)
      	} else {
      		browser()
      	}
      }
    	set(diffdb, i=diffrow, j=value_var, value=as.numeric(ans$value - as.Date(0, origin="1899-12-30", tz='UTC')))
    }
  }
  set(diffdb, i=diffrow, j='status', value=ans$status)
}

create_diffdb<-function(diff_matrix, diff_rownames, diff_colnames, df1_rownames, df2_rownames, df1, df2)
{
  #Returns a letter that encodes data type.
  get_data_type<-function(varname, df)  {
    var <- df[[varname]]
    return(class2vartype(var))
  }

  get_value<-function(rowcol, dt, rownames)  {
    #    if(all(rowcol==c(410,10)))
    #      browser();
    #    cat(paste0(rowcol,'\n'))
    rowname<-diff_rownames[[rowcol[[1]] ]]
    colname<-diff_colnames[[rowcol[[2]] ]]
    var<-dt[[colname]]
    val<-var[[which(rownames==rowname)]]
    if(head(class(val),n=1) %in% c('numeric','integer','factor','labelled','ordered'))  {
      return(as.numeric(val))
    } else if (head(class(val),n=1) == 'Date') {
#      if(rowname=='LISB0736' && colname=='q_5') browser()
      return(as.numeric(val - as.Date(0, origin="1899-12-30", tz='UTC')))
    }
    else {
      return(haven::tagged_na('z'))
    }
  }

  get_label<-function(rowcol, dt, rownames)  {
#  	cat(paste0(rowcol,'\n'))
    rowname<-diff_rownames[[rowcol[[1]] ]]
    colname<-diff_colnames[[rowcol[[2]] ]]
    var<-dt[[colname]]
    val<-var[[which(rownames==rowname)]]
    if(head(class(val),n=1) %in% c('character'))  {
      return(val)
    } else if (head(class(val),n=1) %in% c('numeric','integer','Date'))  {
      return(NA)
    } else if (head(class(val),n=1) %in% c('factor','ordered'))  {
      return(as.character(val))
    } else if (head(class(val),n=1) %in% c('labelled'))  {
      labels<-labelled::val_labels(val)
      if(val %in% labels)   {
        return(names(labels)[match(val,labels)])
      } else {
        return(NA)
      }
      return(as.character(val))
    }
  }


  #  browser()
  mismatch_ind<-which(diff_matrix==1, arr.ind=TRUE)
  row_db<-NULL

  row_db<-data.table(
    rowname=diff_rownames[mismatch_ind[,1] ],
    colname=diff_colnames[mismatch_ind[,2] ],
    rownr1=match(diff_rownames[mismatch_ind[,1] ], df1_rownames),
    rownr2=match(diff_rownames[mismatch_ind[,1] ], df2_rownames),
    df1_data_type=plyr::laply(diff_colnames[mismatch_ind[,2]], get_data_type,  df=df1),
    df1_value=plyr::aaply(mismatch_ind,1, get_value, dt=df1, rownames=df1_rownames),
    df1_label=as.character(plyr::alply(mismatch_ind,1, get_label, dt=df1, rownames=df1_rownames)),
    df2_data_type=plyr::laply(diff_colnames[mismatch_ind[,2]], get_data_type,  df=df2),
    df2_value=plyr::aaply(mismatch_ind,1, get_value, dt=df2, rownames=df2_rownames),
    df2_label=as.character(plyr::alply(mismatch_ind,1, get_label, dt=df2, rownames=df2_rownames))
  )

  return(row_db)
}
