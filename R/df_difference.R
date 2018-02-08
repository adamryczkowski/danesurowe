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
# name_of_datasets='datasets', name_of_variables='questions', name_of_variable='question',
# name_of_cases='patients', name_of_case='patient', flag_include_rownumbers = TRUE )

df_difference<-function(df1, df2, df1_key=NULL, df2_key=NULL, columns_to_ignore=NULL,
                        flag_comment_replace_NA=FALSE,
                        flag_varlist_main_is_short_name=FALSE,
                        flag_varlist_include_secondary_name=TRUE,
                        flag_quote_shortvarname='`', flag_quote_longvarname='_',
                        flag_quote_rowname='`',
                        flag_compare_structure=TRUE, flag_compare_data=TRUE,
                        flag_explain_structure_in_nice_names = TRUE,
                        return_format=c('raw','md'),
                        markdown_header_level = 1,
                        attributes_to_ignore_in_structure=character(0),
                        threshold_count_for_partial_context=3,
                        flag_comment_new_rows = TRUE,
                        flag_comment_deleted_rows = TRUE,
                        flag_comment_new_cols = TRUE,
                        flag_comment_deleted_cols = TRUE,
                        name_of_datasets='datasets', name_of_variables='variables', name_of_variable='variable',
                        name_of_cases='cases', name_of_case='case', flag_include_rownumbers = TRUE,
												flag_include_statistics = TRUE,
												flag_sort_rows = TRUE,
												flag_sort_cols = FALSE
){
	dfnames<-get_differentiated_names(df1, df2)
	if(identical(return_format, c('raw','md'))) {
	  return_format<-'raw'
	}

#  browser() #TODO: Wymyśleć system filtrowania diffdb. Np. aby utoższamić NA z factorem NA.
	# A tibble: 11 x 6
	# df1_data_type df1_value df1_label df2_data_type df2_value df2_label
	# <chr>     <dbl>     <chr>         <chr>     <dbl>     <chr>
	# 	1             F        NA      <NA>             F         8        NA
	# 2             F        NA      <NA>             F         8        NA
	# 3             F        NA      <NA>             F         8        NA
	# 4             F        NA      <NA>             F         8        NA
	# 5             F        NA      <NA>             F         8        NA
	# 6             F        NA      <NA>             F         8        NA
	# 7             F        NA      <NA>             F         8        NA
	# 8             F        NA      <NA>             F         8        NA
	# 9             F        NA      <NA>             F         8        NA
	# 10             F        NA      <NA>             F         8        NA
	# 11             F        NA      <NA>             F         8        NA

	out<-''
  if(flag_compare_data) {
    msg <- paste0('Changes to data when transforming ', flag_quote_longvarname,
    							dfnames$name1, flag_quote_longvarname, ' into ', flag_quote_longvarname,
    							dfnames$name2, flag_quote_longvarname)
    if(flag_include_statistics) {
    	diff_db <- gen_difference_df(df1=df1, df2=df2, df1_key = df1_key, df2_key = df2_key,
    															 columns_to_ignore=columns_to_ignore,
    															 flag_include_new_rows = FALSE,
    															 flag_include_new_cols = FALSE,
    															 flag_include_deleted_rows = FALSE,
    															 flag_include_deleted_cols = FALSE)

    	number_of_all_changes<-nrow(diff_db$diffdb)
    }

    diff_db <- gen_difference_df(df1=df1, df2=df2, df1_key = df1_key, df2_key = df2_key,
                                 columns_to_ignore=columns_to_ignore,
                                 flag_include_new_rows = flag_comment_new_rows,
                                 flag_include_new_cols = flag_comment_new_cols,
    														 flag_include_deleted_rows = flag_comment_deleted_rows,
    														 flag_include_deleted_cols = flag_comment_deleted_cols,
    														 name_of_cases = name_of_cases
    														 )



    if(flag_sort_rows) {
    	diff_db$common_rownames <- sort(diff_db$common_rownames)
    }

    if(flag_sort_cols){
    	diff_db$common_colnames <- sort(diff_db$common_colnames)
    }

    ans <- comment_diffs(
    	diff_list = diff_db, flag_comment_replace_NA = flag_comment_replace_NA,
    	flag_varlist_main_is_short_name = flag_varlist_main_is_short_name,
    	flag_varlist_include_secondary_name = flag_varlist_include_secondary_name,
    	flag_quote_shortvarname = flag_quote_shortvarname,
    	flag_quote_longvarname = flag_quote_longvarname,
    	flag_quote_rowname = flag_quote_rowname,
    	threshold_count_for_partial_context = threshold_count_for_partial_context,
    	name_of_datasets = name_of_datasets, name_of_case = name_of_case, name_of_cases = name_of_cases,
    	name_of_variables = name_of_variables, name_of_variable = name_of_variable
    	)

    diffs_lengths <- purrr::map_dbl(ans$diffs, ~length(.$items))
    diffs <- ans$diffs[rev(order(diffs_lengths))]

    if(length(diffs)>0) {
      cl<-kmeans(log(diffs %>% map('items') %>% map_dbl(length)),length(diffs)^(1/2.5))
      centers<-order(as.numeric(cl$centers))
      lengths<-plyr::mapvalues(cl$cluster, centers, seq_along(centers))

      get_key_pos<-function(type_1row, key) {
        if(type_1row) {
          return(match(key, diff_db$common_rownames))
        } else {
          return(match(key, diff_db$common_colnames))
        }
      }

      indexes<-purrr::map2_dbl(diffs %>% map_lgl('type_1row'), diffs %>% map_chr('key'), get_key_pos)

      diffs<-diffs[order(-lengths, diffs %>% map_lgl('type_1row'), indexes ) ]
    }

    if(return_format=='md') {
    	if(flag_include_statistics) {
    		nadf1<-df1[match(diff_db$common_rownames, diff_db$df1keys),] %>%
    			select_(.dots =  setdiff(diff_db$common_colnames, c(diff_db$df2specific_colnames,diff_db$df1specific_colnames) )) %>%
    			mutate_all(is.na) %>% data.matrix()

    		nadf2<-df2[match(diff_db$common_rownames, diff_db$df2keys),] %>%
    			select_(.dots =  setdiff(diff_db$common_colnames, c(diff_db$df2specific_colnames,diff_db$df1specific_colnames) )) %>%
    			mutate_all(is.na) %>% data.matrix()
#				browser()
    		nadf <- sum((1-nadf1) | (1-nadf2))
    		stats <- paste0(
    			'There are ', danesurowe::report_integer(number_of_all_changes),
    			' changes to the values to the existing ', name_of_cases,
    			' and existing ', name_of_variables, ' between the ', name_of_datasets, ', which is ',
    			danesurowe::report_single_value(100*number_of_all_changes/nadf),'% of all non-missing values. ')
    		if(number_of_all_changes != nrow(diff_db$diffdb)) {
    			stats<-paste0(stats, "Besides, there are additional ",
    									danesurowe::report_integer(nrow(diff_db$diffdb)-number_of_all_changes),
    									" value insertions/deletions due to the insertion or deletion of the ",
    									name_of_cases, " and/or ", name_of_variables, '.' )
    		}
    	} else {
    		stats<-''
    	}

    	common_header <- FALSE
    	any_header <- FALSE
    	out<-paste0(out, '\n',
    							pander::pandoc.header.return(msg, level = markdown_header_level),
    							'\n',
    							paste0(ans$prefix, collapse='\n'), '\n',stats, '\n\n')


    	for(el in diffs) {
    		if(length(el$items)>=threshold_count_for_partial_context) {
    			out<-paste0(out,
    									pander::pandoc.header.return(el$header, level=markdown_header_level+1))
    			any_header <- TRUE
    		} else {
    			if (!common_header) {
    				if(any_header) {
    					out<-paste0(out, pander::pandoc.header.return('Other items',
    																												level=markdown_header_level+1))
    				}
    				common_header <- TRUE
    			}
    		}
    		out<-paste0(out, pander::pandoc.list.return(el$items, style='bullet'), '\n')
    	}
    	out<-paste0(out, '\n')
    } else if (return_format=='raw') {
    	out<-c(out, list(data_diff = diffs, data_diff_header = msg))
    }
  }

  if(flag_compare_structure) {
    ans<-df_structure_difference(df1=df1, df2=df2,
                                 flag_comment_deleted_cols = flag_comment_deleted_rows,
                                 flag_comment_new_cols = flag_comment_new_cols,
                                 attributes_to_ignore = attributes_to_ignore_in_structure,
                                 flag_explain_in_nice_names = flag_explain_structure_in_nice_names,
                                 name_of_variable = name_of_variable, name_of_variables = name_of_variables)

    msg <- paste0('Changes to data structure and layout when transforming ', flag_quote_longvarname,
    							dfnames$name1, flag_quote_longvarname, ' into ', flag_quote_longvarname,
    							dfnames$name2, flag_quote_longvarname)

    diffs_lengths <- purrr::map_dbl(ans$diffs, ~length(.$items))
    diffs <- ans$diffs[rev(order(diffs_lengths))]

    if(return_format=='md') {
    	common_header <- FALSE
    	any_header <- FALSE
    	out<-paste0(out, '\n',
    							pander::pandoc.header.return(msg, level = markdown_header_level))
    	if(length(diffs)==0) {
    		out<-paste0(out,
    								'\nDatabase structure is identical between the datasets.\n')
    	} else {
    		out<-paste0(out,
    								'\nThe paragraph below shows differences in data layout between the datasets\n')

    		for(el in diffs) {
    			if(length(el$items)>threshold_count_for_partial_context) {
    				out<-paste0(out, pander::pandoc.header.return(el$header,
    																											level=markdown_header_level+1))
    				any_header <- TRUE
    			} else {
    				if (!common_header) {
    					if(any_header) {
    						out<-paste0(out, pander::pandoc.header.return('Other items',
    																													level=markdown_header_level+1))
    					}
    					common_header <- TRUE
    				}
    			}
    			out<-paste0(out, pander::pandoc.list.return(el$items, style='bullet'), '\n')
    		}
    		out<-paste0(out, '\n')

    	}

    } else if (return_format=='raw') {
    	out<-c(out, list(data_diff = diffs, data_diff_header = msg))
    }

  }
  return(out)
}


get_differentiated_names<-function(df1, df2) {
  name1<-danesurowe::GetDBName(df1)
  name2<-danesurowe::GetDBName(df2)
  if(name1==name2) {
    name1 <- attr(df1, 'path', exact = TRUE)
    if(is.null(name1)) {
      name1='df1'
    }
    name1 <- basename(name1)
    name2 = attr(df2, 'path', exact = TRUE)
    if(is.null(name2)){
      name2 <- 'df2'
    }
    name2 = basename(name2)
  }

  if(name1==name2) {
    name1 <- attr(df1, 'path', exact = TRUE)
    if(is.null(name1)) {
      name1='df1'
    }
    name2 = attr(df2, 'path', exact = TRUE)
    if(is.null(name2)){
      name2 <- 'df2'
    }
  }


  if(name1==name2) {
    name1='df1'
    name2='df2'
  }
  return(list(name1=name1, name2=name2))
}


get_rownames_dfs<-function(df1, df2, df1_key, df2_key) {
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
  return(list(df1_rownames=df1_rownames, df2_rownames=df2_rownames))
}

gen_difference_df<-function(df1, df2, df1_key=NULL, df2_key=NULL, columns_to_ignore=NULL,
                            flag_include_new_rows=FALSE, flag_include_new_cols=FALSE,
														flag_include_deleted_rows=FALSE, flag_include_deleted_cols=FALSE,
														name_of_cases='cases'
														)
{
  if (!'data.frame' %in% class(df1))  {
    ans<-list(message='First argument is not a data.frame!', result=FALSE)
    return(ans)
  }

  if (!'data.frame' %in% class(df2))  {
    ans<-list(message='Second argument is not a data.frame!', result=FALSE)
    return(ans)
  }

	df1_orig <- copy(df1)
	df2_orig <- copy(df2)

  # Algorithm:
  # 1. Get common set of column names and assume that column that match by name must be equivalent


  df1_names<-colnames(df1)
  df2_names<-colnames(df2)

  common_colnames<-intersect(df1_names,df2_names)
  df1specific_colnames <- setdiff(df1_names, common_colnames)
  df2specific_colnames <- setdiff(df2_names, common_colnames)

  if(flag_include_deleted_cols) {
  	#Adding new cols to df2
  	for(colname in df1specific_colnames) {
  		df2[,(colname):=df1[[colname]][[1]]]
  		df2[1:nrow(df2),(colname):=NA]
  		copy_obj_attributes(df1[[colname]], df2[[colname]])
  	}
  }

  if(flag_include_new_cols) {
  	for(colname in df2specific_colnames) {
  		df1[,(colname):=df2[[colname]][[1]]]
  		df1[1:nrow(df1),(colname):=NA]
  		copy_obj_attributes(df2[[colname]], df1[[colname]])
  	}
  }
  df1_names<-colnames(df1)
  df2_names<-colnames(df2)

  common_colnames<-intersect(df1_names,df2_names)

  if(length(common_colnames)==0) {
  	stop("No common columns (variables) between datasets")
  } else if (length(common_colnames)<5 && ncol(df1) >5 && ncol(df2) > 5 ) {
  	warning(paste0("Only ", length(common_colnames), " common col names between sets"))
  }


  if(!is.null(columns_to_ignore))  {
    common_colnames <- setdiff(common_colnames,columns_to_ignore)
  }

  # 2. Get common set of rownames
  ans <- get_rownames_dfs(df1 = df1, df2 = df2, df1_key = df1_key, df2_key = df2_key)
  df1_rownames<-ans$df1_rownames
  df2_rownames<-ans$df2_rownames

  dupl<-duplicated(na.omit(df1_rownames))
  if(sum(dupl)>0)  {
    stop(paste0("First database has ", sum(dupl), " non-unique ", name_of_cases, " names: ",
                format_case_list(na.omit(df1_rownames)[dupl], name_of_cases = name_of_cases)))
  }

  dupl<-duplicated(na.omit(df2_rownames))
  if(sum(dupl)>0)  {
    stop(paste0("Second database has ", sum(dupl), " non-unique row names: ",
                format_case_list(na.omit(df2_rownames)[dupl], name_of_cases = name_of_cases)))
  }



  common_rownames <- sort(intersect(df1_rownames, df2_rownames))
  df1specific_rownames <- setdiff(df1_rownames, common_rownames)
  df2specific_rownames <- setdiff(df2_rownames, common_rownames)

  if(flag_include_deleted_rows) {
  	#Adding new cols to df2
  	for(rowname in df1specific_rownames) {
  	  empty_row<-c(setNames(list(a=rowname),df2_key), setNames(rep(NA,ncol(df2)-1), setdiff(colnames(df2), df2_key ) ))
  		#empty_row<-rbind(df2[integer(0),], setNames(list(a=rowname),df2_key))
  		df2<-rbind(df2, empty_row)
  	}
  }
  if(flag_include_new_rows) {
  	for(rowname in df2specific_rownames) {
#  		empty_row<-rbind(df1[integer(0),], setNames(list(a=rowname),df1_key), fill=TRUE)
  		empty_row<-c(setNames(list(a=rowname),df1_key), setNames(rep(NA,ncol(df1)-1), setdiff(colnames(df1), df1_key ) ))
  		df1<-rbind(df1, empty_row)
  	}
  }
	ans <- get_rownames_dfs(df1 = df1, df2 = df2, df1_key = df1_key, df2_key = df2_key)
  df1_rownames<-ans$df1_rownames
  df2_rownames<-ans$df2_rownames

  common_rownames <- sort(intersect(df1_rownames, df2_rownames))

  if(length(common_rownames)==0) {
  	stop("No common rows (cases) between data sets")
  } else if (length(common_rownames)<5 && nrow(df1) >5 && nrow(df2) > 5 ) {
  	warning(paste0("Only ", length(common_rownames), " common row between data sets"))
  }


  df1_selected_rows <- match(common_rownames, df1_rownames)
  df2_selected_rows <- match(common_rownames, df2_rownames)

  df1_sorted <- df1[df1_selected_rows,]
  df2_sorted <- df2[df2_selected_rows,]

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
              df1specific_rownames=df1specific_rownames, df2specific_rownames=df2specific_rownames,
  						common_rownames=common_rownames, common_colnames=common_colnames,
  						df1_orig=df1_orig,
  						df2_orig=df2_orig
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
    	if ('numeric' %in% class(var1)) {
    		var1<-as.Date(var1, origin = '1900-01-01')
    		if('POSIXct' %in% class(var1)) {
    			var<-as.POSIXct(var1)
    		} else if ('Date' %in% class(var1)) {
    			#do nothing
    		} else {
    			browser()
    		}
    	}
    } else if (classes_sorted == 'Date|POSIXct,POSIXt') {
    	if('Date' %in% class(var1)) {
    		var2 <- as.Date(var2)
    	} else {
    		var1 <- as.Date(var1)
    	}
      # if(class(var1)=='Date' || 'POSIXct' %in% class(var1)) {
      #   var1 <- as.numeric(as.Date(var1) - as.Date(0, origin="1899-12-30", tz='UTC'))
      # } else {
      #   var2 <- as.numeric(as.Date(var2) - as.Date(0, origin="1899-12-30", tz='UTC'))
      # }
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
  			var1<-suppressWarnings(as.integer(var1))
    	} else {
  			var2<-suppressWarnings(as.integer(var2))
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
    			var1<-var1
    		} else {
    			var1<-suppressWarnings(as.numeric(var1))
    		}
    	} else {
    		if(sum(!is.na(var2))==0)
    		{
    			#Do nothing
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
    	return(list(compvar=1,
    							var1=var1,
    							var2=var2))
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

  return(list(compvar=danesurowe:::compareNA(var1,v2=var2),
              var1=var1,
              var2=var2))
}

#Converts a value var[[row]] into string. All NAs gets translated into NA. Returns list (value, error_code)

convert_to_string<-function(var, row, missing_string='NA', string_quote_character='"',
														other_text_quote_character='`')
{
	if(is.null(var)){
		browser()
	}
  vartype<-class2vartype(var)
#  if(vartype=='B') browser()
  val<-var[[row]]
  if (vartype %in% c('L', 'N')) {
    if (is.na(val))  {
    	if(haven::is_tagged_na(val)) {
    		if(vartype == 'L') {
    			natag<-haven::na_tag(val)
    			label <- names(which(natag == danesurowe::GetNALevels(var)))
    			val_out <- paste0(other_text_quote_character, label, other_text_quote_character)
    		} else {
    			val_out <- paste0(missing_string, '(', haven::na_tag(val), ')')
    		}
    	} else {
    		val_out <- missing_string
    	}
    } else {
      val_out <- paste0(other_text_quote_character,
      									GetLabels(var)[match(val, GetLevels(var))],
      									other_text_quote_character)
    }
  } else if (vartype %in% c('I', 'D', 'S', 'F', '0', 'B'))  {
  	if(is.na(val)) {
  		val_out <- missing_string
  	} else if (vartype == 'S') {
  		val_out <- paste0(string_quote_character, as.character(val), string_quote_character)
  	} else if (vartype %in% c('D', 'F', '0')) {
  		val_out <- paste0(other_text_quote_character, as.character(val),other_text_quote_character)
  	} else {
  		val_out <- as.character(val)
  	}
  } else  {
    browser()
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
  } else if (destvartype=='0') {
  	value<-ans
  	status<-convert_var_to_var.status$OK
  } else {
  	browser()
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
      str_srcval <- convert_to_string(srcvar, row=srcrow, string_quote_character = '',other_text_quote_character = '', missing_string = 'NA')
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

comment_diffs<-function(diff_list,
												flag_comment_replace_NA=FALSE,
                        flag_varlist_main_is_short_name=FALSE,
                        flag_varlist_include_secondary_name=FALSE,
                        flag_quote_shortvarname='', flag_quote_longvarname='',
                        flag_quote_rowname='', threshold_count_for_partial_context=3,
												name_of_datasets='datasets', name_of_variables='variables', name_of_variable='variable',
												name_of_cases='cases', name_of_case='case', flag_include_rownumbers = TRUE)
{
	diffdb<-diff_list$diffdb
	df1<-diff_list$df1_orig
	df2<-diff_list$df2_orig
	df1keys<-diff_list$df1keys
	df2keys<-diff_list$df2keys
	all_rownames<-diff_list$common_rownames
	all_colnames<-diff_list$common_colnames
	df1specific_colnames <- diff_list$df1specific_colnames
	df2specific_colnames <- diff_list$df2specific_colnames
	df1specific_rownames <- diff_list$df1specific_rownames
	df2specific_rownames <- diff_list$df2specific_rownames

	msg<-character(0)

	if(length(df1specific_colnames)>0 && length(df2specific_colnames)>0 ) {
		msg <- paste0("Warning: both ", name_of_datasets, " have unmatched ", name_of_variables, ".")
	}
	if(length(df2specific_colnames)>0 ) {
		msg[[length(msg)+1]] <-
			paste0("Conversion will add the following ", name_of_variables, ": ",
						 format_var_list(colnames = df2specific_colnames, dt = df2,
						 								flag_include_secondary_name = flag_varlist_include_secondary_name,
						 								flag_main_is_short_name = flag_varlist_main_is_short_name,
						 								flag_quote_shortname = flag_quote_shortvarname,
						 								flag_quote_longname = flag_quote_longvarname, name_of_variables = name_of_variables
						 								), '. ')
	}

	if(length(df1specific_colnames)>0 ) {
		msg[[length(msg)+1]] <-
			paste0("The following ", name_of_variables, " were ignored: ",
						 format_var_list(colnames = df1specific_colnames, dt = df1,
						 								flag_include_secondary_name = flag_varlist_include_secondary_name,
						 								flag_main_is_short_name = flag_varlist_main_is_short_name,
						 								flag_quote_shortname = flag_quote_shortvarname,
						 								flag_quote_longname = flag_quote_longvarname,
						 								name_of_variables = name_of_variables
						 								), '. ')
	}
	if(length(df1specific_colnames)==0 && length(df2specific_colnames)==0 ) {
		msg <- paste0("All ", name_of_variables, " names are matched between the ", name_of_datasets, ". ")
	}


	if(length(df1specific_rownames)>0 && length(df2specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("Warning: both ", name_of_datasets, " have unmatched ", name_of_cases, ". ")
	}
	if(length(df2specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("Conversion will add the following ", name_of_cases,  ": ",
																	 format_case_list(case_names = df2specific_rownames,
																	                  flag_quote = flag_quote_rowname,
																	                  name_of_cases = name_of_cases), '. ')
	}

	if(length(df1specific_rownames)>0 ) {
		msg[[length(msg)+1]] <- paste0("The following ", name_of_cases, " were ignored: ",
																	 format_case_list(case_names = df1specific_rownames,
																	                  flag_quote = flag_quote_rowname,
																	                  name_of_cases = name_of_cases), '. ')
	}
	if(length(df1specific_rownames)==0 && length(df2specific_rownames)==0 ) {
		msg[[length(msg)+1]] <- paste0("All ", name_of_cases, " are matched between the ", name_of_datasets, ". ")
	}


	if(flag_comment_replace_NA) {
		db <- data.table(dplyr::filter(diffdb, !(is.na(df2_value) & is.na(df2_label))))
	} else {
		db <- data.table(copy(diffdb))
	}
	out <- list()
	prefix<-msg

	df1<-diff_list$df1
	df2<-diff_list$df2



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
				msg <- comment_one_diff(i, diffdb = tmpdb, df1 = df1, df1keys = df1keys,
				                        df2 = df2, df2keys = df2keys,
																df2_data_type = tmpdb$df2_data_type[[i]])
				outvec[[i]] <- msg
			}

#			browser()
			outvec2 <- tibble(outvec=outvec, key=tmpdb$colname, pos=match(tmpdb$colname, all_colnames))
			msgtab<-outvec2 %>% group_by(outvec) %>%
				summarise(n = n(), poss=sort(pos)[[1]], keys=paste0(key, collapse=',')) %>% arrange(-n, poss)

#			msgtab <- dplyr::arrange(dplyr::as_tibble(table(outvec)), -n)
			msgvec <- rep('', nrow(msgtab))
			for (i in seq(1,nrow(msgtab))) {
			  grp_msg <- msgtab$outvec[[i]]

			  nested_colnames <- tmpdb$colname[which(outvec==grp_msg)]
			  if(nrow(msgtab) > threshold_count_for_partial_context) {
			    msg <- decorate_comment_msg(
			    	all_rownames=all_rownames, all_colnames=all_colnames,
			    	msg=grp_msg, df1=df1, df2=df2,
			    	colnames=nested_colnames,
			    	flag_varlist_main_is_short_name=flag_varlist_main_is_short_name,
			    	flag_varlist_include_secondary_name=flag_varlist_include_secondary_name,
			    	flag_quote_shortvarname=flag_quote_shortvarname,
			    	flag_quote_longvarname=flag_quote_longvarname,
			    	name_of_case = name_of_case, name_of_cases = name_of_cases,
			    	name_of_variables = name_of_variables,
			    	flag_include_rownumbers = flag_include_rownumbers)
			  } else {
			    msg <- decorate_comment_msg(
			    	msg = grp_msg, df1=df1, df2=df2,
			    	all_rownames=all_rownames, all_colnames=all_colnames,
			      df1keys = df1keys, df2keys = df2keys,
			      rownames=rowname_in,
			      colnames=nested_colnames,
			      flag_varlist_main_is_short_name=flag_varlist_main_is_short_name,
			      flag_varlist_include_secondary_name=flag_varlist_include_secondary_name,
			      flag_quote_shortvarname=flag_quote_shortvarname,
			      flag_quote_longvarname=flag_quote_longvarname,
			      flag_quote_rowname = flag_quote_rowname,
			    	name_of_case = name_of_case, name_of_cases = name_of_cases,
			    	name_of_variables = name_of_variables,
			    	flag_include_rownumbers = flag_include_rownumbers)
			  }

			  msgvec[[i]]<-msg
			}

      if(rowname_in %in% df1specific_rownames) {
        msg <- paste0('What exactly was removed from ', name_of_case, ' ')
      } else if (rowname_in %in% df2specific_rownames) {
        msg <- paste0('What exact information was added in ', name_of_case, ' ')
      } else {
        msg <- paste0('Changes to the ', name_of_case, ' ')
      }
			msg <- paste0(msg,  format_case_list(case_names = rowname_in, flag_quote = flag_quote_rowname,
			                                     name_of_cases = name_of_cases))
			#			setattr(msgvec, 'header', msg)

			outrec <- list(header = msg, items=msgvec, type_1row = TRUE, key=rowname_in)

			db <- db[db$rowname != rowname_in,]

		} else {
			colname_in <- names(colnames)[[1]]
			tmpdb <- db[db$colname==colname_in, ]
			setkey(tmpdb, rowname)

			outvec <- rep('', nrow(tmpdb))
			for (i in seq(nrow(tmpdb)))
			{
				msg <- comment_one_diff(i, diffdb = tmpdb, df1 = df1, df1keys = df1keys,
																df2 = df2, df2keys = df2keys,
																df2_data_type = tmpdb$df2_data_type[[i]])
				outvec[[i]] <- msg
			}
			outvec2 <- tibble(outvec=outvec, key=tmpdb$rowname, pos=match(tmpdb$rowname, all_rownames))
			msgtab<-outvec2 %>% group_by(outvec) %>%
				summarise(n = n(), poss=sort(pos)[[1]], keys=paste0(key, collapse=',')) %>% arrange(-n, poss)

			#msgtab <- dplyr::arrange(dplyr::as_tibble(table(outvec)), -n)
			msgvec <- rep('', nrow(msgtab))
			for (i in seq(1,nrow(msgtab))) {
			  grp_msg <- msgtab$outvec[[i]]
			  counts<-msgtab$n[[i]]

			  nested_rownames <- tmpdb$rowname[outvec==grp_msg]

			  if(nrow(msgtab) > threshold_count_for_partial_context) {
			  	msg <- decorate_comment_msg(grp_msg, df1=df1, df2=df2,
			  															all_rownames=all_rownames, all_colnames=all_colnames,
			  															rownames=nested_rownames,
			  															df1keys = df1keys, df2keys = df2keys,
			  															flag_quote_rowname = flag_quote_rowname,
			  															name_of_case = name_of_case, name_of_cases = name_of_cases,
			  															name_of_variables = name_of_variables,
			  															flag_include_rownumbers = flag_include_rownumbers)

			  } else {
			  	msg <- decorate_comment_msg(
			  		grp_msg, df1=df1, df2=df2,
			  		df1keys = df1keys, df2keys = df2keys,
			  		all_rownames=all_rownames, all_colnames=all_colnames,
			  		rownames=nested_rownames,
			  		colnames=colname_in,
			  		flag_varlist_main_is_short_name=flag_varlist_main_is_short_name,
			  		flag_varlist_include_secondary_name=flag_varlist_include_secondary_name,
			  		flag_quote_shortvarname=flag_quote_shortvarname,
			  		flag_quote_longvarname=flag_quote_longvarname,
			  		flag_quote_rowname = flag_quote_rowname,
			  		name_of_case = name_of_case, name_of_cases = name_of_cases, name_of_variables = name_of_variables,
			  		flag_include_rownumbers = flag_include_rownumbers)
			  }

			  msgvec[[i]]<-msg
			}

			if(colname_in %in% df1specific_colnames) {
			  msg <- paste0('What non-empty values were removed with ', name_of_variable, " ")
			} else if (colname_in %in% df2specific_colnames) {
			  msg <- paste0('What values were added to ', name_of_variable, " ")
			} else {
			  msg <- paste0('Changes to the ', name_of_variable, ' ')
			}
			msg <- paste0(msg,
			             format_var_list(colnames =  colname_in, dt = df2,
			                             flag_main_is_short_name = flag_varlist_main_is_short_name,
			                             flag_include_secondary_name = flag_varlist_include_secondary_name,
			                             flag_quote_shortname = flag_quote_shortvarname,
			                             flag_quote_longname = flag_quote_longvarname,
			                             name_of_variables = name_of_variables))

#			setattr(msgvec, 'header', msg)

			outrec <- list(header = msg, items=msgvec, type_1row = FALSE, key=colname_in)
			db <- db[db$colname != colname_in,]

		}
		out <- c(out, list(outrec))
	}

	return(list(prefix=prefix, diffs=out))
}

decorate_comment_msg<-function(msg, df1, df2, colnames=NULL,
															 rownames=NULL, df1keys=NULL, df2keys=NULL,
															 all_rownames=NULL, all_colnames=NULL,
															 flag_varlist_main_is_short_name=FALSE,
															 flag_varlist_include_secondary_name=FALSE,
															 flag_quote_shortvarname='', flag_quote_longvarname='',
															 flag_quote_rowname='', flag_include_rownumbers,
															 name_of_case, name_of_cases, name_of_variables) {
	if (!is.null(colnames)) {
	  varlabels<-format_var_list(colnames=colnames, dt=df1,
	                             flag_main_is_short_name=flag_varlist_main_is_short_name,
	                             flag_include_secondary_name=flag_varlist_include_secondary_name,
	                             flag_quote_shortname=flag_quote_shortvarname,
	                             flag_quote_longname=flag_quote_longvarname,
	                             name_of_variables = name_of_variables
	                             )
		msgA_ <- paste0(varlabels, ' ')
		msg_A <- paste0(' ', varlabels)
		msg_B <- paste0(' in ', varlabels)
	} else {
		msgA_ <- ''
		msg_A <- ''
		msg_B <- ''
	}
	msg <- stringr::str_replace(msg, stringr::fixed("#A_#"), msgA_)
	msg <- stringr::str_replace(msg, stringr::fixed("#_A#"), msg_A)
	msg <- stringr::str_replace(msg, stringr::fixed("#_B#"), msg_B)

	if (!is.null(rownames)) {

	  formatted_rownames <- format_case_list(case_names=rownames, all_cases = all_rownames, name_of_cases = name_of_cases,
	                                         flag_quote=flag_quote_rowname)
	  if(flag_include_rownumbers) {
	    rownumbers1 <- match(rownames, df1keys)
	    formatted_rownames1 <-
	      format_values_as_one_string(rownumbers1,
	                                  complementary_values=setdiff(seq_along(df1keys), rownumbers1),
	                                  plural_form=paste0(name_of_case, ' numbers'))
	    rownumbers2 <- match(rownames, df2keys)

	    if(all(rownumbers1==rownumbers2)) {
	      rowlabel<-paste0(formatted_rownames, ' (that corresponds to ', name_of_case, ' nr ',
	                       formatted_rownames1,')')
	    } else {
	      formatted_rownames2 <-
	        format_values_as_one_string(rownumbers2,
	                                    complementary_values=setdiff(seq_along(df2keys), rownumbers2),
	                                    plural_form=paste0(name_of_case,' numbers'))

	      rowlabel<-paste0(formatted_rownames, ' (that corresponds to source ', name_of_case, ' nr ',
	                       formatted_rownames1, ' and destination ', name_of_case, ' nr ', formatted_rownames2, ')')
	    }
	  } else {
	    rowlabel <- formatted_rownames
	  }

		msg_2 <- paste0(' for ', rowlabel)

	} else {
		msg_2 <- ''
	}
	msg <- stringr::str_replace(msg, stringr::fixed("#_2#"), msg_2)

	return(msg)
}


comment_one_diff<-function(rownr, diffdb, df1, df1keys, df2, df2keys, df2_data_type, name_of_variable='variable')
{
  var2_type <- diffdb$df2_data_type[[rownr]]
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

  colname <- diffdb$colname[[rownr]]
  collabel <- attr(df2[['colname']], 'label', exact = TRUE)
  rowname <- diffdb$rowname[[rownr]]
  df1rownr <- match(rowname, df1keys)
  df2rownr <- match(rowname, df2keys)
  df1_value <-convert_to_string(var = df1[[colname]], row = df1rownr)
  df2_value <-convert_to_string(var = df2[[colname]], row = df2rownr)

  if (is.null(collabel)) {
    collabel <- NA
  }

  status <- diffdb[rownr, 'status']
  if(status==1)
  { # LEVEL_NOT_EXISTING
    msg <- paste0(ifelse(var2_type=='F', 'factor', 'labelled'),
                  " ", name_of_variable, " #A_#",
                  "doesn't have defined level ", format_values(df1_value), "#_2#")
  } else if (status==2) { # STRING_IS_NOT_A_NUMBER
    msg <- paste0("cannot convert value ", format_values(df1_value), " to number#_B##_2#")
  } else if (status==3) { # NOT_WHOLE_NUMBER
    msg <- paste0("cannot convert value ", format_values(df1_value), " to integer#_B##_2#")
  } else if (status==4) { # NOT_A_DATE
    msg <- paste0("cannot convert value ", format_values(df1_value), " to proper date#_B##_2#")
  } else if (status==5) { # LOST_TAGGED_NA
    msg <- paste0("#A_#doesn't allow for tagged NA ", format_values(df2_value), '#_2#')
  } else if (status==6) { # TAGGED_NA_ON_NUMERIC
    msg <- paste0("tagged NA ", format_values(df2_value), " inserted into numeric ", name_of_variable, "#_A##_2#")
  } else if (status==0) { #OK
#    browser()
    if(is.na(df1[[colname]][[match(rowname, df1keys)]]) ) {
  		msg <- paste0(format_values(df2_value), " added#_B##_2#")
    } else if(is.na(df2[[colname]][[match(rowname, df2keys)]]) ) {
      msg <- paste0(format_values(df1_value), " removed#_B##_2#")
    } else {
      msg <- paste0(format_values(df2_value), " replaces value ", format_values(df1_value), '#_B##_2#')
    }
  }
  return(msg)
}

update_diffdt_row<-function(diffrow, df1, df2, diffdb)
{
  full_row<-diffdb[diffrow,]
  varname<-full_row$colname
  #if(varname=='q_6') browser()
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
      if (! 'Date' %in% class(ans$value) ) {
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

create_diffdb<-function(diff_matrix, diff_rownames, diff_colnames,
												df1_rownames, df2_rownames, df1, df2)
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

create_df_from_df_structure<-function(df, flag_add_nice_names=FALSE, default_df_name=NULL, flag_include_vartype=FALSE) {
  outdf<-dplyr::tibble(colname=colnames(df), label=Hmisc::label(df),
                       class=purrr::map_chr(as.list(df), ~paste0(sort(class(.)),collapse=',')),
                       vartype=if(flag_include_vartype) {purrr::map_chr(as.list(df), ~class2vartype(.))} else {NA},
                       theoretical_min_numeric=GetTheoreticalMin(df),
                       theoretical_max_numeric=GetTheoreticalMax(df),
                       force_integers=AreIntegersForced(df),
                       required=IsRequired(df),
                       limit_to_labels=IsLimitedToLabels(df),
                       unit=GetUnits(df),
                       xls_formula=GetExcelFormula(df), r_formula=GetRFormula(df),
                       labels_string=as.character(GetLabelsString(df)))
  all_attributes<-getOption('df_used_attributes')

  ans<-plyr::llply(df, function(v) {
    a<-attributes(v)
    filtered_names<-setdiff(names(a), all_attributes)
    a[filtered_names]
  } )
  other_attributes<-reduce(ans, function(x1, x2) unique(c(x1, names(x2))), .init = names(ans[[1]]) )
  for(i in seq_along(other_attributes)) {
    attrname <- other_attributes[[i]]
    classes<-setdiff(reduce(ans, function(x1, x2) unique(c(x1, class(x2[[attrname]]))), .init = class(ans[[1]][[attrname]])), 'NULL')
    if(length(classes)>1) {
      browser()
      warning(paste0("Atrybut ", attrname, " ma niejednorodny typ danych: ", paste0(classes, collapse=' i ')))
    }
    cat(paste0(attrname,'\n'))
    if('list' %in% classes) {
      vec<-plyr::llply(df, .fun = function(var) {
        val<-attr(var, attrname, exact = TRUE)
        if(is.null(val)) {
          return(NA)
        } else {
          val
        }
      })
    } else {
      vec<-plyr::laply(df, .fun = function(var) {
        val<-attr(var, attrname, exact = TRUE)
        if(is.null(val)) {
          return(NA)
        } else {
          val
        }
      })
    }
    outdf[[attrname]]<-vec
  }

  if(flag_add_nice_names) {
  	setattr(outdf$colname, 'label', 'Internal variable name')
  	setattr(outdf$label, 'label', 'Variable name')
  	setattr(outdf$class, 'label', 'Data storage class')
  	if(flag_include_vartype) {
  	  setattr(outdf$vartype, 'label', 'Type symbol')
  	}
  	setattr(outdf$theoretical_min_numeric, 'label', 'Theoretical min')
  	setattr(outdf$theoretical_max_numeric, 'label', 'Theoretical max')
  	setattr(outdf$force_integers, 'label', 'Force only integral values')
  	setattr(outdf$required, 'label',  'Force only non-missing values')
  	setattr(outdf$limit_to_labels, 'label', 'Force only values that are labelled')
  	setattr(outdf$unit, 'label', 'Variable unit')
  	setattr(outdf$xls_formula, 'label', 'Excel formula')
  	setattr(outdf$r_formula, 'label', 'R formula')
  	setattr(outdf$labels_string, 'label', 'Variable labels dictionary')
    class_levels<-c('real number'='numeric',
#                    'integer'='integer',
                    'text'='character',
#                    'logical'='logical',
                    'nominal'='factor',
                    'ordered nominal'='factor,ordered',
#                    'labelled'='labelled',
                    'date'='Date',
                    'POSIX date'='POSIXlt,POSIXt'
                    )
    all_classes <- unique(outdf$class)
    which_known<-which(class_levels %in% all_classes)
    known_classes<-class_levels[which_known]
    unknown_classes<-all_classes[setdiff(seq_along(all_classes), which_known)]

    myclasses_labels<-c(names(class_levels[map_int(known_classes, ~which(.==class_levels))]),unknown_classes)
    myclasses_names<-c(known_classes, unknown_classes)
    setNames(myclasses_names, myclasses_labels)

    outdf$class <- factor(outdf$class, levels=myclasses_names, labels = myclasses_labels)

  }
  if(flag_include_vartype) {
    all_values<-getOption('all_vartypes')
    outdf$vartype <- factor(outdf$vartype, levels=names(all_values), labels = names(all_values))
  } else {
    outdf <- outdf %>% select(-vartype)
  }

  setattr(outdf,'label', paste0('Structure of ', GetDBName(df, default_df_name)))
  return(outdf)
}

df_structure_difference<-function(df1, df2, attributes_to_ignore='',
                                  flag_comment_new_cols=TRUE, flag_comment_deleted_cols=TRUE,
                                  flag_explain_in_nice_names=TRUE,
                                  name_of_variable='question',
                                  name_of_variables='question'){
  df1_struct <- create_df_from_df_structure(df1, flag_add_nice_names = flag_explain_in_nice_names)
  df2_struct <- create_df_from_df_structure(df2, flag_add_nice_names = flag_explain_in_nice_names)

  ans<-gen_difference_df(df1=df1_struct, df2=df2_struct, df1_key = 'colname', df2_key = 'colname',
                         flag_include_new_rows = flag_comment_new_cols,
                         flag_include_deleted_rows = flag_comment_deleted_cols,
                         columns_to_ignore = attributes_to_ignore, name_of_cases=name_of_variables)

  ans$diffdb <- ans$diffdb %>% filter(status==0)

  a<-comment_diffs(ans, flag_comment_replace_NA = TRUE, flag_include_rownumbers = FALSE,
                   name_of_datasets = 'database structures',
                   name_of_case = name_of_variable, name_of_cases = name_of_variables,
                   name_of_variable = paste0(name_of_variable, ' property'),
                   name_of_variables = paste0(name_of_variables, ' properties'))

  return(a)
}
