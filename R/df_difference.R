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
df_difference<-function(df1, df2, df1_key=NULL, df2_key=NULL, columns_to_ignore=NULL)
{
  if (!'data.frame' %in% class(df1))
  {
    ans<-list(message='First argument is not a data.frame!', result=FALSE)
    return(ans)
  }
  if (!'data.frame' %in% class(df2))
  {
    ans<-list(message='Second argument is not a data.frame!', result=FALSE)
    return(ans)
  }

  # Algorithm:
  # 1. Get common set of column names and assume that column that match by name must be equivalent

  df1_names<-colnames(df1)
  df2_names<-colnames(df2)

  common_colnames<-intersect(df1_names,df2_names)

  if(!is.null(columns_to_ignore))
  {
    common_colnames <- setdiff(common_colnames,columns_to_ignore)
  }

  # 2. Get common set of rownames

  if(!is.null(df1_key))
  {
    if (df1_key %in% colnames(df1))
    {
      df1_rownames <- df1[[df1_key]]
    } else {
      stop(paste0("There is no column '", df1_key, "' in the first data frame"))
    }
  } else {
    if(all(rownames(df1) == seq_along(rownames(df1))))
    {
      if (nrow(df1) != nrow(df2))
      {
        stop("Number of rows in datasets differ, and there is no key variable specified for the first data frame.")
      } else {
        df1_rownames<-seq(nrow(df1))
      }
    } else {
      df1_rownames <- rownames(df1)
    }
  }


  if(!is.null(df2_key))
  {
    if (df2_key  %in% colnames(df2) )
    {
      df2_rownames <- df2[[df2_key]]
    } else {
      stop(paste0("There is no column '", df2_key, "' in the first data frame"))
    }
  } else {
    if( all(rownames(df2) == seq_along(rownames(df2))))
    {
      if (nrow(df1) != nrow(df2))
      {
        stop("Number of rows in datasets differ, and there is no key variable specified for the second data frame")
      } else {
        df2_rownames<-seq(nrow(df2))
      }
    } else {
      df2_rownames <- rownames(df2)
    }
  }


  common_rownames <- sort(intersect(df1_rownames, df2_rownames))
  df1_selected_rows <- match(common_rownames, df1_rownames)
  df2_selected_rows <- match(common_rownames, df2_rownames)

  df1_sorted <- df1[df1_selected_rows]
  df2_sorted <- df2[df2_selected_rows]

  # 3. Do matching among rows using either keys or rowname matching.

  diffdb<-numeric(0)
  for(colname in common_colnames)
  {
#    if(colname=='q_52a') browser()
    var1<-df1_sorted[[colname]]
    var2<-df2_sorted[[colname]]
    if (length(setdiff(class(var1), class(var2)))>0)
    {
      classes_sorted <- paste0(sort(
        c(
          paste0(sort(class(var1)), collapse=','),
          paste0(sort(class(var2)), collapse=',')
        )),collapse='|')
      if(classes_sorted == 'factor|labelled')
      {
        if (class(var1)=='factor')
        {
          var1 <- labelled::to_labelled(var1)
        } else {
          var2 <- labelled::to_labelled(var2)
        }
      } else if (classes_sorted=='Date|numeric')
      {
        if(class(var1)=='numeric') {
          var1 <- as.Date(var1, origin="1899-12-30")
        } else {
          var2 <- as.Date(var2, origin="1899-12-30")
        }
      } else if (classes_sorted=='character|numeric')
      {
        if(class(var1)=='character') {
          var1 <- as.numeric(var1)
        } else {
          var2 <- as.numeric(var2)
        }
      } else if (classes_sorted=='integer|numeric')
      {
        if(class(var1)=='integer') {
          var1 <- as.numeric(var1)
        } else {
          var2 <- as.numeric(var2)
        }
      } else if (classes_sorted=='integer|logical')
      {
        if(class(var1)=='logical') {
          var1 <- as.integer(var1)
        } else {
          var2 <- as.integer(var2)
        }
      } else if (classes_sorted=='labelled|numeric')
      {
        if(class(var1)=='numeric') {
          labels1 <- na.omit(unique(var1))
          if(length(labels1)==0)
          {
            labels1=c('0')
          }
          names(labels1) <- labels1
          var1 <- labelled::labelled(var1, labels1)
        } else {
          labels2 <- na.omit(unique(var2))
          if(length(labels2)==0)
          {
            labels2=c('0')
          }
          names(labels2) <- labels2
          var2 <- labelled::labelled(var2, labels2)
        }
      } else {
        browser()
        stop(paste0("I don't know how to compare between classes ", classes_sorted))
      }
    }

    comp_var<-danesurowe:::compareNA(var1,var2)
    if(sum(is.na(comp_var)>0))
      browser()
    if(length(diffdb)==0)
    {
      diffdb <- data.table(var=comp_var)
      data.table::setnames(diffdb, 'var', colname)
      dt_out1 <- data.table(var=var1)
      data.table::setnames(dt_out1, 'var', colname)
      dt_out2 <- data.table(var=var2)
      data.table::setnames(dt_out2, 'var', colname)
    } else {
      diffdb[,(colname):=comp_var]
      dt_out1[, (colname):=var1]
      dt_out2[, (colname):=var2]
    }

  }

  diff_matrix=data.matrix(diffdb)

  ile_mismatchy <- sum(diff_matrix)

  while(ile_mismatchy>0)
  {
    mismatched_rows<-plyr::aaply(diff_matrix,1,sum)
    mismatched_cols<-plyr::aaply(diff_matrix,2,sum)

    worst_row<-which.max(mismatched_rows)
    worst_col<-which.max(mismatched_cols)

    if(mismatched_rows[worst_row]>mismatched_cols[worst_col])
    {
      #one row
      browser()
      case_value_pairs<-format_difference_in_columns(dt_out1[worst_row], dt_out2[worst_row])
    } else {
      #one column
      worst_colname=names(worst_col)
      where_mismatch<-which(diffdb[[worst_colname]]==1)
      var1<-dt_out1[[worst_colname]][where_mismatch]
      var2<-dt_out2[[worst_colname]][where_mismatch]
      var1<-labelled::copy_labels(df1_sorted[[worst_colname]], var1)
      var2<-labelled::copy_labels(df2_sorted[[worst_colname]], var2)

      #labelled::val_labels(var1)<-labelled::val_labels(dt_out1[[worst_colname]])
      #labelled::val_labels(var2)<-labelled::val_labels(dt_out2[[worst_colname]])
      msg <- paste0("Corrections to the variable ",
                    Hmisc::label(df2[[worst_colname]]), " (", worst_colname, "): ",
                    format_case_value_diff_list(case_names = common_rownames[where_mismatch],
                                                values1 = var1,
                                                values2 = var2,
                                                flag_skip_nothing = TRUE))
      diff_matrix[,worst_col] <- 0
      dt_out2[,(worst_colname):=dt_out1[[worst_colname]] ]
    }
    ile_mismatchy <- sum(diff_matrix)
  }


  # 2. We treat all the mismatched columns as columns that are compared to full NA, so only non-missing elements get reported
  #
  # If there are columns with incompatible types or effectively uncomaptible labels warn user and do - what? I need a list of
  #    rules to apply to make all column come to the common denominator.


  # 3. Do matching among rows using either keys or rowname matching. Non-matched rows tread as rows matched with NA.

  # 3. Create a matrix of m×n, where m = number of all distinct column names in both datasets, and n = number of all distinct
  #    rownames

  # 4. Fill the matrix with either 0 or 1, where 1 = no match and 0 = match. Dusing matching pay attention to type of both columns
  #    - they might not match.

  # 5. Calculate marginal sum of rows and marginal sum of columns of this matrix and sort the matrix by both keys horizontally
  #    and vertically.
  #
  # 6.
}

