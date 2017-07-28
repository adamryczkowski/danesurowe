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

  valid_db <- diff_db$diffdb %>% filter(status==0)

  dest_db <- copy(df2)

  if(nrow(valid_db)>0) {
    for(i in seq(1, nrow(valid_db))) {
      browser()
    }
  }
  unvalid_db <- diff_db$diffdb %>% filter(status!=0)
  return(list(db = dest_db, unprocessed_rows = unvalid_db))
}
