library(testthat)
setwd('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat')
context("Test df difference")

test_that("Check for correct flag of entirely removed rows and columns", {
  df1<-tibble::tibble(field1=paste0('resp',1:5), var1=10*1:5, var2=0.1*1:5)
  df2<-rbind(df1[c(1,2,4,5),], c(field1='respX', var1=1500, var2=pi))

  df_difference(df1 = df1, df2 = df2, df1_key = 'field1', df2_key = 'field1', return_format = 'md')
})

