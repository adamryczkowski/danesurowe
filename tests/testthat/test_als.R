library(testthat)
setwd('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat')
context("Test that reads the whole ALS database")

filename<-'/home/adam/Documents/praca/masia@ibib/ALSdatabase/data/dane surowe2d.xlsm'
dt<-danesurowe::readDaneSurowe4(filename)
