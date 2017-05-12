library(testthat)
setwd('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat')
context("Test reads")

baseline<-readRDS('baseline.rds')
ourversion<-danesurowe::readDaneSurowe3('dane surowe2c.xlsm')
ALSdatabase::do_compare(dfcontrol,ourversion)
test_that("Number of cases", expect_equal(nrow(baseline), nrow(ourversion)))

test_that("Number of variables", expect_equal(ncol(baseline), ncol(ourversion)))

test_that("Names of variables", expect_equal(colnames(baseline), colnames(ourversion)))

test_that("Labels of variables", expect_equal(
  plyr::laply(baseline, function(var) attr(var,'label')),
  plyr::laply(ourversion, function(var) attr(var,'label'))))

test_that("Units of variables", expect_equal(
  plyr::laply(baseline, function(var) attr(var,'unit')),
  plyr::laply(ourversion, function(var) attr(var,'unit'))))

