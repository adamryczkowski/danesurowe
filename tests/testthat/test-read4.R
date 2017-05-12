library(testthat)
setwd('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat')
context("Test reads")

baseline<-readRDS('baseline4.rds')
ourversion<-danesurowe::readDaneSurowe4('dane surowe2d.xlsm')

test_that("Read was successfull", ourversion<-danesurowe::readDaneSurowe4('dane surowe2d.xlsm'))

test_that("Errors and warnings are the same as baseline",
          {
            expect_equal(baseline$warnings, ourversion$warnings)
            expect_equal(baseline$errors, ourversion$errors)
          })

test_that("Database the same as baseline",
          {
            expect_equal(baseline$dt, ourversion$dt)
          })

