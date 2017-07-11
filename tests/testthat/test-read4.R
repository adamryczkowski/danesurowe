library(testthat)
setwd('/home/Adama-docs/Adam/MyDocs/Statystyka/Maszyny/Dab2/R/danesurowe/tests/testthat')
context("Test reads")

df<-danesurowe::readDaneSurowe3('/home/Adama-docs/Adam/MyDocs/praca/masia@ibib/ALSdatabase/LISB/edit format/als (2)_Lisbon reviewed_marta_3april.xlsm')


baseline<-readRDS('baseline4.rds')
debugonce(readDaneSurowe4)
dt<-ourversion<-readDaneSurowe4('dane surowe2d_2.xlsm')
GetProblems(dt)
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

