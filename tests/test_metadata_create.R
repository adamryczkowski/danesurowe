library(testthat)
library(depwalker)

context("Creation of task's metadata")

test_that("Test create metadata", {code<-"x<-1:10";
create.metadata(code, "/tmp/task1")})
