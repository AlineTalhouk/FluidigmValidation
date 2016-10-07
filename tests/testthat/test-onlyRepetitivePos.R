context("onlyRepetitivePos")

test_that("onlyReptitivePos works", {
  toRepeat<-rep(c(1,2,3,4,5,6),6)
  noRepeat<-c(7,8,9,10)
  set.seed(888)
  POS<-sample(c(toRepeat,noRepeat),replace=FALSE)
  val<-runif(40)
  DP<-sample(1:10000,size=40)
  testDf<-data.frame(POS,val,DP)
  testDf<-testDf[order(testDf$POS),]
  #In testDf, only the first 36 rows are repetitive
  testDf.rep<-onlyRepetitivePos(testDf)
  checkTrue(nrow(testDf.rep)==36)
  expect_equal(sum(!testDf.rep$POS==testDf$POS[1:36]),0)
  expect_equal(sum(!testDf.rep$val==testDf$val[1:36]),0)
  expect_equal(sum(!testDf.rep$DP==testDf$DP[1:36]),0)
  expect_error(onlyRepetitivePos(subset(testDf,select=-POS)))
  mostExpensiveCities<-c("San Jose","Shenzhen","Vancouver","New York","Shanghai","Toronto","San Francisco")
  expect_error(onlyRepetitivePos(data.frame(POS=sample(mostExpensiveCities,40,replace=TRUE),val=val,DP=DP)))
})

test_that("onlyRepetitivePos throws error with wrong inputs",{
  toRepeat<-rep(c(1,2,3,4,5,6),6)
  noRepeat<-c(7,8,9,10)
  set.seed(888)
  POS<-sample(c(toRepeat,noRepeat),replace=FALSE)
  val<-runif(40)
  DP<-sample(1:10000,size=40)
  testDf<-data.frame(POS,val,DP)
  testDf<-testDf[order(testDf$POS),]
  expect_error(onlyRepetitivePos(subset(testDf,select=-POS)))
  mostExpensiveCities<-c("San Jose","Shenzhen","Vancouver","New York","Shanghai","Toronto","San Francisco")
  expect_error(onlyRepetitivePos(data.frame(POS=sample(mostExpensiveCities,40,replace=TRUE),val=val,DP=DP)))
})
