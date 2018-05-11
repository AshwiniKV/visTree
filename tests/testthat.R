library(testthat)
library(visTree)
library(partykit)
context("Prediction")

n <- 100
set.seed(10)
x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
Z<-matrix(cbind(x1,x2,x3), ncol = 3)
colnames(Z) <- paste0("x",1:3)

Bl<-matrix(c(B1 = 1, B2 = 2, B3 = 1.5), nrow = 3)
Y<-rnorm(n,mean = Z%*%Bl,sd = 1)
Yl<-cbind(Z,Y)
Y.trainl<-as.data.frame(Yl)

# Conditional Inference tree
cond.tree1<-ctree(Y~x1+x2+x3, data = Y.trainl, control = ctree_control(mincriterion = 0.65))

first_termnode<-partykit::nodeids(cond.tree1, termina= TRUE)[1]
obs.first_tnode<-which(data_party(cond.tree1)$"(fitted)" == first_termnode)
term_mean<-mean(data_party(cond.tree1)$"(response)"[obs.first_tnode])

test_that("Prediction_terminal node",{
  expect_identical(ptree_y(cond.tree1,first_termnode), term_mean)
})

