library(testthat)
library(visTree)
library(partykit)
context("Prediction")

n <- 100
set.seed(10)
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
Z <- matrix(cbind(x1, x2, x3), ncol = 3)
colnames(Z) <- paste0("x", 1:3)

Bl <- matrix(c(B1 = 1, B2 = 2, B3 = 1.5), nrow = 3)
Y <- rnorm(n, mean = Z %*% Bl, sd = 1)
Yl <- cbind(Z, Y)
Y.trainl <- as.data.frame(Yl)

# Conditional Inference tree
cond.tree1 <- ctree(Y ~ x1 + x2 + x3, data = Y.trainl, control = ctree_control(mincriterion = 0.65))

first_termnode <- partykit::nodeids(cond.tree1, terminal = TRUE)[1]
obs.first_tnode <- which(data_party(cond.tree1)$"(fitted)" == first_termnode)
term_mean <- mean(data_party(cond.tree1)$"(response)"[obs.first_tnode])
choose_inner <- nodeids(cond.tree1, terminal = FALSE) %in% nodeids(cond.tree1, terminal = TRUE)
index_left <- nodeids(cond.tree1)[!choose_inner][2]
index_right <- nodeids(cond.tree1)[!choose_inner][5]

test_that("Prediction_terminal node", {
  expect_equal(ptree_y(cond.tree1, first_termnode), term_mean)
})

test_that("Inner nodes - Left split", {
  expect_equal(ptree_left(cond.tree1, 1), nodeids(cond.tree1)[index_left])
})

test_that("Inner nodes - Right split", {
  expect_equal(ptree_right(cond.tree1, 1), nodeids(cond.tree1)[index_right])
})

first_split <- strsplit(partykit:::.list.rules.party(cond.tree1)[1], " & ")[[1]][1]
test_that("Split_node", {
  expect_equal(ptree_criteria(cond.tree1, 1, TRUE), first_split)
})
