context("Configuration loading from file")

test_that("Loading configs from R files works", {
    config <- load.config("config.R")
    expect_equal(config$a, 2)
    expect_equal(config$b[[1]]$param, .1)
    expect_equal(config$b[[2]]$param, .2)
    expect_equal(config$d, "test")
    expect_equal(config$e, 1:5)
})
