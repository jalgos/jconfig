context("Configuration loading from file")

test_that("Loading configs from R files works", {
    config <- load.config("config.R")
    expect_equal(config$a, 2)
    expect_equal(config$b[[1]]$param, .1)
    expect_equal(config$b[[2]]$param, .2)
    expect_equal(config$d, "test")
    expect_equal(config$e, 1:5)
})

test_that("Loading configs from R files works", {
    config <- load.config("config.json")
    expect_equal(config$e, 1:5)
    expect_equal(config$mongo_host, "localhost:11111")
    expect_equal(config$d, "test")
})

test_that("Overwriting fields combined with including works ",
{
    config <- load.config("overwriting.R")
    expect_equal(config$d, "overwritten")
    
    dconfig <- load.config("doubleincluding.R")
    expect_equal(dconfig$d, "overwritten")
    
    overconfig <- load.config("overwritting_sublevel.R")
    expect_equal(overconfig$d, "over_level_overwritten")
    expect_equal(overconfig$e, 1:5)

    mealconfig <- load.config("including_unnamed.R")
    expect_equal(length(mealconfig$ingredients), 2)
    expect_equal(mealconfig$ingredients[[2]]$name, "tomato")
})
