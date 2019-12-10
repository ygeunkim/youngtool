context("Optimism")

test_that(
  "Optimism with MC simulation",
  {
    mc_sample <- mc_data(rnorm, 10, 50)
    # y = 1 + 3 * x + eps
    insample <- compute_insample(
      mc_sample,
      5,
      function(x) {1 + 3 * x},
      rnorm,
      "squared",
      lm,
      y ~ x
    )
    expect_s3_class(insample, c("data.table", "data.frame"))
    expect_named(insample, c("training", "insample", "optimism"))
  }
)
