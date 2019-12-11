context("EPE")

test_that(
  "Expected prediction error with MC simulation",
  {
    mc_sample <- mc_data(rnorm, 10, 50)
    # y = 1 + 3 * x + eps
    epe <- compute_epe(
      mc_sample,
      rnorm,
      5,
      function(x) {1 + 3 * x},
      rnorm,
      "mc", "x", "y",
      "squared",
      distribution = FALSE,
      lm,
      y ~ x
    )
    expect_vector(epe)
    expect_length(epe, 1)
  }
)
