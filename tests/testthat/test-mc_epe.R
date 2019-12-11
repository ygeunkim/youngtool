context("EPE")

test_that(
  "Expected prediction error with MC simulation",
  {
    mc_sample <- mc_data(rnorm, 10, 50)
    # y = 1 + 3 * x + eps
    epe <- compute_epe(
      data = mc_sample,
      randx = rnorm,
      testn = 5,
      fit = function(x) {1 + 3 * x},
      randy = rnorm,
      mcname = "mc", xname = "x", yname = "y",
      error = "squared",
      distribution = FALSE,
      mod = lm,
      formula = y ~ x
    )
    expect_vector(epe)
    expect_length(epe, 1)
  }
)
