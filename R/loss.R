#' Loss operation
#'
#' @description
#' Binary operation method for character
#' @param lhs numeric or character
#' @param rhs numeric or character
#' @return
#' For numeric, compute division. Otherwise, misclassification.
#' @details
#' This function can deal with both regression and classification problem.
#' @export
`%m%` <- function(lhs, rhs) {
  if (is.numeric(lhs) & is.numeric(rhs)) {
    lhs - rhs
  } else {
    lhs == rhs
  }
}

#' Loss function
#'
#' @description
#' Various loss functions
#' @param lhs numeric or character
#' @param rhs numeric or character
#' @param error choice of error. See 'Details'.
#' @return
#' Error between numeric or class.
#' @details
#' This function can compute squared error (\code{"squared"}) and absolute error (\code{"absolute"}) now.
#' @export
loss <- function(lhs, rhs, error = c("squared", "absolute")) {
  error <- match.arg(error)
  if (error == "squared") {
    mean( (lhs %m% rhs)^2 )
  } else if (error == "absolute") {
    mean( abs(lhs %m% rhs) )
  }
}
