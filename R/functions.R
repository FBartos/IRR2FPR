#' @title Compute the proportion of correctly selected candidates
#' @description Compute proportion of correctly selected candidates
#' based on the inter-rater reliability and the
#' proportion of selected candidates
#' @param IRR The inter-rater reliability
#' @param proportion_selected The proportion of selected candidates
#'
#' @return The proportion of correctly selected candidates
#' @export
compute_proportion_of_correctly_selected <- function(IRR, proportion_selected){

  if(any(IRR < 0) || any(IRR > 1))
    stop("IRR must be between 0 and 1.")
  if(any(proportion_selected < 0) || any(proportion_selected > 1))
    stop("proportion_selected must be between 0 and 1.")

  if(length(IRR) == 1 && length(proportion_selected) == 1){
    return(compute_proportion_of_correctly_selected.fun(IRR, proportion_selected))
  }else if(length(IRR) == 1){
    return(sapply(proportion_selected, function(x) compute_proportion_of_correctly_selected.fun(IRR, x)))
  }else if(length(proportion_selected)){
    return(sapply(proportion_selected, function(x) compute_proportion_of_correctly_selected.fun(x, proportion_selected)))
  }else{
    stop("Only IRR or the proportion_selected can have more than one element.")
  }

}

compute_proportion_of_correctly_selected.fun <- function(IRR, proportion_selected){
  pC <- mvtnorm::pmvnorm(
    lower = c(stats::qnorm(proportion_selected, 0, sqrt(IRR)), stats::qnorm(proportion_selected, 0, 1) ),
    upper = c(Inf, Inf),
    sigma = matrix(c(
      IRR,
      IRR,
      IRR,
      1),
      nrow = 2))

  return(as.numeric(pC))
}

#' @title Compute the true positive rate
#' @description Compute the true positive rate
#' based on the inter-rater reliability and the
#' proportion of selected candidates
#' @param IRR The inter-rater reliability
#' @param proportion_selected The proportion of selected candidates
#'
#' @return The true positive rate
#' @export
compute_true_positive_rate <- function(IRR, proportion_selected){

  return(compute_proportion_of_correctly_selected(IRR, 1 - proportion_selected) / proportion_selected)
}

#' @title Compute the false positive rate
#' @description Compute the false positive rate
#' based on the inter-rater reliability and the
#' proportion of selected candidates
#' @param IRR The inter-rater reliability
#' @param proportion_selected The proportion of selected candidates
#'
#' @return The false positive rate
#' @export
compute_false_positive_rate <- function(IRR, proportion_selected){

  return((proportion_selected - compute_proportion_of_correctly_selected(IRR, 1 - proportion_selected)) / proportion_selected)
}

#' @title Compute the false negative rate
#' @description Compute the false negative rate
#' based on the inter-rater reliability and the
#' proportion of selected candidates
#' @param IRR The inter-rater reliability
#' @param proportion_selected The proportion of selected candidates
#'
#' @return The false negative rate
#' @export
compute_false_negative_rate <- function(IRR, proportion_selected){

  return((proportion_selected - compute_proportion_of_correctly_selected(IRR, 1 - proportion_selected)) / (1 - proportion_selected))
}

#' @title Compute IRR from the Spearman-Brown formula
#' @description Compute the inter-rater reliability
#' based on the Spearman-Brown formula
#'
#' @param IRR_1 The inter-rater reliability of the first rater
#' @param n_raters The number of raters
#'
#' @return The inter-rater reliability
#' @export
spearman_brown_formula <- function(IRR_1, n_raters){
  return(n_raters * IRR_1 / (1 + (n_raters - 1) * IRR_1))
}

