#' elaborator_perform_binomial_test - calculates binomial test for the change of a labparameter from one visit to another
#'
#'@param data data set
#'@param treatment treatment variable
#'@param lab_parameter llab parameter variable of the data set
#'@param Visit1 visit time point one
#'@param Visit2 visit time point two
#'@param lab_column lab parameter column
#'
#'@keywords internal

elaborator_perform_binomial_test <- function(data, treatment, lab_parameter, Visit1 = "Randomization", Visit2 = "End of Treatment", lab_column){

  datasub <- data[data$TRTP == treatment & !is.na(data$TRTP) & data[,lab_column] == lab_parameter,]

  incr <- unlist(sapply(unique(datasub$SUBJIDN), function(z){

    tmp1 <- datasub$SUBJIDN == z & datasub$AVISIT == Visit1
    tmp2 <- datasub$SUBJIDN == z & datasub$AVISIT %in% Visit2

    res <- datasub$LBORRES[datasub$SUBJIDN == z & datasub$AVISIT == Visit1] <
                           datasub$LBORRES[datasub$SUBJIDN == z & datasub$AVISIT %in% Visit2]

    if (length(res) == 0 || is.na(res)) return(NA) else if (res == FALSE) {
      if (datasub$LBORRES[tmp1] ==
         datasub$LBORRES[tmp2]) return(NA)
    }
    return(res)
  }))

  if (sum(is.na(incr)) < length(incr)) {
    testres <- stats::binom.test(x = sum(incr, na.rm = TRUE), n = sum(!is.na(incr)), p = 0.5)
  } else {testres <- NA}
}

