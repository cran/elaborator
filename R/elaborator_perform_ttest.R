#' elaborator_perform_ttest - performs t-test for changes of a labparameter from one visit to another
#'
#'@param data data set
#'@param treatment treatment variable of the data set
#'@param lab_parameter lab parameter variable of the data set
#'@param visit1 visit Time point one
#'@param visit2 visit Time point two
#'@param lab_column lab parameter column
#'
#'@keywords internal

elaborator_perform_ttest <- function(data, treatment, lab_parameter, Visit1 = "Randomization", Visit2 = "End of Treatment", lab_column){

  datasub <- data[data$TRTP == treatment & data[,lab_column] == lab_parameter,]

  differ <- unlist(sapply(unique(datasub$SUBJIDN), function(z){
    res <- datasub$LBORRES[datasub$SUBJIDN == z & datasub$AVISIT == Visit1] -
      datasub$LBORRES[datasub$SUBJIDN == z & datasub$AVISIT == Visit2]
    if (length(res) == 0 || is.na(res)) return(NA) else return(res)
  }))
  testres <- stats::t.test(x = differ)
}
