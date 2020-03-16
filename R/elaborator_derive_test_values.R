#' elaborator_derive_test_values - derives p-values/estimates for sign-test or t-test (by applying functions specified above)
#'
#'@param data data set
#'@param signtest boolean value if signtest should be performded (TRUE/FALSE)
#'@param Visit1 visit time point one
#'@param Visit2 visit time point two
#'@param lab_column lab parameter column
#'
#'@keywords internal

elaborator_derive_test_values <- function(data = data,
                           signtest = TRUE,
                           Visit1 = c("Randomization"),
                           Visit2 = c("End of Treatment"),
                           lab_column) {

  infotest <- rep(list(list(p.value = NULL, estimate = NULL)), length(Visit2))

  for(i in 1:length(Visit2)) {

    pval <- sapply(levels(data$TRTP), function(treat){
      sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

        if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
          res <- ifelse(signtest == TRUE,
                        elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value,
                        elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value
          )
        } else {res <- NA}
      })
    })
    pval <- matrix(pval,length(levels(as.factor(as.character(data[, lab_column])))), length(levels(data$TRTP)))
    rownames(pval) <- levels(as.factor(as.character(data[, lab_column])))
    colnames(pval) <- levels(data$TRTP)

    esti <- sapply(levels(data$TRTP), function(treat){
      sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

        if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
          res <- ifelse(signtest == TRUE,
                        as.numeric(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate),
                        as.numeric(elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate))
        } else {res <- NA}
      })
    })
    esti <- matrix(esti, length(as.character(unique(data$PARAMCD))),
                   length(levels(data$TRTP)))
    rownames(esti) <- as.character(unique(data$PARAMCD))
    colnames(esti) <- levels(data$TRTP)

    infotest[[i]]$p.value <- pval
    infotest[[i]]$estimate <- esti
  }

  infotest
}
