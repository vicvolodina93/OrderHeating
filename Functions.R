#' Compute generalized simplex with k = 3.
#'
#' @param x tibble with two columns \code{NPC} and \code{Emissions}.
#'
#' @return a vector of generalized simplex values computed for \code{x}.
#' @export
#'
#' @examples
simplex_calc <- function(x) {
  
  n <- dim(x)[1]
  mm <- combn(n, 3)
  res <- rep(0, dim(mm)[2])
  for(i in 1:dim(mm)[2]) {
    A_mat <- rbind(c(x$NPC[mm[1, i]], x$Emissions[mm[1, i]], 1),
                   c(x$NPC[mm[2, i]], x$Emissions[mm[2, i]], 1),
                   c(x$NPC[mm[3, i]], x$Emissions[mm[3, i]], 1))
    res[i] <- 0.5*determinant(A_mat, logarithm = FALSE)$modulus[1]
  }
  return(res)
}

#' Perform Kolmogorov-Smirnov (KS) test to test first-order stochastic ordering
#' between design options under the selected scenario.
#'
#' @param df tibble with \code{Scenario} and \code{Design} columns.
#' @param scenario a choice of scenario.
#' @param locate an integer corresponding to the column number in \code{df},
#' which contains the variable of interest.
#'
#' @return a list of size 3. Each list component contains the value of KS 
#' distance and p-value. The design options are chosen in turn and appear
#' on the left hand side in the first-order stochastic ordering. For instance, 
#' we test whether design option 1 is dominated by design option 2 and 3, and 
#' provide the corresponding KS distance and p-values in the first component of
#' the list.
#' @export
#'
#' @examples
kolmogorov_smirnov_test <- function(df, scenario, locate) {
  
  
  design_names <- c("design1", "design2", "design3")
  df1 <- filter(df, Scenario == scenario)
  res <- list()
  for(i in 1:length(design_names)) {
    vec_mod <- design_names[-i]
    res_mod <- list()
    for(j in 1:length(vec_mod)) {
      test_D <- ks.test(filter(df1, Design == design_names[i])[locate][[1]],
                        filter(df1, Design == vec_mod[j])[locate][[1]])
      test_p <- ks.test(filter(df1, Design == design_names[i])[locate][[1]],
                        filter(df1, Design == vec_mod[j])[locate][[1]],
                        alternative = c("less"))
      res_mod[[j]] <- tibble(design = vec_mod[j],
                             D = test_D$statistic,
                             p_value = test_p$p.value)
    }
    res[[i]] <- res_mod
  }
  return(res)
}

#' Perform Kolmogorov-Smirnov (KS) test to test first-order stochastic ordering
#' between scenarios under the selected design option.
#'
#' @param df tibble with \code{Scenario} and \code{Design} columns.
#' @param design a choice of design option.
#' @param locate an integer corresponding to the column number in \code{df},
#' which contains the variable of interest.
#'
#' @return a list of size 3. Each list component contains the value of KS 
#' distance and p-value. The scenarios are chosen in turn and appear
#' on the left hand side in the first-order stochastic ordering. For instance, 
#' we test whether Market scenario is dominated by Neutral and Green scenarios 
#' under the selected design option, and provide the corresponding KS distance 
#' and p-values in the first component of the list.
#' @export
#'
#' @examples
kolmogorov_smirnov_test_sc <- function(df, design, locate) {
  
  scenario_names <- c("Market", "Neutral", "Green")
  df1 <- filter(df, Design == design)
  res <- list()
  for(i in 1:length(scenario_names)) {
    vec_mod <- scenario_names[-i]
    res_mod <- list()
    for(j in 1:length(vec_mod)) {
      test_D <- ks.test(filter(df1, Scenario == scenario_names[i])[locate][[1]],
                        filter(df1, Scenario == vec_mod[j])[locate][[1]])
      test_p <- ks.test(filter(df1, Scenario == scenario_names[i])[locate][[1]],
                        filter(df1, Scenario == vec_mod[j])[locate][[1]],
                        alternative = c("less"))
      res_mod[[j]] <- tibble(Scenario = vec_mod[j],
                             D = test_D$statistic,
                             p_value = test_p$p.value)
    }
    res[[i]] <- res_mod
  }
  return(res)
}

