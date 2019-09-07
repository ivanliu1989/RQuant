#' Summary Hypothesis Test
#'
#' @description
#' This function runs an hypothesis test to compare the distribution indicators (e.g. proportions, means, ratios)of two samples.
#' In addition it calculates the p-value, standard error and confidence interval
#'
#' @param pm_ds vector of distribution indicators of the populations to be compared
#' @param n_ds vector of data sizes of the distribution indicators to be compared
#' @param r_ds vector of ratios (in case the distribution indicator is a ratio)
#' @param lower.tail whether the hypothesis test is a lower tail test
#' @param std_dev standard deviation in case the distribution indicator is a mean
#' @param conf_lvl confidence level
#' @param distr_ind distribution indicator to be compared, e.g. proportion, mean or ratio
#'
#' @return The function does not return any value.  But it prints the outcome of the hypotesis test on screen
#'
#' @seealso \code{\link{str}}, \code{\link{}}, \code{\link{head}}
#'
#' @examples
#' #Proportions: fill the pm_ds and n_ds vectors with the 2 proportions and sample sizes respectively
#' test.hypothesis(pm_ds = c(0.0057, 0.0063), n_ds = c(6532, 58,774))
#'
#' #Percentages: as above and set the distribution indicator: distr_ind = "percentage"
#' test.hypothesis(pm_ds = c(0.57, 0.63), n_ds = c(6532, 58,774), distr_ind = "percentage")
#'
#' #Ratios: fill the pm_ds, n_ds and r_ds with the proportions, sample sizes and ratios respectively
#' test.hypothesis(pm_ds = c(0.0068, 0.0124, 0.0066, 0.0090), n_ds = c(6000, 54000, 6000, 54000), r_ds = c(1.82, 1.36), distr_ind = "ratio")
#'
#' #Means: fill the n_ds with the sample size; fill the pm_ds vector with the means to be compared.
#' #provide the standard deviation of the sample by setting std_dev
#' test.hypothesis(pm_ds= c(68, 72), n_ds = c(100), std_dev = 10, distr_ind = "mean")
#'
#' #Rates: fill the pm_ds with the weighted averages to be compared; set the distribution indicator
#' test.hypothesis(pm_ds= c(2.1, 1.8), distr_ind = "rate")
#'
#' @export
test.hypothesis <- function(pm_ds,
                            n_ds = NULL,
                            r_ds = NULL,
                            lower.tail = TRUE,
                            std_dev = NULL,
                            conf_lvl = 0.95,
                            distr_ind = "proportion"){

    se <- 0

    if(distr_ind == "proportion" || distr_ind == "percentage"){

        pm_ds_pooled <- (pm_ds[1]*n_ds[1] + pm_ds[2]*n_ds[2])/(n_ds[1] + n_ds[2])
        se <- sqrt((pm_ds_pooled*(ifelse(distr_ind == "percentage", 100, 1)-pm_ds_pooled)*(1/n_ds[1] + 1/n_ds[2])))

    }

    else if(distr_ind == "ratio"){

        se <- exp(sqrt(1/(pm_ds[1]*n_ds[1]) + 1/(pm_ds[2]*n_ds[2]) + 1/(pm_ds[3]*n_ds[3]) + 1/(pm_ds[4]*n_ds[4])))

    }

    else if(distr_ind == "mean"){

        se <- std_dev/sqrt(n_ds[1])

    }

    else if(distr_ind == "rate"){

        se <- sqrt(pm_ds[2])

    }

    if (se > 0){

        z <- ifelse(distr_ind == "ratio", abs((r_ds[1] - pm_ds[2])/se), abs((pm_ds[1] - pm_ds[2])/se))

        p_value <- ifelse(distr_ind == "rate", 1 - ppois(pm_ds[1], pm_ds[2], lower.tail = lower.tail), 1 - pnorm(z, lower.tail = lower.tail))
        ling_var <- ifelse(p_value < 1-conf_lvl, "a", "no")
        cat("There is", ling_var, "statistically significant difference between",
            ifelse(distr_ind == "ratio", r_ds[1], pm_ds[1]), "and", ifelse(distr_ind == "ratio", r_ds[2], pm_ds[2]),
            "; the p-value is:", p_value,
            "; the standard error is:", se,
            "; the confidence interval is:", qnorm(conf_lvl, lower.tail = lower.tail)*se )

    }

    else {print("Cannot establish statistical significance")}

}
