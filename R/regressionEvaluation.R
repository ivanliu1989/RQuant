#' Evaluate Regression Model Results
#'
#' Evaluate the performance of a regression model.
#'
#' @param obs A vector of actual outcomes.
#' @param pred A vector of fitted values.
#' @param model Optional, the model used to predict \code{fitted values} from \code{actual outcomes}.
#' @param seed Random seeds for reproducibility.
#' @param sample_size the maximum sample size, in numbers or percents of total observations, will be used to visualise regression effects.
#'
#' @return A \code{list} of \code{data.table} and \code{ggplot2}
#' @examples
#' data(mtcars)
#' fit <- glm(mpg ~ cyl + disp + hp + drat, data=as.data.frame(mtcars), family = gaussian())
#' actuals <- mtcars$mpg
#' probs <- predict(fit, mtcars)
#' res <- regressionEvaluation(pred = probs,
#' obs = actuals,
#' model = fit,
#' sample_size=1e5,
#' seed = 1234)
#' res$descriptive_statistics
#' res$goodness_of_fit
#' res$model_details
#' res$visualisation$residual_vs_fitted
#' res$visualisation$residual_vs_order
#' res$visualisation$histograms
#' res$visualisation$prediction_intervals
#' res$check_model_assumptions
#'
#' @export
regressionEvaluation <- function(obs,
                                 pred,
                                 model = NULL,
                                 sample_size = 1000,
                                 seed = 1234){

    library(ggplot2)
    library(plyr)
    library(data.table)

    # Decriptive statistics ---------------------------------------------------
    # Number of obs
    n.obs <- length(obs)
    # Actual Percentile
    obs.summary <- summary(obs)
    # Pred Percentile
    pred.summary <- summary(pred)
    # Residuals
    residual <- obs - pred
    # Residuals Percentile
    residual.summary <- summary(residual)

    descriptive_statistics <- list(
        n.obs = n.obs,
        obs.summary = obs.summary,
        pred.summary = pred.summary,
        residual.summary = residual.summary
    )

    # Goodness-of-Fit ---------------------------------------------------------
    # Sum of Squares Total
    sum_of_squares_total <- sum((obs - mean(obs, na.rm = T))^2, na.rm = T)
    # Sum of Squares Regression
    sum_of_squares_regression <- sum((pred - mean(pred, na.rm = T))^2, na.rm = T)
    # Sum of Squares Error
    sum_of_squares_error <- sum((obs - pred)^2, na.rm = T)
    # explained variance score
    explained_variance <- 1 - var(obs - pred)/var(obs)
    # r.squared / coefficient of determination
    r_squared <- 1 - sum((obs-pred)^2, na.rm = T)/sum((obs-mean(obs,na.rm=T))^2, na.rm = T)
    # mean absolute error
    mean_absolute_error <- mean(abs(obs - pred), na.rm = T)
    # mean squared error
    mean_square_error <- mean((obs - pred)^2, na.rm = T)
    # returns Root Mean Squared Error
    root_mean_squared_error <- sqrt(mean((pred - obs)^2, na.rm = T))
    # median absolute error
    median_abosolute_error <- median(abs(obs - pred), na.rm = T)
    # AIC
    if(!is.null(model) & any(class(model)%in%c('lm','glm'))){
        # AIC, BIC
        AIC <- AIC(model)
    }else{
        AIC = NULL
    }
    # model details
    if(!is.null(model)){
        model_details <- summary(model)
    }else{
        model_details <- NULL
    }

    goodness_of_fit <- data.table(Metrics = c('sum_of_squares_total',
                                              'sum_of_squares_regression',
                                              'sum_of_squares_error',
                                              'explained_variance',
                                              'r_squared',
                                              'mean_absolute_error',
                                              'mean_square_error',
                                              'root_mean_squared_error',
                                              'median_abosolute_error',
                                              'AIC'),
                                  Value = round(c(sum_of_squares_total,
                                                  sum_of_squares_regression,
                                                  sum_of_squares_error,
                                                  explained_variance,
                                                  r_squared,
                                                  mean_absolute_error,
                                                  mean_square_error,
                                                  root_mean_squared_error,
                                                  median_abosolute_error,
                                                  AIC), 3)
    )


    # Sampling for plots ------------------------------------------------------
    if(!is.null(sample_size) & is.numeric(sample_size)){
        if(sample_size < 1 & sample_size > 0){
            sample_ix <- sample(1:n.obs, min(n.obs, n.obs * sample_size))
        }else if(sample_size > 0){
            sample_ix <- sample(1:n.obs, min(n.obs, sample_size))
        }else{
            warnings('Wrong number for sample size.')
            sample_ix <- 1:n.obs
        }
        obs <- obs[sample_ix]
        pred <- pred[sample_ix]
        residual <- residual[sample_ix]
    }

    # Plots -------------------------------------------------------------------
    # t1<-theme(
    #     axis.title.x = element_text(face="bold", color="black", size=10),
    #     axis.title.y = element_text(face="bold", color="black", size=10),
    #     plot.title = element_text(face="bold", color = "black", size=12)
    # )
    df <- data.table(residual = residual,
                     pred = pred,
                     obs = obs)

    # residuals versus the fitted valued
    rvf_p <- ggplot(df, aes(x = pred, y = residual)) +
        theme_rquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        geom_point(size=2, aes(color = abs(residual))) +
        geom_hline(yintercept = 0, color="black") +
        scale_color_continuous(name="Residuals",
                               breaks = with(df, c(min(abs(residual)), median(abs(residual)), max(abs(residual)))),
                               labels = c("min", "median", "max")) +
        geom_smooth() +
        # t1 +
        labs(x="Fitted value", y = "Residuals", title= "Residuals vs the fitted values")

    # histogram of the residuals, actuals and fitted
    hist.df <- data.table(
        type=factor(c(rep('obs', length(obs)), rep('pred', length(pred)), rep('residual', length(residual)))),
        value=c(obs, pred, residual)
    )
    mu <- ddply(hist.df, "type", summarise, grp.mean=mean(value))
    hist_p <- ggplot(hist.df, aes(x=value, fill=type)) +
        theme_rquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth=1)+
        geom_density(alpha=0.6)+
        geom_vline(data=mu, aes(xintercept=grp.mean),
                   linetype="dashed")+
        labs(title="Histogram of the actual, fitted and residuals",x="Value", y = "Density")


    # residuals versus the order of the data
    rvo_p <- ggplot(df, aes(x = 1:length(residual), y = residual)) +
        theme_rquantquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        geom_point(size=2, aes(color = abs(residual))) +
        geom_line() +
        geom_hline(yintercept = 0, color="black") +
        scale_color_continuous(name="Residuals",
                               breaks = with(df, c(min(abs(residual)), median(abs(residual)), max(abs(residual)))),
                               labels = c("min", "median", "max")) +
        labs(x="Observation Order", y = "Residuals", title= "Residuals vs the order of the data")


    # Prediction Intervals (95%)
    pi_p <- ggplot(df, aes(x = pred, y = obs)) +
        theme_rquantquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        geom_point(size=2, aes(color = abs(residual))) +
        scale_color_continuous(name="Residuals",
                               breaks = with(df, c(min(abs(residual)), median(abs(residual)), max(abs(residual)))),
                               labels = c("min", "median", "max")) +
        geom_smooth() +
        labs(x="Fitted values", y = "Observed values", title= "Observed vs fitted values")


    visualisation <- list(
        residual_vs_fitted = rvf_p,
        residual_vs_order = rvo_p,
        histograms = hist_p,
        prediction_intervals = pi_p
    )


    # glm.check.assumptions ---------------------------------------------------
    if(!is.null(model) & any(class(model) %in% c('glm'))){
        check_model_assumptions <- check.glm.assumptions(model)
    }else(
        check_model_assumptions <- NULL
    )

    # results -----------------------------------------------------------------
    res <- list(descriptive_statistics = descriptive_statistics,
                goodness_of_fit = goodness_of_fit,
                model_details = model_details,
                visualisation = visualisation,
                check_model_assumptions = check_model_assumptions,
                config = list(
                    finish=Sys.time()
                    ,n.test=n.obs
                )
    )
    class(res) = c(class(res), "regressionModelEvaluation")
    return(res)

}



#' Email a Regression Evaluation Report
#'
#' @description
#' Given the result of \code{regressionEvaluation} email a standardised report to one or more recipients.
#'
#' The emailed report contains a residual vs fitted plot,
#' a residual vs order plot,
#' a histogram of actual, predictions and residuals' distributions,
#' a prediction intervals plot,
#' and a plot of cumulative % qualified versus cumulative % targetted.
#' If the model is provided, some basic descriptions about the model will also be reported.
#' If the model provided is a glm, \code{glm.check.assumptions} function will be used to check the assumptions of the model(e.g. linearity).
#'
#' @param evaluationResults The result from running \code{regressionEvaluation}.
#' @param toAddress Character vector, containing the email address(es) to send the report to, when completed.
#' @param additionalMessage  A string that will appear in the subject of the email
#' @param plotSize  Numeric scalar containing the side length of the resulting square png files, in pixels.
#'
#' @return The result of the call to \code{sendmail}
#'
#' @seealso \code{\link{RQuantSendMail}}, \code{\link{regressionEvaluation}}
#'
#' @examples
#' set.seed(123)
#' data(mtcars)
#' fit <- glm(mpg ~ cyl + disp + hp + drat, data=as.data.frame(mtcars), family = gaussian())
#' actuals <- mtcars$mpg
#' probs <- predict(fit, mtcars)
#' res <- regressionEvaluation(pred = probs,
#' obs = actuals,
#' model = fit,
#' sample_size=1e5,
#' seed = 1234)
#'
#' regressionEmailReport(res,"ivan@xxx.com.au")
#'
#' @export
regressionEmailReport = function(
    evaluationResults
    ,toAddress='your.name@rquant.com'
    ,additionalMessage = ""
    ,plotSize = 10
){
    # checks on user inputs --------------------------------------------
    if(!"regressionModelEvaluation" %in% class(evaluationResults)){
        stop("first argument should be of class 'regressionModelEvaluation'")
    }

    # make temp directory -----------------------------------------------------

    my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
    dir.create(my.dir)

    # plots ---------------------------------------------------------------

    ggsave(filename = paste(my.dir, 'residual_vs_fitted.png', sep='/'),
           evaluationResults$visualisation$residual_vs_fitted,
           width = plotSize*1.5,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'residual_vs_order.png', sep='/'),
           evaluationResults$visualisation$residual_vs_order,
           width = plotSize*1.5,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'histograms.png', sep='/'),
           evaluationResults$visualisation$histograms,
           width = plotSize*1.5,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'prediction_intervals.png', sep='/'),
           evaluationResults$visualisation$prediction_intervals,
           width = plotSize*1.5,
           height = plotSize,
           units = "cm")

    chk_html <- NULL
    if(!is.null(evaluationResults$check_model_assumptions)){

        ggsave(filename = paste(my.dir, 'chk_residuals.png', sep='/'),
               evaluationResults$check_model_assumptions$chk_residuals$qqplot,
               width = plotSize*1.5,
               height = plotSize,
               units = "cm")

        ggsave(filename = paste(my.dir, 'chk_independence_data_points.png', sep='/'),
               evaluationResults$check_model_assumptions$chk_independence_data_points,
               width = plotSize*1.5,
               height = plotSize,
               units = "cm")


        png(filename=paste(my.dir, "chk_linearity.png",sep='/'), width=plotSize*1.5, height=plotSize,units = "cm",res = 300)
        print(evaluationResults$check_model_assumptions$chk_linearity)
        dev.off()

        chk_html <- c(
            toHTML(hr())
            ,toHTML("Check GLM Assumptions")
            ,toHTML(evaluationResults$check_model_assumptions$chk_multi_collinearity)
            ,'<img width="400" src="cid:chk_residuals.png">'
            ,'<img width="400" src="cid:chk_independence_data_points.png">'
            ,'<img width="400" src="cid:chk_linearity.png">'
        )

    }
    # png(filename=paste(my.dir, "deciles2.png",sep='/'), width=plotSize, height=plotSize,units = "cm",res = 300)
    # print(evaluationResults$mosaicPlot)
    # dev.off()
    #
    # png(filename=paste(my.dir, "ROC.png",sep='/'), width=plotSize, height=plotSize,units = "cm",res = 300)
    # print(evaluationResults$rocCurve)
    # dev.off()

    # send mail ---------------------------------------------------------------

    my.png.files<-list.files(my.dir,pattern="*.png",full.names=TRUE)
    my.txt.file<-list.files(my.dir,pattern="*.txt",full.names=TRUE)

    config.vect<-unlist(lapply(evaluationResults$config, as.character))
    config.df <- data.frame(
        param = names(config.vect),
        values = unlist(config.vect)
    )

    # defTbl <- data.frame(rbind(c('A', 'B'), c('C', 'D')))
    # rownames(defTbl) <- c('Event', 'No Event')
    # colnames(defTbl) <- c('Event', 'No Event')


    my.body <- c(
        toHTML("Regression performance report", "h2")
        # ,toHTML("Some text goes in here if needed.")

        ,toHTML("Summary stats are", "h3")
        ,"<ul>"
        ,toHTML(paste("Total Observations - ", evaluationResults$descriptive_statistics$n.obs), "li")
        ,"</ul>"
        ,toHTML(hr())

        ,toHTML("Observations Summary", "h3")
        ,htmlTableQuant(data.frame(Item = names(evaluationResults$descriptive_statistics$obs.summary),
                                   Value = round(as.numeric(evaluationResults$descriptive_statistics$obs.summary),3))
        )

        ,toHTML("Predictions Summary", "h3")
        ,htmlTableQuant(data.frame(Item = names(evaluationResults$descriptive_statistics$pred.summary),
                                   Value = round(as.numeric(evaluationResults$descriptive_statistics$pred.summary),3))
        )

        ,toHTML("Residual Summary", "h3")
        ,htmlTableQuant(data.frame(Item = names(evaluationResults$descriptive_statistics$residual.summary),
                                   Value = round(as.numeric(evaluationResults$descriptive_statistics$residual.summary),3))
        )

        ,toHTML("Goodness of Fit", "h3")
        ,htmlTableQuant(as.data.frame(evaluationResults$goodness_of_fit))

        ,toHTML("Model Configuration Parameters", "h3")
        ,htmlTableQuant(config.df)
        ,toHTML(hr())
        ,toHTML("Plots", "h3")
        ,'<img width="400" src="cid:residual_vs_fitted.png">'
        ,'<img width="400" src="cid:residual_vs_order.png">'
        ,'<img width="400" src="cid:histograms.png">'
        ,'<img width="400" src="cid:prediction_intervals.png">'
        # ,paste0('<img width="400" src="', my.dir, "/residual_vs_fitted.png", '" >')
        # ,paste0('<img width="400" src="', my.dir, "/residual_vs_order.png", '" >')
        # ,paste0('<img width="400" src="', my.dir, "/histograms.png", '" >')
        # ,paste0('<img width="400" src="', my.dir, "/prediction_intervals.png", '" >')

        # if they are embeded, they are not attached
        # if they are not embedded, they are attached
        ,chk_html
        ,toHTML(hr())
        ,toHTML("Definitions", "h3")
        ,
        "<p>
    The formulas used (from caret documentation) are:
    </p><p>
    SST = sum((obs - mean(obs))^2)
    </p><p>
    SSR = sum((pred - mean(pred))^2)
    </p><p>
    SSE = sum((obs - pred)^2)
    </p><p>
    Variance Explained = 1 - var(obs - pred)/var(obs)
    </p><p>
    R Squared = 1 - sum((obs-pred)^2)/sum((obs-mean(obs))^2)
    </p><p>
    MAE <- mean(abs(obs - pred))
    </p><p>
    MSE <- mean((obs - pred)^2)
    </p><p>
    RMSE <- sqrt(mean((pred - obs)^2))
    </p><p>
    MAE (Median) <- median(abs(obs - pred))
    </p>"
    )

    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = c(my.png.files, my.txt.file)
    )

    model.name<-ifelse(is.null(evaluationResults$model_details),'Regression report',class(evaluationResults$model_details))

    my.subject<-paste(
        model.name
        ,'varExp',round(evaluationResults$goodness_of_fit$Value[4], 3)
        ,'rmse',round(evaluationResults$goodness_of_fit$Value[8], 3)
        ,additionalMessage
        ,sep='-')

    tryCatch({
        result<-RQuantSendMail(
            to = toAddress
            ,subject=my.subject
            ,msg=my.msg$html
            ,attach.files = my.msg$attach.files)
    })

    # clean up ----------------------------------------------------------------

    unlink(my.dir, recursive = T)

    return(result)
}


#' GLM Assumptions
#'
#' @description
#' This function checks the most prevalent model assumptions for LM and GLM models, including multi-collinearity, linearity,
#' distribution check (QQ-plot) and homoscedasticity
#'
#' @param model_obj model object of the LM or GLM model under scrutiny
#'
#' @return The function returns a list. The list entries contain visuals and values illustrating the model's compliance with its relevant
#' model assumptions
#'
#' @seealso \code{\link{glm}}, \code{\link{lm}}, \code{\link{pairs.panels}}
#'
#' @examples
#'
#' fit <- glm(mpg ~ drat + wt, data=mtcars, family = gaussian())
#' assessment <- check.glm.assumptions(fit)
#' print(assessment)
#' assessment$chk_residuals
#' assessment$chk_homoscedasticity
#'
#' @export

check.glm.assumptions <- function(model_obj, modelConfig = NULL){

    library(lattice)

    #Verify if the residuals distributed in line with the specified distribution
    chk_residuals <- function(model_object){

        func <- NULL
        if(attributes(model_object)$class[1] == "lm"){
            func = rnorm(length(model_object$residuals), mean(model_object$residuals), sd(model_object$residuals))
        }else{
            if(model_object$family$family == "gaussian"){func = rnorm(length(model_object$residuals), mean(model_object$residuals), sd(model_object$residuals))}
            if(model_object$family$family == "quasi"){func = rnorm(length(model_object$residuals), mean(model_object$residuals), sd(model_object$residuals), variance = "constrant")}
            if(model_object$family$family == "binomial"){func = rbinom(length(model_object$residuals), model_object$weights)}
            if(model_object$family$family == "quasibinomial"){func = rbinom(length(model_object$residuals))}
            if(model_object$family$family == "Gamma"){func = rgamma(length(model_object$residuals), shape = 1)}
            if(model_object$family$family == "inverse.gaussian"){func = 1/rnorm(length(model_object$residuals), mean(model_object$residuals), sd(model_object$residuals))}
            if(model_object$family$family == "poisson" | model_object$family$family == "quasipoisson"){func = rpois(length(model_object$residuals), mean(model_object$residuals))}
        }


        quantile_th <- quantile(func, probs = seq(0, 1, 0.01), na.rm = FALSE, names = TRUE, type = 7)
        quantile_sa <- quantile(model_object$residuals, probs = seq(0, 1, 0.01), na.rm = FALSE, names = TRUE, type = 7)
        df <- data.frame(quantile_th, quantile_sa)
        slope <- 1

        if(model_object$family$family == "gaussian"){
            normilityCheck = list(
                shapiro.test = shapiro.test(model_object$residuals[sample(1:length(model_object$residuals), min(5000, length(model_object$residuals)))]),
                qqCorrelation = cor(df$quantile_th,df$quantile_sa)
            )
        }else{
            normilityCheck = list()
        }


        p <- ggplot(df, aes(x=df$quantile_th, y= df$quantile_sa))+
            geom_point() +
            geom_abline(slope = slope) +
            xlab("Theoretical Quantiles") +
            ylab("Sample Quantiles") +
            ggtitle(paste("QQ Plot For a ", model_object$family$family, "Distribution"))
        return(list(
            qqplot = p,
            normilityCheck = normilityCheck
        ))

    }

    #verify if the mean of the residuals approximates zero
    chk_mean_residuals <- function(model_object){

        df <- data.frame(
            obs = 1:length(model_object$residuals),
            residuals = model_object$residuals)
        p <- ggplot(df, aes(x = obs, y = residuals)) +
            geom_point() +
            geom_smooth() +
            geom_hline(yintercept = mean(df$residuals)) +
            xlab("Data Points") +
            ylab("Residuals") +
            ggtitle("Mean Value Residuals")
        return(p)
    }


    #check for homoscedasticity/homogeneity in variance
    chk_homoscedasticity <- function(model_object){

        df <- data.frame(
            fitted.values = model_object$fitted.values,
            residuals = model_object$residuals)
        p <- ggplot(df, aes(x = fitted.values, y = residuals)) +
            geom_point() +
            geom_smooth() +
            xlab("Fitted Values") +
            ylab("Residuals") +
            ggtitle("Homoscedasticity")
        return(p)
    }


    #check independence of data points
    chk_independence_data_points <- function(model_object){

        infl <- influence.measures(model_object)
        df <- data.frame(infl$infmat[,7], model_object$residuals)
        p <- ggplot(df, aes(x=df[,1], y=df[,2])) +
            geom_point() +
            geom_text(aes(label=row.names(df)), size=2) +
            xlab("Leverage") + ylab("Residuals") + ggtitle("Leverage")
        return(p)

    }


    #check (linear) relationships between variabels
    chk_linearity <- function(model_object){

        # p <- splom(mtcars[all.vars(fit$formula)], main = "Linearity")
        # p <- splom(model_object$data[all.vars(model_object$formula)], main = "Linearity")
        require(psych)
        pairs.panels(model_object$data[all.vars(model_object$formula)], main = "Linearity")
        p<-recordPlot()
        return(p)
    }


    #check mulit-collinearity
    chk_multi_collinearity <- function(model_object){

        kap <- kappa(model_object)

        if(kap >= 30){

            return(paste0("Kappa = ", kap, " . The collinearity is troubling"))
        }
        else{if(kap >= 10){return(paste0("Kappa = ", kap, " . The collearity is moderate"))}
            else{return(paste0("Kappa = ", kap, " . The collearity is reasonable"))}
        }


        if(kap >= 30){return(paste0("Kappa = ", kap, " . The collinearity is troubling"))}
        else{if(kap >= 10){return(paste0("Kappa = ", kap, " . The collearity is moderate"))}
            else{return(paste0("Kappa = ", kap, " . The collearity is reasonable"))}
        }



    }

    list(
        rmse = sqrt(mean((model_obj$residuals)^2)),
        aic = model_obj$aic
    )

    summaryStats = list(
        N = length(model_obj$residuals)
    )


    lst <- list(summaryStats,
                chk_residuals(model_obj),
                chk_mean_residuals(model_obj),
                chk_homoscedasticity(model_obj),
                chk_independence_data_points(model_obj),
                chk_linearity(model_obj),
                chk_multi_collinearity(model_obj))
    names(lst) <- c("summaryStats","chk_residuals", "chk_mean_residuals", "chk_homoscedasticity", "chk_independence_data_points", "chk_linearity", "chk_multi_collinearity")

    # if(lst$chk_residuals$normilityCheck$shapiro.test$p.value<.05 |
    #    lst$chk_residuals$normilityCheck$qqCorrelation <.8){
    #   warning("residuals not normal")
    # }
    #
    # assessment$summary$rmse = runif(1)
    #
    # if(!is.null(modelConfig$maxrmse)){
    #   if(assessment$summary$rmse > modelConfig$maxrmse){
    #     warning("something is fishy")
    #   }
    # }
    #
    # if(!is.null(modelConfig$maxrmse)){
    #   if(assessment$summary$rmse > modelConfig$maxrmse){
    #     warning("something is fishy")
    #   }
    # }

    return(lst)
}




