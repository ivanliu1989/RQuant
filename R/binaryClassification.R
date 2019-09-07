#' Generate a standard ROC curve
#'
#' @description
#' Evaluate a model with a ROC curve
#'
#'
#' @param pred A \code{prediction} object.
#' @param header An optional string to add to the plot title
#'
#' @return A ggplot
#'
#' @seealso \code{\link{binaryClassifierEvaluation}}
#'
#' @examples
#' set.seed(1234)
#' probs <- pnorm(rnorm(100,-1,1))
#' actuals <- rbinom(100,1,probs)
#' pred <- prediction(probs, actuals)
#' generateROCCurve(pred, header = "A Model")
#'
#' @export
generateROCCurve = function(pred, header = ""){

    library(ROCR)
    library(data.table)
    library(ggplot2)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")  # false positive by true positive
    auc <- performance(pred, measure = "auc")


    aucData = list(fpr = perf@x.values[[1]], tpr = perf@y.values[[1]])
    setDT(aucData)

    p = ggplot(aucData, aes(x = fpr, y= tpr)) + geom_line() +
        theme_rquantquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        xlab("False Positive Rate") +
        ylab("True Positive Rate") + coord_fixed() +
        geom_line(aes(x = tpr, y = tpr), lty = 5) +
        ggtitle(paste0("ROC Curve", ifelse(header== "", "", paste0(" - ", header)))) +
        geom_label(x = .75, y = .19, colour = quantColours(2),size = 4,
                   label = paste0("Area under the curve\n = ", round(auc@y.values[[1]],3)))

    return(p)
}


#' Generate a standard Precision/Recall plot
#'
#' @description
#' Evaluate a model with a Precision vs Recall plot
#'
#' @param pred A \code{prediction} object.
#' @param header An optional string to add to the plot title
#'
#' @return A ggplot
#'
#' @seealso \code{\link{binaryClassifierEvaluation}}
#'
#' @examples
#' set.seed(1234)
#' probs <- pnorm(rnorm(100,-1,1))
#' actuals <- rbinom(100,1,probs)
#' pred <- prediction(probs, actuals)
#' generatePrecisionRecallCurve(pred, header = "A Model")
#' @export
generatePrecisionRecallCurve = function(pred, header = ""){

    library(ResourceSelection)
    library(data.table)
    library(ggplot2)
    perf <- performance(pred, "prec", "rec")  # false positive by true positive

    precRecData = list(rec = perf@x.values[[1]], prec = perf@y.values[[1]])
    setDT(precRecData)

    p = ggplot(precRecData, aes(x = rec, y= prec)) +
        theme_rquantquant(logo = 'rquant', simple_theme = T, rquant_font = T, rquant_colours = T, opacity = 0.05, position = 0) +
        geom_line() +
        xlab("Recall - TP / (TP + FN)") +
        ylab("Precision - TP / (TP + FP)") + coord_fixed() +
        ggtitle(paste0("Precision vs Recall", ifelse(header== "", "", paste0(" - ", header))))

    return(p)
}


#' Binary Classifier Effects
#'
#' @description
#' Produce a list of ggplots, showing the relationship between each predictor variable and a probability.
#'
#' Given a data frame and a vector of probabilities, such as those produced by a binary classifier
#' model, this function produces a list of ggplots, showing the relationship between each of the named
#' variables and the probability.
#'
#' It produces a violin plot for factors, a scatter plot with a loess fitted curve for numeric, and an
#' error otherwise.
#'
#' @param probs A vector of estimated probabilities, from 0 to 1.
#' @param df A data frame containing variables that were used to predict some binary outcome.
#' @param var.names A vector of column names from df.
#' @param max.n The maximum number of points to include in the resulting scatter plots.
#' @param log.names A vector of column names to plot in log scale
#' @param display A logical, that determines is the resulting plots are displayed or just returned.
#'
#' @return A list of ggplot objects, suitable for printing or saving.
#'
#' @seealso \code{\link{qplot}}
#'
#' @examples
#' df<-mtcars
#' df$am<-as.factor(df$am)
#' model<-glm(vs ~ am*mpg, data=df, family=binomial)
#' probs<-model$fit
#' binaryClassifierEffect(probs,df,c('am','mpg'))
#'
#' @export
binaryClassifierEffect<-function(probs,df,var.names,max.n=1e3,display=FALSE,log.names=NULL){

    library(ggplot2)
    df$logit<-log(probs/(1-probs))
    df<-df[probs>0,]
    df<-df[sample(min(max.n, nrow(df))),]
    results<-list()

    for(var.name in var.names) {
        df$predictor <- df[[var.name]]
        my.class<-class(df$predictor)

        if(!(my.class %in% c('factor','numeric')))
            stop(paste('Selected variables have to be either factor or numeric,',var.name,'is',my.class))

        if(class(df$predictor)=='factor')
            p<-qplot(predictor, logit, data=df
                     , geom='violin', fill=predictor, draw_quantiles=0.5)
        if(class(df$predictor)=='numeric')
            if(var.name %in% log.names)
                p<-qplot(predictor, logit, data=df
                         , geom='point', log ='x')+geom_smooth(se=FALSE)
        else
            p<-qplot(predictor, logit, data=df
                     , geom='point')+geom_smooth(se=FALSE)
        p<-p+xlab(var.name)+ylab('Logit(Probability)')
        results[[var.name]]<-p
        if(display) print(p)
    }
    return(results)
}

#' Evaluate a Binary Classification Model
#'
#' @description
#'Evaluate the performance of a model with two outcomes, 0 and 1.
#'
#' Given a vector of estimated probabilities of success and a vector of actual successes and failures, this function
#' creates some standardised graphs and summaries and returns them as a list that can be consumed by other
#' functions such as \code{binaryClassifierEmailReport} or a \code{shiny} app.
#'
#' The result contains a histogram of estimated probabilities,
#' the ROC plot with the calculated area under the curve,
#' two density plots, one violin, one standard,
#' two loss curves as a function of deciles of the calculated probabilies,
#' and a plot of cumulative % qualified versus cumulative % targetted.
#' It contains some stats such as Area under the ROC curve, Root Mean Square Error and the confusion matrix.
#' It does not require the actual model.  If the model is supplied, a summary
#' of the model is also included in the output.
#'
#' @param probs A vector of estimated probabilities, from 0 to 1.
#' @param actuals A vector of actual outcomes, 0 or 1.
#' @param thresholdConfusion a threshold to convert probabilities to 0's or 1's. Takes the median of the \code{probs}
#' as a default.
#' @param model Optional, the model used to estimate \code{probs} from \code{actuals}
#' @param config A list containing details of the call to the function, such as lenght of vectors or time of completion.
#'    This is simply printed and attached to the email.
#'
#' @return A list with an additional class \code{binaryModelEvaluation}
#'
#' @seealso \code{\link{binaryClassifierEmailReport}}
#'
#' @examples
#' set.seed(123)
#' probs <- pnorm(rnorm(100,-1,1))
#' hist(probs)
#' actuals <- rbinom(100,1,probs)
#' plot(as.factor(actuals),probs)
#'
#' binaryClassifierEvaluation(probs, actuals)
#'
#' my.data<-data.frame(actuals,probs)
#' mod.fit <- glm(actuals ~ probs , data=my.data, family=binomial)
#'
#' binaryClassifierEvaluation(probs, actuals
#'    , model = mod.fit
#'    , config = list(
#'        finish = Sys.time()
#'        ,n.text = length(probs)
#'        )
#'    )
#'
#' @export
binaryClassifierEvaluation<-function(
    probs
    ,actuals
    ,thresholdConfusion = median(probs)
    ,model=NULL
    ,config=list(
        finish=Sys.time()
        ,n.test=length(probs)
    )
){

    require(ggplot2)
    require(plyr)
    require(caret)
    require(ROCR)
    require(ResourceSelection)

    if(length(probs) != length(actuals)){
        stop("prods and actuals should be of equal length")
    }

    # histogram ---------------------------------------------------------------
    probabilityHistogram = qplot(probs
                                 ,geom="histogram"
                                 ,main=paste0(c('Histogram of Probabilities', model$name), collapse = " - ")
                                 ,xlim=c(0,1)
                                 ,xlab='Probability'
                                 ,ylab='Frequency'
                                 ,fill=I("lightgrey")
                                 ,col= I("grey"))

    # density and violin plot -------------------------------------------------------------

    x <- data.frame(actuals,probs)
    x <- subset(x, probs>0 & probs<1)
    x$logit.p <- log(x$probs/(1-x$probs))

    violinPlot = qplot(as.factor(actuals)
                       ,logit.p
                       ,data=x
                       ,geom=c('violin')
                       ,fill=as.factor(actuals)
                       ,xlab='Actuals'
                       ,ylab='Logit(p)'
    )

    probabilityDensity = qplot(probs
                               ,data=x
                               ,col=as.factor(actuals)
                               ,geom='density'
                               ,xlab='Probabilities'
                               ,ylab='Density'
    )

    # cumulative% -------------------------------------------------------------

    x <- arrange(x, probs)
    my.breaks<-unique(quantile(x$logit.p,seq(0,1,length=21)))
    x$quantile<-cut(x$logit.p, my.breaks, labels=FALSE, ordered_result=TRUE, include.lowest=TRUE)

    small<-ddply(x, .(quantile), summarise, qualified=sum(actuals), targetted=length(actuals))
    small<-arrange(small, -quantile)

    small$targetted.p <- 100*cumsum(small$targetted)/sum(small$targetted)
    small$qualified.p <- 100*cumsum(small$qualified)/sum(small$qualified)

    cumulativePercent = qplot(targetted.p
                              ,qualified.p
                              , data=small
                              ,label = round(qualified.p,1)
                              ,xlim=c(0,100)
                              , ylim=c(0,100)
                              ,geom=c('point','line')
                              , xlab = "Targeted %"
                              , ylab = "Cumulative %"
                              ,main = paste0(c('Cumulative Accuracy', model$name), collapse = " - ")) +
        geom_vline(xintercept = small$targetted.p[10],colour="red", linetype = "longdash") +
        geom_hline(yintercept = small$qualified.p[10],colour="red", linetype = "longdash") +
        geom_label()

    # deciles -----------------------------------------------------------------

    my.levels<-unique(quantile(probs,seq(0,1,0.1)))
    my.deciles<-cut(probs, my.levels)
    my.results<-table(my.deciles,actuals)
    my.resultsSummary = 100*table(my.deciles,actuals)[,'1']/rowSums(table(my.deciles,actuals))

    decilePlot = qplot(1:length(my.resultsSummary), my.resultsSummary
                       ,geom=c('point','line')
                       ,main='Actuals by Deciles of Probability'
                       ,xlab='Decile of Estimated Probability of Success'
                       ,ylab='Actual Outcome %') +
        scale_x_continuous(breaks=1:10)

    # alternative deciles plot ------------------------------------------------

    mosaicplot(my.results
               ,main=paste0(c('Actuals by Deciles of Probability', model$name), collapse = " - ")
               ,xlab='Decile of Estimated Probability of Success'
               ,ylab='Actual Outcome'
               ,las=2
               ,col=c(0,quantColours())
    )
    mosaicPlot = recordPlot()


    # confusion matrix --------------------------------------------------------

    preds <- as.numeric(probs > thresholdConfusion)  # predicted outcome
    accuracy <- table(preds, actuals)  # classification table

    accuracy.rate <- sum(diag(accuracy))/sum(accuracy)

    accuracy.df <- as.data.frame.matrix(accuracy)
    accuracy.df <- accuracy.df[c(2:1),c(2:1)]     # Transpose on off-diagonal to get the '1' row/column first

    accuracy.pct <- accuracy.df
    accuracy.pct <- round(100*accuracy.pct / sum(accuracy.pct),1)
    accuracy.pct <- as.data.frame(lapply(accuracy.pct, as.character))
    accuracy.pct <- as.data.frame(lapply(accuracy.pct, function(x) paste(x, '%', sep = '')))
    colnames(accuracy.pct) <- colnames(accuracy.df)
    rownames(accuracy.pct) <- rownames(accuracy.df)

    my.confusionMatrix <- confusionMatrix(accuracy, positive = "1")

    # ROC curve ---------------------------------------------------------------

    my.pred <- prediction(probs, actuals)
    perf <- performance(my.pred, measure = "tpr", x.measure = "fpr")  # false positive by true positive

    auc <- performance(my.pred, measure = "auc")
    # auc is of class performance
    # like a list, but you use @ instead of $ to extract an element

    rocCurve = generateROCCurve(my.pred)
    precVsRecall = generatePrecisionRecallCurve(my.pred)

    byClassDf <- data.frame(round(my.confusionMatrix$byClass, 2))
    colnames(byClassDf) <- 'Value'

    summaryStats = list(
        Nobservations = length(probs),
        accuracy.rate = accuracy.rate,
        area.under.curve = auc@y.values[[1]],
        rmse = sqrt(mean((probs - actuals)^2)),
        errorRate = 1 - accuracy.rate,
        HLtest = hoslem.test(actuals, probs, g=10),
        summaryAccuracy = my.confusionMatrix,
        accuracy.df = accuracy.df,
        accuracy.pct = accuracy.pct,
        by.class = byClassDf
    )

    # Put it together ---------------------------------------------------------------

    result = list(
        summaryStats = summaryStats,
        probabilityHistogram = probabilityHistogram,
        violinPlot = violinPlot,
        probabilityDensity = probabilityDensity,
        cumulativePercent = cumulativePercent,
        mosaicPlot = mosaicPlot,
        decilePlot = decilePlot,
        rocCurve = rocCurve,
        precVsRecall = precVsRecall,
        model = model,
        config = config
    )

    class(result) = c(class(result), "binaryModelEvaluation")

    return(result)
}


#' Email a Binary Classifier Report
#'
#' @description
#' Given the result of \code{binaryClassifierEvaluation} email a standardised report to one or more recipients.
#'
#' The emailed report contains a histogram of estimated probabilities,
#' the ROC plot with the calculated area under the curve,
#' two density plots, one violin, one standard,
#' two loss curves as a function of deciles of the calculated probabilies,
#' and a plot of cumulative % qualified versus cumulative % targetted.
#' It contains some stats such as Area under the ROC curve, Root Mean Square Error and the confusion matrix.
#' It does not require the actual model.  If the model is supplied, a summary
#' of the model is also included in the output.
#'
#' @param evaluationResults The result from running \code{binaryClassifierEvaluation}.
#' @param toAddress Character vector, containing the email address(es) to send the report to, when completed.
#' @param additionalMessage  A string that will appear in the subject of the email
#' @param plotSize  Numeric scalar containing the side length of the resulting square png files, in pixels.
#' @param emailFrontEnd  A string of html that goes at the front of the email
#'
#' @return The result of the call to \code{sendmail}
#'
#' @seealso \code{\link{RQuantSendMail}}, \code{\link{binaryClassifierEvaluation}}
#'
#' @examples
#' set.seed(123)
#' probs <- pnorm(rnorm(100,-1,1))
#' hist(probs)
#' actuals <- rbinom(100,1,probs)
#' plot(as.factor(actuals),probs)
#'
#' binaryClassifierEmailReport(binaryClassifierEvaluation(probs, actuals),"ivan.liuyanfeng@gmail.com")
#'
#' @export
# try catch and if error clean up temp directory
binaryClassifierEmailReport <-function(
    evaluationResults
    ,toAddress='your.name@rquant.com'
    ,additionalMessage = ""
    ,plotSize = 10
    ,emailFrontEnd = ""
){


    # checks on user inputs --------------------------------------------
    if(!"binaryModelEvaluation" %in% class(evaluationResults)){
        stop("first argument should be of class 'binaryModelEvaluation'")
    }

    # make temp directory -----------------------------------------------------

    my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
    dir.create(my.dir)

    # plots ---------------------------------------------------------------

    ggsave(filename = paste(my.dir, 'histogram.png', sep='/'),
           evaluationResults$probabilityHistogram,
           width = plotSize,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'violin.png', sep='/'),
           evaluationResults$violinPlot,
           width = plotSize,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'density.png', sep='/'),
           evaluationResults$probabilityDensity,
           width = plotSize,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'cumulative.png', sep='/'),
           evaluationResults$cumulativePercent,
           width = plotSize,
           height = plotSize,
           units = "cm")

    ggsave(filename = paste(my.dir, 'deciles.png', sep='/'),
           evaluationResults$decilePlot,
           width = plotSize,
           height = plotSize,
           units = "cm")


    png(filename=paste(my.dir, "deciles2.png",sep='/'), width=plotSize, height=plotSize,units = "cm",res = 300)
    print(evaluationResults$mosaicPlot)
    dev.off()

    png(filename=paste(my.dir, "ROC.png",sep='/'), width=plotSize, height=plotSize,units = "cm",res = 300)
    print(evaluationResults$rocCurve)
    dev.off()

    # send mail ---------------------------------------------------------------

    my.png.files<-list.files(my.dir,pattern="*.png",full.names=TRUE)
    my.txt.file<-list.files(my.dir,pattern="*.txt",full.names=TRUE)

    config.vect<-unlist(lapply(evaluationResults$config, as.character))
    config.df <- data.frame(
        param = names(config.vect),
        values = unlist(config.vect)
    )

    defTbl <- data.frame(rbind(c('A', 'B'), c('C', 'D')))
    rownames(defTbl) <- c('Event/1', 'No Event/0')
    colnames(defTbl) <- c('Event/1', 'No Event/0')

    my.body <- c(
        ifelse(class(emailFrontEnd) == "character", emailFrontEnd, ""),
        toHTML("Binary Classifier performance report", "h2")
        # ,toHTML("Some text goes in here if needed.")

        ,toHTML("Summary stats are", "h3")
        ,"<ul>"
        ,toHTML(paste("The classification rate correct is", round(evaluationResults$summaryStats$accuracy.rate,3)), "li")
        ,toHTML(paste("The area under the ROC curve is",round(evaluationResults$summaryStats$area.under.curve,3)), "li")
        ,toHTML(paste("The Root Mean Square Error is", round(evaluationResults$summaryStats$rmse,3)), "li")
        ,toHTML(paste("The error rate (1-classification rate) is", round(evaluationResults$summaryStats$errorRate,3)), "li")
        ,toHTML(paste("The number of observations was", evaluationResults$summaryStats$Nobservations), "li")
        ,toHTML(paste("The HL test statistic P-value is", round(evaluationResults$summaryStats$HLtest$p.value,3)), "li")
        ,"</ul>"
        ,toHTML(hr())

        ,toHTML("Confusion Matrix (counts)", "h3")
        ,htmlTableQuant(evaluationResults$summaryStats$accuracy.df, TRUE, "Actuals&nbsp;►<br>▼&nbsp;Preds")

        ,toHTML("Confusion Matrix (%)", "h3")
        ,htmlTableQuant(evaluationResults$summaryStats$accuracy.pct, TRUE, "Actuals&nbsp;►<br>▼&nbsp;Preds")

        ,toHTML("Caret Statistics", "h3")
        ,htmlTableQuant(data.frame(evaluationResults$summaryStats$by.class), TRUE)

        ,toHTML("Model Configuration Parameters", "h3")
        ,htmlTableQuant(config.df)
        ,toHTML(hr())
        ,toHTML("Plots", "h3")
        ,'<img width="400" src="cid:ROC.png">'
        ,'<img width="400" src="cid:violin.png">'
        ,'<img width="400" src="cid:density.png">'
        ,'<img width="400" src="cid:deciles.png">'
        ,'<img width="400" src="cid:deciles2.png">'
        ,'<img width="400" src="cid:cumulative.png">'
        ,'<img width="400" src="cid:histogram.png">'
        # ,paste0('<img width="400" src="', my.dir, "/ROC.png", '">')
        # ,paste0('<img width="400" src="', my.dir, "/violin.png", '">')
        # ,paste0('<img width="400" src="', my.dir, "/density.png", '">')
        # ,paste0('<img width="400" src="', my.dir, "/deciles.png", '">')
        # ,paste0('<img width="400" src="', my.dir, "/deciles2.png", '">')
        # ,paste0('<img width="400" src="', my.dir, "/cumulative.png", '">')
        # if they are embeded, they are not attached
        # if they are not embedded, they are attached

        ,toHTML(hr())
        ,toHTML("Definitions", "h3")
        ,"Suppose a 2x2 table with notation:"
        ,htmlTableQuant(defTbl, TRUE, "Actuals&nbsp;►<br>▼&nbsp;Preds")
        ,
        "<p>
        The formulas used (from caret documentation) are:
        </p><p>
        Sensitivity = A/(A+C)
        </p><p>
        Specificity = D/(B+D)
        </p><p>
        Prevalence = (A+C)/(A+B+C+D)
        </p><p>
        PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))
        </p><p>
        NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))
        </p><p>
        Detection Rate = A/(A+B+C+D)
        </p><p>
        Detection Prevalence = (A+B)/(A+B+C+D)
        </p><p>
        Balanced Accuracy = (Sensitivity+Specificity)/2
        </p>"
    )

    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = c(my.png.files, my.txt.file)
    )

    model.name<-ifelse(is.null(evaluationResults$model$name),'BC report',evaluationResults$model$name)

    my.subject<-paste(
        model.name
        ,round(evaluationResults$summaryStats$accuracy.rate,3)
        ,round(evaluationResults$summaryStats$area.under.curve,3)
        ,additionalMessage
        ,sep='-')

    result = "Email failure~"
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






