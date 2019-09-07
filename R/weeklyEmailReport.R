#' Weekly Forex Sentiment Email Report
#'
#' Weekly Forex Sentiment Email Report
#'
#' @param inst1 currency symbol 1
#' @param inst2 currency symbol 2
#' @param image_size image size embeded in email
#' @param font_size size of font
#' @param toAddress a vector of email addresses
#'
#' @examples
#' generateForexSentimentReport(inst1 = "AUD", inst2 = "USD",
#' font_size = 10, image_size = 700,
#' toAddress = c("ivan.liuyanfeng@gmail.com"))
#'
#' @export
generateForexSentimentReport <- function(inst1, inst2, font_size = 6, image_size = 800, toAddress = c("ivan.liuyanfeng@gmail.com")){

    library(ggplot2)
    library(qcc)
    library(data.table)

    # get data
    # saveRDS(INSTRUMENTS,'~/analytics/common/OANDA_FX_INSTRUMENTS.rds')
    INSTRUMENTS = readRDS('~/analytics/common/OANDA_FX_INSTRUMENTS.rds')
    symbol = paste0(toupper(inst1), toupper(inst2))
    twitter.dir = paste0('~/analytics/common/twitter/',symbol,'/')
    if(!dir.exists(twitter.dir)){
        stop("No data found!")
    }
    title = paste0(toupper(inst1),"/",toupper(inst2))
    OandaInst = paste0(toupper(inst1),"_",toupper(inst2))
    if(!OandaInst %in% INSTRUMENTS) {
        OandaInst = paste0(toupper(inst2),"_",toupper(inst1))
        if(!OandaInst %in% INSTRUMENTS) {
            stop("Instruments are not available...")
        }
    }

    tweets.files = list.files(twitter.dir, full.names = TRUE)
    for(i in 1:length(tweets.files)){
        if(i ==1){
            tweets = readRDS(tweets.files[i])
        }else{
            tweets$v = c(tweets$v, readRDS(tweets.files[i])$v)
            tweets$d = rbind(tweets$d, readRDS(tweets.files[i])$d)
        }
    }
    tweets$v = unique(tweets$v)
    tweets$d = unique(tweets$d)

    # make temp directory -----------------------------------------------------
    my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
    dir.create(my.dir)

    # 1. Anomalies Detection --------------------------------------------------
    anomalies <- twitterAnomalyDetect(tweets, lastDay = Sys.Date()-1, title = title, alpha = 0.8, fontsize = font_size)

    # anomalies$tweetsDistribution
    ggsave(filename = paste(my.dir, '1_1_tweetsDistribution.png', sep='/'),
           width = 16.4, height = 9.7,
           multiplot(anomalies$timeDist,
                     anomalies$timeDistDayOf,
                     anomalies$anomalies$plot, cols = 1))
    #dev.off()

    # 2. Emotional Valence ----------------------------------------------------
    emotion = twitterEmotionalValence(tweets, title, fontsize = font_size)
    emotionTwitter = emotion$orig[order(emotionalValence), .(text_raw,created,retweetCount,emotionalValence)]

    # emotion$emotionDailyPlot
    cowplot::ggsave(filename = paste(my.dir, '2_1_emotionDailyPlot.png', sep='/'),
           width = 16.4, height = 9.7,
           cowplot::plot_grid(emotion$emotionDailyPlot,
                              emotion$emotionPlotDailyBox,
                              nrow=2, labels="AUTO", label_size=8, align="v"))
    # emotion$emotionPlot
    cowplot::ggsave(filename = paste(my.dir, '2_2_emotionPlot.png', sep='/'),
           width = 16.4, height = 9.7,
           cowplot::plot_grid(emotion$emotionPlot,
                              emotion$emotionPlotViolin,
                              nrow=2, labels="AUTO", label_size=8, align="v"))

    png(filename=paste(my.dir, '2_3_emotionWordCloudPlot.png', sep='/'),
        width=15, height=10,units = "cm",res = 300)
    wordcloud::comparison.cloud(emotion$wcdt,
                                max.words = 100, min.freq = 2, random.order=FALSE,
                                rot.per = 0, colors = quantColours(inv = 3)[c(2,4,6)] , vfont = c("sans serif", "plain"))
    dev.off()

    # 3. Twitter Cluster ------------------------------------------------------
    clusterGraph = twitterClusterGraph(tweets, minDocFreq = 0.98, groups = 6, title =title) # Save file names

    png(filename=paste(my.dir, '3_1_twitterClusterGraph.png', sep='/'),
        width=15, height=10,units = "cm",res = 300)
    plot(clusterGraph$plot, layout=layout.fruchterman.reingold(clusterGraph$plot))
    title(clusterGraph$title,
          col.main=quantColours()[-c(2:4)][1], cex.main=1.5, family="serif")
    dev.off()

    # 4. Oanda Eco Calendar ---------------------------------------------------
    ecoCal = drawEconomicCalendar(OandaInst, font_size = font_size)
    # multiplot(ecoCal$p1,ecoCal$p2,ecoCal$p3,ecoCal$p4, cols = 1)
    ggsave(filename = paste(my.dir, '4_EconomicCalendar.png', sep='/'),
           width = 16.4, height = 9.7,
           multiplot(ecoCal$p1,ecoCal$p2,ecoCal$p3,ecoCal$p4, cols = 1))


    # 5. Oanda Historical Positions -------------------------------------------
    HistPos <- HistoricalPositionOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = OandaInst, PERIOD = '3 months')
    x = as.data.frame(HistPos[, .(timestamp, as.numeric(long_position_ratio))])
    x$V3 <- c(NA, diff(log(x$V2)))
    x = na.omit(x)
    object <- qcc(x$V3,  type = "xbar.one", nsigmas = 2, confidence.level = 0.99)
    # qcc.ggplot(object)
    ggsave(filename = paste(my.dir, '5_1_OandaHistPosRet.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object, font_size = font_size,
                      title = paste0("Quality Control - Oanda Historical Positions (1st Diff) of ", title)))
    object2 = object
    object2$statistics = x$V2
    # qcc.ggplot(object2, rawplot = TRUE)
    ggsave(filename = paste(my.dir, '5_2_OandaHistPosRaw.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object2, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Historical Positions of ", title)))


    # 6. Oanda Commitment of Traders ------------------------------------------
    cotOanda = COT.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = OandaInst)
    x = log(as.numeric(cotOanda$`Overall Interest`))
    object = qcc(diff(x), type = "xbar.one")
    # qcc.ggplot(object)
    ggsave(filename = paste(my.dir, '6_1_OandaCOTRet.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Commitment of Traders (1st Diff) of ", title)))
    object2 = object
    object2$statistics = as.numeric(cotOanda$`Overall Interest`)
    # qcc.ggplot(object2, rawplot = TRUE)
    ggsave(filename = paste(my.dir, '6_2_OandaCOTRaw.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object2, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Commitment of Traders of ", title)))


    # 7. Cusum of Selected Pairs ----------------------------------------------
    instCandles = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = OandaInst,
                                            price = 'M', granularity = 'D', count = 250)
    for(i in 4:7){
        instCandles[, i] <- as.numeric(instCandles[, i])
    }
    instCandles_p = na.omit(diff(as.matrix(log(instCandles[, 4:7]))))
    instCandles_v = as.numeric(instCandles$volume)

    object = qcc(instCandles_v[1:200], newdata = instCandles_v[201:250], type = "xbar.one")
    # qcc.ggplot(object)
    ggsave(filename = paste(my.dir, '7_1_qccOandaVolume.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Forex Volume of ", title)))

    object = qcc(instCandles_p[1:200,], newdata = instCandles_p[201:249,], type = "xbar", nsigmas = 3)
    # object = qcc(instCandles_p[1:200,1], newdata = instCandles_p[201:249,1], type = "xbar.one", nsigmas = 2, confidence.level = 0.95)
    # qcc.ggplot(object)
    ggsave(filename = paste(my.dir, '7_2_qccOandaPrice.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Forex Prices (returns) of ", title)))
    object2 = object
    object2$statistics = instCandles$mid.c[2:201]
    object2$newstats = instCandles$mid.c[202:250]
    # qcc.ggplot(object2, rawplot = TRUE)
    ggsave(filename = paste(my.dir, '7_3_qccOandaPriceRaw.png', sep='/'),
           width = 16.4, height = 9.7,
           qcc.ggplot(object2, rawplot = TRUE, font_size = font_size,
                      title = paste0("Quality Control - Oanda Forex Prices of ", title)))

    # 8. HTML -----------------------------------------------------------------
    my.png.files<-list.files(my.dir,pattern="*.png",full.names=TRUE)
    my.txt.file<-list.files(my.dir,pattern="*.txt",full.names=TRUE)
    emotionTwitter$retweetCount <- NULL
    names(emotionTwitter) <- c("Tweets", "Created", "Emotion Valence Score")
    emotionTwitter$`Emotion Valence Score` = round(emotionTwitter$`Emotion Valence Score`, digits = 2)
    emotionTwitter$Tweets <- iconv(emotionTwitter$Tweets, to = "UTF-8", sub = " ")  # Convert to basic ASCII text to avoid silly characters

    my.body <- c(
        toHTML(paste0(title, " - Weekly Sentiment & Economic Calendar Analysis"), "h2")
        ,toHTML(hr())

        ,toHTML("Twitter Topic Anomalies Detection", "h3")
        ,"<p>
        The distributions of number of tweets of the FX pairs AND the anomalies detection of number of tweets:
        </p>"
        ,'<img width="800" src="cid:1_1_tweetsDistribution.png">'
        ,toHTML(hr())

        ,toHTML("Twitter Sentiment Analysis", "h3")
        ,"<p>
        Tweets Emotion Analysis - Daily
        </p>"
        ,'<img width="800" src="cid:2_1_emotionDailyPlot.png">'
        ,"<p>
        Tweets Emotion Analysis - Weekly
        </p>"
        ,'<img width="800" src="cid:2_2_emotionPlot.png">'
        ,"<p>
        Tweets Emotion Analysis - Word Cloud
        </p>"
        ,'<img width="600" src="cid:2_3_emotionWordCloudPlot.png">'
        ,"<p>
        Top 5 Positive Tweets:
        </p>"
        ,htmlTableQuant(as.data.frame(head(emotionTwitter[order(emotionTwitter$`Emotion Valence Score`, decreasing = T),], 5)))
        ,"<p>
        Top 5 Negative Tweets
        </p>"
        ,htmlTableQuant(as.data.frame(head(emotionTwitter, 5)))
        ,toHTML(hr())

        ,toHTML("Twitter Topic Clustering", "h3")
        ,'<img width="600" src="cid:3_1_twitterClusterGraph.png">'
        ,toHTML(hr())

        ,toHTML("Oanda Economic Calendar", "h3")
        ,'<img width="800" src="cid:4_EconomicCalendar.png">'
        ,"<p>
        Detailed Economic Calendar / Events in last week
        </p>"
        ,htmlTableQuant(as.data.frame(ecoCal$EcoCalendar[as.IDate(timestamp) >= Sys.Date()-7,]))
        ,toHTML(hr())

        ,toHTML("Oanda Historical Positions", "h3")
        ,"<p>
        Quality Control Charts for Historical Positions of FX Pairs
        </p>"
        ,'<img width="800" src="cid:5_1_OandaHistPosRet.png">'
        ,'<img width="800" src="cid:5_2_OandaHistPosRaw.png">'
        ,toHTML(hr())

        ,toHTML("Commitment of Traders", "h3")
        ,"<p>
        Quality Control Charts for Commitment of Traders (COT) of FX Pairs
        </p>"
        ,'<img width="800" src="cid:6_1_OandaCOTRet.png">'
        ,'<img width="800" src="cid:6_2_OandaCOTRaw.png">'
        ,toHTML(hr())

        ,toHTML("Volume & Prices Anomalies Detection", "h3")
        ,"<p>
        Quality Control Charts / Anomalies Detection of Historical Volume of FX Pairs
        </p>"
        ,'<img width="800" src="cid:7_1_qccOandaVolume.png">'
        ,"<p>
        Quality Control Charts / Anomalies Detection of Historical Prices of FX Pairs
        </p>"
        ,'<img width="800" src="cid:7_2_qccOandaPrice.png">'
        ,'<img width="800" src="cid:7_3_qccOandaPriceRaw.png">'
        ,toHTML(hr())
    )
    if(!is.null(image_size)){
        my.body = gsub(pattern = 'width="800"', replacement = paste0('width="', image_size, '"'), my.body)
    }

    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = c(my.png.files, my.txt.file)
    )

    my.subject<-paste0(
        title,
        " - Weekly Sentiment & Economic Calendar Analysis (", Sys.Date(), ")")

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



#' Oanda Weekly Cointegrated Pairs Searc
#'
#' Weekly Search Cointegrated Pairs  over all tradable Forex pairs on Oanda Platform
#'
#' @param total_points total data points (days) to look back for testing
#' @param testPeriod 0-1, proportion of data points to be used as testing set
#' @param toAddress a vector of email addresses
#'
#' @examples
#' generateOandaCointegrationReport(total_points = 250, testPeriod = 0.05, toAddress = c("ivan.liuyanfeng@gmail.com"))
#'
#' @export
generateOandaCointegrationReport <- function(total_points = 250, testPeriod = 0.05, toAddress = c("ivan.liuyanfeng@gmail.com")){

    library(ggplot2)
    library(data.table)

    INSTRUMENTS = readRDS('~/analytics/common/OANDA_FX_INSTRUMENTS.rds')

    # make temp directory -----------------------------------------------------
    my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"), "/")
    dir.create(my.dir)

    res = RQuantTrader::searchCointegratedPairsOanda(total_points = total_points, testPeriod = testPeriod, INSTRUMENTS = INSTRUMENTS, dir = my.dir)
    # res$res
    # res$path

    tot_inst <- length(INSTRUMENTS)
    tot_pairs <- tot_inst * (tot_inst-1)
    ret_pairs <- nrow(res$res)
    res_dt = as.data.frame(res$res)
    for(i in 5:ncol(res_dt)){
        res_dt[, i] <- round(as.numeric(res_dt[, i]), 4)
    }

    # 2. HTML -----------------------------------------------------------------
    my.body <- c(
        toHTML(paste0("Oanda Weekly Cointegrated Pairs Search - ", Sys.Date()), "h2")
        ,toHTML(hr())

        ,toHTML("Summary", "h3")
        ,paste0("<p>
        Total Tradable Forex Instruments: ",tot_inst,"
        </p>")
        ,paste0("<p>
        Total Tested Pairs: ", tot_pairs,"
        </p>")
        ,paste0("<p>
        Returned Cointegrated Pairs: ", ret_pairs, "
        </p>")
        ,toHTML(hr())

        ,toHTML("Johansen and Augmented Dickey Fuller Scores", "h3")
        ,htmlTableQuant(res_dt)
        ,toHTML(hr())

        ,toHTML("Detailed Report and Train / Test Periods Mean Reversion Charts", "h3")
        ,"<p>
        Please see attached pdf report.
        </p>"
        ,toHTML(hr())
    )

    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = res$path
    )

    my.subject<-paste0("Oanda Weekly Cointegrated Pairs Search - ", Sys.Date())

    tryCatch({
        result<-RQuantSendMail(
            to = toAddress
            ,subject=my.subject
            ,msg=my.msg$html
            ,attach.files = my.msg$attach.files)
    })

    unlink(my.dir, recursive = T)

    return(result)
}
