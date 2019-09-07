# logistic function
logistic <- function(x){
    exp(x) / (1 + exp(x))
}

# help to construct an individual feature importance table
tbTodtWithMoreCols <- function(tb, featureNames){
    dt <- as.data.table(tb)
    setnames(dt, names(dt), c("featuresOnPath", "weights"))
    diff.names <- setdiff(featureNames, dt$featuresOnPath)

    dt.featuresWtMore <- rbind(dt, data.table(featuresOnPath = diff.names, weights = rep(0, length(diff.names))))
    return(dt.featuresWtMore)
}

# help to search through a tree to find the gain, paths, and feature weights
walkThroughTree <- function(tree, dt.singleRow){

    featureNames <- names(dt.singleRow)

    cur.id <- tree[1]$ID
    feature <- "!Leaf"

    featuresOnPath <- character()
    gain <- 0

    while(feature != "Leaf"){
        cur.row <- tree[ID == cur.id]
        cur.id <- cur.row$ID
        feature <- cur.row$Feature

        featuresOnPath <- c(featuresOnPath, cur.row$Feature)
        featuresOnPath <- featuresOnPath[featuresOnPath != "Leaf"]
        # weird scenario where Yes.Feature != "Leaf" but the next Feature == "Leaf"

        next.ID <- ifelse(as.double(dt.singleRow[[cur.row$Feature]]) < as.double(cur.row$Split)
                          , cur.row$Yes
                          , cur.row$No)
        cur.id <- next.ID

        if(feature == "Leaf"){
            gain <- cur.row$Quality
        }

    }
    tb.featuresWt <- gain * (table(featuresOnPath) / length(featuresOnPath))
    featuresWt <- tbTodtWithMoreCols(tb.featuresWt, featureNames)
    return(list(gain = gain, featuresOnPath = featuresOnPath, featuresWt = featuresWt))

}

# help to aggregate the gains by features on the patsh from all trees
sumOnCommonCols <- function(d1, d2){
    ddply(rbind(d1, d2), .(featuresOnPath), summarize, weights = sum(weights))
}

# my radial plot
CreateRadialPlot <- function(plot.data,
                             axis.labels=colnames(plot.data)[-1],
                             grid.min=-0.5,  #10,
                             grid.mid=0,  #50,
                             grid.max=0.5,  #100,
                             centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                             plot.extent.x.sf=1.2,
                             plot.extent.y.sf=1.2,
                             x.centre.range=0.02*(grid.max-centre.y),
                             label.centre.y=FALSE,
                             grid.line.width=0.5,
                             gridline.min.linetype="longdash",
                             gridline.mid.linetype="longdash",
                             gridline.max.linetype="longdash",
                             gridline.min.colour="grey",
                             gridline.mid.colour="blue",
                             gridline.max.colour="grey",
                             grid.label.size=4,
                             gridline.label.offset=-0.02*(grid.max-centre.y),
                             label.gridline.min=TRUE,
                             axis.label.offset=1.15,
                             axis.label.size=3,
                             axis.line.colour="grey",
                             group.line.width=1,
                             group.point.size=4,
                             background.circle.colour="yellow",
                             background.circle.transparency=0.2,
                             plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                             legend.title="Cluster",
                             legend.text.size=grid.label.size,
                             title = "") {

    require(ggplot2)
    var.names <- colnames(plot.data)[-1]  #'Short version of variable names
    #axis.labels [if supplied] is designed to hold 'long version' of variable names
    #with line-breaks indicated using \n

    #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
    plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
    plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf

    #Check supplied data makes sense
    if (length(axis.labels) != ncol(plot.data)-1)
        return("Error: 'axis.labels' contains the wrong number of axis labels")
    if(min(plot.data[,-1])<centre.y)
        return("Error: 'plot.data' contains value(s) < centre.y")
    if(max(plot.data[,-1])>grid.max)
        return("Error: 'plot.data' contains value(s) > grid.max")

    #Declare required internal functions

    CalculateGroupPath <- function(df) {
        #Converts variable values into a set of radial x-y coordinates
        #Code adapted from a solution posted by Tony M to
        #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r

        #Args:
        #  df: Col 1 -  group ('unique' cluster / group ID of entity)
        #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

        path <- as.factor(as.character(df[,1]))

        ##find increment
        angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))

        ##create graph data frame
        graphData= data.frame(seg="", x=0,y=0)
        graphData=graphData[-1,]

        for(i in levels(path)){

            pathData = subset(df, df[,1]==i)

            for(j in c(2:ncol(df))){

                #pathData[,j]= pathData[,j]

                graphData=rbind(graphData, data.frame(group=i,
                                                      x=pathData[,j]*sin(angles[j-1]),
                                                      y=pathData[,j]*cos(angles[j-1])))
            }
            ##complete the path by repeating first pair of coords in the path
            graphData=rbind(graphData, data.frame(group=i,
                                                  x=pathData[,2]*sin(angles[1]),
                                                  y=pathData[,2]*cos(angles[1])))

        }

        #Make sure that name of first column matches that of input data (in case !="group")
        colnames(graphData)[1] <- colnames(df)[1]

        graphData #data frame returned by function

    }

    CaclulateAxisPath = function(var.names,min,max) {
        #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)

        #Args:
        #var.names - list of variables to be plotted on radar plot
        #min - MININUM value required for the plotted axes (same value will be applied to all axes)
        #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)

        #var.names <- c("v1","v2","v3","v4","v5")
        n.vars <- length(var.names) # number of vars (axes) required

        #Cacluate required number of angles (in radians)
        angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)

        #calculate vectors of min and max x+y coords
        min.x <- min*sin(angles)
        min.y <- min*cos(angles)
        max.x <- max*sin(angles)
        max.y <- max*cos(angles)

        #Combine into a set of uniquely numbered paths (one per variable)
        axisData <- NULL
        for (i in 1:n.vars) {
            a <- c(i,min.x[i],min.y[i])
            b <- c(i,max.x[i],max.y[i])
            axisData <- rbind(axisData,a,b)
        }

        #Add column names + set row names = row no. to allow conversion into a data frame
        colnames(axisData) <- c("axis.no","x","y")
        rownames(axisData) <- seq(1:nrow(axisData))

        #Return calculated axis paths
        as.data.frame(axisData)
    }


    funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
        #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
    }

    ### Convert supplied data into plottable format

    # (a) add abs(centre.y) to supplied plot data
    #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
    # in user-supplied data]
    plot.data.offset <- plot.data
    plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
    #print(plot.data.offset)

    # (b) convert into radial coords
    group <-NULL
    group$path <- CalculateGroupPath(plot.data.offset)
    #print(group$path)

    # (c) Calculate coordinates required to plot radial variable axes
    axis <- NULL
    axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
    #print(axis$path)

    # (d) Create file containing axis labels + associated plotting coordinates

    #Labels
    axis$label <- data.frame(
        text=axis.labels,
        x=NA,
        y=NA )
    #print(axis$label)

    #axis label coordinates
    n.vars <- length(var.names)
    angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
    axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
    #print(axis$label)

    # (e) Create Circular grid-lines + labels

    #caclulate the cooridinates required to plot circular grid-lines for three user-specified
    #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
    gridline <- NULL
    gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
    gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
    gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
    #print(head(gridline$max$path))

    #gridline labels
    gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                     text=as.character(grid.min))
    gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                     text=as.character(grid.max))
    gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                     text=as.character(grid.mid))
    #print(gridline$min$label)
    #print(gridline$max$label)
    #print(gridline$mid$label)


    ### Start building up the radar plot

    # Delcare 'theme_clear', with or without a plot legend as required by user
    #[default = no legend if only 1 group [path] being plotted]
    theme_clear <- theme_bw() +
        theme(axis.text.y=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              legend.key=element_rect(linetype="blank"))

    if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")

    #Base-layer = axis labels + plot extent
    # [need to declare plot extent as well, since the axis labels don't always
    # fit within the plot area automatically calculated by ggplot, even if all
    # included in first plot; and in any case the strategy followed here is to first
    # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
    # then centred labels for axis labels almost immediately above/below x= 0
    # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
    # This building up the plot in layers doesn't allow ggplot to correctly
    # identify plot extent when plotting first (base) layer]

    #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
    base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
        geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
                  aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
        scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) +
        scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))

    # + axis labels for any vertical axes [abs(x)<=x.centre.range]
    base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)

    # + axis labels for any vertical axes [x>x.centre.range]
    base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)

    # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
    base <- base + theme_clear

    #  + background circle against which to plot radar data
    base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                                fill=background.circle.colour,
                                alpha=background.circle.transparency)

    # + radial axes
    base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                             colour=axis.line.colour)

    # ... + group (cluster) 'paths'
    base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                             size=group.line.width) + scale_color_brewer(palette="PRGn")

    # ... + group points (cluster data)
    base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)

    #... + amend Legend title
    if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)

    # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
    base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                              lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
    base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                              lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
    base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                              lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)

    # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
    # if (label.gridline.min==TRUE) {
    #     base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,fontface="bold",size=grid.label.size, hjust=1) }
    # base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,fontface="bold",size=grid.label.size, hjust=1)
    # base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,fontface="bold",size=grid.label.size, hjust=1)
    #
    # ... + centre.y label if required [i.e. value of y at centre of plot circle]
    if (label.centre.y==TRUE) {
        centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
        base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,fontface="bold",size=grid.label.size, hjust=0.5) }

    # add title
    if(nrow(plot.data) == 1){
        base <- base + labs(title = plot.data$group)
    }

    return(base)

}

# my binaryClassificationEffect
myBinaryClassifierEffect <- function(probs,df,var.names,max.n=1e3,display=FALSE,log.names=NULL){

    df$logit<-log(probs/(1-probs))
    df<-df[probs>0,]
    df<-sample_n(df, min(max.n, nrow(df)))
    results<-list()

    for(var.name in var.names) {
        df$predictor <- df[[var.name]]
        my.class<-class(df$predictor)

        if(!(my.class %in% c('factor','numeric')))
            stop(paste('Selected variables have to be either factor or numeric,',var.name,'is',my.class))

        if(class(df$predictor)=='factor')
            # p<-qplot(predictor, logit, data=df
            #          , geom='violin', fill=predictor, draw_quantiles=0.5)
            p <- ggplot(data = df, aes(x = predictor, y = logit)) +
            geom_violin(aes(fill=predictor), draw_quantiles=0.5, show.legend = F) +
            xlab(var.name) +
            ylab("log score") +
            theme_bw()
        if(class(df$predictor)=='numeric')
            if(var.name %in% log.names)
                # p<-qplot(predictor, logit, data=df
                #          , geom='point', log ='x')+geom_smooth(se=FALSE)
                p <- ggplot(data = df, aes(x = predictor, y = logit)) +
            geom_point(aes(log = 'x'), show.legend = F) +
            geom_smooth(se=FALSE) +
            xlab(var.name) +
            ylab("log score") +
            theme_bw()
        else
            p<-ggplot(data = df, aes(x = predictor, y = logit)) +
            geom_point(aes(log = 'x'), show.legend = F) +
            geom_smooth(se=FALSE) +
            xlab(var.name) +
            ylab("log score") +
            theme_bw()
        p<-p+xlab(var.name)+ylab('log score')
        results[[var.name]]<-p
        if(display) print(p)
    }
    return(results)
}

# shows the detail of how a score is calcualted given an xgboost model
xgboostScoreDetail <- function(model, modelType = c("binaryClassification", "multiClassification", "regression"), target, dt.singleRow, nclasses = 0){

    if(modelType == "multiClassification" & nclasses == 0) stop("multiClassfication model requires a valid nclasses!")

    require(plyr)
    require(data.table)

    setDT(dt.singleRow)
    # get the tree structure --------------------------------------------------
    if(target %in% names(dt.singleRow)){
        dt.singleRow <- dt.singleRow[, !c(target), with = F]
    }
    featureNames <- names(dt.singleRow)
    dt.tree <- xgb.model.dt.tree(feature_names = featureNames, model = model)
    dt.tree <- dt.tree[order(as.double(dt.tree$Tree), as.double(gsub("[[:digit:]]-", "", dt.tree$ID)))]

    # search trees ------------------------------------------------------------
    if(modelType == "multiClassification"){
        ntrees <- max(dt.tree$Tree) + 1
        nsteps <- ntrees / nclasses

        ls.multiTrees <- list()
        for(class in 1:nclasses){
            tree <- seq(class, ntrees, nclasses)
            ls.multiTrees[[class]] <- dt.tree[Tree %in% (tree - 1)]
        }

        ls.classTree <- list()
        for(j in 1:length(ls.multiTrees)){
            ls.tree <- list()
            ret <- list()

            uniqueTrees <- unique(ls.multiTrees[[j]]$Tree)

            for(i in 1:length(uniqueTrees)){
                ls.tree[[i]] <- dt.tree[Tree == uniqueTrees[i]]
                ret[[i]] <- walkThroughTree(ls.tree[[i]], dt.try)

                # cat(paste("class:", j, "; tree:", i, "\n"))
            }
            ls.classTree[[j]] <- ret
        }
    } else{
        ls.tree <- list()
        ret <- list()
        for(i in 1:(max(dt.tree$Tree) + 1)){
            ls.tree[[i]] <- dt.tree[Tree == i - 1]
            ret[[i]] <- walkThroughTree(ls.tree[[i]], dt.singleRow)
            # cat(paste(i, "\n"))
        }
    }

    # construct the individual feature importance table -----------------------
    if(modelType == "multiClassification"){
        ls.classFeatureWt <- list()
        for(k in 1:length(ls.classTree)){
            ls.featuresWt <- lapply(ls.classTree[[k]], function(x)x$featuresWt)

            dt.featuresWt <- Reduce(sumOnCommonCols, ls.featuresWt)
            dt.featuresWt <- dt.featuresWt[order(-abs(dt.featuresWt$weights)), ]

            setDT(dt.featuresWt)
            ls.classFeatureWt[[k]] <- dt.featuresWt

        }
    } else {
        ls.featuresWt <- lapply(ret, function(x)x$featuresWt)

        dt.featuresWt <- Reduce(sumOnCommonCols, ls.featuresWt)
        dt.featuresWt <- dt.featuresWt[order(-abs(dt.featuresWt$weights)), ]

        setDT(dt.featuresWt)
    }


    # calculate the prediction ------------------------------------------------

    if(modelType == "binaryClassification"){
        pred <- logistic(sum(dt.featuresWt$weights))
    } else if(modelType == "regression"){
        pred <- sum(dt.featuresWt$weights)
    } else if(modelType == "multiClassification"){
        ls.rawScore <- list()
        for(p in 1:length(ls.classFeatureWt)){
            ls.rawScore[[p]] <- exp(sum(ls.classFeatureWt[[p]]$weights))
        }

        vec.probs <- unlist(ls.rawScore) / sum(unlist(ls.rawScore))
        dt.probs <- data.table(class = 1:nclasses
                               , probs = vec.probs)
        pred <- dt.probs
    }

    # Overall feature importance ----------------------------------------------

    importance <- xgb.importance(feature_names = featureNames, model = model)

    if(modelType == "multiClassification"){
        return(list(summaryTrees = ls.classTree
                    , featureWtsIndividual = ls.classFeatureWt
                    , pred = pred
                    , featureImportanceOverall = importance))
    } else{
        return(list(summaryTrees = ret
                    , featureWtsIndividual = dt.featuresWt
                    , pred = pred
                    , featureImportanceOverall = importance))
    }

}


# help to generate a shinyApp
xgboostScoreExplainShinyApp <- function(ret.scoreExplain, model, modelType, target, sampleData, dt.singleRow, top_n_features){
    require(ggplot2)
    require(RColorBrewer)
    require(shiny)
    require(grid)
    require(gridExtra)
    require(caret)
    require(FNN)

    setDT(sampleData)
    setDT(dt.singleRow)

    if(target %in% names(sampleData)){
        sampleData <- sampleData[, !c(target), with = F]
    }
    if(target %in% names(dt.singleRow)){
        dt.singleRow <- dt.singleRow[, !c(target), with = F]
    }

    if(top_n_features > 0){
        colsTopN <- ret.scoreExplain$featureWtsIndividual[1:top_n_features]$featuresOnPath
        dt.singleRow.top <- dt.singleRow[, colsTopN, with = F]
        sampleData.top <- sampleData[, colsTopN, with = F]
    } else{
        colsTopN <- names(dt.singleRow)
        dt.singleRow.top <- dt.singleRow
        sampleData.top <- sampleData
    }


    # data <- rbind(dt.singleRow, sampleData)
    sampleData <- sampleData[complete.cases(sampleData)]

    ## dummy vars
    dataSampleForKNN <- sample_n(sampleData, min(999, nrow(sampleData)))
    dataSampleForKNN <- rbind(dt.singleRow, dataSampleForKNN)

    dummies <- dummyVars(~., dataSampleForKNN, contrasts = T)
    dataDummy <- predict(dummies, newdata = dataSampleForKNN)
    dataDummy <- data.table(dataDummy)

    ## check class for factors
    colClass <- unlist(lapply(dataSampleForKNN, class))
    colsFactor <- names(colClass[colClass %in% c("factor", "character")])
    colsFactor <- paste0(colsFactor, collapse = "|")

    ## scale data for people like you radar plot
    if(colsFactor != ""){
        preProcValues <- preProcess(dataDummy[, !grepl(colsFactor, names(dataDummy)), with = F]
                                    , method = c("range"))
    } else{
        preProcValues <- preProcess(dataDummy
                                    , method = c("range"))
    }

    dataDummyScaled <- predict(preProcValues, dataDummy)
    dt.singleRow.dummyScaled <- dataDummyScaled[1, ]



    ## explain the try for setting up the initial layout (really need the nonZero thing?)
    featuresWt.nonZero <- ret.scoreExplain$featureWtsIndividual[weights != 0]

    ## for plotting the exp plot
    x <- seq(-5, 5, by = .01)
    y <- logistic(x)
    data.exp <- data.table(x = x, y = y)

    ## find the nearest neighbours
    # remove NA
    dataDummyScaled <- dataDummyScaled[complete.cases(dataDummyScaled)]

    ## predict the sample data
    m.data <- data.matrix(dataSampleForKNN)
    ddata <- xgb.DMatrix(data = m.data, missing = NaN)
    pred.data <- predict(model, ddata)
    dt.plot.pred.data <- data.table(preds = pred.data)[complete.cases(dataDummyScaled)]

    ind.nearest <- as.numeric(knnx.index(dataDummyScaled[-1, ], dataDummyScaled[1, ], k = 30))
    dt.nearest <- dataSampleForKNN[ind.nearest + 1]
    m.nearest <- data.matrix(dt.nearest)
    dnearest <- xgb.DMatrix(data = m.nearest, missing = NaN)
    pred.nearest <- predict(model, dnearest)
    left.nearest <- ind.nearest[pred.nearest == min(pred.nearest)][1]
    right.nearest <- ind.nearest[pred.nearest == max(pred.nearest)][1]
    dt.left.nearest <- dataSampleForKNN[left.nearest][, colsTopN, with = F]
    dt.right.nearest <- dataSampleForKNN[right.nearest][, colsTopN, with = F]
    dt.left.dummyScaled <- dataDummyScaled[left.nearest][, colsTopN, with = F]
    dt.right.dummyScaled <- dataDummyScaled[right.nearest][, colsTopN, with = F]

    m.singleRow <- data.matrix(dt.singleRow)
    dsingleRow <- xgb.DMatrix(data = m.singleRow, missing = NaN)

    dt.radar <- data.table(group = c("You", "Leftmost", "Rightmost")
                           , rbind(dt.singleRow.dummyScaled[, colsTopN, with = F]
                                   , dt.left.dummyScaled
                                   , dt.right.dummyScaled))

    dt.nearestPpl <- data.frame(Person = c("You", "Leftmost", "Rightmost")
                                , Score = as.character(c(predict(model, dsingleRow)
                                                         , min(pred.nearest)
                                                         , max(pred.nearest)))
                                , rbind(dt.singleRow[, colsTopN, with = F]
                                        , dt.left.nearest
                                        , dt.right.nearest))

    ## if too many featuers, it is gonna be slow
    if(ncol(dt.singleRow) <= 20){
        ## ui
        ui <- shinyUI(fluidPage(

            # Application title
            titlePanel("xgboost Score Explaination"),

            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    uiOutput("inputs")
                    , br()
                    , actionButton("reset", "Reset")
                ),

                # Show a plot of the generated distribution
                mainPanel(
                    tabsetPanel(
                        tabPanel("Score", plotOutput("scorePlots"))
                        , tabPanel("People Like You", plotOutput("radar"), tableOutput("pplLikeYou"))
                        , tabPanel("Features"
                                   , plotOutput("featurePlots", height = 150)
                                   , br()
                                   , uiOutput("selectFeature")
                                   , plotOutput("featureEffectPlots")
                        )
                        , tabPanel("Score Breakdown by Features", htmlOutput("texts", container = div))
                    )
                )
            )
        ))

        ## server
        server <- shinyServer(function(input, output, session) {

            ## dynamic sliderbars
            output$inputs <- renderUI({
                input$reset

                featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath

                if(top_n_features > 0){
                    featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath[1:top_n_features]
                }

                lapply(featuresOnPath, function(feature) {
                    if(class(dt.singleRow.top[[feature]]) == "numeric"){
                        sliderInput(inputId = paste0("input_", feature)
                                    , label = feature
                                    , min = min(sampleData.top[[feature]], na.rm = T)
                                    , max = max(sampleData.top[[feature]], na.rm = T)
                                    , value = dt.singleRow[[feature]])
                    } else if(class(dt.singleRow.top[[feature]]) %in% c("factor", "character")){
                        selectInput(inputId = paste0("input_", feature)
                                    , label = feature
                                    , choices = as.list(levels(sampleData.top[[feature]]))
                                    , selected = dt.singleRow[[feature]]
                        )
                    }

                })

            })

            ## reactively calculates prediction and individual feature importance
            ls.featuresWt <- reactive({
                dt.play <- dt.singleRow

                featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath

                if(top_n_features > 0){
                    featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath[1:top_n_features]
                }

                for(feature in featuresOnPath){
                    dt.play[[feature]] <- input[[paste0("input_", feature)]]
                }

                new.ret.scoreExplain <- xgboostScoreDetail(model, modelType, target = target, dt.play)
                new.ret.scoreExplain$featureWtsIndividual$featuresOnPath <- factor(new.ret.scoreExplain$featureWtsIndividual$featuresOnPath
                                                                                   , levels = unique(new.ret.scoreExplain$featureWtsIndividual$featuresOnPath)[order(abs(new.ret.scoreExplain$featureWtsIndividual$weights))])

                featuresWeights <- new.ret.scoreExplain$featureWtsIndividual[weights != 0]
                dt.play.plot <- dt.play

                if(top_n_features > 0){
                    featuresWeights <- new.ret.scoreExplain$featureWtsIndividual[1:top_n_features][weights != 0]
                    dt.play.plot <- dt.play[, colsTopN]
                }

                list(pred = new.ret.scoreExplain$pred
                     , dt = featuresWeights
                     , dtRaw = new.ret.scoreExplain$featureWtsIndividual[weights != 0]
                     , dt.play = dt.play.plot)


            })

            ## plots for score interpretation
            output$scorePlots <- renderPlot({

                ls.ret <- ls.featuresWt()

                # dist plot
                plot.dist <- ggplot(dt.plot.pred.data, aes(preds)) +
                    geom_histogram(binwidth = 1 / length(dt.plot.pred.data$preds)) +
                    xlab("Score") +
                    ylab("Fequency") +
                    theme_bw() +
                    geom_vline(xintercept = ls.ret$pred, colour = "salmon", linetype = "dashed") +
                    annotate("label", fill = "salmon", label = round(ls.ret$pred, 3), x = ls.ret$pred + .001, y = 100, colour = "white")

                # exp plot
                plot.exp <- ggplot(data.exp, aes(x = x, y = y)) +
                    geom_line() +
                    xlab("Raw Score") +
                    ylab("Score") +
                    theme_bw() +
                    geom_hline(yintercept = ls.ret$pred, colour = "salmon", linetype = "dashed") +
                    annotate("label", fill = "salmon", label = round(ls.ret$pred, 3), x = -4, y = ls.ret$pred + .1, colour = "white")

                ls.ret$dt$featuresOnPath <- factor(ls.ret$dt$featuresOnPath
                                                   , levels = unique(ls.ret$dt$featuresOnPath)[order(abs(ls.ret$dt$weights))])


                grid.arrange(plot.dist, plot.exp, ncol = 1)

            })

            ## plots for feature importance
            output$featurePlots <- renderPlot({

                ls.ret <- ls.featuresWt()

                # individual feature importance plot
                plot.featuresImp <- ggplot(ls.ret$dt
                                           , aes(x = featuresOnPath
                                                 , y = abs(weights)
                                                 , fill = featuresOnPath)) +
                    geom_bar(stat = "identity", position = "identity", show.legend = F) +
                    xlab("Important Features") +
                    ylab("Individual Importance") +
                    coord_flip() +
                    theme_bw() +
                    theme(axis.text.x = element_blank())

                # feature weights plot
                plot.featuresWt <- ggplot(ls.ret$dt
                                          , aes(x = featuresOnPath
                                                , y = weights
                                                , fill = featuresOnPath)) +
                    geom_bar(stat = "identity", position = "identity", show.legend = F) +
                    geom_hline(yintercept = 0, colour = "#990000", linetype = "dashed") +
                    # ylim(c(-.05, .05)) +
                    geom_label(aes(label = round(weights, 3), hjust = ifelse(weights <= 0, 1, 0)), show.legend = F) +
                    ylab("Gain (Quantified Importance)") +
                    coord_flip() +
                    theme_bw() +
                    theme(axis.title.y = element_blank(),
                          axis.text.x = element_blank())


                grid.arrange(plot.featuresImp, plot.featuresWt, ncol = 2)

            })

            ## dynamic select a feature
            output$selectFeature <- renderUI({
                # ls.ret <- ls.featuresWt()
                div(style="display:inline-block"
                    , selectInput(inputId = "selectedFeature"
                                  , label = "Select a feature for detailed effect"
                                  , choices = as.list(ret.scoreExplain$featureWtsIndividual$featuresOnPath)
                                  , selected = ret.scoreExplain$featureWtsIndividual$featuresOnPath[1])
                    , style="float:right")



            })

            output$featureEffectPlots <- renderPlot({
                ls.ret <- ls.featuresWt()

                ## for plotting the feature plots
                input$selectedFeature

                myBinaryClassifierEffect(pred.data, dataSampleForKNN, input$selectedFeature, log.names = "score")

            })

            ## score breakdown
            output$texts <- renderUI({

                ls.ret <- ls.featuresWt()

                str <- character()
                for(i in 1:nrow(ls.ret$dtRaw)){
                    str <- paste(str, paste0(ls.ret$dtRaw[i]$featuresOnPath, ": ", ls.ret$dtRaw[i]$weights), "<br/>")
                }
                head <- paste("<strong> Scoring Detail </strong>", "<br/>", "-----------------------------------------------------------------------------", "<br/>")
                str <- paste(head, str, "--------------------------", "<br/>")
                str <- paste(str, "<strong>SUM </strong>:", sum(ls.ret$dtRaw$weights), "<br/>")
                str <- paste(str, "-----------------------------------------------------------------------------", "<br/>")
                str <- paste0(str, " exp(<strong>", sum(ls.ret$dtRaw$weights), "</strong>) /", " (1 + exp(<strong>", sum(ls.ret$dtRaw$weights), "</strong>)) = ", ls.ret$pred)
                str <- paste("<div align='right' style = 'background-color = #F5F6CE'>", str, "</div>")
                HTML(str)
            })

            ## people like you plot
            output$radar <- renderPlot({

                # print(CreateRadialPlot(dt.you, plot.extent.x = 1.5))
                p1 <- CreateRadialPlot(setDF(dt.radar)[1, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                p2 <- CreateRadialPlot(setDF(dt.radar)[2, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                p3 <- CreateRadialPlot(setDF(dt.radar)[3, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                grid.arrange(p1, p2, p3, ncol = 3)
            })

            ## people like you table
            output$pplLikeYou <- renderTable({
                dt.nearestPpl
            }
            , include.rownames = F)
        })
    } else{
        ## ui
        ui <- shinyUI(fluidPage(

            # Application title
            titlePanel("xgboost Score Explaination"),

            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    uiOutput("inputs")
                    , br()
                    , actionButton("reset", "Reset")
                ),

                # Show a plot of the generated distribution
                mainPanel(
                    tabsetPanel(
                        tabPanel("Score", plotOutput("scorePlots"))
                        , tabPanel("People Like You", plotOutput("radar"), tableOutput("pplLikeYou"))
                        , tabPanel("Features"
                                   , plotOutput("featurePlots", height = 150)
                                   , br()
                                   , uiOutput("selectFeature")
                                   , plotOutput("featureEffectPlots")
                        )
                        , tabPanel("Score Breakdown by Features", htmlOutput("texts", container = div))
                    )
                )
            )
        ))

        ## server
        server <- shinyServer(function(input, output, session) {

            ## dynamic sliderbars
            output$inputs <- renderUI({
                input$reset

                featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath

                if(top_n_features > 0){
                    featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath[1:top_n_features]
                }

                lapply(featuresOnPath, function(feature) {
                    if(class(dt.singleRow.top[[feature]]) == "numeric"){
                        sliderInput(inputId = paste0("input_", feature)
                                    , label = feature
                                    , min = min(sampleData.top[[feature]], na.rm = T)
                                    , max = max(sampleData.top[[feature]], na.rm = T)
                                    , value = dt.singleRow[[feature]])
                    } else if(class(dt.singleRow.top[[feature]]) %in% c("factor", "character")){
                        selectInput(inputId = paste0("input_", feature)
                                    , label = feature
                                    , choices = as.list(levels(sampleData.top[[feature]]))
                                    , selected = dt.singleRow[[feature]]
                        )
                    }

                })

            })

            ## reactively calculates prediction and individual feature importance
            ls.featuresWt <- reactive({
                dt.play <- dt.singleRow

                featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath

                if(top_n_features > 0){
                    featuresOnPath <- ret.scoreExplain$featureWtsIndividual$featuresOnPath[1:top_n_features]
                }

                for(feature in featuresOnPath){
                    dt.play[[feature]] <- input[[paste0("input_", feature)]]
                }

                new.ret.scoreExplain <- ret.scoreExplain
                new.ret.scoreExplain$featureWtsIndividual$featuresOnPath <- factor(new.ret.scoreExplain$featureWtsIndividual$featuresOnPath
                                                                                   , levels = unique(new.ret.scoreExplain$featureWtsIndividual$featuresOnPath)[order(abs(new.ret.scoreExplain$featureWtsIndividual$weights))])

                featuresWeights <- new.ret.scoreExplain$featureWtsIndividual[weights != 0]
                dt.play.plot <- dt.play

                if(top_n_features > 0){
                    featuresWeights <- new.ret.scoreExplain$featureWtsIndividual[1:top_n_features][weights != 0]
                    dt.play.plot <- dt.play[, colsTopN]
                }
                mx.play <- data.matrix(dt.play)
                dmx.play <- xgb.DMatrix(mx.play)
                list(pred = predict(model, dmx.play)
                     , dt = featuresWeights
                     , dtRaw = new.ret.scoreExplain$featureWtsIndividual[weights != 0]
                     , dt.play = dt.play.plot
                )


            })

            ## plots for score interpretation
            output$scorePlots <- renderPlot({

                ls.ret <- ls.featuresWt()

                # dist plot
                plot.dist <- ggplot(dt.plot.pred.data, aes(preds)) +
                    geom_histogram(binwidth = 1 / length(dt.plot.pred.data$preds)) +
                    xlab("Score") +
                    ylab("Fequency") +
                    theme_bw() +
                    geom_vline(xintercept = ls.ret$pred, colour = "salmon", linetype = "dashed") +
                    annotate("label", fill = "salmon", label = round(ls.ret$pred, 3), x = ls.ret$pred + .001, y = 100, colour = "white")

                # exp plot
                plot.exp <- ggplot(data.exp, aes(x = x, y = y)) +
                    geom_line() +
                    xlab("Raw Score") +
                    ylab("Score") +
                    theme_bw() +
                    geom_hline(yintercept = ls.ret$pred, colour = "salmon", linetype = "dashed") +
                    annotate("label", fill = "salmon", label = round(ls.ret$pred, 3), x = -4, y = ls.ret$pred + .1, colour = "white")

                # ls.ret$dt$featuresOnPath <- factor(ls.ret$dt$featuresOnPath
                #                                    , levels = unique(ls.ret$dt$featuresOnPath)[order(abs(ls.ret$dt$weights))])
                #

                grid.arrange(plot.dist, plot.exp, ncol = 1)

            })

            ## plots for feature importance
            output$featurePlots <- renderPlot({

                ls.ret <- ls.featuresWt()

                # individual feature importance plot
                plot.featuresImp <- ggplot(ls.ret$dt
                                           , aes(x = featuresOnPath
                                                 , y = abs(weights)
                                                 , fill = featuresOnPath)) +
                    geom_bar(stat = "identity", position = "identity", show.legend = F) +
                    xlab("Important Features") +
                    ylab("Individual Importance") +
                    coord_flip() +
                    theme_bw() +
                    theme(axis.text.x = element_blank())

                # feature weights plot
                plot.featuresWt <- ggplot(ls.ret$dt
                                          , aes(x = featuresOnPath
                                                , y = weights
                                                , fill = featuresOnPath)) +
                    geom_bar(stat = "identity", position = "identity", show.legend = F) +
                    geom_hline(yintercept = 0, colour = "#990000", linetype = "dashed") +
                    # ylim(c(-.05, .05)) +
                    geom_label(aes(label = round(weights, 3), hjust = ifelse(weights <= 0, 1, 0)), show.legend = F) +
                    ylab("Gain (Quantified Importance)") +
                    coord_flip() +
                    theme_bw() +
                    theme(axis.title.y = element_blank(),
                          axis.text.x = element_blank())


                grid.arrange(plot.featuresImp, plot.featuresWt, ncol = 2)

            })

            ## dynamic select a feature
            output$selectFeature <- renderUI({
                # ls.ret <- ls.featuresWt()
                div(style="display:inline-block"
                    , selectInput(inputId = "selectedFeature"
                                  , label = "Select a feature for detailed effect"
                                  , choices = as.list(ret.scoreExplain$featureWtsIndividual$featuresOnPath)
                                  , selected = ret.scoreExplain$featureWtsIndividual$featuresOnPath[1])
                    , style="float:right")



            })

            output$featureEffectPlots <- renderPlot({
                ls.ret <- ls.featuresWt()

                ## for plotting the feature plots
                input$selectedFeature

                myBinaryClassifierEffect(pred.data, dataSampleForKNN, input$selectedFeature, log.names = "score")

            })

            ## score breakdown
            output$texts <- renderUI({

                ls.ret <- ls.featuresWt()

                str <- character()
                for(i in 1:nrow(ls.ret$dtRaw)){
                    str <- paste(str, paste0(ls.ret$dtRaw[i]$featuresOnPath, ": ", ls.ret$dtRaw[i]$weights), "<br/>")
                }
                head <- paste("<strong> Scoring Detail </strong>", "<br/>", "-----------------------------------------------------------------------------", "<br/>")
                str <- paste(head, str, "--------------------------", "<br/>")
                str <- paste(str, "<strong>SUM </strong>:", sum(ls.ret$dtRaw$weights), "<br/>")
                str <- paste(str, "-----------------------------------------------------------------------------", "<br/>")
                str <- paste0(str, " exp(<strong>", sum(ls.ret$dtRaw$weights), "</strong>) /", " (1 + exp(<strong>", sum(ls.ret$dtRaw$weights), "</strong>)) = ", ls.ret$pred)
                str <- paste("<div align='right' style = 'background-color = #F5F6CE'>", str, "</div>")
                HTML(str)
            })

            ## people like you plot
            output$radar <- renderPlot({

                # print(CreateRadialPlot(dt.you, plot.extent.x = 1.5))
                p1 <- CreateRadialPlot(setDF(dt.radar)[1, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                p2 <- CreateRadialPlot(setDF(dt.radar)[2, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                p3 <- CreateRadialPlot(setDF(dt.radar)[3, ]
                                       , grid.min = 0, grid.mid = .5, grid.max = 1
                                       , plot.extent.x = 1.5
                                       , background.circle.colour = "white"
                                       , legend.title = "Person")

                grid.arrange(p1, p2, p3, ncol = 3)
            })

            ## people like you table
            output$pplLikeYou <- renderTable({
                dt.nearestPpl
            }
            , include.rownames = F)
        })
    }


    ## return shiny app
    return(shinyApp(ui, server))


}

#' Explain an observation's score from an xgboost model and show an shiny app
#'
#' @note The function now only supports binaryClassification model. These two limitations will be enabled in the future.
#'
#' @description
#' By taking the trained xgboost model and a single observation (without the dependent variable) as the input, the function produces a summary of each tree including information such as gain, features on the path, and feature weights. In addition, the function ouputs the aggregated individual feature importance, the calculated prediction of the observation, as well as the overall feature importance.
#'
#' @param model A xgboost model
#' @param modelType Either c("binaryClassification", "regression")
#' @param target The target variable
#' @param dt.singleRow A single row of observation
#' @param nclasses Number of classes if you are explaining a mulitiClassification model, default is 3
#' @param shiny T/F, whether or not to generate an shiny app as part of the returned object
#' @param sampleData The sample data which which was used to train the model
#'
#' @return a list containing summaryTrees, featureWtsIndividual, pred, featureImportanceOverall and (shiny)
#'
#' @examples
#' require(xgboost)
#'
#' # regression --------------
#' data(mtcars)
#'
#' m.train <- data.matrix(mtcars[, -1])
#' dtrain <- xgb.DMatrix(data = m.train, label = mtcars$mpg, missing = NaN)
#'
#' params <- list(objective = "reg:linear",
#'                booster = "gbtree",
#'                eval_metric = "rmse",
#'                eta                 = 0.02,
#'                max_depth           = 4,
#'                subsample           = 0.83,
#'                colsample_bytree    = 0.77
#' )
#'
#' model <- xgb.train(params = params
#'                    , data = dtrain
#'                    , nrounds = 3
#'                    # , watchlist = watchlist
#'                    , maximize = F
#'                    , print.every.n = 1
#'                    # , early.stop.round = 1
#' )
#'
#' dt.singleRow<- mtcars[1, ]
#' m.try <- data.matrix(dt.singleRow[, -1])
#' dtry <- xgb.DMatrix(data = m.try, label = dt.singleRow$mpg, missing = NaN)
#'
#' res <- xgboostScoreExplain(model, modelType = "regression", target = "mpg", dt.singleRow = dt.singleRow)
#'
#' # binaryClassification --------------
#' set.seed(42)
#' x = iris
#' x$Species = ifelse(x$Species == "versicolor",1, 0)
#' dat = splitDataToTrainTestDataFrame(x,.9,.25)
#' mod = xgboostModel(dat,"Species",nrounds = 200, early.stop.round = 5, verbose = 1)
#'
#' # single row of observation
#' xx <- x[1, -5]
#'
#' # model
#' model <- mod$model
#'
#' m.try <- data.matrix(xx)
#' dtry <- xgb.DMatrix(data = m.try, label = x[1,]$Species, missing = NaN)
#'
#' # score explain
#' res <- xgboostScoreExplain(model = model, modelType = "binaryClassification", target = "Species", dt.singleRow = xx, shiny = T, sampleData = x)
#' res
#'
#'
#' # multiClassification -----------------------------------------------------
#' require(data.table)
#' require(xgboost)
#' require(caret)
#' data(iris)
#'
#' setDT(iris)
#'
#' iris[, target := ifelse(Species == "setosa", 0
#'                         , ifelse(Species == "versicolor", 1, 2))]
#' iris[, Species := NULL]
#'
#' # add some noise
#' set.seed(1)
#' iris <- rbind(iris
#'               , data.table(Sepal.Length = rnorm(20, mean = 4, 1)
#'                            , Sepal.Width = rnorm(20, mean = 2, .5)
#'                            , Petal.Length = rnorm(20, mean = 2, .1)
#'                            , Petal.Width = rnorm(20, mean = 1, .01)
#'                            , target = sample(0:2, 20, replace = T)
#'               )
#' )
#'
#' set.seed(1)
#' ind.train <- createDataPartition(iris$target, p = .8, list = F)
#' dt.train <- iris[ind.train]
#' dt.valid <- iris[!ind.train]
#'
#' mx.train <- as.matrix(dt.train[, !c("target"), with = F])
#' mx.valid <- as.matrix(dt.valid[, !c("target"), with = F])
#'
#' dmx.train <- xgb.DMatrix(data = mx.train, label = dt.train$target)
#' dmx.valid <- xgb.DMatrix(data = mx.valid, label = dt.valid$target)
#'
#' watchlist <- list(valid = dmx.valid, train = dmx.train)
#' params <- list(objective = "multi:softmax"
#'                , booster = "gbtree"
#'                , num_class = 3
#'                , eta = 1)
#'
#' set.seed(1)
#' mod <- xgb.train(params = params
#'                  , data = dmx.train
#'                  , watchlist = watchlist
#'                  , nrounds = 10
#'                  , verbose = 1
#'                  , print.every.n = 1
#'                  , early.stop.round = 3
#'                  , maximize = F)
#'
#' dt.try <- dt.valid[, !c("target"), with = F][1]
#' dt.try
#'
#' res <- xgboostScoreExplain(model = mod, modelType = "multiClassification", target = "target", dt.singleRow = dt.try, nclasses = 3, shiny = F, sampleData = x)
#' res
#'
#'
#' @seealso \code{\link{xgboostModel}}
#'
#' @import plyr
#' @import data.table
#' @import ggplot2
#' @import RColorBrewer
#' @import shiny
#' @import grid
#' @import gridExtra
#' @import caret
#' @import FNN
#' @export
xgboostScoreExplain <- function(model
                                , modelType = c("multiClassification", "binaryClassification", "regression")
                                , target = "target"
                                , dt.singleRow
                                , nclasses = 3
                                , shiny = F
                                , sampleData = NULL
                                , top_n_features = 10
){
    if(!modelType %in% c("binaryClassification", "multiClassification", "regression")) stop("Please specify a valid modelType")
    if(modelType != "binaryClassification"){
        shiny == F
    }
    ret.scoreExplain <- xgboostScoreDetail(model = model
                                           , modelType = modelType
                                           , target = target
                                           , dt.singleRow = dt.singleRow
                                           , nclasses = nclasses)
    if(shiny == T){
        if(is.null(sampleData)){
            warning("Need to provide sampleData and target if you set shiny = T!")
        } else{
            ret.shiny <- xgboostScoreExplainShinyApp(ret.scoreExplain = ret.scoreExplain
                                                     , model = model
                                                     , modelType = modelType
                                                     , target = target
                                                     , sampleData = sampleData
                                                     , dt.singleRow = dt.singleRow
                                                     , top_n_features = top_n_features)
            ret.scoreExplain$shiny <- ret.shiny
        }
    }

    return(ret.scoreExplain)

}
