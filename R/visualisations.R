#' Return the Quant colours.
#'
#' This function takes no arguments and returns a vector of 7 Hex codes
#' describing the colours used in the rquant logo.
#'
#'@examples
#'\dontrun{
#'quantclrs <- quantColours(inv = 3)
#'scales::show_col(quantclrs)
#'}
#' @export
quantColours = function(i = NULL, inv = 1){
    # x = c("#812990", "#001C86", "#0473B6", "#00833F", "#79C143", "#799A0D", "#FFCC31")
    if(inv==1){
        x = c("#004165", "#4984b3", "#007dba", "#49c7eb", "#b9e0f6", "#747678", "#df7a50", "#fdc82f", "#00833F") # "#5bc6e8",
    }else if(inv==2){
        x = c("#004165", "#49c7eb", "#fdc82f", "#4984b3", "#df7a50", "#00833F", "#007dba", "#b9e0f6", "#747678") # "#5bc6e8",
    }else{
        x = c("#004165", "#ff0000", "#49c7eb", "#fdc82f", "#df7a50", "#4984b3", "#00833F", "#007dba", "#b9e0f6", "#747678") #
    }

    # this is not very robust - what if 1 not in 1:6 or NA
    if(is.null(i)){
        return(x)
    }else{
        return(x[i])
    }
}



#' Return the Quant Report header text and header background colours.
#'
#' This function takes no arguments and returns a \emph{list} of 2 Hex codes
#' describing the colours used in the Quant header.
#'
#'@examples
#'\dontrun{
#'quantheader <- quantFormatting()
#'scales::show_col(unlist(quantheader))
#'}
#' @export
quantFormatting = function(){
    formatting = list(
        table = list(
            headerBackgroundColour = quantColours(1),
            headerFontColour = "#FFFFFF"
        )
    )
    return(formatting)
}


#' Create a Spider graph Plotly object
#'
#' @description
#' Generic function to create an interactive Spider graph in Plotly
#'
#' @param dataset An input dataset to display in the plot
#' @param benchmark The function used to create the benchmark category in the plot (Eg: mean, median, other bespoke function)
#'
#' @return A plotly object
#'
#' @details
#' The input dataset needs to have:
#' \itemize{
#'  \item First column must be a vector of characters representing the names of categories to be compared
#'  \item The remaining columns must only contain numeric values that can be converted into spider graph coordinates
#' }
#'
#' @examples
#' # Transforming some data into the format required with the first column containing names
#' test <- mtcars[1:5,1:7]
#' test$name <- rownames(test)
#' test <- test[,c(8,1:7)]
#'
#' spiderPlotly(test)
#'
#' @export
spiderPlotly = function(dataset, benchmark = mean){

    library(plotly)
    library(reshape2)

    dataset <- as.data.frame(dataset)
    names(dataset)[1] <- "name"

    # Calculate some sort of 'benchmark' summary statistic
    # I'll use average for now

    means <- as.data.frame(t(apply(dataset[,-1],2,benchmark)))
    means[,ncol(means) + 1] <- "Benchmark"
    means <- means[,c(ncol(means),1:(ncol(means)-1))]
    names(means)[1] <- names(dataset)[1]
    baseData <- rbind(dataset,means)

    # Perform some sort of scaling so we can plot it on a nice, compact, standardised axes
    # I've chosen ranking at the moment for our scaling method

    # Take a copy of base data for later on
    orig <- baseData

    dat <- as.data.frame(apply(baseData[,-1],2,rank))
    dat <- cbind(baseData[,1],dat)
    names(dat)[1] <- names(baseData)[1]

    # Grab a few parameters for plotting later on
    namesOfLabels <- names(dat)[2:ncol(dat)]
    numComparisonElements <- ncol(dat)-1
    minVal <- floor(min(dat[,-1]))
    maxVal <- ceiling(max(dat[,-1]))

    # Melt
    dat <- melt(dat, id = names(dat)[1])

    # Function to get polar coordinates for plotting around a circle

    getPolarCoord <- function(r, matrix = F, na = F){
        # Get starting angle and angle increments
        theta <- 0
        dtheta <- 360 / length(r)
        dtheta <- (pi / 180) * dtheta  # in radians

        # Get polar coordinates
        x <- c()
        y <- c()

        for(i in 1:length(r)){

            x <- c(x, r[i] * cos(theta))
            y <- c(y, r[i] * sin(theta))

            theta <- theta + dtheta
        }

        x[length(x) + 1] <- x[1]
        y[length(y) + 1] <- y[1]

        if(na == T){
            x[length(x) + 1] <- NA
            y[length(y) + 1] <- NA
        }


        if(matrix == T){
            return(cbind(x, y))
        }else{
            return(list(x = x,
                        y = y))
        }
    }

    # Collection of co-ords for all plots

    tmp <- by(dat, dat[,1], function(r){
        x <- getPolarCoord(r[,3])
        x <- cbind(x$x, x$y)
        x <- data.frame(rbind(r, r[1,]), x = x[,1], y = x[,2])
        return(x)
    })

    coords <- data.frame()
    for (i in 1:length(tmp)){
        coords <- rbind(coords,tmp[[i]])
    }

    coords$rowNum <- c(1:nrow(coords))


    # Get original values for labelling

    orig <- melt(orig, id = names(orig)[1])

    tmp2 <- by(orig, orig[,1], function(r){
        x <- getPolarCoord(r[,3])
        x <- cbind(x$x, x$y)
        x <- data.frame(rbind(r, r[1,]), x = x[,1], y = x[,2])
        return(x)
    })

    coordsOrig <- data.frame()
    for (i in 1:length(tmp2)){
        coordsOrig <- rbind(coordsOrig,tmp2[[i]])
    }

    origLabels <- data.frame(coordsOrig, txt = paste(coordsOrig$name, "<br>",
                                                     coordsOrig$variable, ":",
                                                     coordsOrig$value))
    origLabels$rowNum <- c(1:nrow(origLabels))
    origLabels <- origLabels[,c(6,7)]

    df <- merge(coords,origLabels,by = names(coords)[6],all.x=T)
    df <- df[,-1]



    # Plot

    ## Base plot

    smooth <- 1
    bgcolor <- "white"

    p <- plot_ly(data = df,
                 x = x, y = y, mode = "lines",
                 group = 'name',
                 fill = "toself",
                 line = list(smoothing = smooth), #shape = "spline"),
                 hoverinfo = "text",
                 text = txt) %>%

        add_trace(data = df,
                  x = x, y = y, mode = "markers",
                  marker = list(color = "white",
                                size = 4,
                                line = list(width = 1)),
                  hoverinfo = "none",
                  showlegend = F) %>%

        layout(xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                            domain = c(0.02, 0.48)),
               yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                            domain = c(0, 0.9)),
               font = list(family = "serif", size = 15),
               legend = list(x = 0.6, y = 0.9, bgcolor = "transparent"),
               # hovermode = "closest",
               plot_bgcolor = bgcolor,
               paper_bgcolor = bgcolor)


    ## Add radial grids
    # Given how many 'radial grids' we want, this function returns evenly spaced distances
    # Adding 'scale to the radial axes'

    evenLegendSplitter <- function(min,max,n){
        interval <- (max - min)/(n-1)
        lengths <- seq(min,max, by = interval)
        return(lengths)
    }

    circles <- evenLegendSplitter(minVal,maxVal,5)

    grid <- data.frame()
    for (i in 1:length(circles)){
        tmp <- getPolarCoord(rep(circles[i],round(circles[i]*100)),T,T)
        grid <- rbind(grid,tmp)
    }

    grid <- as.data.frame(grid)

    p <- add_trace(p, data = grid,
                   x = x, y = y, mode = "lines",
                   line = list(color = "#57788e", dash = "4px", width = 1),
                   showlegend = F,
                   hoverinfo = "none")

    ## Add on the lines pointing outwards from the origin to represent each of the attributes we
    ## want to represent on our spider graph

    # This function returns co-ordinates for the lines given how many how long we want them and how many attributes

    inner <- getPolarCoord(rep(0.9*minVal, numComparisonElements))
    outer <- getPolarCoord(rep(1.1*maxVal, numComparisonElements))

    x = t(cbind(inner$x, outer$x))
    y = t(cbind(inner$y, outer$y))

    x <- as.numeric(apply(x, 2, function(vec){
        return(c(vec, NA))
    }))

    y <- as.numeric(apply(y, 2, function(vec){
        return(c(vec, NA))
    }))

    linegrid <- data.frame(x = x, y = y)

    p <- add_trace(p, data = linegrid,
                   x = x, y = y, mode = "lines",
                   line = list(color = "#57788e", dash = "4px", width = 1),
                   showlegend = F,
                   hoverinfo = "none")


    ## Add text for each of the attributes

    labels <- paste0(namesOfLabels)

    p <- add_trace(p, data = getPolarCoord(rep((1.1*maxVal)+1, length(labels))),
                   x = x, y = y, mode = "text", text = labels,
                   showlegend = F,
                   hoverinfo = "none")

    return(p)

}


#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @examples
#' x <- 1:10
#' y1 <- 1:10
#' y2 <- 21:30
#'
#' plt1 <- ggplot(data=data.frame(x=x, y=y1)) + geom_point(aes(x,y))
#' plt2 <- ggplot(data=data.frame(x=x, y=y1)) + geom_line(aes(x,y))
#' plt3 <- ggplot(data=data.frame(x=x, y=y1 + rnorm(10))) + geom_line(aes(x,y))
#'
#' multiplot(plt1,plt2,plt3, cols = 2)
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
        }
    }
}

#' Use a ggplot2 theme with RQuant aesthetics
#'
#' Adds a nice background, changes colours (and fill colours) to utilise
#' RQuant colours, changes text to a RQuant font standin, changes text size,
#' and adds an optional RQuant logo. The logo can be rendered at half
#' or full opacity and can be renderd in the centre or the corner at different sizes. \cr
#' If you want to render the logo \emph{behind} the graphical objects just put your
#'  call to \code{\link{theme_rquant}} earlier than your call to those objects.
#'
#'
#'@param simple_theme logical. Use a simpler theme? default is TRUE.
#'@param rquant_font logical. Use the RQuant font? (Bold Calibri is used as a stand-in)
#'@param rquant_colours logical. Use the RQuant colours for colour and fill?
#'@param font_size text size (in pts)
#'@param logo character. one of \code{c('none','RQuant')}
#'@param opacity numeric. 0 to 1, default 0.5
#'@param inv 1,2,3 color themes
#'@param position an integer usually less than 8. different logos have different options. The first 5 options are: \cr
#'0 - centre, large \cr
#'1 - top-left, medium \cr
#'2 - top-right, medium \cr
#'3 - bottom-right, medium \cr
#'4 - bottom-left, medium
#'
#'
#'@examples
#'\dontrun{
#'set.seed(1)
#'dt <- data.frame(x = 1:10, y = 1:10, z = runif(10)*10, c  = sample(LETTERS[1:5],10, replace = TRUE))
#'dt
#'
#'p <- ggplot(dt) +
#'geom_bar(aes(x= x, y = y, fill = rev(c)), stat = "identity") +
#'geom_point(aes(x= x, y = z, col = c), size = 5) +
#'ggtitle("RQuant Plot")
#'
#'p
#'
#'# Add RQuant colours
#'p + theme_rquant(simple_theme = FALSE, rquant_font = FALSE)
#'
#'# Add a visually pleasing theme (theme_minimal())
#'p + theme_rquant(rquant_font = FALSE)
#'
#'# Add RQuant font
#'p + theme_rquant()
#'
#'# Add the RQuant logo
#'p + theme_rquant(logo = "RQuant")
#'
#'# Add a transparent logo
#'p + theme_rquant(logo = "RQuant", opacity = 0.3)
#'
#'# change the logos position and size
#'p + theme_rquant(logo = "RQuant", opacity = 1, position = 1)
#'p + theme_rquant(logo = "RQuant", opacity = 1, position = 3)
#'
#'# Add a logo which sits 'behind' the graphical objects
#'ggplot(dt) +
#'theme_rquant(logo = "RQuant", opacity = 0.5, position = 0) +
#'geom_bar(aes(x= x, y = y, fill = rev(c)), stat = "identity") +
#'geom_point(aes(x= x, y = z, col = c), size = 5) +
#'ggtitle("RQuant Plot")
#'
#'}
#'
#'@export
theme_rquant <- function(simple_theme = TRUE,
                          rquant_font = TRUE,
                          rquant_colours = TRUE,
                          font_size = 12L,
                          logo = "none",
                          opacity = 0.1,
                          position = 0L,
                          inv = FALSE){

    library(ggplot2)
    library(png)
    library(grid)
    library(gridExtra)

    if(!is.logical(simple_theme)) stop("argument 'simple_theme' must be logical")
    if(!is.logical(rquant_font)) stop("argument 'rquant_font' must be logical")
    if(!is.logical(rquant_colours)) stop("argument 'rquant_colours' must be logical")

    logo <- tolower(logo)
    if(!logo %in% c("rquant","none") ) stop("logo must be 'rquant' or 'none'.\nCase does not matter.")

    if(logo != "none"){
        # alpha = switch(opacity,
        #                "full" = "100",
        #                "half" = "50")
        # if(is.null(alpha)) stop("opacity argument must be either 'full' or 'half'")

        logo_path <- "/home/ivanliu/analytics/RQuant/image/RQuant.png"

        if (!file.exists(logo_path)) stop("File does not exist. Feel free to create your own logo and ask Ivan about implementing it.")

        img <- readPNG(logo_path)
        logo_object <- rasterGrob(img, interpolate=TRUE)
        w <- matrix(rgb(img[,,1],img[,,2],img[,,3],img[,,4] * opacity), nrow=dim(img)[1])
    }

    result_theme <- list( if(simple_theme) theme_minimal() else NULL,
                          if(logo != "none") annotation_custom(rasterGrob(w), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) else NULL,
                          if(rquant_colours) scale_color_manual(values = quantColours(inv = inv)) else NULL,
                          if(rquant_colours) scale_fill_manual(values = quantColours(inv = inv)) else NULL,
                          if(rquant_font){
                              theme(text = element_text(family = 'carlito',
                                                        face = 'bold'))}else{NULL},
                          theme(text = element_text(size = font_size)))


    result_theme

}
