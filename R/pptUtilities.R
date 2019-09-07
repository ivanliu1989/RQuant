#' The RQuant standard powerpoint template
#'
#' Read in the RQuant powerpoint template
#'
#' @export
getPowerpointTemplate = function(){
    library(ReporteRs)
    return(pptx(title = "test", "./image/RQuant_template.pptx"))
}

#' Variable effects to ppt
#'
#' uses \code{ReporteRs} to create slides
#'
#' @examples
#' df<-mtcars
#' df$am<-as.factor(df$am)
#' model<-glm(vs ~ am*mpg, data=df, family=binomial)
#' probs<-model$fit
#' eff = binaryClassifierEffect(probs,df,c('am','mpg'))
#'
#' doc <-getPowerpointTemplate()
#' doc = generateFeatureEffectsSlides(doc,eff, nToShow = 12, rows = 2, cols = 3)
#' writeDoc(doc, file = "test.pptx")
#' msg = buildhtmlmsg("", "test.pptx")
#' rquantSendEmail("ivan.liuyanfeng@gmail.com", subject = "test pptx", msg)
#' @export
generateFeatureEffectsSlides = function(doc, eff, nToShow = length(eff), rows = 1, cols = 1,
                                        baseTitle = "Feature effects"){
    require(ReporteRs)
    require(gridExtra)

    nPerSlide = rows * cols
    Nplots = min(length(eff), nToShow)
    slideIndex = split(1:Nplots, ceiling(seq_along(1:Nplots)/nPerSlide))
    Nslides = length(slideIndex)
    for(i in 1:Nslides){
        doc = addSlide( doc, slide.layout = "Title and Content" )
        slideTitle = ifelse(i == 1, baseTitle, paste(baseTitle, "cont."))
        doc = addTitle( doc, slideTitle)
        thesePlots = list()
        for(j in 1:length(slideIndex[[i]])){
            thesePlots[[j]] = eff[[slideIndex[[i]][j]]]
        }
        # p = multiplot(plotlist = thesePlots,  cols = min(2, length(thesePlots)))
        p = arrangeGrob(grobs = thesePlots,nrow=rows, ncol=cols,top = NULL)
        doc = addPlot( doc, function( ) grid::grid.draw(p) )
    }

    return(doc)
}




# binaryClassifierEvaluationSlides = function(doc, eval){
#
#   # distributions
#   doc = addSlide( doc, slide.layout = "Title and Content" )
#   doc = addTitle( doc, "Distribution of probabilities")
#   distPlots = list(eval$probabilityHistogram, eval$probabilityDensity)
#   p = arrangeGrob(grobs = distPlots,nrow=1, ncol=2,top = NULL)
#   doc = addPlot( doc, function( ) grid::grid.draw(p) )
#
#
#   doc = addSlide( doc, slide.layout = "Title and Content" )
#   doc = addTitle( doc, "Distribution of probabilities")
#   distPlots = list(eval$probabilityHistogram, eval$probabilityDensity)
#   p = arrangeGrob(grobs = distPlots,nrow=1, ncol=2,top = NULL)
#   doc = addPlot( doc, function( ) grid::grid.draw(p) )
#
#   eval$rocCurve
#
#   return(doc)
# }

# }

