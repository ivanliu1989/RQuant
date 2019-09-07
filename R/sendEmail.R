#' Send an Email through R
#'
#' Send an email through R.
#'
#' @param to A character vector of recipient valid email addresses.
#' @param subject Subject of the email.
#' @param msg Body of the email as text. If the parameter body refers to an existing file location, the text of the file is parsed as body of the email.
#' @param attach.files A character vector of paths in the file system linking to files or *valid* URLs to be attached to the email (see details for more info on attaching URLs)
#' @param file.names A character vector of names of attached files
#' @param file.descriptions A character vector of descriptions of attached files
#'
#' @examples
#' RQuantSendMail(to = "ivan.liuyanfeng@gmail.com", subject = "An email from R", msg = "test", attach.files = NULL)
#'
#' @export
RQuantSendMail = function(to = c("ivan.liuyanfeng@gmail.com"), subject = NULL, msg = NULL, attach.files = NULL, file.names = NULL, file.descriptions = NULL){
    library(mailR)

    sink(file = './emailhtml.html')
    cat(msg)
    sink()

    tryCatch({
        send.mail(from = "ivan.liuyanfeng@gmail.com",
                  to = to,
                  subject = subject,
                  body = './emailhtml.html',
                  html = TRUE,
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ivan.liuyanfeng@gmail.com", passwd = "", ssl = TRUE),
                  authenticate = TRUE,
                  attach.files = attach.files,
                  file.names = file.names, # optional parameter
                  file.descriptions = file.descriptions, # optional parameter
                  send = TRUE)
    }, finally = {
        # unlink(path, recursive = T, force = T)
        cat("Reports sent! Please check your email box!")
    })

    unlink('./emailhtml.html')
}



#' Build HTML email message
#'
#' Build an email message from a vector or list of elements and
#' include any attachments.
#'
#' @param body A \code{list} or \code{vector} of html elements to go into the
#' main message of the email
#' @param attachmentFileNames A \code{list} or \code{vector} of paths refering to the
#' attachements included in the email
#'
#' @examples
#' x = c(toHTML("Test", "h2"), br(), toHTML("test text"))
#' buildhtmlmsg(x)
#'
#' @export
buildhtmlmsg = function(body, attachmentFileNames=NULL){

    html = paste(
        htmlIntro(),
        paste(unlist(body),collapse = " "),
        htmlEnd()
    )
    #   writeLines(html,"test.html")
    # msg=mime_part(html)
    # msg[["headers"]][["Content-Type"]] <- "text/html"

    fullmsg = list(html = html,
                   attach.files = attachmentFileNames
                   )
    # if(length(attachmentFileNames)>0){
    #     fullmsg = sapply(unlist(attachmentFileNames),function(x){return(mime_part(x))})
    #     fullmsg$msg = msg
    # }else{
    #     fullmsg = list(msg)
    # }
    return(fullmsg)
}

#' Convert text to HTML element
#'
#' @param x A string of text
#' @param tag Defaults to "p". May be any of "a", "em", "strong", "cite", "q", "dfn", "abbr", "data", "time", "code", "var", "samp", "kbd", "mark", "ruby", "rb", "rt", "rp", "rtc", "bdi", "bdo", "span", "br", "wbr", "small", "i", "b", "u", "s", "sub", "sup", "p", "pre", "blockquote", "ol", "ul", "li", "dl", "dt", "dd", "figure", "figcaption", "div", "main", "hr"
#'
#' @examples
#' toHTML("hello")
#' toHTML("hello", "h2")
#'
#' @export
toHTML = function(x, tag = "p"){
    validTags = c("a","em","strong","cite","q","dfn","abbr","data","time","code","var","samp","kbd","mark","ruby","rb","rt","rp","rtc","bdi","bdo","span","br","wbr","small","i","b","u","s","sub","sup","p","pre","blockquote","ol","ul","li","dl","dt","dd","figure","figcaption","div","main","hr", paste0("h", 1:6), "table", "tr", "td")
    if(!tag%in%validTags){
        warning("invalid tag:",tag,".Converting to <p>")
        tag = "p"
    }
    textStyle= 'style="font-family:arial;"'
    res=paste0('<',tag,' ',textStyle,'>',x,'</',tag,'>')
    return(res)
}


#' Create HTML table
#'
#' This function creates a HTML table from a data frame and applies
#' rquant branding standards (eg colours / fonts etc) so that it
#' can be fowared on to clients
#'
#' Newlines have been added to prevent the output falling foul of
#' the 1000-character line limit of SMTP.  The mailserver inserts
#' its own newlines, breaking the code.
#'
#' @param x A \code{data.frame}
#' @param showRowNames A boolean flag on whether to show the row names from data frame
#' @param cornerText A string to display in the top left cell when \code{showRowNames} is used
#'
#' @examples
#' htmlTableQuant(mtcars)
#' htmlTableQuant(mtcars, showRowNames = TRUE)
#' htmlTableQuant(mtcars, showRowNames = TRUE, cornerText = "X")
#'
#' @export
htmlTableQuant = function(dat, showRowNames=FALSE, cornerText=''){
    dat[] <- lapply(dat, as.character)
    dat[is.na(dat)] = ""
    headrowformat = paste0('style="color:#FFFFFF;text-align:center;background: ', quantColours(1),';font-weight: bold;"')
    headcolformat = paste0('style="color:#FFFFFF;text-align:center;background: ', quantColours(1),';font-weight: bold; padding:5px 15px"')
    oddrowformat = 'style="background: #CBCBCB;"'
    evenrowformat = 'style="background: #DCDCDC;"'
    cellstyle = 'style="padding:5px 15px"'
    altrowformats = c(oddrowformat,evenrowformat)
    tableStart = '\n\n<table style="border-spacing: 0px 2px;font-family:arial;font-size:14px;text-align:center;border-color:#FFFFFF;">\n'
    tableEnd = "</table>\n\n"
    if (!showRowNames) {
        header = paste("<tr ",headrowformat,'><td ',cellstyle,'>',paste(colnames(dat),collapse=paste('</td><td ',cellstyle,'>')),"</td></tr>\n")
    } else {
        header = paste("<tr ",headrowformat,'><td ',cellstyle,'>',paste(c(cornerText,colnames(dat)),collapse=paste('</td><td ',cellstyle,'>')),"</td></tr>\n")
    }
    rowSet = paste(sapply(1:nrow(dat),function(x){
        if (!showRowNames) {
            res = paste("<tr ",altrowformats[x%%2+1],'><td ',cellstyle,'>',
                        paste(subset(dat,1:nrow(dat)==x),collapse=paste('</td>\n<td ',cellstyle,'>')),
                        "</td></tr>\n")
        } else {
            res = paste("<tr ",altrowformats[x%%2+1],'><td ', headcolformat, '>', subset(rownames(dat),1:nrow(dat)==x),
                        '</td><td ',cellstyle,'>',
                        paste(subset(dat,1:nrow(dat)==x),collapse=paste('</td>\n<td ',cellstyle,'>')),
                        "</td></tr>\n")
        }
        return(res)
    }
    ),collapse=" ")
    res = paste0(tableStart,header,rowSet,tableEnd)
    return(res)
}





#' hr
#'
#' @export
hr = function(){
    res = "<hr/>"
    return(res)
}


#' br
#'
#' @export
br = function(){
    return("<br/>")
}


#' Convert a date into Day-Month format
#' as a standard
#' @keywords internal
prettyDate = function(x){
    return(format(as.Date(x),"%d-%b"))
}

#' Create the header HTML for an email message
#' @keywords internal
htmlIntro = function(){
    res = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
  Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
  <!--html xmlns="http://www.w3.org/1999/xhtml"-->
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>
  <body style="font-family:arial;>'
    return(res)
}

#' Close of a HTML  email message
#' @keywords internal
htmlEnd = function(){
    res='</body>
  </html>'
    return(res)

}

#' Convert a vector or list into a html list
#'
#' @export
asHTMLList = function(x){
    x = unlist(x)
    if(length(x) == 0) return("")
    toHTML(paste0(paste0("<li>", x, "</li>"), collapse=""),"ul")
}
