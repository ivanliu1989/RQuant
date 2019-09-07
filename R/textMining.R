#' Generate n-grams from a document
#'
#' splits strings into n-grams with given minimal and maximal numbers of grams.
#'
#' @param ngram_df a data.frame with strings to be tokenized.
#' @param split_num number of chunks
#' @param grams n grams
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd", n = 100, since = as.character(Sys.Date()-7), until = as.character(Sys.Date()))
#' tweets_ngrams <- ngramify(tweets$d, split_num=1, grams = 3)
#'
#' @seealso \link{tfidfTransformer}
#' @export
ngramify <- function(ngram_df, split_num=1, grams = 3){

    library(RWeka)
    cat(paste('Input data frame (rows:',nrow(ngram_df), '| size:',round(object.size(ngram_df)/1024/1024,0),
              'mb) \n are going to split into', split_num, 'and', grams, 'grams prediction chunks...'))
    cat(paste('\n (Step 1 of 5) Start to create chunks...'))

    chunks <- list()
    for (i in 1:split_num){
        chunks[[i]] <- ngram_df[(ceiling(nrow(ngram_df)/split_num)*(i-1)+1):(ceiling(nrow(ngram_df)/split_num)*i),1]
    }
    rm(ngram_df); gc()

    cat(paste('\n (Step 2 of 5) Start to convert chunks into n-grams matrix...'))
    ngram_chunks <- list()
    for (j in 1:split_num){
        ngram_chunks[[j]] <- NGramTokenizer(chunks[[j]], Weka_control(min=grams,max=grams))
    }
    rm(chunks); gc()

    cat(paste('\n (Step 3 of 5) Start to integrate chunks into one matrix...'))
    ngram_chunks_all <- c()
    for (z in 1:split_num){
        ngram_chunks_all <- c(ngram_chunks_all, ngram_chunks[[z]])
    }
    rm(ngram_chunks); gc()

    cat(paste('\n (Step 4 of 5) Start to calculate the frequency of each term...'))
    ngram_freq_tb <- sort(table(ngram_chunks_all), decreasing=T)
    rm(ngram_chunks_all); gc()

    cat(paste('\n (Step 5 of 5) Finishing the process...'))
    ngram_pred <- data.frame(terms = names(ngram_freq_tb), freq = ngram_freq_tb, row.names = NULL, stringsAsFactors = F)
    rm(ngram_freq_tb); gc()

    return(ngram_pred)
}


#' Replace Punctuation
#'
#' Replace punctuation with a " " in charactors
#'
#' @param strings strings
#'
#' @seealso \link{tfidfTransformer}
#' @export
replacePunctuation <- function(strings){
    gsub("[[:punct:]]", " ", strings)
    return(strings)
}


#' Generate n-grams from a document
#'
#' splits strings into n-grams with given minimal and maximal numbers of grams.
#'
#' @param text_vector a vector of strings to be tokenized.
#' @param ngrams number of grams for ngrams transformation
#' @param minDocFreq minimum frequency for each document to be kept
#' @param wordLengths minimum length of a valid word to be kept
#' @param wordLengths_max maximum length of a valid word to be kept
#' @param idf inverse-document-frequency OR term-frequency, TRUE/FALSE
#' @param cores number of cores for parallel computing
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd", n = 100, since = as.character(Sys.Date()-7), until = as.character(Sys.Date()))
#' tfidf.dt =  tfidfTransformer(tweets$d$text, ngrams = 1, minDocFreq = 2, wordLengths = 3, wordLengths_max = 20, idf = TRUE, cores = 6)
#' head(as.matrix(tfidf.dt))
#'
#' @seealso \link{tfidfTransformer}
#' @export
tfidfTransformer <- function(text_vector, ngrams = 1, minDocFreq = 2, wordLengths = 3, wordLengths_max = 20, idf = TRUE, cores = 6){

    library(tm)
    # library(RWeka)
    library(parallel)
    options(mc.cores=1)

    cat('removing special characters... \n')
    text_vector <- iconv(text_vector, to = 'utf-8', sub=' ')

    cat('load data... \n')
    review_source <- VectorSource(text_vector)

    cat('create corpus... \n')
    corpus <- Corpus(review_source)

    cat('to lower case... \n')
    corpus <- tm_map(corpus, content_transformer(tolower), lazy = T)

    cat('remove/replace punctuation... \n')
    corpus <- tm_map(corpus, content_transformer(replacePunctuation), lazy = T)
    corpus <- tm_map(corpus, removePunctuation, lazy = T)

    cat('remove numerical characters... \n')
    corpus <- tm_map(corpus, removeNumbers, lazy = T)

    cat('remove stop words... \n')
    corpus <- tm_map(corpus, removeWords, stopwords('english'), lazy = T)

    cat('strip white space... \n')
    corpus <- tm_map(corpus, stripWhitespace, lazy = T)

    cat('stemming... \n')
    corpus <- tm_map(corpus, stemDocument, 'english', lazy = T)

    if(idf){
        if(ngrams > 1){
            cat(paste0('tf-idf with ',ngrams,'-grams. Minimum word length: ', wordLengths, '. Minimum frequencies: ', minDocFreq, '.  \n'))
            # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))

            BigramTokenizer <- function(x){
                unlist(lapply(ngrams(words(x), ngrams), paste, collapse = ""), use.names = FALSE)
            }
            dtm <- DocumentTermMatrix(corpus,
                                      control = list(tokenize = BigramTokenizer,
                                                     minDocFreq = minDocFreq,
                                                     wordLengths = c(wordLengths, wordLengths_max),
                                                     weighting = weightTfIdf)
                                      # ,bounds=list(global=c(floor(length(corpus)*0.05), Inf))
            )
        }else{
            cat(paste0('tf-idf with ',ngrams,'-grams. Minimum word length: ', wordLengths, '. Minimum frequencies: ', minDocFreq, '.  \n'))
            dtm <- DocumentTermMatrix(corpus,
                                      control = list(minDocFreq = minDocFreq,
                                                     wordLengths = c(wordLengths, wordLengths_max),
                                                     weighting = weightTfIdf)
                                      # ,bounds=list(global=c(floor(length(corpus)*0.05), Inf))
            )
        }
    }else{
        if(ngrams > 1){
            cat(paste0('term-frequency with ',ngrams,'-grams. Minimum word length: ', wordLengths, '. Minimum frequencies: ', minDocFreq, '.  \n'))
            # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
            BigramTokenizer <- function(x){
                unlist(lapply(ngrams(words(x), ngrams), paste, collapse = ""), use.names = FALSE)
            }
            dtm <- DocumentTermMatrix(corpus,
                                      control = list(tokenize = BigramTokenizer,
                                                     minDocFreq = minDocFreq,
                                                     wordLengths = c(wordLengths, wordLengths_max))
                                      # ,bounds=list(global=c(floor(length(corpus)*0.05), Inf))
            )
        }else{
            cat(paste0('term-frequency with ',ngrams,'-grams. Minimum word length: ', wordLengths, '. Minimum frequencies: ', minDocFreq, '.  \n'))
            dtm <- DocumentTermMatrix(corpus,
                                      control = list(minDocFreq = minDocFreq,
                                                     wordLengths = c(wordLengths, wordLengths_max))
                                      # ,bounds=list(global=c(floor(length(corpus)*0.05), Inf))
            )
        }
    }
    return(dtm)
}



#' Function to clean your tweets text
#'
#' Some pre-process the data in some standard ways.
#'
#' @param tweets tweets retrieved from \code{tweet_corpus} function
#' @param rmDuplicates if remove duplicated tweets
#' @param cores number of cores for parallel computing
#' @param stems customized stems to be removed
#' @param partial partial cleaning. step 1 to 11
#'
#' @details
#' 1. Convert to basic ASCII text to avoid silly characters\cr
#' 2. Make everything consistently lower case\cr
#' 3. Remove the "RT" (retweet) so duplicates are duplicates\cr
#' 4. Remove links\cr
#' 5. Remove punctuation\cr
#' 6. Remove tabs\cr
#' 7. "&" is "&amp" in HTML, so after punctuation removed ...\cr
#' 8. Leading blanks\cr
#' 9. Lagging blanks\cr
#' 10. General spaces (should just do all whitespaces no?)\cr
#' 11. Get rid of duplicates!\cr
#' 12. Convert to tm corpus\cr
#' 13. Remove English stop words.\cr
#' 14. Remove numbers.\cr
#' 15. Stem the words.\cr
#' 16. Remove the customized stems\cr
#'
#' @seealso \link{tweet_corpus}
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd", n = 100, since = as.character(Sys.Date()-7), until = as.character(Sys.Date()))
#' tweets <- text_clean(tweets$v, rmDuplicates = FALSE, cores = 6, stems = c("audusd"))
#'
#' @export
text_clean <- function(docvec, rmDuplicates = FALSE, cores = 6, stems = NULL, partial = FALSE){
    # Okay, here's where things get tricky. See references for examples.
    # Problems? Depends if your system is using parallel processing. If
    # it is, you'll need to use mc.cores parameters as shown later. That
    # took me awhile to get! Thanks to the references for clearing that up.

    # Here we pre-process the data in some standard ways. I'll post-define each step
    cat('Convert to basic ASCII text to avoid silly characters... \n')
    docvec <- iconv(docvec, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters

    cat('Make everything consistently lower case... \n')
    docvec <- tolower(docvec)  # Make everything consistently lower case

    cat('Remove the "RT" (retweet) so duplicates are duplicates... \n')
    docvec <- gsub("rt", " ", docvec)  # Remove the "RT" (retweet) so duplicates are duplicates

    cat('Remove user names... \n')
    docvec <- gsub("@\\w+", " ", docvec)  # Remove user names (all proper names if you're wise!)

    cat('Remove links... \n')
    docvec <- gsub("http.+ |http.+$", " ", docvec)  # Remove links

    cat('Remove punctuation... \n')
    docvec <- gsub("[[:punct:]]", " ", docvec)  # Remove punctuation

    cat('Remove tabs... \n')
    docvec <- gsub("[ |\t]{2,}", " ", docvec)  # Remove tabs

    cat('"&" is "&amp" in HTML, so after punctuation removed... \n')
    docvec <- gsub("amp", " ", docvec)  # "&" is "&amp" in HTML, so after punctuation removed ...

    cat('Remove leading blanks... \n')
    docvec <- gsub("^ ", "", docvec)  # Leading blanks

    cat('Remove lagging blanks... \n')
    docvec <- gsub(" $", "", docvec)  # Lagging blanks

    cat('Remove general spaces... \n')
    docvec <- gsub(" +", " ", docvec) # General spaces (should just do all whitespaces no?)


    if(rmDuplicates) {
        cat('Remove duplicates... \n')
        docvec <- unique(docvec)  # Now get rid of duplicates!
    }

    if(!partial){
        # Convert to tm corpus and use its API for some additional fun
        cat('Create corpus object... \n')
        corpus <- Corpus(VectorSource(docvec))  # Create corpus object

        # Remove English stop words. This could be greatly expanded!
        # Don't forget the mc.cores thing
        cat('Remove English stop words... \n')
        corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=cores)

        # Remove numbers. This could have been done earlier, of course.
        cat('Remove numbers... \n')
        corpus <- tm_map(corpus, removeNumbers, mc.cores=cores)

        # Stem the words. Google if you don't understand
        cat('Stem the words... \n')
        corpus <- tm_map(corpus, stemDocument, mc.cores=cores)

        # Remove the stems associated with our search terms!
        if(!is.null(stems)){
            cat('Remove the stems associated with our search terms... \n')
            corpus <- tm_map(corpus, removeWords, stems, mc.cores=cores)
        }

        dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),
                              stringsAsFactors=F)

        return(list(corpus = corpus,
                    dataframe = dataframe))
    }else{
        return(list(corpus = NULL,
                    dataframe = docvec))
    }

}


#' Plot a word cloud
#'
#' Plot a word cloud
#'
#' @param corpus the words
#' @param min.freq words with frequency below min.freq will not be plotted
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param random.order plot words in random order. If false, they will be plotted in decreasing frequency
#'
#' @details
#' If freq is missing, then words can either be a character vector, or Corpus. If it is a vector and freq is missing, standard stop words will be removed prior to plotting.
#'
#' @seealso \link{tweet_corpus}
#' @seealso \link{tweet_clean}
#' @seealso \link{tweet_clean_corpus}
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd", n = 100, since = as.character(Sys.Date()-7), until = as.character(Sys.Date()))
#' tweets <- text_clean(tweets$v, rmDuplicates = FALSE, cores = 6, stems = NULL)
#' wordCloudVis(tweets$corpus)
#'
#' @export
wordCloudVis <- function(corpus, min.freq=2, max.words = 150, random.order = TRUE){

    library(wordcloud)
    # Why not visualize the corpus now?
    # Mine had a lot to do with {solar, power, renew, new, can, save, suppl, wind, price, use}
    pal <- brewer.pal(8, "Dark2")
    wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)

}


#' Topic Modeling
#'
#' Topic Modeling based on Correlated Topic Model (Estimate a CTM model using for example the VEM algorithm.) and
#' Latent Dirichlet Allocation (Estimate a LDA model using for example the VEM algorithm or Gibbs Sampling.)
#'
#' @param corpus the words
#' @param k number of topics
#'
#' @details
#' LDA:\cr
#' The C code for LDA from David M. Blei and co-authors is used to estimate and fit a latent dirichlet allocation model with the VEM algorithm. For Gibbs Sampling the C++ code from Xuan-Hieu Phan and co-authors is used.\cr
#' When Gibbs sampling is used for fitting the model, seed words with their additional weights for the prior parameters can be specified in order to be able to fit seeded topic models.\cr
#' \cr
#' CTM:\cr
#' The C code for CTM from David M. Blei and co-authors is used to estimate and fit a correlated topic model.\cr
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd", n = 100, since = as.character(Sys.Date()-7), until = as.character(Sys.Date()))
#' tweets <- text_clean(tweets$v, rmDuplicates = FALSE, cores = 6, stems = NULL)
#' wordCloudVis(tweets$corpus)
#' topicModeling(tweets$corpus, k = 5)
#'
#' @export
topicModeling <- function(corpus, k = 10){

    library(NLP)
    library(tm)
    library(topicmodels)
    library(SnowballC)
    # Now for Topic Modeling

    # Get the lengths and make sure we only create a DTM for tweets with
    # some actual content
    doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
    dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
    # model <- LDA(dtm, 10)  # Go ahead and test a simple model if you want



    # Now for some topics
    SEED = sample(1:1000000, 1)  # Pick a random seed for replication

    # This might take a minute!
    models <- list(
        CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
        VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
        VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
        Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                     thin = 100,    iter = 1000))
    )



    # There you have it. Models now holds 4 topics. See the topicmodels API documentation for details

    # Top 10 terms of each topic for each model
    # Do you see any themes you can label to these "topics" (lists of words)?
    themes <- lapply(models, terms, 10)

    # matrix of tweet assignments to predominate topic on that tweet
    # for each of the models, in case you wanted to categorize them
    assignments <- sapply(models, topics)

    return(list(themes = themes,
                assignments = assignments))

}





# tokenization <- function (docs, trans = c(T,T,T,T,T,T,T,T), ChartoSpace = c('/','@','\\|'),
#                           stopWords = 'english', ownStopWords = c(), profanity = data.frame()) {
#
#     library(tm)
#     library(SnowballC)
#
#     cat(paste('\nPlease wait for initializing and summrising the input files......'))
#
#     cat(paste('\nDocuments below will be processed soon!\n'))
#     print(summary(docs))
#     cat(paste('\nStart tokenization processes...'))
#
#     # cat(paste('\nuse the utf8 encoding on the macintosh without the need to recompile...'))
#     # tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#     # tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
#
#     # Simple Transformation
#     cat(paste('\n\n1.Simple Transformation:', trans[1]))
#     if(trans[1] == T){
#         toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
#         for (i in ChartoSpace){
#             docs <- tm_map(docs, toSpace, i)
#             cat(paste('\n ->Character:', i, 'has been transformed into white space!'))
#         }
#     }
#     # Specific Transformations/Profanity filtering
#     cat(paste('\n2.Specific Transformations/Profanity filtering:', trans[8]))
#     if(trans[8] == T){
#         cat(paste('\n', nrow(profanity), 'words will be filtered, following is a sample of the words:\n'))
#         print(head(profanity,5))
#         toString <- content_transformer(function(x, from, to) gsub(from, to, x))
#         for (i in 1:nrow(profanity)){
#             # cat(paste('\n ->Transfer', profanity[i,1], 'to', profanity[i,2]))
#             docs <- tm_map(docs, toString, profanity[i,1], profanity[i,2])
#         }
#         cat('\n ->Specific Transformations/Profanity filtering have been done to raw document!')
#     }
#     # Lowercase
#     cat(paste('\n3.Lowercase Transformation:', trans[2]))
#     if(trans[2] == T){
#         docs <- tm_map(docs, content_transformer(tolower))
#         cat('\n ->All CAPITAL characters have been transformed to lower cases!')
#     }
#
#     # Remove Numbers
#     cat(paste('\n4.Remove Numbers:', trans[3]))
#     if(trans[3] == T){
#         docs <- tm_map(docs, removeNumbers)
#         cat('\n ->All NUMBERs have been eliminated from raw document!')
#     }
#
#     # Remove Punctuations
#     cat(paste('\n5.Remove Punctuations:', trans[4]))
#     if(trans[4] == T){
#         docs <- tm_map(docs, removePunctuation)
#         cat('\n ->All Punctuations have been eliminated from raw document!')
#     }
#
#     # Remove English Stop Words
#     cat(paste('\n6.Remove Stop Words:', trans[5]))
#     if(trans[5] == T){
#         cat(paste('\n->Remove', stopWords, 'Stop Words:'))
#         cat(paste('\n->Stop Words including:\n' ))
#         print(stopwords(stopWords))
#         cat(paste('\n->',length(stopwords(stopWords)), 'words in total'))
#         docs <- tm_map(docs, removeWords, stopwords(stopWords))
#         cat('\n ->Stop Words have been eliminated from raw document!')
#     }
#
#     # Remove Own Stop Words
#     cat(paste('\n7.Remove Own Stop Words:', trans[6]))
#     if(trans[6] == T){
#         cat(paste('\n ->Remove Own Stop Words:'))
#         cat(paste('\n ->Stop Words including:\n'))
#         print(ownStopWords)
#         cat('\n ->', paste(length(ownStopWords), 'words in total'))
#         docs <- tm_map(docs, removeWords, ownStopWords)
#         cat('\n ->Own Stop Words have been eliminated from raw document!')
#     }
#
#     # Strip Whitespace
#     cat(paste('\n8.Strip Whitespace:', trans[7]))
#     if(trans[7] == T){
#         docs <- tm_map(docs, stripWhitespace)
#         cat('\n ->Whitespaces have been stripped from raw document!')
#     }
#
#     # Complete messages
#     cat('\n\nDocument has been tokenized!')
#     cat(summary(docs))
#     return(docs)
# }






