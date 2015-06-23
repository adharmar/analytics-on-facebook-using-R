# packages are imported as necessary
library(shiny)
library(Rfacebook)
library(d3Network)
library(RCurl)
library(rjson)
library(ggplot2)
library(wordcloud)
library(tm)
library(sentiment)



# token generated here: https://developers.facebook.com/tools/explorer
# copy paste the access token HERE

token <- "CAACEdEose0cBADcWsE0AcEeCnTDYc4lEtMgoJ17h57yULZCG2T8FzH0vexvmZC4lDF7GZBvvz2rBeZC6IrTIWHjCheJjMZAjPlb4yhxBDjXD5ZCxiolP7jDoQoFaE85Gq9CtzNFqVUdzZBZAa424H5sGVZCXk3ZAmAInSbJYtH1ZCJrPFw7mRabH2qZA6KcU4angwGCXmeFHaBrDMbO4R2BJ3ffKgbf4FGEscUkZD"


me <- getUsers("me", token, private_info = TRUE)
me$name # my name
#get all my friends who use Graph API Explorer
myfriends<-getFriends(token, simplify=FALSE)

# get the network of my friends who use Graph API Explorer

mat <- getNetwork(token, format = "edgelist", verbose = FALSE)

# Visualize the friends network by d3Network

d <- data.frame(mat)
#d$X1 <- as.numeric(d$X1)  # anonymize
#d$X2 <- as.numeric(d$X2)

# write the new network information to xz.html for refreshed graph next time the application is used
d3SimpleNetwork(d, height=300, width=700, fontsize=15, file="xz.html" )



shinyServer(function(input, output) {

  
  # get user input of the name of the person's data to be analyzed as a reactive input
  uname<-eventReactive(input$go, {
    input$nameattrib
  })
  
  xname<-NA
  fbPosts<-NA
  
  # perform the function fbfn reactively based on uname function
  # this function actively crawls posts of user from Facebook
  
  fbfn<-reactive({
    xname<<-uname()

    uid<-NA
    found<-0
    
    for (i in 1:nrow(myfriends)){
      if(myfriends[i, "name"]==xname)
      {
        uid<-myfriends[i, "id"]
        found<-1
      }
    }
 
        
    # query facebook to get all user posts
    
    facebook <- function(query,token){
      myresult <- list()
      i <- 0 
      next.path<-sprintf( "https://graph.facebook.com/v2.2/%s&access_token=%s",query, token)
      # download all my posts
      while(length(next.path)!=0) {
        i<-i+1
        #You might get some unexpected escape (with warning), you should keep them
        myresult[[i]]<-fromJSON(getURL(next.path, ssl.verifypeer = FALSE, useragent = "R" ),unexpected.escape = "keep")
        next.path<-myresult[[i]]$paging$'next'
      }   
      return (myresult)  
    }
    
    # formulate query
    xquery<-paste(uid,"/posts?fields=story,message,comments.limit(1).summary(true),likes.limit(1).summary(true),created_time",sep="")
    
    # get all posts by user
    myposts<-facebook(xquery,token)
    
    # parse the list, extract number of likes/comments and the corresponding text (status) and id
    parse.master <- function(x, f)
      sapply(x$data, f)
    parse.likes <- function(x) if(!is.na(unlist(x)['likes.summary.total_count'])) (as.numeric(unlist(x)['likes.summary.total_count'])) else 0
    mylikes <- unlist(sapply(myposts, parse.master, f=parse.likes))
    parse.comments <- function(x) if(!is.na(unlist(x)['comments.summary.total_count'])) (as.numeric(unlist(x)['comments.summary.total_count'])) else 0
    mycomments <- unlist(sapply(myposts, parse.master, f=parse.comments))
    # Clean text to remove odd characters
    mycomments <- sapply(mycomments,function(row) iconv(row, "latin1", "ASCII", sub=""))
    parse.messages <- function(x) if(!is.null(x$message)){ x$message} else{if(!is.null(x$story)){x$story} else {NA}}
    mymessages <- unlist(sapply(myposts, parse.master, f=parse.messages))
    # Clean text to remove odd characters
    mymessages <- sapply(mymessages,function(row) iconv(row, "latin1", "ASCII", sub=""))
    parse.id <- function(x) if(!is.null(x$id)){ x$id} else{NA}
    myid <- unlist(sapply(myposts, parse.master, f=parse.id))
    parse.time <- function(x) if(!is.null(x$created_time)){x$created_time} else{NA}
    mytime <- unlist(sapply(myposts, parse.master, f=parse.time))
    mytime
    mytime<-(as.POSIXlt(mytime,format="%Y-%m-%dT%H:%M:%S"))
    #put everything into a data.frame
    fbPosts<<-data.frame(postId=myid,message=mymessages,likes.count=mylikes,comments.count=mycomments,time=mytime,year=mytime$year+1900,dom=mytime$mday,hour=mytime$hour,wd=weekdays(mytime),month=months(mytime))
    
  })
  
  # function to get the most liked status/post from list of posts
  output$mostliked<-renderTable({
    fbfn()
    #most liked
    temp<-fbPosts[which.max(fbPosts$likes.count),]
    temp$postId<-NULL
    temp$time<-NULL
    temp$dom<-NULL
    temp$wd<-NULL
    temp$hour<-NULL
    names(temp)[1]<-"Post"
    names(temp)[2]<-"No.of Likes"
    names(temp)[3]<-"No.of Comments"
    names(temp)[4]<-"Year"
    names(temp)[5]<-"Month"
    temp[2]<-as.integer(temp[2])
    temp[4]<-as.integer(temp[4])
    temp
  })
  
# function to get the most commented post/status from list of posts, as of now this is not enabled
#   output$mostcmtd<-renderTable({
#     fbfn()
#     #most commented
#     temp<-fbPosts[which.max(fbPosts$comments.count),]
#     temp$postId<-NULL
#     temp$time<-NULL
#     temp$dom<-NULL
#     temp$wd<-NULL
#     temp$hour<-NULL
#     names(temp)[1]<-"Post"
#     names(temp)[2]<-"No.of Likes"
#     names(temp)[3]<-"No.of Comments"
#     names(temp)[4]<-"Year"
#     names(temp)[5]<-"Month"
#     temp[2]<-as.integer(temp[2])
#     temp[4]<-as.integer(temp[4])
#     temp
#   })

# plot analysis of user post frequency with respect to day of week
  output$Plot1 <- renderPlot({
      fbfn()
 
      fbPosts$wdfact<-factor(fbPosts$wd)
      wdcount<-table(fbPosts$wdfact)
      wdcount
      wdmat<-as.data.frame(wdcount)
      colnames(wdmat)[1]<-"Weekday"
      wdmat
      wdmat$Weekday <- factor(wdmat$Weekday, levels= c("Sunday", "Monday", 
                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
      wdmat<-wdmat[order(wdmat$Weekday), ]
      ggplot(wdmat, aes(x = Weekday,y=Freq, fill= Weekday)) + geom_bar(stat='identity')+
        labs(x="Day of Week", y="Number of Posts") +
        ggtitle( "Post Frequency with respect to Day of Week")
    })
# plot analysis of user post frequency with respect to time of day
    output$Plot2 <- renderPlot({
      fbfn()
      #posts with time of the day
      fbPosts$hourfact<-factor(fbPosts$hour)
      hourcount<-table(fbPosts$hourfact)
      hourmat<-as.data.frame(hourcount)
      colnames(hourmat)[1]<-"Hour"
      hourmat$Hour <- factor(hourmat$Hour, levels= c("0", "1","2","3","4","5","6","7","8","9","10","11","12",
                                                     "13","14","15","16","17","18","19","20","21","22","23"))
      hourmat<-hourmat[order(hourmat$Hour), ]
      ggplot(hourmat, aes(x = Hour,y=Freq, fill= Hour)) + geom_bar(stat='identity')+
        labs(x="Hour", y="Number of Posts") +
        ggtitle( "Post Frequency with respect to Time")
    })
# plot analysis of user post frequency with respect to month of year
    output$Plot3 <- renderPlot({
      fbfn()
      #posts wrt month
      fbPosts$monthfact<-factor(fbPosts$month)
      monthcount<-table(fbPosts$monthfact)
      monthmat<-as.data.frame(monthcount)
      colnames(monthmat)[1]<-"Month"
      monthmat$Month <- factor(monthmat$Month, levels= c("January", "February","March","April","May","June","July","August","September"
                                                         ,"October","November","December"))
      monthmat<-monthmat[order(monthmat$Month), ]
      ggplot(monthmat, aes(x = Month,y=Freq, fill= Month)) + geom_bar(stat='identity')+
        labs(x="Month", y="Number of Posts") +
        ggtitle( "Post Frequency with respect to Month")
    })
# plot analysis of user post frequency on a yearly basis starting from 2010
    output$Plot4 <- renderPlot({
      fbfn()
      #posts wrt year
      fbPosts$yearfact<-factor(fbPosts$year)
      yearcount<-table(fbPosts$yearfact)
      yearmat<-as.data.frame(yearcount)
      colnames(yearmat)[1]<-"Year"
      yearmat$Year <- factor(yearmat$Year, levels= c("2010", "2011","2012","2013","2014","2015"))
      yearmat<-yearmat[order(yearmat$Year), ]
      ggplot(yearmat, aes(x =Year,y=Freq, fill= Year)) + geom_bar(stat='identity')+
        labs(x="Year", y="Number of Posts") +
        ggtitle( "Post Frequency with respect to Year")
    })
# plot analysis of words used by user with respect to frequency
  output$WC<-renderPlot({
    fbfn()
    # forming word cloud
    
    #badPosts removal
    specificnames<-unlist(unique(strsplit(tolower(xname)," ")))
    toMatch<-c("You changed your profile", specificnames)
    grep(paste(toMatch,collapse="|"),fbPosts$message)
    CorPosts<-subset(fbPosts, !(rownames(fbPosts) %in% grep(paste(toMatch,collapse="|"),fbPosts$message)))
    #fbPosts<-subset(fbPosts, !(rownames(fbPosts) %in% grep("Anand Kumar",fbPosts$message)))
    corp<- Corpus(VectorSource(CorPosts$message))
    #inspect(corp)
    corp <- tm_map(corp, stripWhitespace)
    corp <- tm_map(corp, tolower)
    corp <- tm_map(corp, removeWords, stopwords("english"))
    # namevector has words to avoid    
    namevector<-c("shared","album","changed","photo",
                  "video","link","profile","picture","likes","posted","added","facebook","updated","others")
    specificnames<-unlist(unique(strsplit(tolower(xname)," ")))
    namevector<-c(namevector, specificnames)
    
    corp <- tm_map(corp, removeWords, namevector)
    corp <- tm_map(corp, stemDocument)
    corp <- tm_map(corp, PlainTextDocument)
    
    
    #title for word cloud
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, cex=1.6, "Word Cloud based on User Posts")
    
    wordcloud(corp, scale=c(5,0.2), min.freq=2, max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"), main="Title")
  })
  

  # function sentfn retrieves and cleans data necessary for sentiment analysis
  # it also classifies words with respect to emotion and polarity
  sentfn<-function(){
    fbfn()
    
    some_txt<<-fbPosts$message
    

    
    # remove retweet entities
    some_txt<<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt<<- gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt<<- gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt<<- gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt<<- gsub("http\\w+", "", some_txt)
    # remove unnecessary spaces
    some_txt<<- gsub("[ \t]{2,}", "", some_txt)
    some_txt<<- gsub("^\\s+|\\s+$", "", some_txt)
    
    # define "tolower error handling" function 
    try.error<- function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error<-tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y<-tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    some_txt<<- sapply(some_txt, try.error)
    
    # remove NAs in some_txt
    some_txt<<-some_txt[!is.na(some_txt)]
    names(some_txt)<-NULL
    
    
    
    
    # classify emotion
    class_emo <<- classify_emotion(some_txt, algorithm="bayes", prior=1.0)
    # get emotion best fit
    emotion <<- class_emo[,7]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] <<- "unknown"
    
    # classify polarity
    class_pol <<- classify_polarity(some_txt, algorithm="bayes")
    # get polarity best fit
    polarity <<- class_pol[,4]
    
    # data frame with results
    sent_df <<- data.frame(text=some_txt, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
    # sort data frame
    sent_df <<- within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    #assign("a", "sent_df", envir = .GlobalEnv)
  }
  # plot analysis of user sentiment with respect to emotion
  output$sentanal<-renderPlot({
    
    sentfn()
    
    # plot distribution of emotions wrt emotion
    ggplot(sent_df, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
      labs(x="Emotion Categories", y="Number of Posts") +
      ggtitle( "Sentiment Analysis of Posts\n(Classification by Emotion)")    
    
  })
# plot analysis of user sentiment with respect to polarity
  output$binsent<-renderPlot({
    
    sentfn()
    
    # plot distribution of emotions wrt polarity
    ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy")+
      labs(x="Emotion Categories", y="Number of Posts") +
      ggtitle( "Sentiment Analysis of Posts\n(Classification by Polarity)")   
  })
# plot analysis of user word usage with respect to emotions
  output$sentcloud<-renderPlot({
    
    sentfn()
    
    
    elivec<-c("shared","album","changed","photo",
              "video","link","profile","picture","likes","posted","added","facebook","updated","others")
    eliname<-unlist(unique(strsplit(tolower(xname)," ")))
    elivec<-c(elivec, eliname)
    
    #sent_df<<-subset(sent_df, !(rownames(sent_df) %in% grep(paste(elivec,collapse="|"),sent_df$message)))
    
    # separating text by emotion
    emos = levels(factor(sent_df$emotion))
    nemo = length(emos)
    emo.docs = rep("", nemo)
    for (i in 1:nemo)
    {
      tmp = some_txt[emotion == emos[i]]
      emo.docs[i] = paste(tmp, collapse=" ")
    }
    
    # remove stopwords
    emo.docs = removeWords(emo.docs, stopwords("english"))
    # create corpus
    corpus = Corpus(VectorSource(emo.docs))
    corpus <- tm_map(corpus, removeWords, elivec)
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    colnames(tdm) = emos
    
    #title for compcloud
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, cex=1.6, "Word Cloud based on Emotional Sentiment of User")
    # comparison word cloud
    comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                     scale = c(3,.5), random.order = FALSE, title.size = 1.5, main="Title")
  })

})
