install.packages(c("httr", "jsonlite", "lubridate", "devtools"))
#dat <- get_marketcap_ticker_all(currency = "USD")
#head(dat, n=5)

currencies <- c("USD")

#' To extract Global Market Cap of Cryptocurrency Market

get_global_marketcap <- function(currency = 'USD') {
  stopifnot(currency %in% currencies)
  data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.coinmarketcap.com/v1/global/?convert=',currency))))
}

#' To extract Global Market Cap of Leading Cryptocurrencies

get_marketcap_ticker_all <- function(currency = 'USD') {
  stopifnot(currency %in% currencies)
  data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency))))
}

head(get_marketcap_ticker_all(currency='USD'), n=15)

#' To plot Top 5 Cryptocurrencies
#'
#' @param currency currency code - Default is 'USD'
#' @return A ggplot of top Cryptocurrencies based on their rank
#' @examples
#' plot_top_5_currencies('EUR')
#' plot_top_5_currencies('GBP')
#' @export

library(ggplot2)

plot_top_5_currencies <- function(currency = 'USD') {
  options(scipen = 99)
  stopifnot(currency %in% currencies)
  temp <- data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency))))
  temp <- temp[1:5,]
  as.numeric(temp$price_usd)
  sort(as.numeric(temp$price_usd))
  order(as.numeric(temp$price_usd))
  temp <- temp[order(as.numeric(temp$price_usd), decreasing=TRUE),]
  ggplot2(temp$id, temp$price_usd)
  #temp$price_usd <- as.numeric(temp$price_usd)
  ggplot2::ggplot(temp, ggplot2::aes_string('name','price_usd'))
  +ggplot2::geom_bar(stat = 'identity')
}

plot_top_5_currencies()

###############
# STOCKTWIT ###
###############

Stock twits
Site domain: www.stephenhwang.com
Consumer key: 70f9972820a64d66
Consumer secret: d1b94d1044935c8c9f4664b0ca326a9a6fb89298
Request token URL: https://api.stocktwits.com/api/2/oauth/token
Authorize URL: https://api.stocktwits.com/api/2/oauth/authorize
DeAuthorization URL: http://www.stephenhwang.com



currency='BTC.X'
dat <- data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.stocktwits.com/api/2/streams/symbol/ric/',currency,'.json'))))
head(dat)
tweet <- dat$messages.body
sentiment <- dat$messages.entities$sentiment

data.frame(cbind(sentiment, tweet))


currency='BTC.X'
dat <- data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.stocktwits.com/api/2/streams/symbol/',currency,'.json'))))
dat$messages.body





###########################
# Initial Twitter setup ###
###########################
# Install and Activate Packages
#install.packages("streamR", "RCurl", "RJSONIO")
library(streamR)
library(RCurl)
#devtools::install_github("duncantl/RJSONIO")
library(RJSONIO)
library(stringr)

# PART 1: Declare Twitter API Credentials & Create Handshake
setwd("/Users/stephenhwang/Desktop/crypto_twitter")

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "TUfTpzqcv1Vq4AqqB2b2UAtzE" # From dev.twitter.com
consumerSecret <- "0BihjEUaiM5Wt5QMELymHLQop3GBJweKrqnF8MGr6i9WdwfozQ" # From dev.twitter.com

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# PART 2: Save the my_oauth data to an .Rdata file
save(my_oauth, file = "my_oauth.Rdata")

#keywords <- c("$BTC", "$ETH", "ethereum", "bitcoin", "ripple", "$XRP")
keywords <- c("$BTC", "#Bitcoin", "Bitcoin")
keywords <- c("Trump", "Obama")

setwd("/Users/stephenhwang/Desktop/crypto_twitter")
# THIS IS THE MONEY MAKER
###############################################
# Collect tweets via a stream of 60 seconds ###
###############################################
library(streamR)
load("my_oauth.Rdata")
file.remove("tweets_coins.json")
file.remove("test_urls.txt")
file.remove("test_text.txt")
timeout.sec = 300;
filterStream(file.name = "tweets_coins.json", # Save tweets in a json file
             track = keywords, # Collect tweets mentioning btc, eth, etc.
             language = "en",
             timeout = timeout.sec, # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

#tweets.df <- parseTweets("tweets_coins.json", simplify = FALSE) #Simplify = FALSE means location data is included
tweets.df <- parseTweets("tweets_coins.json", simplify = TRUE)
head(tweets.df, n=5)
# Tweet text
tweets.df$text
# Table containing # of tweets per user
screenname.table <- as.data.frame(table(tweets.df$screen_name))
screenname.table <- screenname.table[order(screenname.table$Freq, decreasing = TRUE),]
head(screenname.table)
head(timeout.sec/screenname.table$Freq)
# Find user that sends out one or more tweets every 20 seconds = most likely a bot
bot.index <- which((timeout.sec/screenname.table$Freq) < 20 )
# Save twitter bot names
load("twitterBots.Rdata")
bot <- unique(c(bot, as.character(screenname.table$Var1[bot.index])))
bot
save(bot, file = "twitterBots.Rdata")

# filter out these bots

# Cleanup
remove.phrase <- c("randomly pick", )
shill.accounts <- c("@DYNAMITEDORK", "@ATEKAssetScan" )
tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) # Remove emoticons

head(tweets.df$text )
as.character(shill.accounts[1]) %in% as.character(tweets.df$text[3])
which(as.character(shill.accounts[1]) %in% as.character(tweets.df$text))


url <- na.omit(data.frame(tweets.df$url))
url
write.table(url, "test_urls.txt", quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t")

text <- na.omit(data.frame(tweets.df$text))
text
write.table(text, "test_text.txt", quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", fileEncoding='UTF-8')

###################################################################################################
# Sentiment analysis
###################################################################################################
library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results

# Import word lists
posText <- read.delim("./ref/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("./ref/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')


# Extract tweets
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
api_key <- "TUfTpzqcv1Vq4AqqB2b2UAtzE" # From dev.twitter.com
api_secret <- "0BihjEUaiM5Wt5QMELymHLQop3GBJweKrqnF8MGr6i9WdwfozQ"
access_token <- "1711485338-VfdVHp4SDDxqw6ymXrkVDbFG7v7iPfaMgdZj18i"
access_token_secret <- "csCl6rD7vYI0aMeYCHJQ0WZGDzs4VBjA0DT4NKSaSoqZ8"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

bitcoin = searchTwitter('bitcoin', n=500)
bitcoin = sapply(bitcoin, function(t) t$getText() )
bitcoin_cash = searchTwitter('$BTC', n=500)
bitcoin_cash = sapply(bitcoin_cash, function(t) t$getText() )
ethereum = searchTwitter('ethereum', n=500)
ethereum = sapply(ethereum, function(t) t$getText() )
ethereum_cash = searchTwitter('$ETH', n=500)
ethereum_cash = sapply(ethereum_cash, function(t) t$getText() )
ripple = searchTwitter('ripple', n=500)
ripple = sapply(ripple, function(t) t$getText() )
ripple_cash = searchTwitter('$XRP', n=500)
ripple_cash = sapply(ripple_cash, function(t) t$getText() )
bitcoin = c(bitcoin, bitcoin_cash)
ethereum = c(ethereum, ethereum_cash)
ripple = c(ripple, ripple_cash)

# # of tweets
noof_tweets = c(length(bitcoin), length(ethereum),length(ripple))
noof_tweets
# Concatenate
crypto = c(bitcoin, ethereum, ripple)
head(crypto)
########################
# Sentiment analysis ###
########################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to lapply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}


# Sentiment analysis score
scores = score.sentiment(crypto, pos.words, neg.words, .progress='text')
head(scores)

# Create scores variable
scores$crypto = factor(rep(c("bitcoin", "ethereum","ripple"), noof_tweets))

# Calculate sentiment scores
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

# Split by coin
btc <- subset(scores, scores$crypto=="bitcoin")
eth <- subset(scores,scores$crypto=="ethereum")
xrp <- subset(scores,scores$crypto=="ripple")

# Create polarity var
btc$polarity <- ifelse(btc$score >0,"positive",ifelse(btc$score < 0,"negative",ifelse(btc$score==0,"Neutral",0)))
eth$polarity <- ifelse(eth$score >0,"positive",ifelse(eth$score < 0,"negative",ifelse(eth$score==0,"Neutral",0)))
xrp$polarity <- ifelse(xrp$score >0,"positive",ifelse(xrp$score < 0,"negative",ifelse(xrp$score==0,"Neutral",0)))

# Plot
qplot(factor(polarity), data=btc, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("BTC")

qplot(factor(score), data=eth, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("ETH")

df = ddply(scores, c("crypto"), summarise,
           pos_count=sum( positive ),
           neg_count=sum( negative ),
           neu_count=sum(neutral))
df$total_count = df$pos_count +df$neg_count + df$neu_count
df$pos_prcnt_score = round( 100 * df$pos_count / df$total_count )
df$neg_prcnt_score = round( 100 * df$neg_count / df$total_count )
df$neu_prcnt_score = round( 100 * df$neu_count / df$total_count )
attach(df)
#Positive
lbls <-paste(df$crypto,df$pos_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(pos_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Positive Comparative Analysis - Crypto")
#Negative
lbls <-paste(df$crypto,df$neg_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neg_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = " Negative Comparative Analysis - Crypto")

###################################################################################################

# GET COINMARKET CAP TICKER
data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://api.coinmarketcap.com/v1/ticker/?convert=',currency))))

# GGet current price for XRP in BTC, ETH, LTC, USD
https://min-api.cryptocompare.com/data/price?fsym=XRP&tsyms=BTC,ETH,LTC,USD


######################
# GENERATE CANDLES ###
######################
# Bifinex - NOT FOR US
install.packages('quantmod')
library(plotly)
library(quantmod)
btc.candle <- data.frame(jsonlite::fromJSON(RCurl::getURL('https://api.bitfinex.com/v2/candles/trade:1m:tBTCUSD/hist')))
colnames(btc.candle) <- c('Millisecond Time Stamp', 'Open', 'Close', 'High', 'Low', 'Volume')
head(btc.candle)


# Bittrex
#https://www.cryptocompare.com/api/#-api-data-histominute-
fromcoin='ETH'
tocoin='BTC'
aggregate=5 # this equals # of mins, so aggregate = 1 is 1min, 30 is a half hour, etc
limit=12 # this is the number of 1 min data to grab, so if we want to see 30min aggregates for the past 6 hours, then we would need 6 * 60 = 360 minutes
dat <- data.frame(jsonlite::fromJSON(RCurl::getURL(paste0('https://min-api.cryptocompare.com/data/histominute?fsym=',fromcoin,'&tsym=',tocoin,'&limit=',limit,'&aggregate=',aggregate,'&e=bittrex'))))

dat <- dat[,c('Data.time', 'Data.close', 'Data.high', 'Data.low', 'Data.open', 'Data.volumefrom', 'Data.volumeto', 'TimeTo', 'TimeFrom')]
head(dat, n=30)
dim(dat)

mn <- min(dat$Data.low)
mx <- max(dat$Data.high)

xs <- c(1:nrow(dat))
xs2 <- seq(1,nrow(dat),5)

color_list <- ifelse(dat$Data.close >= dat$Data.open, "green4", "red")

#plot candles
png(paste0(paste0(fromcoin,'_',tocoin),"_candles.png"), res=500, height=1000, width=1800)
par(mar=c(3,3,3,3))
plot(dat$Data.high, main=paste0(fromcoin,'/',tocoin), xaxt="n", xlab="", ylab="USD", ylim=c(mn, mx), type="n")
par(new=T)
plot(dat$Data.low, main="", axes=F, xlab="", ylab="", ylim=c(mn, mx), type="n")
segments( x0=xs, y0=dat$Data.open, x1=xs, y1=dat$Data.close, col=color_list, lwd=15)
segments( x0=xs, y0=dat$Data.low, x1=xs, y1=dat$Data.high, col=color_list, lwd=2)
axis(1, at=xs2, labels=dat$Data.time[xs2], las=2, cex.axis=1)
dev.off()
#




















###################
# ARCHIVE BELOW ###
###################

###############################################
# Collect tweets via a stream of 60 seconds ###
###############################################
library(streamR)
load("my_oauth.Rdata")

filterStream(file.name = "tweetsLocation.json", # Save tweets in a json file
             track = keywords, # Collect tweets mentioning btc, eth, etc.
             language = "en",
             location = c(-119, 33, -117, 35), # latitude/longitude pairs providing southwest and northeast corners of the bounding box.
             timeout = 60, # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

tweets.df <- parseTweets("tweetsLocation.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
head(tweets.df)






#############################
# Initial StockTwit setup ###
#############################
# Install and Activate Packages
#install.packages("streamR", "RCurl", "RJSONIO")
library(streamR)
library(RCurl)
#devtools::install_github("duncantl/RJSONIO")
library(RJSONIO)
library(stringr)

# PART 1: Declare Twitter API Credentials & Create Handshake
setwd("/Users/stephenhwang/Desktop/crypto_twitter")

library(ROAuth)
requestURL <- "https://api.stocktwits.com/api/2/oauth/token"
accessURL <- "https://api.stocktwits.com/api/2/oauth/token"
authURL <- "https://api.stocktwits.com/api/2/oauth/authorize"
consumerKey <- "70f9972820a64d66" # From dev.twitter.com
consumerSecret <- "d1b94d1044935c8c9f4664b0ca326a9a6fb89298" # From dev.twitter.com

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# PART 2: Save the my_oauth data to an .Rdata file
save(my_oauth, file = "my_oauth.Rdata")




cURL [ about cURL ]
// Token example
curl -X GET -u <username> -p https://api.stocktwits.com/api/2/oauth/authorize -d 'client_id=<client id>&response_type=token&redirect_uri=http://www.example.com&scope=read,watch_lists,publish_messages,publish_watch_lists,follow_users,follow_stocks'

// Code example
curl -X GET https://api.stocktwits.com/api/2/oauth/authorize -d 'client_id=<client id>&response_type=code&redirect_uri=http://www.example.com&scope=read,watch_lists,publish_messages,publish_watch_lists,follow_users,follow_stocks'


########
library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low) %>%
  layout(title = "Basic Candlestick Chart")

plot_ly(df, x = ~Date, type="candlestick",
        open = ~AAPL.Open, close = ~AAPL.Close,
        high = ~AAPL.High, low = ~AAPL.Low)
layout(title = "Basic Candlestick Chart")


library(plotly)
p <- plot_ly(df, x = ~Date, color = ~state, type = "box")
p
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
Sys.setenv("plotly_username"="biohack92")
Sys.setenv("plotly_api_key"="Xaf76AUzbmitkQkJ7rfB")


api_create(p, filename = "midwest-boxplots")
chart_link
