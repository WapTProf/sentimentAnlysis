# =========================================================
#  Stock Sentiment Analysis
# =========================================================
# Initialize
	# set Working Directory
	setwd('c:/Users/Chris/Desktop/rSentiment')
	
	# load required packages
	libs <- c("tm", "tm.plugin.webmining", "tm.plugin.sentiment", "XML", "openNLP", "plyr")
	lapply(libs, require, character.only=T)

# Establish company to gather info on
symbol <- 'AAPL'
exchange <- 'NASDAQ:AAPL'
company <- 'Apple'

# Implement Selected Sources
	# Google 
	googleFinance <- WebCorpus(GoogleFinanceSource(exchange))
	googleNews <- WebCorpus(GoogleNewsSource(company))
	googleBlog <- WebCorpus(GoogleBlogSearchSource(company))
	
	# Reuters
	reutersBNews <- WebCorpus(ReutersNewsSource("businessNews"))
	reutersCNews <- WebCorpus(ReutersNewsSource("companyNews"))
	
	# Yahoo
	yahooFinance <- WebCorpus(YahooFinanceSource(symbol))
	yahooNews <- WebCorpus(YahooNewsSource(company))

# To load saved objects
load('googleFinance.RData')
load('googleNews.RData')
load('googleBlog.RData')
load('reutersBNews.RData')
load('reutersCNews.RData')
load('yahooFinance.RData')
load('yahooNews.RData')

# Save Sources
save(googleFinance, file = "googleFinance.RData") 
save(googleNews, file = "googleNews.RData") 
save(googleBlog, file = "googleBlog.RData") 
save(reutersBNews, file = "reutersBNews.RData") 
save(reutersCNews, file = "reutersCNews.RData") 
save(yahooFinance, file = "yahooFinance.RData") 
save(yahooNews, file = "yahooNews.RData") 

# To update objects
googleFinance <- corpus.update(googleFinance)
googleNews <- corpus.update(googleNews)
googleBlog <- corpus.update(googleBlog)
reutersBNews <- corpus.update(reutersBNews)
reutersCNews <- corpus.update(reutersCNews)
yahooFinance <- corpus.update(yahooFinance)
yahooNews <- corpus.update(yahooNews)

# Combine Corporas
mydata.corpus <- c(googleFinance, googleNews, googleBlog, reutersBNews, reutersCNews,  yahooFinance, yahooNews) 

# Focus on relevant sentences that contain the stock 
# symbol instead of tagging the entire news story
sentences <- sentDetect(mydata.corpus)
filteredSentences <- sentences[grepl(symbol,sentences)]

# Look at the headlines
heading <- sapply(mydata.corpus,FUN=function(x){attr(x,"Heading")})

# filteredDesc contains stock specific current news
desc <- sapply(mydata.corpus,FUN=function(x){ attr(x,"Description")})
filteredDesc <- desc[grepl(symbol,desc)]

# Build a Term-Document Matrix
	# clean text
	termFreq_control <- list(tolower = TRUE, tokenize=scan_tokenizer, removePunctuation = TRUE, removeNumbers = TRUE, stopwords=TRUE, wordLengths=c(2,25))
	
	# Term-Document Matrix
	mydata.dtm <- TermDocumentMatrix(mydata.corpus, control=termFreq_control)
	
	# Inspect DTM
	mydata.dtm
	
	# Inspect Most Popular Words
	findFreqTerms(mydata.dtm, lowfreq=30)
	
	# Find associated Terms
	aterm = 'put term here'
	findAssocs(mydata.dtm, aterm, 0.20)

# Make a Hierarchical Agglomerative cluster plot
# Remove sparse terms to simplify the cluster plot
# Note: tweak the sparse parameter to determine the number of words.
# About 10-30 words is good.
mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.90)

	# convert the sparse term-document matrix to a standard data frame
	mydata.df <- as.data.frame(inspect(mydata.dtm2))
 
	# inspect dimensions of the data frame
	nrow(mydata.df)
	ncol(mydata.df)	
	
# Cluster
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram?
 
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")	
	
# The terms higher in the plot are more popular, and terms close to each other are more associated.	
