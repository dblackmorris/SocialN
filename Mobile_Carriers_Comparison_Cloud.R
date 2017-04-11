# load required packages
require(twitteR)
require(tm)
require(wordcloud)
require(RColorBrewer)

consumer_key <- '5Uo7Hm3Pw3Yp7mEJ9J9XT5K2w'
consumer_secret <- '47e9QhfusCfxi9AMJX8Mr4lJhmAvSsFKyFR6lrrpVagOx9S38Y'
access_token <- '2609984724-CCVaObCqEaXxDOcYJs3NK57PKStNHenpFDBpMjD'
access_secret <- '3OQNHwLu5RPWxA1cs7hM4OG9OZ36FRWNKNmDnp3yi42pY'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


# Collect tweets from mobile companies
# AT&T
att_tweets = userTimeline("ATT", n=1000)
# Verizon
ver_tweets = userTimeline("Verizon", n=1000)
# T-Mobile
mob_tweets = userTimeline("TMobile", n=1000)
# Metro PCS
pcs_tweets = userTimeline("MetroPCS", n=1000)

# Get the text contained in the tweets
att_txt = sapply(att_tweets, function(x) x$getText())
ver_txt = sapply(ver_tweets, function(x) x$getText())
mob_txt = sapply(mob_tweets, function(x) x$getText())
pcs_txt = sapply(pcs_tweets, function(x) x$getText())

# Create a function to clean texts
# (there are different ways to clean texts, this is just one option)
clean.text = function(x)
{
	# to lowercase
	#x = tolower(x)
	# remove rt (retweets)
	x = gsub("rt", "", x)
	# remove at people
	x = gsub("@\\w+", "", x)
	# remove punctuation marks
	x = gsub("[[:punct:]]", "", x)
	# remove numbers
	x = gsub("[[:digit:]]", "", x)
	# remove links http
	x = gsub("http\\w+", "", x)
	# remove tabs and extra spaces
	x = gsub("[ |\t]{2,}", "", x)
	# remove blank spaces at the beginning
	x = gsub("^ ", "", x)
	# remove blank spaces at the end
	x = gsub(" $", "", x)
    # result
	return(x)
}

# Clean texts
att_clean = clean.text(att_txt)
ver_clean = clean.text(ver_txt)
mob_clean = clean.text(mob_txt)
pcs_clean = clean.text(pcs_txt)

# Let's join the texts in a vector for each company
att = paste(att_clean, collapse=" ")
ver = paste(ver_clean, collapse=" ")
mob = paste(mob_clean, collapse=" ")
pcs = paste(pcs_clean, collapse=" ")

# Put everything in a single vector
all = c(att, ver, mob, pcs)

# Remove stopwords
all = removeWords(all, 
	c(stopwords("english"), "att", "verizon", "tmobile", "metropcs"))

# Create Corpus and term-document matrix
corpus = Corpus(VectorSource(all))
tdm = TermDocumentMatrix(corpus)

# Convert 'tdm' to matrix and add column names
tdm = as.matrix(tdm)
colnames(tdm) = c("ATT", "Verizon", "T-Mobile", "MetroPCS")

# Plot comparisonwordcloud 
comparison.cloud(tdm, random.order=FALSE, 
	colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
	title.size=1.5, max.words=500)

# Plot commonality wordcloud
commonality.cloud(tdm, random.order=FALSE, 
	colors = brewer.pal(8, "Dark2"), title.size=1.5)


# If you want to save the images in nice pdf format
pdf("CarriersCompCloud.pdf", width=8, height=8)
comparison.cloud(tdm, random.order=FALSE, 
	colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
	title.size=1.5, max.words=500)
dev.off()

pdf("CarriersCommCloud.pdf", width=8, height=8)
commonality.cloud(tdm, random.order=FALSE, 
	colors = brewer.pal(8, "Dark2"),
	title.size=1.5)
dev.off()
