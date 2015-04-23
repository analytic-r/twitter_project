# Step 1: 
# Invoke the required libraries, part 1.
library(twitteR) # setup_twitter_oauth, searchTwitter
library(ggplot2) #ggplot
library(stringr) # str_trim, substr
library(plyr) # ddply
library(reshape2) # melt
library(tm) # tm_map, Corpus
library(wordcloud) # wordcloud

# Step 2:
# Provide one's credentials to access tweets. The "x" characters are to hidden mine.
setup_twitter_oauth(consumer_key ='xxxxxxxxxxxxxxxxxxxxxx',
                    consumer_secret ='xxxxxxxxxxxxxxxxxxxxxxxx',
                    access_token='xxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxx',
                    access_secret='xxxxxxxxxxxxxxxxx')

# Step 3:
# Look up 10000 tweets about "daredevil" in English, between the specified interval.
tweets <- searchTwitter(searchString = "daredevil", lang="en",
                        since = "2015-04-10", until = "2015-04-11", n = 10000)

# Step 4:
# Make tweets plain and separate words with alphabetic character set.
tweets.w <- sapply(tweets, function(x) x$getText())
tweets.w <- iconv(tweets.w, to='UTF-8', sub='byte')
tweets.w <- tolower(tweets.w)
tweets.w <- gsub("rt", "", tweets.w)
tweets.w <- gsub("@\\w+", "", tweets.w)
tweets.w <- gsub("#\\w+", "", tweets.w)
tweets.w <- gsub('[[:punct:]]', '', tweets.w)
tweets.w <- gsub('[[:cntrl:]]', '', tweets.w)
tweets.w <- gsub("http\\w+", "", tweets.w)
tweets.w <- gsub("[ |\t]{2,}", "", tweets.w)
tweets.w <- gsub("^ ", "", tweets.w)
tweets.w <- gsub(" $", "", tweets.w)
tweets.w <- str_split(tweets.w, '\\s+')

# Step 5
# Create a data frame with the tweets timestamp
results <- data.frame()
for (i in 1:length(tweets)) {
    temp <- tweets[[i]]$created
    results <- rbind(results, temp)
}
names(results) <- "created"

# Step 6:
# Define evaluator function whick matches the tweets' words with sentiment words and add values
evaluator <- function(word_list) {
    scores <<- data.frame()
    for (i in tweets.w) {
        tweets.words <<- unlist(i)
        matches <- word_list$V2[match(tweets.words, word_list$V1)]
        matches <- matches[!is.na(matches)]
        score <- sum(matches)   
        scores <- rbind(scores, score)
    }
    scores <- scores[!is.na(scores)]
    return(matrix(scores))
}
# The link to these files are here: https://github.com/analytic-r/twitter_project/tree/master/_support
word_list_long <<- read.csv("wordwithStrength.csv", sep = ";", header = FALSE)
word_list_short <<- read.csv("pn_words.csv", sep = ";", header = FALSE)

# Run the evaluator function against the long and short word list
results$score_long <- evaluator(word_list_long)
results$score_short <- evaluator(word_list_short)

# Step 7:
# Create columns with the tweets' hour and minute values
results$hours = format(as.POSIXct(results$created, origin = "1970-01-01"), format="%H")
results$mins = format(as.POSIXct(results$created, origin = "1970-01-01"), format="%M")

# Summarize the values and merge them together
scores_merged <- ddply(results, c("hours","mins"), summarise, N = length(created), avg_long = mean(score_long))
scores_merged$created <- as.POSIXct(factor(paste0(as.character(scores_merged$hour),
                                                  ':',as.character(scores_merged$min))), format="%H:%M")
scores_short <- ddply(results, c("hours","mins"), summarise, N = length(created), avg_short = mean(score_short))
scores_merged$avg_short <- scores_short$avg_short

# Step 8:
# Create a long list to be able to plot them.
long <- melt(scores_merged, id.vars = c("hours", "mins", "N", "created"))

# Plot the two results into a faceted plot
ggplot(data=long, aes(x=created, y=value, colour=variable)) + geom_line() + geom_hline(colour = "black", size = 1) +
    stat_smooth(method = "lm", fullrange = TRUE, colour = "darkgreen", fill = "lightgreen") +
    scale_x_datetime(limits = c(as.POSIXct(strptime(min(long$created), "%Y-%m-%d %H:%M")),
                                as.POSIXct(strptime(max(long$created), "%Y-%m-%d %H:%M")))) +
    labs(title = "Daredevil", x = "Time", y = "Average Sentiment Score") + ylim(min(long$value), max(long$value)) + theme_bw() + facet_grid(variable ~ .)

# Step 9:
# Remove common ("the", "a", "an", etc) words from the tweets and create a wordcloud
tweets.text.corpus <- Corpus(VectorSource(tweets.w))
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
wordcloud(tweets.text.corpus,min.freq = 1, scale=c(5.5,0.7), rot.per=.15,
          colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 70)

# Step 10:
# Detach "ggplot" library, so as "ggmap" could work without problem. Invoke the required library, part 2.
detach("package:ggplot2", unload=TRUE)
library(ggmap) # geocode, get_map, qmap

# Step 11:
# Extract locations of the twitters, then make it as plain text as possible. Use only 1000 tweets in this example to save time.
locations <- matrix()
tw_names <- unique(sapply(tweets, function(x) x$screenName))
locs <- sapply(lookupUsers(tw_names[1:1000]), function(x) x$location)
locs <- iconv(locs, to='UTF-8', sub='byte')
locs <- gsub('[:.)(]', '', locs)
locs <- gsub('[[:digit:]]', '', locs)
locs <- str_trim(locs, side = "both")
locs <- substr(locs, 0,35)
for (i in 1:length(locs)) {
    location <- locs[i][[1]]
    locations <- rbind(locations, location)
}

location_matrix <- data.frame(names = tw_names[1:1000], locations = na.omit(locations), row.names = NULL, stringsAsFactors = FALSE)

# Remove locations having less than three characters
locations <- location_matrix$locations[nchar(location_matrix$locations) > 2]

# Get latitude and longitude to the identified places
map_points <- geocode(locations)
map_points <- na.omit(map_points)

# Step 12:
# Two ways of getting maps with locations. The first one is the North-America map.
map <- get_map(location = "america", zoom = 3, maptype = "satellite")
ggmap(map) + geom_point(aes(x = lon, y = lat, size = 2, colour = "red", alpha = .5), data = map_points)

# Europe map
qmap('europe', zoom = 4) + geom_point(aes(x = lon, y = lat), data = map_points, colour = 'red', fill = 'red', size = 2)