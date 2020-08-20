Joseph Amsler POTUS Twitter Analysis
================
Joseph Amsler
May 2020

Introduction

The idea for this project came to me a couple years ago while I was on
the desk at my previous job on Wall Street. On our floor, we had CNBC
running all day, every day in order for us to be able to stay current
with the news and see how the markets were doing. While significant
movements in the markets are usually brought on by economic reports,
political events, or policy changes, the market was experiencing
significant volatility one day, and it was because of none of the above.
It was simply this:

``` r
knitr::include_graphics("Trump_tweet_analysis.png")
```

![](Trump_tweet_analysis.png)<!-- -->

Generally speaking, a market drop more than \~1% is pretty significant.
That day, the DOW dropped 3%. This got me thinking - could Donald Trump,
one of the most powerful men in the world, who, to put it lightly, tends
to be a bit of a loose cannon when it comes to what he says on twitter,
really have the power to move markets on a consistent basis through his
twitter account? Let’s find out:

First, I imported Trump’s tweet data from the time he was inagurated to
when I started this project (late 2019) and reformed the dataframe into
an analyzable format

``` r
jdata <- read_json("Trump_Twitter_Data.json", simplifyVector = TRUE)

data_frame <- as.data.frame(jdata)

data_frame1 = select(data_frame,-id_str,-source)


data_frame1$substring1 = substr(data_frame1$created_at, 5, 10)
data_frame1$substring2 = substr(data_frame1$created_at,29,30)
data_frame1$date = (paste(data_frame1$substring1, data_frame1$substring2, sep = " "))

data_frame2 <- data_frame1 %>%
  select(date,text)

data_frame2$date = as.Date(data_frame2$date,format="%b %d %y")
data_frame2$text = gsub("\\https.*","",data_frame2$text)

data_frame3 = data_frame2
```

Placed dummy text into blank tweet cells (to not have null text values)

``` r
for (z in 1:length(data_frame3$text)) {
  if (data_frame3$text[z] == "") {
    data_frame3$text[z] = "text text text"
  }
}
```

Obtained S\&P 500 Historical Data via yahoo finance

``` r
sp500Data <- read.csv("^GSPC.csv")
sp500Data$date = as.Date(sp500Data$date, format = "%Y-%m-%d")
```

Joined the Trump Tweet table and S\&P 500 historical data table

``` r
merged_tables <-merge(data_frame3, sp500Data, by.data_frame3 = "date", by.sp500Data = "date", all.x = TRUE)
summary(merged_tables)
```

    ##       date                text                Open           High     
    ##  Min.   :2017-02-12   Length:11761       Min.   :2322   Min.   :2332  
    ##  1st Qu.:2018-05-26   Class :character   1st Qu.:2683   1st Qu.:2695  
    ##  Median :2019-03-11   Mode  :character   Median :2840   Median :2852  
    ##  Mean   :2018-12-13                      Mean   :2801   Mean   :2812  
    ##  3rd Qu.:2019-08-20                      3rd Qu.:2952   3rd Qu.:2955  
    ##  Max.   :2019-12-13                      Max.   :3147   Max.   :3176  
    ##                                          NA's   :3431   NA's   :3431  
    ##       Low           Close        Adj.Close        Volume         
    ##  Min.   :2321   Min.   :2328   Min.   :2328   Min.   :1.350e+09  
    ##  1st Qu.:2660   1st Qu.:2682   1st Qu.:2682   1st Qu.:3.204e+09  
    ##  Median :2825   Median :2840   Median :2840   Median :3.440e+09  
    ##  Mean   :2788   Mean   :2801   Mean   :2801   Mean   :3.527e+09  
    ##  3rd Qu.:2936   3rd Qu.:2946   3rd Qu.:2946   3rd Qu.:3.776e+09  
    ##  Max.   :3143   Max.   :3169   Max.   :3169   Max.   :7.609e+09  
    ##  NA's   :3431   NA's   :3431   NA's   :3431   NA's   :3431       
    ##    PercChange    
    ##  Min.   :-4.098  
    ##  1st Qu.:-0.263  
    ##  Median : 0.071  
    ##  Mean   : 0.043  
    ##  3rd Qu.: 0.478  
    ##  Max.   : 4.959  
    ##  NA's   :3431

``` r
merged_tables1 <- merged_tables %>%
  fill(Open, High, Close, Low, Close, Adj.Close, Volume, PercChange,.direction = "up")
```

Next, because Trump tends to be very emotional in his tweets, I wanted
to see if there was any correlation between the sentiment of his tweets
and volatility in the market (i.e. does an angry Trump on twitter
correlate to higher market volatility?) To do this, I classified each
tweet as a specific emotion using the NRC sentiment package

``` r
class_table <- get_sentiments("nrc")

merged_tables2 <- merged_tables1 %>%
  select(date,text,PercChange) 

sentiment <- merged_tables2 %>%
  unnest_tokens(word,text)
```

Created blank columns for each sentiment

``` r
merged_tables2$positive = NA
merged_tables2$trust = NA
merged_tables2$negative = NA
merged_tables2$anticipation = NA
merged_tables2$fear = NA
merged_tables2$anger = NA
merged_tables2$joy = NA
merged_tables2$sadness = NA
merged_tables2$disgust = NA
merged_tables2$surprise = NA


WordCount <-sentiment %>%
  group_by(word) %>%
  summarise(Count = n()) %>%
  anti_join(stop_words) %>%
  arrange(desc(Count))
```

    ## Joining, by = "word"

Merged sentiment value with word counts

``` r
merge_sentiment_count <- merge(class_table, WordCount, by.class_table = "word", by.WordCount = "word") %>%
  group_by(sentiment) %>%
  summarise(Count = sum(Count)) %>%
  arrange(desc(Count))

merge_sentiment_table <- merge(class_table, WordCount, by.class_table = "word", by.WordCount = "word") %>%
  arrange(desc(Count))
```

For fun, I made a word cloud to see in general the most common words -
the results aren’t too surprising…

``` r
set.seed(1234)
trump_word_cloud = wordcloud(words = WordCount$word, freq = WordCount$Count, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The following will read text and determine emotion of that day

``` r
for (k in 1:length(merged_tables2$date)){
  new_vector = tolower(strsplit(merged_tables$text[k],"\\s+")[[1]])
  new_dataframe = data.frame(matrix(ncol = 2, nrow =length(new_vector)))
  x = c("word", "sentiment")
  colnames(new_dataframe) = x
  for (i in 1:length(new_vector)) {
    new_dataframe$word[i] = new_vector[i]
  }

  



dataframe_sentiment_by_day <-  new_dataframe %>% 
  inner_join(merge_sentiment_table, by = "word") %>%
  select(word,sentiment.y)
  

    
#Setting counters to zero


  
  positive = 0
  trust = 0
  negative = 0
  anticipation = 0
  fear = 0
  anger = 0
  joy = 0
  sadness = 0
  disgust = 0
  surprise = 0



#The following forloop will increment the values based on prevalence in the days' tweet


  
for (i in 1:length(dataframe_sentiment_by_day$sentiment.y)) {
    tryCatch(
    if(dataframe_sentiment_by_day$sentiment.y[i] == "positive") {
      positive = positive + 1.02
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "trust") {
      trust = trust + 1.01
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "negative") {
      negative = negative + 1.03
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "anticipation") {
      anticipation = anticipation + .999
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "fear") {
      fear = fear + 1.05
    },
    error = function (e) {NULL}) 
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "anger") {
      anger = anger + 1.04
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "joy") {
      joy = joy + 1
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "sadness") {
      sadness = sadness + 1.07
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "disgust") {
      disgust = disgust + 1.08
    },
    error = function (e) {NULL})
    tryCatch(
    if (dataframe_sentiment_by_day$sentiment.y[i] == "surprise") {
      surprise = surprise + 1.09
    },
    error = function (e) {NULL})
  
  
  merged_tables2$positive[k] = positive
  merged_tables2$trust[k] = trust
  merged_tables2$negative[k] = negative
  merged_tables2$anticipation[k] = anticipation
  merged_tables2$fear[k] = fear
  merged_tables2$anger[k] = anger
  merged_tables2$joy[k] = joy
  merged_tables2$sadness[k] = sadness
  merged_tables2$disgust[k] = disgust
  merged_tables2$surprise[k] = surprise

}
}
```

The following uses mean imputation to fill in missing values for daily
S\&P movements

``` r
merged_tables2 <- merged_tables2 %>%
  mutate(PercChange = ifelse(!is.na(PercChange), PercChange,mean(PercChange, na.rm = TRUE)))
```

The following creates a classifcation vector that will classify each day
of tweets as a specific emotion

``` r
merged_tables2$classification = NA


for (j in 1:length(merged_tables2$classification)) {
    if(((!is.na(merged_tables2$positive[j])) & (merged_tables2$positive[j] > merged_tables2$trust[j]) & (merged_tables2$positive[j] > merged_tables2$negative[j]) & (merged_tables2$positive[j] > merged_tables2$anticipation[j]) & (merged_tables2$positive[j] > merged_tables2$fear[j]) & (merged_tables2$positive[j] > merged_tables2$anger[j])
       & (merged_tables2$positive[j] > merged_tables2$joy[j]) & (merged_tables2$positive[j] > merged_tables2$sadness[j]) & (merged_tables2$positive[j] > merged_tables2$disgust[j]) & (merged_tables2$positive[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "positive"
    }
  

    if(((!is.na(merged_tables2$trust[j])) & (merged_tables2$trust[j] > merged_tables2$positive[j]) & (merged_tables2$trust[j] > merged_tables2$negative[j]) & (merged_tables2$trust[j] > merged_tables2$anticipation[j]) & (merged_tables2$trust[j] > merged_tables2$fear[j]) & (merged_tables2$trust[j] > merged_tables2$anger[j])
       & (merged_tables2$trust[j] > merged_tables2$joy[j]) & (merged_tables2$trust[j] > merged_tables2$sadness[j]) & (merged_tables2$trust[j] > merged_tables2$disgust[j]) & (merged_tables2$trust[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "trust"
    }

    if(((!is.na(merged_tables2$negative[j])) & (merged_tables2$negative[j] > merged_tables2$positive[j]) & (merged_tables2$negative[j] > merged_tables2$trust[j]) & (merged_tables2$negative[j] > merged_tables2$anticipation[j]) & (merged_tables2$negative[j] > merged_tables2$fear[j]) & (merged_tables2$negative[j] > merged_tables2$anger[j])
          & (merged_tables2$negative[j] > merged_tables2$joy[j]) & (merged_tables2$negative[j] > merged_tables2$sadness[j]) & (merged_tables2$negative[j] > merged_tables2$disgust[j]) & (merged_tables2$negative[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "negative"
    }
    if(((!is.na(merged_tables2$anticipation[j])) & (merged_tables2$anticipation[j] > merged_tables2$positive[j]) & (merged_tables2$anticipation[j] > merged_tables2$trust[j]) & (merged_tables2$anticipation[j] > merged_tables2$negative[j]) & (merged_tables2$anticipation[j] > merged_tables2$fear[j]) & (merged_tables2$anticipation[j] > merged_tables2$anger[j])
          & (merged_tables2$anticipation[j] > merged_tables2$joy[j]) & (merged_tables2$anticipation[j] > merged_tables2$sadness[j]) & (merged_tables2$anticipation[j] > merged_tables2$disgust[j]) & (merged_tables2$anticipation[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "anticipation"
    }
    if(((!is.na(merged_tables2$fear[j])) & (merged_tables2$fear[j] > merged_tables2$positive[j]) & (merged_tables2$fear[j] > merged_tables2$trust[j]) & (merged_tables2$fear[j] > merged_tables2$negative[j]) & (merged_tables2$fear[j] > merged_tables2$anticipation[j]) & (merged_tables2$fear[j] > merged_tables2$anger[j])
          & (merged_tables2$fear[j] > merged_tables2$joy[j]) & (merged_tables2$fear[j] > merged_tables2$sadness[j]) & (merged_tables2$fear[j] > merged_tables2$disgust[j]) & (merged_tables2$fear[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "fear"
    }
    if(((!is.na(merged_tables2$anger[j])) & (merged_tables2$anger[j] > merged_tables2$positive[j]) & (merged_tables2$anger[j] > merged_tables2$trust[j]) & (merged_tables2$anger[j] > merged_tables2$negative[j]) & (merged_tables2$anger[j] > merged_tables2$anticipation[j]) & (merged_tables2$anger[j] > merged_tables2$fear[j])
            & (merged_tables2$anger[j] > merged_tables2$joy[j]) & (merged_tables2$anger[j] > merged_tables2$sadness[j]) & (merged_tables2$anger[j] > merged_tables2$disgust[j]) & (merged_tables2$anger[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "anger"
    }
    if(((!is.na(merged_tables2$joy[j])) & (merged_tables2$joy[j] > merged_tables2$positive[j]) & (merged_tables2$joy[j] > merged_tables2$trust[j]) & (merged_tables2$joy[j] > merged_tables2$negative[j]) & (merged_tables2$joy[j] > merged_tables2$anticipation[j]) & (merged_tables2$joy[j] > merged_tables2$fear[j])
           & (merged_tables2$joy[j] > merged_tables2$anger[j]) & (merged_tables2$joy[j] > merged_tables2$sadness[j]) & (merged_tables2$joy[j] > merged_tables2$disgust[j]) & (merged_tables2$joy[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "joy"
    }
    if(((!is.na(merged_tables2$sadness[j])) & (merged_tables2$sadness[j] > merged_tables2$positive[j]) & (merged_tables2$sadness[j] > merged_tables2$trust[j]) & (merged_tables2$sadness[j] > merged_tables2$negative[j]) & (merged_tables2$sadness[j] > merged_tables2$anticipation[j]) & (merged_tables2$sadness[j] > merged_tables2$fear[j])
       & (merged_tables2$sadness[j] > merged_tables2$anger[j]) & (merged_tables2$sadness[j] > merged_tables2$joy[j]) & (merged_tables2$sadness[j] > merged_tables2$disgust[j]) & (merged_tables2$sadness[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "sadness"
    }
    if(((!is.na(merged_tables2$disgust[j])) & (merged_tables2$disgust[j] > merged_tables2$positive[j]) & (merged_tables2$disgust[j] > merged_tables2$trust[j]) & (merged_tables2$disgust[j] > merged_tables2$negative[j]) & (merged_tables2$disgust[j] > merged_tables2$anticipation[j]) & (merged_tables2$disgust[j] > merged_tables2$fear[j])
           & (merged_tables2$disgust[j] > merged_tables2$anger[j]) & (merged_tables2$disgust[j] > merged_tables2$joy[j]) & (merged_tables2$disgust[j] > merged_tables2$sadness[j]) & (merged_tables2$disgust[j] > merged_tables2$surprise[j]))) {
      merged_tables2$classification[j] = "disgust"
    }
    if(((!is.na(merged_tables2$surprise[j])) & (merged_tables2$surprise[j] > merged_tables2$positive[j]) & (merged_tables2$surprise[j] > merged_tables2$trust[j]) & (merged_tables2$surprise[j] > merged_tables2$negative[j]) & (merged_tables2$surprise[j] > merged_tables2$anticipation[j]) & (merged_tables2$surprise[j] > merged_tables2$fear[j])
       & (merged_tables2$surprise[j] > merged_tables2$anger[j]) & (merged_tables2$surprise[j] > merged_tables2$joy[j]) & (merged_tables2$surprise[j] > merged_tables2$sadness[j]) & (merged_tables2$surprise[j] > merged_tables2$disgust[j]))) {
      merged_tables2$classification[j] = "surprise"
    }
}
merged_tables2$classification <- ifelse(is.na(merged_tables2$classification), 'None', merged_tables2$classification)
```

The following changes the PercChange to absolute value

``` r
merged_tables2$PercChange = abs(merged_tables2$PercChange)
```

The following defines “market moving” at a percent change greater than
0.7

``` r
merged_tables2$market_moving = 0

for (q in 1:length(merged_tables2$PercChange)) {
  if (merged_tables2$PercChange[q] > 0.7) {
    merged_tables2$market_moving[q] = 1
  }
}
```

Created emotion index for each tweets

``` r
merged_tables2$emotionIndex = NA

for (r in 1:length(merged_tables2$emotionIndex)) {
  merged_tables2$emotionIndex[r] = merged_tables2$positive[r] + merged_tables2$trust[r] + merged_tables2$negative[r] + merged_tables2$anticipation[r] + merged_tables2$fear[r] + merged_tables2$anger[r] + merged_tables2$joy[r] + merged_tables2$sadness[r] + merged_tables2$disgust[r] + merged_tables2$surprise[r]
}
```

Checking distribution of data for market moving vs not

``` r
class_plot = merged_tables2 %>%
  ggplot(aes(x = classification)) +
  geom_bar()
class_plot
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
dist_plot = merged_tables2 %>%
  ggplot(aes(market_moving)) +
  geom_bar() +
  theme_minimal()
dist_plot  
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
boxplot = merged_tables2 %>%
  ggplot(mapping = aes(x = as.factor(classification), y = PercChange)) +
  geom_boxplot()
boxplot
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
addmargins(table(merged_tables2$market_moving))
```

    ## 
    ##     0     1   Sum 
    ##  8399  3362 11761

There is about a \~20% chance there is a market moving day, and most of
his tweets fall into the “positive” or “negative” category

The following groups dataframe by date

``` r
merged_tables_by_date = merged_tables2 %>%
  select(-text, -classification) %>%
  group_by(date) %>%
  summarise_all(funs(sum))
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
merged_tables_bydate_and_perc_change = merged_tables2 %>%
  select(date, PercChange, emotionIndex) %>%
  group_by(date) %>%
  summarise(PercChange = mean(PercChange),
                emotionIndex = sum(emotionIndex))


for (f in 1:length(merged_tables_by_date$market_moving)){
if(merged_tables_by_date$market_moving[f]> 0) {
  merged_tables_by_date$market_moving[f] = 1
}
}
```

Now, I want to attempt to build a regression to see if there’s any
significance between emotion and large market movements

``` r
log_test_table = merged_tables_by_date

logTest = glm(market_moving ~ positive + negative + trust + fear + joy + anticipation + surprise + anger + disgust + sadness, data = log_test_table, family = binomial)
summary(logTest)
```

    ## 
    ## Call:
    ## glm(formula = market_moving ~ positive + negative + trust + fear + 
    ##     joy + anticipation + surprise + anger + disgust + sadness, 
    ##     family = binomial, data = log_test_table)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4875  -0.8221  -0.7464   1.3433   2.1486  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.104479   0.115402  -9.571  < 2e-16 ***
    ## positive      0.048723   0.020395   2.389  0.01690 *  
    ## negative     -0.027785   0.023717  -1.172  0.24140    
    ## trust         0.008337   0.025429   0.328  0.74302    
    ## fear          0.009946   0.030811   0.323  0.74685    
    ## joy          -0.009919   0.038584  -0.257  0.79712    
    ## anticipation -0.025139   0.033892  -0.742  0.45825    
    ## surprise     -0.116190   0.039836  -2.917  0.00354 ** 
    ## anger         0.071423   0.037825   1.888  0.05899 .  
    ## disgust      -0.068952   0.037234  -1.852  0.06405 .  
    ## sadness       0.020552   0.034524   0.595  0.55165    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1216.1  on 1022  degrees of freedom
    ## Residual deviance: 1190.9  on 1012  degrees of freedom
    ## AIC: 1212.9
    ## 
    ## Number of Fisher Scoring iterations: 4

Unfortunately, there doesn’t really seem to be much here. There’s the
presence of some statistical significance, but not enough to
definitively justify a correlation.

Let’s try something else. Maybe if I can bucket his tweets into
different topics using topic modeling, a specific topic might have a
higher correlation with volatility.

The below cleans and preps his tweets for insertion into the topic
modeling functions.

``` r
topic_dataframe <- as.data.frame(cbind(merged_tables2$market_moving, merged_tables2$text))  %>%
  mutate(market_moving = V1,
         text = V2,
         doc_id = 0) %>%
  select(text, market_moving, doc_id)

toggle = 1
for (i in 1:length(topic_dataframe$doc_id)) {
  topic_dataframe$doc_id[i] = toggle
  toggle = toggle + 1
  
}


cleanDataframe = topic_dataframe %>%
  mutate(text = as.character(text), 
         text = str_replace_all(text, "\n", " "),   
         text = str_replace_all(text, "(\\[.*?\\])", ""),
         text = str_squish(text), 
         text = gsub("([a-z])([A-Z])", "\\1 \\2", text), 
         text = removeWords(text, c("’", stopwords(kind = "en"))), 
         text = removePunctuation(text), 
         text = removeNumbers(text)) %>%
  as.data.frame()


set.seed(1001)

holdoutRows = sample(1:nrow(cleanDataframe), 100, replace = FALSE)

tweetText = textProcessor(documents = cleanDataframe$text[-c(holdoutRows)],
                          metadata= cleanDataframe[-c(holdoutRows), ],
                          stem = FALSE,
                          lowercase = FALSE)
```

    ## Building corpus... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Removing numbers... 
    ## Creating Output...

``` r
tweetprep <- prepDocuments(documents = tweetText$documents,
                           vocab = tweetText$vocab,
                           meta = tweetText$meta)
```

    ## Removing 7579 of 15927 terms (7579 of 179570 tokens) due to frequency 
    ## Removing 8 Documents with No Words 
    ## Your corpus now has 11649 documents, 8348 terms and 171991 tokens.

Then, I ran a k Test to determine optimal number of topics

``` r
#kTest <- searchK(documents = tweetprep$documents,
#vocab = tweetprep$vocab,
#K = c(3,4,5,10,20), verbose = FALSE)

load("kTest.RData")
plot(kTest)
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

It looks like 10 topics is the best, based on semantic coherence.

The below then buckets each tweet into 10 topics:

topics10 = stm(documents =
tweetprep\(documents,  vocab = tweetprep\)vocab, seed = 1001, K = 10,
verbose = FALSE)

``` r
load("topcis10.RData")
plot(topics10)
```

![](Trump-Tweet-Analysis-Final_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
labelTopics(topics10)
```

    ## Topic 1 Top Words:
    ##       Highest Prob: great, text, house, white, united, states, vote 
    ##       FREX: great, text, white, united, vote, state, military 
    ##       Lift: “are, abbott, aboard, admiral, amir, andrews, announce 
    ##       Score: text, great, honor, vote, endorsement, white, vets 
    ## Topic 2 Top Words:
    ##       Highest Prob: president, this, dont, election, first, working, party 
    ##       FREX: dont, election, party, right, pelosi, but, nancy 
    ##       Lift: “foreign, “guts”, “impeachment”, “nancy, “political, “ukraine, “weve 
    ##       Score: president, this, dont, election, party, right, first 
    ## Topic 3 Top Words:
    ##       Highest Prob: trump, thank, donald, real, america, today, president 
    ##       FREX: trump, thank, donald, real, america, today, day 
    ##       Lift: earlier, ensure, hearts, joined, orlando, police, rating 
    ##       Score: trump, donald, thank, real, america, today, again 
    ## Topic 4 Top Words:
    ##       Highest Prob: country, must, can, they, our, usa, dollars 
    ##       FREX: must, can, usa, dollars, tariffs, stop, not 
    ##       Lift: address<U+0001F1FA><ed><U+00A0><U+00BC>, antisemite, apprehend, ballots, beaten, besides, bible 
    ##       Score: country, must, tariffs, can, dollars, countries, usa 
    ## Topic 5 Top Words:
    ##       Highest Prob: the, news, fake, people, media, like, nothing 
    ##       FREX: the, news, fake, media, like, nothing, fox 
    ##       Lift: sinclair, amazon, anger, anyway, are, books, bwatters 
    ##       Score: news, fake, the, media, nothing, cnn, story 
    ## Topic 6 Top Words:
    ##       Highest Prob: amp, democrats, time, much, want, one, dems 
    ##       FREX: amp, democrats, time, much, one, history, rep 
    ##       Lift: “all, activists, admitting, ambassadors, arrests, associates, censure 
    ##       Score: amp, democrats, dems, much, rep, one, time 
    ## Topic 7 Top Words:
    ##       Highest Prob: get, many, never, new, years, china, jobs 
    ##       FREX: years, jobs, trade, deal, republicans, democrat, come 
    ##       Lift: area, ban, difficult, negotiating, risk, simply, unfairly 
    ##       Score: trade, jobs, years, never, deal, china, get 
    ## Topic 8 Top Words:
    ##       Highest Prob: will, border, people, now, big, back, wall 
    ##       FREX: will, border, back, wall, security, mexico, help 
    ##       Lift: afghanistan, barrier, faster, patrol, ‘historic, “green”, “open 
    ##       Score: will, border, wall, back, security, mexico, southern 
    ## Topic 9 Top Words:
    ##       Highest Prob: made, now, witch, hunt, collusion, russia, fbi 
    ##       FREX: witch, hunt, collusion, russia, fbi, mueller, campaign 
    ##       Lift: “biden, “bob, “bruce, “collusion”, “consumer, “dossier”, “how 
    ##       Score: fbi, hillary, collusion, clinton, witch, hunt, mueller 
    ## Topic 10 Top Words:
    ##       Highest Prob: just, good, obama, see, world, north, economy 
    ##       FREX: just, good, see, world, economy, long, best 
    ##       Lift: “schiffs, adams, bedbugs, buck, carbon, carlson, cathedral 
    ##       Score: just, good, obama, korea, north, economy, best

``` r
findThoughts(topics10, texts = tweetprep$meta$text, n = 1)
```

    ## 
    ##  Topic 1: 
    ##       It   great honor  welcome Prime Minister Giuseppe Conte IT  Italy   White House Join us  pm E   joint press conference 
    ##  Topic 2: 
    ##       I dont know  Juaquin Castro     lesser brother   failed presidential candidate   makes  fool   every time  opens  mouth Juaquin    man   brother    brother according     much Keep fighting Juaquin 
    ##  Topic 3: 
    ##       RT replouiegohmert Honored heroic wounded warriors today   event hosted  All Saints Episcopal School  TX God bless  troops… 
    ##  Topic 4: 
    ##       Tariffs    tremendous positive impact   Steel Industry Plants  opening    US Steelworkers  working   big dollars  flowing   Treasury Other countries use Tariffs     use  foolish people scream 
    ##  Topic 5: 
    ##       The failing nytimes writes false story  false story   They  even call  verify  facts   story A Fake News Joke 
    ##  Topic 6: 
    ##       RT Rep Gosar Nancy Pelosis impeachment resolution  restricts  participation  duly elected Members  Congress   part… 
    ##  Topic 7: 
    ##       Just    good call  Swedish PM Stefan Löfven  assured   American citizen AAP Rocky will  treated fairly Likewise I assured   AAP    flight risk  offered  personally vouch   bail   alternative 
    ##  Topic 8: 
    ##       FLGov Scott   relentless  securing  funding  fix  algae problem  Lake Okeechobee   will solve  Congress must follow    Governments plan   Everglades Reservoir Bill Nelson    help 
    ##  Topic 9: 
    ##       Wow Nellie Ohr Bruce Ohrs wife   Russia expert   fluent  Russian She worked  Fusion GPS    paid  lot Collusion Bruce   boss   Department  Justice   unbelievably still  
    ##  Topic 10: 
    ##       Alec Baldwin whose dying mediocre career  saved   terrible impersonation    SNL now says playing   agony Alec   agony     forced  watch Bring back Darrell Hammond funnier   far greater talent

``` r
head(topics10$theta[,1], 10)
```

    ##  [1] 0.13505261 0.58781859 0.04423092 0.37919998 0.01917663 0.04854029
    ##  [7] 0.01727371 0.50148609 0.29809553 0.53767480

``` r
str(topics10$theta, 10)
```

    ##  num [1:11649, 1:10] 0.1351 0.5878 0.0442 0.3792 0.0192 ...

Based on the available data from the above topics: I have assigned each
topic the following label:

1: US White House & Military (18%) 3: Donald Trump (wtf??) (15%) 5: Fake
News (again… wtf) (12%) 9: Witch Hunt (11%) 8: US / Mexico border (11%)
6: Democrats (7%) 7: American Jobs (7%) 4: Foreign Policy and Trade (7%)
10: State of US Economy (6%) 2: US Election and Nancy Pelosi (6%)

Couple of observations here: 1. He literally spends 15% of his tweets
talking about himself… 2. Another \~23% he spends complaining about fake
news and the witch hunt against him 3. Poor Nancy Pelosi….. While these
observations are, well, quite hilarious, I don’t see him complaining
about unfair treatment from Democrats as being market moving. His tweets
on foreing policy, on the other hand, could be…

Now that I have a topic assigned to each tweet, I want to isolate the
tweets that fall into the China / trade policy category.

``` r
TopicModel2 = as.data.frame(cbind(tweetprep$meta$text, tweetprep$meta$market_moving,tweetprep$meta$doc_id, topics10$theta))
TopicModel2$V4 = as.numeric(as.character(TopicModel2$V4))
TopicModel2$V5 = as.numeric(as.character(TopicModel2$V5))
TopicModel2$V6 = as.numeric(as.character(TopicModel2$V6))
TopicModel2$V7 = as.numeric(as.character(TopicModel2$V7))
TopicModel2$V8 = as.numeric(as.character(TopicModel2$V8))
TopicModel2$V9 = as.numeric(as.character(TopicModel2$V9))
TopicModel2$V10 = as.numeric(as.character(TopicModel2$V10))
TopicModel2$V11 = as.numeric(as.character(TopicModel2$V11))
TopicModel2$V12 = as.numeric(as.character(TopicModel2$V12))
TopicModel2$V13 = as.numeric(as.character(TopicModel2$V13))


TopicModel2$V4Max = as.numeric(0)
TopicModel2$V8Max = as.numeric(0)
TopicModel2$V2 = as.numeric(as.character(TopicModel2$V2)) - 1 
colnames(TopicModel2) = c("text", "Market_Moving_Dummy", "ID", "Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10", "4Max", "8Max")
```

Now, the below is running a logistic regression to see if there is a
correlation between “Market moving” and the probability of being within
topic 4

``` r
log_table = TopicModel2 %>%
  select(-"4Max", -"8Max", -"ID")


logit_mod <-
  glm(Market_Moving_Dummy~ Topic4 , family = binomial(link = 'logit'), data = log_table)

summary(logit_mod)
```

    ## 
    ## Call:
    ## glm(formula = Market_Moving_Dummy ~ Topic4, family = binomial(link = "logit"), 
    ##     data = log_table)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2312  -0.8161  -0.7907   1.5103   1.6534  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.08726    0.03185 -34.139  < 2e-16 ***
    ## Topic4       2.07105    0.29057   7.127 1.02e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13934  on 11648  degrees of freedom
    ## Residual deviance: 13885  on 11647  degrees of freedom
    ## AIC: 13889
    ## 
    ## Number of Fisher Scoring iterations: 4

VERY statistically significant\! With a coefficient of 2.07 and a
p-value of 1.02e-12, I can say there is statistical significance between
the probability a tweet belongs to the “Foreign Policy and Trade” topic
and the probability the tweet is associated with market volatility. In
other words, the more Trump tweets about foreign policy and the ongoing
trade war with China, make sure you watch the market for that day
because it has probably moved a fair amount.

The below tweets are examples of those that follow the most into the
“Foreign Policy and Trade” category and thus have the highest
correlation to market volatility. Unfortunately, the infamous “covfefe”
tweet didn’t make the cut…

``` r
knitr::include_graphics("Trump_tweets.png")
```

![](Trump_tweets.png)<!-- -->

In conclusion, it’s clear that Wall Street is paying attention to what
Trump is saying on his twitter account, and clearly doesn’t like it when
he threatens to heighten trade escalations via twitter. The cool thing
about this project is that it definitely doesn’t have to be confined to
just Donald Trump either - observations similar to this can be observed
everywhere. For example, when Elon Musk, another incredibly powerful
individual who is very liberal with what he says on twitter, tweets that
his stock price is too high, Tesla’s stock price tanks 10%. Im the
future, I would love to be able to apply this process to other high
rankings officials and the stock of their corresponding organizations.
Until next time\!

![](trump_masterOfTwitter.gif)
