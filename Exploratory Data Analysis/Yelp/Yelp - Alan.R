#Statistics Assignment 2 COVID & Yelp written by Alan Nguyen

#Packages for both analysis
library(tidyverse)
library(tibble)
library(scales)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(ggrepel)


#--------------DAta Preperation ------------- #
#Clear any exsisting data
rm(list = ls())

#Adding data 
owid.data <- read.csv("owid-covid-data.csv")

#Data Cleaning 
df.owid <- as_tibble(owid.data)

#capturing greater than 100 days
df.owid <- df.owid[df.owid$total_cases >= 100,]
df.100days <- df.owid[,c('location','total_cases')]

#Remove NA
df.100days <- drop_na(df.100days)

#Add Days columns and adjust to start with 0 
df.100days$days <- ave(df.100days$total_cases, df.100days$location, FUN = seq_along)
df.100days$days <- df.100days$days - 1

#Filtering countries
countries.req <- c('Australia', 'China', 'India', 'New Zealand', 'Sweden', 'Russia', 'United Kingdom','United States')
df.loc <- df.100days %>%
  filter(location %in% countries.req)


#--------Total country numbers Q2 ----------- #

#Adding country name to the last point
df_total_cases <- df.loc %>% group_by(location) %>% summarise(total_cases = max(total_cases), days = max(days))
df_total_cases <- df_total_cases[order(df_total_cases$total_cases),]


#Graphing total cases
graph_total <- ggplot(df_total_cases, aes(x = reorder(location, total_cases), y = total_cases, fill = location)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= comma(total_cases)),  position = position_dodge(width = 0.9), vjust = -.5) +
  theme(legend.position="none") +
  scale_y_continuous(label = comma) +
  labs(title = "Total Covid Cases",
       x = "Countries",
       y = "Cases",
       subtitle = "Total Covid cases as at 5th October 2021",
       caption = "Source: Our World in Data")

print(graph_total)



#-------Cumulative cases count Q3 --------- #

#Graphing cumulative cases using log scale
country_cases <- ggplot(df.loc, aes(x = days, y = total_cases)) +
  geom_line(aes(color = location), size = 1) + 
  scale_y_log10(limits = c(100,1e8)) +
  labs(title = "Cumulative cases count - log scale (post-100 cases)",
       subtitle = "Cases as at 5th October 2021 \nCases ignores deaths and recoveries",
       caption = "Source: Our World in Data",
       x = "Days since 100th confirmed",
       y = "Cumulative Cases",
       color = "Countries") + 
  theme_minimal(base_size = 12) + 
  geom_label_repel(data = df_total_cases, aes(label = location))

print(country_cases)


#-------Total number of cases per 100k population Q4 ----- #
#Taking required fields
df.cases <- df.owid[,c("iso_code", "date","total_cases","population","life_expectancy")] 
df.cases <- drop_na(df.cases) 

#creating a column and calculating population cases per 100k 
df.cases$total_case_per_100k <- df.cases$total_cases/(df.cases$population/100000)

#Taking the last date 
df.cases <- df.cases %>%
  filter(date %in% "2021-10-05")

#Summary 
df.cases %>%
  summarise(mean(life_expectancy),
            mean(total_case_per_100k),
            cor(life_expectancy,total_case_per_100k),
            sd(life_expectancy),
            sd(total_case_per_100k))

#Graphing
graph.lf <- ggplot(df.cases, aes(x = life_expectancy, y = total_case_per_100k)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  annotate(x=55, y=25000, 
           label=paste("Person's R = ", round(cor(df.cases$life_expectancy, df.cases$total_case_per_100k),2)), 
           geom="text", size=5) + 
  labs(title = "Relationship between Life expectancy and total cases per 100k life expectancy",
       subtitle = "185 countries",
       caption = "Source: Our World in Data",
       x = "Life expectancy",
       y = "Total cases per 100,000") + 
  theme_minimal()
print(graph.lf)

#Correlation Test
cor.test(df.cases$life_expectancy, df.cases$total_case_per_100k)



#----------------------------------------------------------------#
#-------------------------- YELP---------------------------------#
#----------------------------------------------------------------#

# ----------- DATA Prep  ---------- #
#Clear any exsisting list
rm(list = ls())


#Loading yelp data 
df.data <- read.csv("yelp_reviews.csv")
df.data <- as_tibble(df.data)


#Mode function for graphing
Mode <- function(x) {
  a <- table(x)
  as.numeric(names(a)[a == max(a)])}

#Taking the columns required
df.data <- df.data[,c('stars','review_length','pos_words','neg_words','net_sentiment','votes_useful')]


#remove zeroes
df.data[df.data$review_length == 0,] <- NA
df.data <- na.omit(df.data)


#------Statistical Summaries Q2 -------#

#Descriptive summary
summary(df.data)

#Star summary
by.stars.sum <- df.data %>% group_by(stars) %>% count(stars)

#Star barchart
graph.star <- ggplot(by.stars.sum, aes(x = stars, y= n, fill = stars)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= comma(n)),  position = position_dodge(width = 0.9), vjust = -.5) +
  labs(title = "Review star ratings",
       x = "Stars",
       y = "Reviews") +
  theme_minimal()
print(graph.star)


#review length summmary
summary(df.data$review_length)
sd(df.data$review_length)
q <- df.data %>%
  summarise(lower = quantile(review_length, probs = .25),
            upper = quantile(review_length, probs = .75),)
Mode(df.data$review_length)

#review length graph
graph.review <- ggplot(df.data, aes(x = review_length)) +
  geom_histogram(binwidth = 10, colour="black", fill = "lightblue", boundary = 0) + 
  scale_x_continuous(breaks = seq(0, 1100, 50)) +
  labs(title = "Review length histogram",
       subtitle = "Red line represent quartile 1 and 3, blue line represents mean",
       x = "Review length(Characters)",
       y = "Frequency",
       caption = ) + 
  geom_vline(aes(xintercept = mean(review_length)), color = 'blue', linetype='dashed', size = 1) + 
  geom_vline(data = q, aes(xintercept = lower), color = 'red', linetype='dashed', size = 1) +
  geom_vline(data = q, aes(xintercept = upper), color = 'red', linetype='dashed', size = 1)
print(graph.review)


#Pos & Neg words summary
df.pos <- df.data[,"pos_words"]
df.neg <- df.data[,"neg_words"]
df.net <- df.data[,"net_sentiment"]


#------Positive word------#
#IQR *1.5
quan <- quantile(df.pos$pos_words, prob = 0.75, rm = T)
bench.pos <- quan + 1.5*IQR(df.pos$pos_words)
df.pos.wo <- df.pos %>%
  filter(pos_words < bench.pos)

#Graph with outliers
ggplot(df.pos, aes(y = pos_words)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Positive Words",
       title = "Positive Words with outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Graph without outliers  
ggplot(df.pos.wo, aes(y = pos_words)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Positive Words",
       title = "Positive Words without outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())  

#Summary positive word
summary(df.pos)
summary(df.pos.wo)


#------Negative word------#
#IQR *1.5
summary(df.neg)

quan.n <- quantile(df.neg$neg_words, prob = 0.75)
bench.neg <- quan + 1.5*IQR(df.neg$neg_words)
df.neg.wo <- df.neg %>%
  filter(neg_words < bench.neg)

#Graph with outliers
ggplot(df.neg, aes(y = neg_words)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Negative Words",
       title = "Negative Words with outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Graph without outliers  
ggplot(df.neg.wo, aes(y = neg_words)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Negative Words",
       title = "Negative Words without outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Summary negative word
summary(df.neg)
summary(df.neg.wo)


#------Net Summary ------#

#IQR * 1.5
summary(df.net)
net.upper <- quantile(df.net$net_sentiment, prob = 0.75)
net.lower <- quantile(df.net$net_sentiment, prob = 0.25)
bench.net <- net.upper + 1.5*IQR(df.net$net_sentiment)
bench.net2 <- net.lower - 1.5*IQR(df.net$net_sentiment)
df.net.wo <- df.net %>%
  filter(net_sentiment < net.upper & net_sentiment > net.lower)

#Graph with outliers 
ggplot(df.net, aes(y = net_sentiment)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Net sentiment",
       title = "Net sentiment with outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Graph without outliers  
ggplot(df.net.wo, aes(y = net_sentiment)) +
  geom_boxplot() + 
  labs(y = "Count",
       x = "Net sentiment",
       title = "Net sentiment without outliers") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Summary Net sentiment
summary(df.net)
summary(df.net.wo)

# ------- quick analysis of total words ------ #
total_count <- data.frame("typeofword" = c("Positive words", "Negative words"),
                          "value" = c(sum(df.data$pos_words), sum(df.data$neg_words)))

total_count
ggplot(total_count, aes(x = typeofword, y = value, fill = typeofword)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= comma(value)),  position = position_dodge(width = 0.9), vjust = -.5) +
  labs(title = "Negative vs Postive words",
       x = "",
       y = "No. Words") +
  scale_y_continuous(label=comma) +
  theme_minimal() + 
  theme(legend.position = "none")




#------Word Frequency Q3 & Q4 ------#

#Frequency table for positive words
df.pos <- df.data %>% 
  group_by(pos_words) %>%
  summarise(
    Freq = n())

df.pos <- df.pos[c(2:21),]

#Graphing positive graph
graph.pos <- ggplot(df.pos, aes(x = pos_words, y = Freq, fill = Freq)) +
  geom_bar(stat="identity") +
  labs(title = "Positive words in reviews",
       y = "Total Reviews",
       x = "Number of positive words") +
  scale_x_continuous(breaks=seq(0,20,1)) +
  scale_y_continuous(label = comma) +
  scale_fill_gradient2(low = "white", high = "orange")

print(graph.pos)


#Frequency table for negative words (20)
df.neg <- df.data %>% 
  group_by(neg_words) %>%
  summarise(
    Freq = n())

df.neg <- df.neg[c(2:21),]

#Graphing negative
graph.neg <- ggplot(df.neg, aes(x = neg_words, y = Freq, fill = Freq)) +
  geom_bar(stat="identity") +
  labs(title = "Negative words in reviews",
       y = "Total Reviews",
       x = "Number of negative words") +
  scale_x_continuous(breaks=seq(0,20,1)) +
  scale_y_continuous(label = comma) +
  scale_fill_continuous(labels=comma) +
  scale_fill_gradient2(low = muted("red"),
                       mid = "white",
                       high = muted("blue"),
                       midpoint = 0,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",aesthetics = "colour")
print(graph.neg)


#------ Net Sentiment Q4 ---------#
#Frequency Table for Net Sentiment
df.net<-df.data %>%
  group_by(net_sentiment)%>%
  summarise(
    Freq= n()) 

df.net<-df.net[c(2:21),]

#Graph Net
ggplot(df.net, aes(x=net_sentiment, y=Freq)) +
  geom_bar(stat="identity", fill="purple")+
  labs(title = "Net sentiment and reviews",
       y = "Total Reviews",
       x = "Net sentiments")



#------- Stars vs Review Length Q5 -------#

#Statistical Summary Groupby Stars
data.summary<-df.data %>% 
  group_by(stars) %>%
  summarise(mean = mean(review_length), 
            sd = sd(review_length),
            Median = median(review_length),
            IQR = IQR(review_length),
            Q1 = quantile(review_length, prob = 0.25),
            Q3 = quantile(review_length, prob =0.75))
data.summary

#box plot 
stargraph <- ggplot(df.data, aes(group =stars, y=review_length, color = stars)) + 
  geom_boxplot()+
  facet_wrap(~stars) +
  labs(title = "Stars",
       y = "Review Length") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
print(stargraph)

#dot plot for the median
mediangraph <-ggplot(data.summary, aes(x= factor(stars), y=Median)) + 
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=1.2) +
  labs(title = "Median Stars breakdown",
       y = "Median", 
       x = "Stars") 
print(mediangraph)

#------Stars and their overall usefulness Q6 ------ #

#Group
df.data%>% 
  group_by(factor(stars)) %>%
  summarise(cor = cor(review_length, votes_useful))

#Graphing overall review length, votes useful and stars
length.star.graph <- ggplot(df.data, aes(x = review_length, y = votes_useful)) + 
  geom_point(aes(color = factor(stars))) +
  theme_bw() +
  labs(title = "Review Length and Votes Useful",
       x = "Review Length",
       y = "Votes Useful",
       color = "Stars")
print(length.star.graph)

#Graphing star review breakdown
starbdgraph <- ggplot(df.data, aes(x = review_length, y = votes_useful)) + 
  geom_point() +
  labs(title = "Review Length brokendown by star ratings",
       x = "Review Length",
       y = "Votes Useful") +
  theme_minimal() +
  facet_wrap(~stars)
print(starbdgraph)

#Plotting Correlation taking data and rename for presenting purpsoes
corsum <- df.data[,c("votes_useful","review_length","stars")]
names(corsum) <- c("Votes Useful","Review Length","Stars")
r=cor(corsum,use="complete.obs")
round(r,4)

#Graphging correlation
ggcorrplot(r,
           hc.order = TRUE,
           type = "lower", 
           lab = TRUE)




 







