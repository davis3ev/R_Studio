# read in IMDB data
IMDB.df <- read.csv("IMDBMovieData.csv")

# summary statistics for numeric variables
summary(Filter(is.numeric, IMDB.df))

# correlation matrix for numeric variables
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, IMDB.df), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, IMDB.df), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(15,15))

# handling missing numeric data
# deleting records
dim(IMDB.df[is.na(IMDB.df$num_critic_for_reviews) |
              is.na(IMDB.df$duration) |
              is.na(IMDB.df$actor_3_facebook_likes) |
              is.na(IMDB.df$actor_1_facebook_likes) |
              is.na(IMDB.df$facenumber_in_poster) |
              is.na(IMDB.df$num_user_for_reviews) |
              is.na(IMDB.df$actor_2_facebook_likes), ])
IMDB.df <- IMDB.df[!(is.na(IMDB.df$num_critic_for_reviews) |
                       is.na(IMDB.df$duration) |
                       is.na(IMDB.df$actor_3_facebook_likes) |
                       is.na(IMDB.df$actor_1_facebook_likes) |
                       is.na(IMDB.df$facenumber_in_poster) |
                       is.na(IMDB.df$num_user_for_reviews) |
                       is.na(IMDB.df$actor_2_facebook_likes)), ]
dim(IMDB.df)

# imputing the median
IMDB.df$director_facebook_likes[is.na(IMDB.df$director_facebook_likes)] <- 
  median(IMDB.df$director_facebook_likes, na.rm = TRUE)

IMDB.df$gross[is.na(IMDB.df$gross)] <- median(IMDB.df$gross, na.rm = TRUE)

IMDB.df$budget[is.na(IMDB.df$budget)] <- median(IMDB.df$budget, na.rm = TRUE)

IMDB.df$title_year[is.na(IMDB.df$title_year)] <- median(IMDB.df$title_year, na.rm = TRUE)

IMDB.df$aspect_ratio[is.na(IMDB.df$aspect_ratio)] <- 
  median(IMDB.df$aspect_ratio, na.rm = TRUE)

summary(Filter(is.numeric, IMDB.df))

#keywords
#parse out the keywords from the pipe-delimited string and determine keyword frequency
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$plot_keywords),split = "|", fixed = TRUE))))
#list the 20 most frequent keywords
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 20)
#top 5 most frequent keywords:
#create binary dummy variable for keyword "love"
IMDB.df$key_love <- ifelse(grepl("love", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for keyword "friend"
IMDB.df$key_friend <- ifelse(grepl("friend", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for keyword "murder"
IMDB.df$key_murder <- ifelse(grepl("murder", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for keyword "death"
IMDB.df$key_death <- ifelse(grepl("death", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for keyword "police"
IMDB.df$key_police <- ifelse(grepl("police", IMDB.df$plot_keywords), 1,0)

#Genres
#parse out the keywords from the pipe-delimited string and determine genre frequency
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$genres),split = "|", fixed = TRUE))))
#list the 20 most frequent keywords
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 20)
#create binary dummy variable for Genres "Drama"
IMDB.df$key_Drama <- ifelse(grepl("Drama", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "Comedy"
IMDB.df$key_Comedy <- ifelse(grepl("Comedy", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "Thriller"
IMDB.df$key_Thriller <- ifelse(grepl("Thriller", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "Action"
IMDB.df$key_Action <- ifelse(grepl("Action", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "Romance"
IMDB.df$key_Romance <- ifelse(grepl("Romance", IMDB.df$plot_keywords), 1,0)

#Language
#parse out the keywords from the pipe-delimited string and determine language frequency
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$language),split = "|", fixed = TRUE))))
#list the 20 most frequent keywords
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 20)
#create binary dummy variable for Genres "English"
IMDB.df$key_English <- ifelse(grepl("English", IMDB.df$plot_keywords), 1,0)

#Country
#parse out the keywords from the pipe-delimited string and determine country frequency
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$country),split = "|", fixed = TRUE))))
#list the 20 most frequent keywords
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 20)
#create binary dummy variable for Genres "USA"
IMDB.df$key_USA <- ifelse(grepl("USA", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "UK"
IMDB.df$key_UK <- ifelse(grepl("UK", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "France"
IMDB.df$key_France <- ifelse(grepl("France", IMDB.df$plot_keywords), 1,0)
#create binary dummy variable for Genres "Canada"
IMDB.df$key_Canada <- ifelse(grepl("Canada", IMDB.df$plot_keywords), 1,0)

#Color
#parse out the keywords from the pipe-delimited string and determine color frequency
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$color),split = "|", fixed = TRUE))))
#fequency of black or white
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 2)
#create binary dummy variable for color "color
IMDB.df$key_color <- ifelse(grepl("color", IMDB.df$plot_keywords), 1,0)

#call variable names
t(t(names(IMDB.df)))



# create new data frame containing only the variables to be used for analysis
IMDBsub.df <- IMDB.df[, c(3:6, 8:9, 13:14, 16, 19, 23:44)]




install.packages("fastDummies")
library(fastDummies)
IMDBlangdummies <- dummy_cols(IMDB.df$language)
View(IMDBsub.df)
set.seed(1)
train.rows <- sample(rownames(IMDBsub.df), nrow(IMDBsub.df)*0.6)
imdb.train <- IMDBsub.df[train.rows, ]
imdb.valid <- setdiff(rownames(IMDBsub.df), train.rows)
imdb.vdata <- IMDBsub.df[imdb.valid,]
imdb.tdata <- imdb.train  
plot(imdb_score ~ duration, data= imdb.tdata)
model0 <- lm(imdb_score ~ duration, data = imdb.tdata)
model0
abline(model0, col="red",lty = 2, lwd = 2)
sum.model0 <- summary(model0)
sum.model0
