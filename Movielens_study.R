#Load the packages needed or install them if you have not done it.
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(magrittr)) install.packages("magrittr")
if(!require(caret)) install.packages("caret")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table) # for fread if required
library(ggrepel) # for some plots it modifies the position of labels 
library(ggthemes) # cool ggplot themes, check it out
library(tidyr)
library(tidyverse)# The pipe sign %>% is in this package
library(magrittr) #The pipe sign %<>% and %$% are in this package
library(lubridate)#The function "as_datetime"
library(caret)#The function "createDataPartition.
#Due to the network connection problem, I downloaded the file through the browser.
#The zip file can be downloaded from the github repository.

dl<-("./ml-10m.zip") #This is the relative path for R to find the file.
                     #Make sure you have put the zip file in your current working directory.
                     #Use "getwd()"to get your current working directory.


#The codes below are provided in the lesson material.
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
  validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
temp<-validation
# The codes above are provided in the class material


#First of all let me acquaint you with the overall contents of this dataset.
##To see the first several rows.
head(edx)
##To check some basic information of the table.
summary(edx)
###The summary result shows no NAs in the dataset.

#Then,let's do some visualisation to search for some relations between the ratings and predictors.

##The distribution of the total number of ratings of each movie.
  edx %>% 
  count(movieId,name="total_ratings_movie") %>% 
  ggplot(aes(total_ratings_movie)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black",fill="yellow", show.legend = FALSE)+
                 scale_x_log10() + 
                 ggtitle("Movies total Ratings number distribution")
###The range of n is very huge so we modify the x scale into the norm of log10(x) so the image will not be too long.
###Due to the deformation of the x scale,it is not a normal distribution as the image shows. 
###Let's see the quantile of the total ratings.
  edx %>% count(movieId,name="total_ratings")%$%quantile(total_ratings,prob=seq(0,1,0.1))

  
##The mean ratings of a movie versus the times it was rated  
  
  
edx%>%group_by(movieId) %>%
summarize(total_ratings = n(),mean_rating = mean(rating))%>%
ggplot(aes(total_ratings, mean_rating)) +
geom_point() +
geom_smooth()+
scale_x_log10()+
ggtitle("ratings_vs_times")
###We could see that more ratings indicate a higher and more stable score.
###Regression should be employed to modify the movies that were quite obscure and got less ratings.
  
  
  
  
###In order to extract the release year, we will use the function "str_extract" and  design a regex for this purpose.
edx <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
temp<-temp%>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
###Use the :str_extract function two times in case some movie title contain four number in succession.
##To see the number of movies released every year, use the code below.

edx%>%group_by(releaseyear)%>%summarise(movie_number=n_distinct(movieId))%>%
  ggplot(aes(releaseyear,movie_number))+geom_point()+geom_smooth()+ggtitle("Movie released every year")

###We can see a sudden increase in movie number in around 1980s.


##The distribution of the total number of ratings of each user.
edx %>% 
  count(userId,name="total_ratings_user") %>% 
  ggplot(aes(total_ratings_user)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black",fill="yellow", show.legend = FALSE)+
  scale_x_log10() + 
  ggtitle("Users total Ratings number distribution")
###It is obvious that most users rated movies less than 1000
###To see the quantile of the total ratings of every user.
edx %>% count(userId,name="total_ratings")%$%quantile(total_ratings,prob=seq(0,1,0.1))
###According to the result we know that 90% users rated less than 301 movies.


##The mean ratings of a user gave to movies


edx%>%group_by(userId)%>%
  summarise(total_ratings=n(),mean_rating=mean(rating))%>%
  ggplot(aes(total_ratings,mean_rating))+
  geom_point()+
  scale_x_log10()+
  geom_smooth()+
  ggtitle("ratings_vs_uesers")
###The condition is quite similar with the total number a movie was rated.
###Users who made more ratings are much more stable in their mean ratings given to movies.



##To see the distribution of the total number of ratings of movies released in every year.
edx %>% 
  count(releaseyear,name="total_ratings_release_year") %>% 
  ggplot(aes(releaseyear,total_ratings_release_year)) + 
  geom_point()+
  geom_smooth()
  ggtitle("Ratings number distribution for movies released in every year")

##To see the mean ratings of movies released in different year.  
  
  
  edx%>%group_by(releaseyear)%>%
    summarise(mean_rating=mean(rating))%>%
    ggplot(aes(releaseyear,mean_rating))+
    geom_point()+
    geom_smooth()+
    ggtitle("ratings_vs_releaseyear")    
###Obviously there is some relation between the mean ratings of a movie and the year it was released.
    
###Compare this with the the movie released every year.More movies were released after 1990s,
###But the rating numbers conversely was decreasing.
  
  
##To see the relation between the ratings and movie genre.
  
  
###Most movies belong to more than just one genre and makes it difficult to 
###split them into different types. So we will not use it for the prediction. 
###To see first 10 movie's genre and all genres 
edx$genres[1:10]
n_distinct(edx$genres)
###It is quite complicated.


##The last one factor is the time of users give their ratings.


###In the dataset the time was stored as seconds it happened after the standard
###start time which is  January 1,1970.We can transform seconds into weeks to
###diminish the time point.
edx%<>%mutate(date = as_datetime(timestamp))
edx%>%mutate(date=round_date(date, unit = "week"))%>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()+ggtitle("Comment time vs ratings")
###The connection between the time and ratings is not strong.



##To sum up,we will use three predictors which are movie ID,user Id and the release year.


#To build the prediction model


##We assume that all the movies has the same basic ratings and is influenced by the chosen predictors.
##And the ratings are given by the formula ratings=basic ratings + bias of movie ID +bias of user ID+
##bias of release year

##For the estimate of the model,we build the RMSE to compute the distance between the estimated ratings 
##and the true ratings

##To compute basic ratings of all movies
mu<-mean(edx$rating)
###Now we can check the RMSE for the first time to see our precision. 
###Define the function RMSE
RMSE<-function(pred_value,true_value)
{sqrt(mean((pred_value-true_value)^2))
}
RMSE(mu,edx$rating)


##Then we will generate the bias of movie ID with regularization


###First try to narrow the scope of alpha
alpha<-seq(0,100,10)
bias_pre<-edx%>%group_by(movieId)%>%summarise(sum=sum(rating-mu),n=n())
Regular<- sapply(alpha,function(alpha)
  {
  bias<- bias_pre%>%left_join(edx,.,by = "movieId")%>%
  mutate(bias=sum/(n+alpha))%>%
  mutate(pred=mu+bias)
  return(RMSE(bias$pred,edx$rating))
  })
alpha[which.min(Regular)]
min(Regular)
###The we change the range of alpha get more precise result
alpha<-seq(0,10,1)
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(edx,.,by = "movieId")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=mu+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Narrow the range of alpha smaller
alpha<-seq(0,1,0.1)
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(edx,.,by = "movieId")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=mu+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Now we see that alpha=0 is the best.
###So the bias_movie is certain now
alpha_movie<-0
bias_reg<-edx%>%group_by(movieId)%>%summarise(bias_movie=sum(rating-mu)/(n()+alpha_movie))%>%
  left_join(edx,.,by="movieId")%>%mutate(pred=mu+bias_movie)
###To see the first several lines and the RMSE
head(bias_reg)
RMSE(bias_reg$pred,edx$rating)


##On the basis of the bias if movie Id and in similar way we find the bias_user


alpha<-seq(0,100,10)
bias_pre<-bias_reg%>%group_by(userId)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "userId")%>%
    mutate(bias=sum/(n+alpha))%>%mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###We see the best alpha is 0,then continue to narrow the scope of alpha
alpha<-seq(0,10,1)
bias_pre<-bias_reg%>%group_by(userId)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "userId")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Now we see the alpha is still  0
alpha<-seq(0,1,0.1)
bias_pre<-bias_reg%>%group_by(userId)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "userId")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Now alpha equals to 0,then we can get the bias of users
alpha_user<-0
bias_reg<-bias_reg%>%group_by(userId)%>%summarise(bias_user=sum(rating-pred)/(n()+alpha_user))%>%
  left_join(bias_reg,.,by="userId")%>%mutate(pred=pred+bias_user)
head(bias_reg)
RMSE(bias_reg$pred,bias_reg$rating)
###Now we have quite made a progress in reducing the RMSE


##Now it is the last one factor "releaseyear"
alpha<-seq(0,100,10)
bias_pre<-bias_reg%>%group_by(releaseyear)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "releaseyear")%>%
    mutate(bias=sum/(n+alpha))%>%mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###We see the best alpha is 0,then continue to narrow the scope of alpha
alpha<-seq(0,10,1)
bias_pre<-bias_reg%>%group_by(releaseyear)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "releaseyear")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Now we see the alpha is still  0
alpha<-seq(0,1,0.1)
bias_pre<-bias_reg%>%group_by(releaseyear)%>%summarise(sum=sum(rating-pred),n=n())
Regular<- sapply(alpha,function(alpha)
{
  bias<- bias_pre%>%left_join(bias_reg,.,by = "releaseyear")%>%
    mutate(bias=sum/(n+alpha))%>%
    mutate(pred=pred+bias)
  return(RMSE(bias$pred,edx$rating))
})
alpha[which.min(Regular)]
min(Regular)
###Now alpha equals to 0,then we can get the bias of releaseyear
alpha_releaseyear<-0

bias_reg<-bias_reg%>%group_by(releaseyear)%>%summarise(bias_releaseyear=sum(rating-pred)/(n()+alpha_releaseyear))%>%
  left_join(bias_reg,.,by="releaseyear")%>%mutate(pred=pred+bias_releaseyear)
head(bias_reg)
RMSE(bias_reg$pred,bias_reg$rating)
###Now we get a better RMSE.

##To test our model on the validation dataset temp.


##We compute the prediction of ratings with the formula:pred=mu+bias_movie+bias_user+bias_releaseyear
###First We compute mu+bias_movie
pred_temp_1<- bias_reg%>%group_by(movieId)%>%summarise(bias_movie=mean(bias_movie))%>%
            left_join(temp,.,by = "movieId")%>%mutate(pred=mu+bias_movie)
head(pred_temp_1)
RMSE(pred_temp_1$pred,temp$rating)
###Then we include the bias_user
pred_temp_2<-bias_reg%>%group_by(userId)%>%summarise(bias_user=mean(bias_user))%>%
            left_join(pred_temp_1,.,by = "userId")%>%mutate(pred=pred+bias_user)
RMSE(pred_temp_2$pred,temp$rating)
###At last it is the releaseyear
pred_temp_3<-bias_reg%>%group_by(releaseyear)%>%summarise(bias_releaseyear=mean(bias_releaseyear))%>%
            left_join(pred_temp_2,.,by = "releaseyear")%>%mutate(pred=pred+bias_releaseyear)
RMSE(pred_temp_3$pred,temp$rating)
range(pred_temp_3$pred)
###We see some unnormal values that are smaller than 0 or larger than 5
###We convert number smaller than 0 to 0 and that larger than 5 to 5.
pred_temp<-pred_temp_3%>%mutate(pred=ifelse(pred<0,0,ifelse(pred>5,5,pred)))
###Now we can check the final RMSE of the model.
RMSE(pred_temp$pred,temp$rating)
###The result is quite satisfying and we find a acceptable estimation of the ratings.
###The accuracy of prediction and guessing with the mean rating.
mean(floor(pred_temp$pred)+ifelse(mod(pred_temp$pred,1)<0.25,0,ifelse(mod(pred_temp$pred,1)<0.75,0.5,1))==temp$rating)
mean(floor(mu)+ifelse(mod(mu,1)<0.25,0,ifelse(mod(pred_temp$pred,1)<0.75,0.5,1))==temp$rating)






























