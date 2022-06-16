
#load libraries 

install.packages("here")
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(reshape2)
library(GGally)
library(scales)
library(ggpubr)
library(tidyverse)
library(readxl)
install.packages("janitor")
install.packages("FactoMineR")
install.packages("factoextra")
library(factoextra)
library(dplyr)
library(ggplot2)
library(MASS)
library(corrplot)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(nnet)
library(rpart)

#Uploading the data
red_wine <- read_csv(here('data/winequality-red-kaggle.csv'))
red_wine<-read.csv("data/winequality-red.csv")
red_wine <- as_tibble(red_wine)
red_wine

# Find dimensions of red_wine dataframe
dim(red_wine)

# List red_wine dataframe's column names, types and a subset of values

str(red_wine)

#we can see that all the variables are numerical
#except the quality which is integer

#Creating a new Variable called 'Rating'
#A wine is categorized into Bad,when its value is 4 or less. 
#A wine is categorized into Average,when its value falls in the range of 5 to 7.
#A wine is categorized into good,when its value is  8 and above. 

red_wine$rating <- ifelse(red_wine$quality < 5, 'bad', ifelse(
  red_wine$quality < 7, 'average', 'good'))
red_wine$rating <- ordered(red_wine$rating,
                           levels = c('bad', 'average', 'good'))

# Display summary statistics for each variable

summary(red_wine)

#Transforming quality from an Integer to a Factor
red_wine$quality <- factor(wine$quality, ordered = T)

# Check how many missing values are in each column/variable
#and sum them up per column

colSums(is.na(red_wine))
#we can see that their is no missing values 

#Drawing a histogram to take a look at the each variable distribution 

draw_hist <- function(dataframe, variable)
{
  # Save histogram definition to the plot variable
  plot <- ggplot(data = dataframe, aes(x = variable)) + 
    geom_histogram(color = 'black', fill = '#099DD9') +
    xlab(deparse(substitute(variable)))
  return(plot)
}
grid.arrange(draw_hist(red_wine, red_wine$fixed.acidity),
             draw_hist(red_wine, red_wine$volatile.acidity),
             draw_hist(red_wine, red_wine$citric.acid),
             draw_hist(red_wine, red_wine$residual.sugar),
             draw_hist(red_wine, red_wine$chlorides),
             draw_hist(red_wine, red_wine$free.sulfur.dioxide),
             draw_hist(red_wine, red_wine$total.sulfur.dioxide),
             draw_hist(red_wine, red_wine$density),
             draw_hist(red_wine, red_wine$pH),
             draw_hist(red_wine, red_wine$sulphates),
             draw_hist(red_wine, red_wine$alcohol),
             draw_hist(red_wine, red_wine$quality),
             ncol = 3)
#and we can see that only 2 variables that has a normal distribution (density,pH)
# the rest of variables are more or less right skewed ,
#and the quality variable has a semi-normal discrete distribution



#select values based on there rating 
#bad rating 
red_wine %>%
  filter(rating == 'bad')
#average rating
red_wine %>%
  filter(rating == 'average')
#good rating 
red_wine %>%
  filter(rating == 'good')
#average rating with a 9 or more alcohol 
red_wine %>%
  filter(rating == 'average' & alcohol >= 9)  

# Distribution of fixed.acidity variable.
#we had to change the visulization to a better representation of the data 
#it shows that there is one peak at 7 in the histogram plot.and it has outliers
#we can see that citric.acid has an outlier 
summary(red_wine$fixed.acidity)

p1 <-ggplot(aes(x=fixed.acidity), data = red_wine ) +
  geom_bar()

p2 <-ggplot(aes(x=fixed.acidity,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()

grid.arrange(p1,p2,ncol=1)

# Distribution of citric.acid variable.
#it shows that there are two peaks at 0 and 0.49 in the histogram plot.and it has outliers
#we can see that citric.acid has an outlier 
summary(red_wine$citric.acid)

p1 <-ggplot(aes(x=(citric.acid)), data = red_wine ) +
  geom_bar()

p2 <-ggplot(aes(x=citric.acid,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()

grid.arrange(p1,p2,ncol=2)

# Distribution of volatile.acidity
#it shows that there are two peaks at 0.6 and 0.4 in the histogram plot.and it has outliers
p1 <-ggplot(aes(x=log(volatile.acidity)), data = red_wine ) +
  geom_bar()
p2 <-ggplot(aes(x=volatile.acidity,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$volatile.acidity)

#Distribution of residual.sugar
#it shows one peak at 2. and it has an outliers 
p1 <-ggplot(aes(x=residual.sugar), data = red_wine ) +
  geom_bar()
p2 <-ggplot(aes(x=residual.sugar,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$residual.sugar)

# Distribution of chlorides
#it shows one peak at 0.08. and it has an outliers 
p1 <-ggplot(aes(x=chlorides), data = red_wine ) +
  geom_bar()
p2 <-ggplot(aes(x=chlorides,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$chlorides)

# Distribution of free.sulfur.dioxide
#it shows one peak at 5. and it has an outliers and it shows a normal distribution

p1 <-ggplot(aes(x=free.sulfur.dioxide), data = red_wine ) +
  geom_bar()
p2 <-ggplot(aes(x=free.sulfur.dioxide,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$free.sulfur.dioxide)

# Distribution of density
#it shows one peak at #it shows one peak at 0.99. and it has an outliers 
#The summary results shows that the mean and median values
#are pretty close which means we have a normal distribution .

p1 <-ggplot(aes(x=density,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
p2 <-ggplot(aes(x=density), data = red_wine ) +
  geom_bar()

grid.arrange(p1,p2,ncol=1)
summary(red_wine$density)


# Distribution of total.sulfur.dioxide
#it shows that it has a peak value around 20. and it has an outliers 

p1<-ggplot(aes(x=total.sulfur.dioxide,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
p2<-ggplot(aes(x=total.sulfur.dioxide), data = red_wine ) +
  geom_bar()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$total.sulfur.dioxide)

# Distribution of pH
#The summary results shows that the mean and median values 
#are pretty close which means we have a normal distribution .

p1 <-ggplot(aes(x=pH,color = I('black'),fill=I('black')), data = red_wine ) +
  geom_bar()
p2 <-ggplot(aes(x=pH,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$pH)

# Distribution of sulphates 
#it shows that it has a peak value around 0.6.and it has an outliers 

p1 <-ggplot(aes(x=sulphates), data = red_wine ) +
  geom_bar()
p2<-ggplot(aes(x=sulphates,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$sulphates)

# Distribution of alcohol
#it shows that it has a peak value around 9.4. and it has an outliers 
p1<-ggplot(aes(x=alcohol,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram()
p2 <-ggplot(aes(x=alcohol), data = red_wine ) +
  geom_bar()
grid.arrange(p1,p2,ncol=1)
summary(red_wine$alcohol)

# Distribution of quality
#it shows that there are two peaks at 5 and 6 in the histogram plot.

ggplot(aes(x=quality,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram(stat="count")

summary(red_wine$quality)

# Distribution of rating
#the distribution shows that most of the wines falls under the average wine quality. 

ggplot(aes(x=rating,color = I('black'),fill=I('#099DD9')), data = red_wine ) +
  geom_histogram( stat = "count")

summary(red_wine$rating)

# Distribution of free.sulfur.dioxide vs rating
#we can see that 
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}
correlations <- c(
  simple_cor_test(red_wine$fixed.acidity, red_wine$quality),
  simple_cor_test(red_wine$volatile.acidity, red_wine$quality),
  simple_cor_test(red_wine$citric.acid, red_wine$quality),
  simple_cor_test(red_wine$residual.sugar, red_wine$quality),
  simple_cor_test(red_wine$chlorides, red_wine$quality),
  simple_cor_test(red_wine$free.sulfur.dioxide, red_wine$quality),
  simple_cor_test(red_wine$total.sulfur.dioxide, red_wine$quality),
  simple_cor_test(red_wine$density, red_wine$quality),
  simple_cor_test(red_wine$pH, red_wine$quality),
  simple_cor_test(red_wine$sulphates, red_wine$quality),
  simple_cor_test(red_wine$alcohol, red_wine$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'residual.sugar',
                         'chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'sulphates', 'alcohol')
correlations



