

install.packages('memisc')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages("RColorBrewer")
install.packages("psych")


library(gridExtra)
library(grid)
library(dplyr)
library(GGally)
library(scales)
library(ggpubr)
library(tidyverse)
library(readxl)
library(factoextra)
library(ggplot2)
library(corrplot)
library(factoextra)
library(tidyr)
library(nnet)
library(rpart)
library(janitor)
library(here)

#Uploading the data
red_wine<-read.csv(here("data/winequality-red.csv"))
red_wine <- as_tibble(red_wine)
red_wine

#cleaning the data set names 
  clean_names(red_wine)

  # List red_wine dataframe's column names, types and a subset of values
  
  str(red_wine)
  
  # Find dimensions of red_wine data frame
  dim(red_wine)
  

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
red_wine$quality <- factor(red_wine$quality, ordered = T)

# Check how many missing values are in each column/variable
#and sum them up per column

colSums(is.na(red_wine))
#we can see that their is no missing values 

#display only the variables that has a 10 or greater of alcohol 
alcohol10 <- red_wine[red_wine$alcohol >= 10, ]
alcohol10

#From the 1599 rows it shows that 919 rows are 10 or greater 

#Order the data frame by the pH 

P_h_ascend <- red_wine[order(red_wine$pH), ]
P_h_ascend

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
#Drawing a histogram to take a look at the each variable distribution 


#and we can see that only 2 variables that has a normal distribution (density,pH)
# the rest of variables are more or less right skewed ,
#and the quality variable has a semi-normal discrete distribution


# Distribution of fixed.acidity variable.

#it shows that there is one peak at 7 in the histogram plot.and it has outliers

summary(red_wine$fixed.acidity)

fixed_acidity_h<-ggplot(aes(x=fixed.acidity,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()
fixed_acidity_h

# Distribution of citric acid variable
#it shows that there are two peaks at 0 and 0.49 in the histogram plot.and it has outliers
#we can see that citric.acid has an outlier 
summary(red_wine$citric.acid)

citric_acid_h <-ggplot(aes(x=citric.acid,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()


# Distribution of volatile.acidity
#it shows that there are two peaks at 0.6 and 0.4 in the histogram plot.and it has outliers
summary(red_wine$volatile.acidity)

volatile_acidity_h <-ggplot(aes(x=volatile.acidity,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()
 


#Distribution of residual.sugar
#it shows one peak at 2. and it has an outliers 
summary(red_wine$residual.sugar)

residual_sugar_h <-ggplot(aes(x=residual.sugar,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()


# Distribution of chlorides
#it shows one peak at 0.08. and it has an outliers 
summary(red_wine$chlorides)

chlorides_h <-ggplot(aes(x=chlorides,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()
 


# Distribution of free.sulfur.dioxide
#it shows one peak at 5. and it has an outliers and it shows a normal distribution
summary(red_wine$free.sulfur.dioxide)

free_sulfur_dioxide_h <-ggplot(aes(x=free.sulfur.dioxide,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()
 


# Distribution of density
#it shows one peak at #it shows one peak at 0.99. and it has an outliers 
#The summary results shows that the mean and median values
#are pretty close which means we have a normal distribution .
summary(red_wine$density)

density_h<-ggplot(aes(x=density,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()


# Distribution of total.sulfur.dioxide
#it shows that it has a peak value around 20. and it has an outliers 
summary(red_wine$total.sulfur.dioxide)

total_sulfur_dioxide_h<-ggplot(aes(x=total.sulfur.dioxide,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()



# Distribution of pH
#The summary results shows that the mean and median values 
#are pretty close which means we have a normal distribution .
summary(red_wine$pH)

pH_h <-ggplot(aes(x=pH,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()


# Distribution of sulphates 
#it shows that it has a peak value around 0.6.and it has an outliers 
summary(red_wine$sulphates)

sulphates_h<-ggplot(aes(x=sulphates,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()

sulphates_h

# Distribution of alcohol
#it shows that it has a peak value around 9.4. and it has an outliers 
summary(red_wine$alcohol)

alcohol_h<-ggplot(aes(x=alcohol,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram()

# Distribution of quality
#it shows that there are two peaks at 5 and 6 in the histogram plot.
summary(red_wine$quality)

quality_h<-ggplot(aes(x=quality,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram(stat="count")

# Distribution of rating
#the distribution shows that most of the wines falls under the average wine quality. 
summary(red_wine$rating)

rating_h<-ggplot(aes(x=rating,color = I('black'),fill=I('lightblue')), data = red_wine ) +
  geom_histogram( stat = "count")
rating_h

grid.arrange(fixed_acidity_h,volatile_acidity_h,citric_acid_h,residual_sugar_h,
             chlorides_h,free_sulfur_dioxide_h,total_sulfur_dioxide_h,
             density_h,pH_h,alcohol_h,ncol=2 ,bottom = "Variables Distributions ")


#--good
ggplot(data = red_wine, aes(x = rating, y = pH, fill = rating)) +
  geom_bar(stat = "identity")


###fixed acidity

ggplot(data = red_wine,
       aes(y = fixed.acidity, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(4,16)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#sulphates
ggplot(data = red_wine,
       aes(y = sulphates, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#citric acid
ggplot(data = red_wine,
       aes(y = citric.acid, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0,3)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
#volatile acidity
ggplot(data = red_wine,
       aes(y = volatile.acidity, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0,2)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#residual sugar
ggplot(data = red_wine,
       aes(y = residual.sugar, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0,16)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#chlorides
ggplot(data = red_wine,
       aes(y = chlorides, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0,1)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#total sulfur dioxide
total_sulfur_dioxide_alco <-ggplot(data = red_wine,
       aes(y = total.sulfur.dioxide, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(5,300)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
                     total_sulfur_dioxide_alco

#free sulfur dioxide
free_sulfur_dioxide_alco <- ggplot(data = red_wine,
       aes(y = free.sulfur.dioxide, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0,75)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
                     free_sulfur_dioxide_alco

#density
ggplot(data = red_wine,
       aes(y = density, x = alcohol,
           color = rating)) +
  geom_point(alpha = 0.98, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(0.98,1.01)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))

#pH
ph_alco <- ggplot(data = red_wine,aes(y = pH, x = alcohol,
 color = rating)) +
  geom_point(alpha = 0.8, size = 1) +
  geom_smooth(method = "lm", se = FALSE,size=1)  +
  scale_y_continuous(limits=c(2.6,4.05)) +
  facet_wrap(~rating) +
  scale_color_brewer(type='seq',
                     guide=guide_legend(title='Quality'))
ph_alco
