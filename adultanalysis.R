summary(adult)

str(adult, vec.len = 2, strict.width = "no", width = 30)
table(adult$workclass)
#### DATA CLEANING ####

# combining data
adult$workclass <- as.character(adult$workclass)
adult$workclass[adult$workclass == "Without-pay"|
                  adult$workclass == "Never-worked"] <- "Unemployed"
adult$workclass[adult$workclass == "State-gov" |
                  adult$workclass == "Local-gov"] <- "SL-gov"
adult$workclass[adult$workclass == "Self-emp-inc" |
                  adult$workclass == "Self-emp-not-inc"] <- "Self-employed"
table(adult$workclass)
table(adult$`marital-status`)
adult$`marital-status`<- as.character(adult$`marital-status`)
adult$`marital-status`[adult$`marital-status` == "Married-AF-spouse" |
                       adult$`marital-status` == "Married-civ-spouse" |
                       adult$`marital-status` == "Married-spouse-absent"] <- "Married"

adult$`marital-status`[adult$`marital-status` == "Divorced" |
                       adult$`marital-status` == "Separated" |
                       adult$`marital-status` == "Widowed"] <- "Not-Married"
table(adult$`marital-status`)

library(data.table)
setnames(adult, "native-country", "region")

adult$region <- as.character(adult$region)
table(adult$region)
north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")

adult$region[adult$region %in% north.america] <- "North America"
adult$region[adult$region %in% asia] <- "Asia"
adult$region[adult$region %in% south.america] <- "South America"
adult$region[adult$region %in% europe] <- "Europe"
adult$region[adult$region %in% other] <- "Other"

table(adult$region)


# turning variables back to factors
adult$region<- as.factor(adult$region)
adult$`marital-status`<- as.factor(adult$`marital-status`)
adult$workclass <- as.factor(adult$workclass)

# removing NAs from dataset 
library(Amelia)
missmap(adult, y.at = 1, y.labels = "", col = c("yellow", "black"), legend = FALSE)
adult[adult== "?"]<-NA
table(adult$`native-country`)

sum(is.na(adult))
adult<-na.omit(adult)
table(adult$region)

# HOURSXWEEK
library(tidyverse)
ggplot(adult, aes(`hours-per-week`)) + geom_histogram()
# box plot: mean number of working hours x week is 41 
# marked as the red dot on the box plot, and at least 
# 50% of the people taking part in the survey, 
# work between 40 and 45 hours x week:
ggplot(aes(x = factor(0), y = `hours-per-week`),
       data = adult) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  xlab(label = "") +
  ylab(label = "working hours x week") +
  ggtitle("Box Plot of working hours x week") 
# since there are a lot of outliers we will group
# the working hours in 5 categories and we will
# create a new factor variable with 5 levels 
# corresponding to these 5 categories:

adult$hours_w[adult$`hours-per-week` < 40] <- " less_than_40"
adult$hours_w[adult$`hours-per-week` >= 40 & 
                   adult$`hours-per-week` <= 45] <- " between_40_and_45"
adult$hours_w[adult$`hours-per-week` > 45 &
                   adult$`hours-per-week` <= 60  ] <- " between_45_and_60"
adult$hours_w[adult$`hours-per-week` > 60 &
                   adult$`hours-per-week` <= 80  ] <- " between_60_and_80"
adult$hours_w[adult$`hours-per-week` > 80] <- " more_than_80"


# We want to make the new variable ???hours_w??? a factor variable, therefore:


adult$hours_w <- factor(adult$hours_w,
                           ordered = FALSE,
                           levels = c(" less_than_40", 
                                      " between_40_and_45", 
                                      " between_45_and_60",
                                      " between_60_and_80",
                                      " more_than_80"))



summary(adult$hours_w)


# CAPITALGAIN
summary(adult$`capital-gain`)
summary(adult$`capital-loss`)
# notice that 91,6% is the percentage of zeros in "capital-gain"
# (too big):
(nrow(subset(adult, adult$`capital-gain`==0))/nrow(adult))*100
# notice that 95,3% is the percentage of zeros in "capital-loss"
# (too big):
(nrow(subset(adult, adult$`capital-loss`==0))/nrow(adult))*100
# compute the mean:
mean.gain <- mean(adult$`capital-gain`)
mean.gain # = 1114,333
mean.loss <- mean(adult$`capital-loss`)
mean.loss # = 88,729

# We generate new variables "cap_gain" and "cap_loss"
# by grouping the existing variables ???capital_gain???
# and ???capital_loss??? into three categories.
# We do this because there are too many zeros in the variables
# ???capital_gain??? and ???capital_loss??? and this can seriously
# disrupt the analysis.
# Display the quantiles of the nonzero "capital_gain" and "capital_loss":
q.gain <- quantile(x = subset(adult$`capital-gain`, adult$`capital-gain` > 0), 
                   probs = seq(0, 1, 0.25))

q.loss <- quantile(x = subset(adult$`capital-loss`, adult$`capital-loss` > 0),
                   probs = seq(0, 1, 0.25))

library(knitr)
kable(x = data.frame(Capital_Gain = q.gain, Capital_Loss = q.loss),
      caption = "Quantiles of the Nonzero Capital")

df <- adult[adult$`capital-gain` > 0, ]

ggplot(data = df, 
       aes(x = df$`capital-gain`)) +
  geom_histogram(binwidth = 5000,
                 colour = "black",
                 fill = "darkgoldenrod1",
                 alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 4000, 100)) +
  labs(x = "Capital gain", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Gain") 

# The histogram confirms once more what we have
# already established, namely, that the majority
# of people with positive capital gain have a
# capital gain between 0 and 25,000 dollars,
# and there are also about 150 people with
# capital gain of around 100,000 dollars.
# We also note that the biggest number of people
# with positive capital gain are those with
# about 5,000 dollars.
# We also display the histogram for the nonzero
# capital loss:
df <- adult[adult$`capital-loss` > 0, ]

ggplot(data = df, 
       aes(x = df$`capital-loss`)) +
  geom_histogram(colour = "black",
                 fill = "tomato3",
                 alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 5000, 250)) +
  scale_y_continuous(breaks = seq(0, 450, 50)) +
  labs(x = "Capital loss", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Loss") 
# From the histogram we also see that the biggest
# number of people have a capital loss of about
# 1,875 dollars.
# we will group the values of the variables
# ???capital_loss??? and ???capital gain??? into
# categories and we will create two new factor
# variables called ???cap_gain??? and ???cap_loss???.
# We do the grouping in the following way:
adult <- mutate(adult, 
                   cap_gain = ifelse(adult$`capital-gain` < 3464, " Low",
                                     ifelse(adult$`capital-gain` >= 3464 & 
                                              adult$`capital-gain` <= 14080, " Medium", " High")))


adult$cap_gain <- factor(adult$cap_gain,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))

adult <- mutate(adult, 
                   cap_loss = ifelse(adult$`capital-loss` < 1672, " Low",
                                     ifelse(adult$`capital-loss` >= 1672 & 
                                              adult$`capital-loss` <= 1977, " Medium", " High")))


adult$cap_loss <- factor(adult$cap_loss,
                            ordered = TRUE,
                            levels = c(" Low", " Medium", " High"))

#EDUCATION

class(adult$education)   
adult$education=as.factor(adult$education)
levels(adult$education)[c(4,14)]="Preschool-4th"
 table(adult$education)              
 
 level.ed.in <- levels(adult$education)
 lg.lev.ei <- lapply(level.ed.in, function(v){
   
   ggplot(data = subset(adult, adult$education == v), 
          aes(x = subset(adult, adult$education == v)$income, 
              fill = subset(adult, adult$education == v)$income)) +
     geom_bar(aes(y = (..count..)/sum(..count..))) +
     geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                   y = percent_rank((..count..)/sum(..count..)) ), 
               stat = "count",
               vjust =  c(2, 0.5),
               size = 3) +
     labs(x = "Income", 
          y = "",
          fill = "Income") +
     ggtitle(v) +  
     theme(legend.position = 'none',
           plot.title = element_text(size = 11, face = "bold")) +    
     scale_y_continuous(labels = percent_rank) })
 
 library(gridExtra)
 grid.arrange(grobs = lg.lev.ei[1:4], ncol = 4)
 grid.arrange(grobs = lg.lev.ei[5:8], ncol = 4)
 grid.arrange(grobs = lg.lev.ei[9:12], ncol = 4)
 grid.arrange(grobs = lg.lev.ei[13:15], ncol = 4)
 
# Reorder factor levels by count
region.ordered <- reorder(adult$region, adult$region, length)
region.ordered <- factor(region.ordered, levels = rev(levels(region.ordered)))

ggplot(adult, aes(region.ordered)) + geom_bar(aes(fill = income), color = "black")

#### CLASSIFICATION ALGORITHMS ####
#### Logistic Regression ####
attach(adult)

# The purpose of this model is to classify people into two groups, 
# below 50k or above 50k in income. We will build the model using 
# training data, and then predict the salary class using the test group.

library(ISLR)
library(caTools)

split <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, split == TRUE)
test <- subset(adult, split == FALSE)


train$income <- as.factor(train$income)
log.model <- glm(income ~ .- fnlwgt - `capital-gain` - `capital-loss` - `hours-per-week`, data = train, family = "binomial")
summary(log.model)
predict <- predict(log.model, test, type = "response")
table(test$income, predict >= 0.5)

(9642+2054)/(9642+741+1373+2054)
# the mean is equal to 0.847 so the model is pretty accurate

#### COLLINEARITY ####
#find collinearity (if there's any) of predictor variables
summary(log.model)$coefficients[,1:2]

# Therefore, we have to exclude it from the list of predictor
# variables and fit the model again. We will use the R function
# ???findLinearCombos??? from the package ???caret??? to test whether
# the covariate ???education_num??? is collinear with some of
# the other predictors.
# it returns a list that enumerates these dependencies
# and a vector of column positions that can be removed to
# eliminate the linear dependencies:


library(car)
vif(log.model)
library(caret)
library(GGally)
pairs.log.model1 <- ggpairs(log.model[,1:5])
pairs.log.model


# Below we list the unique combinations of values for
# the variables ???education??? and ???education_num???.
# We do this with the help of the R function ???unique???
# from the ???base??? package. As the documentation reads,
# for a data frame the function ???unique??? returns a
# ???data frame with the same columns but with duplicate
# rows removed (and with row names from the first
# occurrences of the unique rows)???. The variable 
# ???education??? has a total of 16 factor levels and we
# see that to each level of ???education??? corresponds
# a number from 1 to 16 from the variable ???education_num???:
unique.combinations <- unique(adult[,c("education","educational-num")])
unique.combinations[order(unique.combinations$`educational-num`),]

# remove the covariate ???education_num??? and fit a new model:
log.model.wld <- glm(income ~ .- `educational-num` - fnlwgt - `capital-gain` - `capital-loss` - `hours-per-week`, data = train, family = "binomial")
summary(log.model.wld)
vif(log.model.wld)
# Therefore, we can conclude that there are no indications of collinearity
# among the explanatory variables in the model ???glm.model.wld???.


# try to use a backward stepwise selection in order to
# understand if any variable can be omitted: in the sense that we want to
# understand if the model gets better in terms of LikelihoodRatioTests, by
# removing variables one by one.
step.lrt <- step(log.model.wld, direction = "backward", test = "LRT")
5# The function is ???drop1()???. It drops one predictor 
# at a time from the full model ???glm.model.wld??? and then 
# compares the restricted and full model in terms of goodness 
# of fit with the help of the likelihood ratio test (LRT) statistic.
# As we see from the output above, the p-values indicate that 
# all predictors in the model are significant and should be retained.

# MODIFY THE DATASET IN ORDER TO HAVE A MORE BALANCED DATA
library(scales)
library(ggplot2)

ggplot(data = adult.oversampled, 
       mapping = aes(x = adult.oversampled$income, fill = adult.oversampled$income)) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..)/sum(..count..)),
                          y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Income", 
       y = "",
       fill = "Income") +
  scale_y_continuous(labels = percent)
# we notice how 75,2% of people earn less than 50K a year and 
# only 24,8% earn more than 50K a year.
# Although this statistic is not surprising, it makes the data 
# imbalanced and leads to low sensitivity of the fitted classification model.
# We recall that as the ???positive??? class we consider people with income greater 
# than 50K. Thus, the SENSITIVITY is the proportion of observations with income 
# values equal to " >50K" that are correctly identified as such, and the SPECIFICITY 
# is the proportion of records with income values equal to " <=50K" that are correctly 
# identified as such.
# We level the data by adding copies of observations from the under-represented class.
# This technique is called OVER-SAMPLING, or sampling with replacement and its goal is z2
# to build better classification models with higher sensitivity.

# First we create a temporary dataset called adult.rich which contains all 
# observations with income greater than 50K a year:
adult$income <- as.factor(adult$income)
adult.rich <- subset(adult, adult$income == ">50K")
rownames(adult.rich) <- 1:nrow(adult.rich)
# Then we use the function createDataPartition() from the caret package
# to randomly sample 80% of the these observations and we create the dataset
# adult.oversampled which consists of the original observations from the
# adult set and the over-sampled instances:
  
IndRich <- createDataPartition(y = adult.rich$income,
                               times = 1,
                               p = 0.8,
                               list = FALSE)
adult.oversampled <- rbind(adult, adult.rich[IndRich, ])
rownames(adult.oversampled) <- 1:nrow(adult.oversampled)
# then we remove the temporary dataset "adult.rich"
rm(adult.rich) 

summary(adult.oversampled$income) 
# income <=50K:34611
#         >50K:20560
summary(adult$income)
# income <=50K:34611
#         >50K:11422

# After we enriched the ???adult??? dataset with observations
# over-sampled from the under-represented class of people
# who earn more than 50K a year, we fit a new logistic
# regression model called ???log.model.over":
log.model.over <- glm(income ~ .- `educational-num` - fnlwgt - `capital-gain` - `capital-loss` - `hours-per-week`, data = adult.oversampled, family = "binomial")
summary(log.model.over)
# First we test the performance of the model on the
# training set ??? namely, the ???adult.oversampled??? dataset:
library(lattice)
library(caret)
predicted.train <- predict(log.model.over, newdata = adult.oversampled, type = "response")
predicted.train <- ifelse(predicted.train > 0.5, " >50K", " <=50K")
predicted.train <- as.factor(predicted.train)
stat.Logi.over <- confusionMatrix(adult.oversampled$income, data = predicted.train, positive = levels(test$income)[2])
# We observe that the overall accuracy drops a little from 84.89% (for the model trained on the original
# unbalanced dataset) to 82.17%, whereas the sensitivity significantly increases from 60.34% to 73.93%.
# The specificity decreases with 5.95% from 93.03% to 87.08%.

levels(predicted.train)
levels(adult.oversampled$income)
class(adult.oversampled$income)
predicted.test <- predict(log.model.over, newdata = test, type = "response")
predicted.test <- ifelse(predicted.test > 0.5, " >50K", " <=50K")
stat.Logi.over <- confusionMatrix(data = predicted.test, reference= test$income, positive= levels(test$income)[2])
# We see that the accuracy on the test set drops down a little bit from 84.75% for the model built on the
# original unbalanced train set to 83.53% for the model built on the over-sampled train dataset.
# On the other hand, the sensitivity significantly increases from 59.57% to 73.27% and the specificity
# decreases from 92.96% to 86.88%.



