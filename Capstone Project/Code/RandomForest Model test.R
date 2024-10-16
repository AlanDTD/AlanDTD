#Testing RandomForest Model

library(ggplot2)
library(randomForest)

wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), header = TRUE, sep = ";") # This command is used to load the dataset

head(wine)


barplot(table(wine$quality))

wine$taste <- ifelse(wine$quality < 5, "bad", "good")
wine$taste[wine$quality == 5] <- "normal"
wine$taste[wine$quality == 6] <- "normal"
wine$taste <- as.factor(wine$taste)
str(wine$taste)
wine


set.seed(123)
samp <- sample(nrow(wine), 0.8 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]



ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=taste),color='black',bins=50) 



#Model 
model <- randomForest(taste ~ . - quality, data = train, ntree = 1000, mtry = 5)
model$confusion



# The next step is to validate our model using the test data

prediction <- predict(model, newdata = test)
table(prediction, test$taste)
prediction

# Now, let’s display the predicted vs. the actual values


results<-cbind(prediction,test$taste)
results
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
View(results)


# Finally, let’s calculate the accuracy of the model
sum(prediction==test$taste) / nrow(test) # The output is as shown below


