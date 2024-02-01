# Load csvs
train = read.csv("./train.csv")
test = read.csv("./test.csv")

# Age has missing values so we substitute them using the average age
train$Age[is.na(train$Age)] = mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age, na.rm = TRUE)

# Remove variables that wont help with analysis (id, nname, ticket number, cabin and embarked port)
train = train[c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare")]
str(train)

# Build a logistic regression model
model = glm(Survived~., family = binomial, data = train)

# Check for multicolinearity
library(car)
vif(model) # All values close to 1 so no multicolinearity

# Refine the model
summary(model) # Check all variables are significant

# We can see that parch and fare are both likely to be chance, so we remove them
model = glm(Survived~. - Parch - Fare, family = binomial, data = train)
summary(model)

# Fit the model to the test data to produce a submission
prediction = predict(model, type = "response", newdata = test)

test$Survived = as.numeric(prediction >= 0.5) # Fill in the datatframe with our guesses

# Produce the final submission
submission = data.frame(test[c("PassengerId","Survived")])
write.table(submission, "submission.csv", row.names=FALSE, sep=",")
