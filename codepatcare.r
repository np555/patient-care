# Read data
quality = read.csv("quality.csv")

# Inspect data
str(quality)
summary(quality)

table(quality$PoorCare)
# Baseline accuracy
98/131

# install and load packages
install.packages("caTools")
library(caTools)

# Randomly split data
set.seed(100)
split <- sort(sample(nrow(hr), nrow(hr)*.7))
split

# Creating training and testing datasets
qtrain = quality[split,]
qtest = quality[-split,]

# Building Logistic Regression Model
qmod = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(qmod)

# Make predictions on training set
predtrain = predict(qmod, type="response")

# Analyze predictions
summary(predtrain)
tapply(predtrain, qtrain$PoorCare, mean)


# Confusion matrix for threshold of 0.5
table(qtrain$PoorCare, predtrain > 0.5)

# Sensitivity and specificity
10/25
70/74

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predtrain > 0.7)

# Sensitivity and specificity
8/25
73/74

# Confusion matrix for threshold of 0.2
table(qtrain$PoorCare, predtrain > 0.2)

# Sensitivity and specificity
16/25
54/74


# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(predtrain, qtrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Prediction on Test Set
predictest = predict(QualityLog, type = "response", newdata = qtest)
table(qtest$PoorCare,predictTest >= 0.3)
