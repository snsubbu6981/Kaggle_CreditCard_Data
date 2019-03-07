### Objective: Apply both KNN and K-Means on credit card fraud dataset and see if they are able to pick up 
##             fraud instances


## Read fraud dataset
train.data = read.csv("C:\\Users\\snarayanaswamy\\Downloads\\creditcard.csv", header = TRUE)
variable.names(train.data) ##lists out all variable names
summary(train.data) ## Min, Max and quartiles
head(train.data) ## prints first 6 observations in no order
dim(train.data) ## dimensions of dataset rows, columns
str(train.data1) ## variable types and sample values

library(dplyr)
train.data1 = select(train.data, -Class, Amount) ## Removing Class variable from train.data

## Dont convert Class as a factor as K-Means requires all variables to be of numeric type
train.data$Class = as.factor(train.data$Class) ##converting integer variable into a class/factor
table = table(train.data$Class) ##checking frequencies of factor variable
prop.table(table)


## Running K-Means
## Qn1: Can fraud_flag be a class/factor variable? Yes but K-means can handle only numeric data

## Qn2: Can we pass variables of different string types into K-Means algorithm? NO
        ## data set should be a numeric vector or a data frame with all numeric columns

## Qn3: Does correlation among variables impact clustering?

## Qn4: How do you know there is an outlier present? By calculating distance of individual record from respective centroid

## Qn5: How do I score individual transactions real time using K-Means?
  ## Once the model is built and loaded into decsioning system, an incoming data instance just has to be transformed 
  ## into its corresponding feature vector. This vector is fed into the clustering model, where a cluster assignment 
  ## and a distance calculation to the center is made. 
  ## Now you only need to compare this distance with the pre-assigned threshold.
  ## Pre-assigned threhold is based on historical fraud tag
  ## For instance, if a segment has very high outliers/fraud tag, we need to set distance accordingly

## Qn6: Should I standardize all variables prior to running K-means? YES

## Qn7: Does standardizing levels of binary variable make sense for K-means? VERY SUBJECTIVE QUESTION!!

## Qn8: How many variables can I use in K-means?

## Qn9: What kind of variables have I used in the K-means clustering?
  ## Utilization
  ## Email score
  ## Risk score from underwriting fraud model
  ## IP address distance
  ## 

## Objective function: K-means aims to partition the points into k groups such that the sum of squares from 
                       ## points to the assigned cluster centers is minimized

## How to determine anomalies?
## 1. Determine a good vs. bad clusters (based on historical fraud tag)
## 2. Density of clusters, dense and big clusters could be good vs. small/sparse clusters are bad
## 3. Calculate a global distance threshold - calculate distance between a new transaction point & it's center

library(fpc) ##install this library to run plotcluster function
fit2=kmeans(train.data1, centers = 50, iter.max=10000, nstart = 1, algorithm = c("Lloyd"), trace = FALSE) 
table(fit2$cluster)
fit2
plotcluster(train.data, fit2$cluster) 
fit2$centers ##Matrix of cluster centers - center for each cluster by variable
fit2$cluster

##calculating (distance)2 from each datapoint and it's center
distances = sqrt(rowSums((train.data1 - fit2$centers)^2)) 
outliers = order(distances, decreasing=T)[1:50]
print(outliers)
print(train.data[outliers,])

## calculating optimal number of clusters based on within groups sum of squares
wss=(nrow(train.data1) - 1) * sum(apply(train.data1, 2, var))
for (i in 2:12) wss[i]=kmeans(train.data1, centers = i)$tot.withinss
plot(1:12, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
wss

?kmeans
