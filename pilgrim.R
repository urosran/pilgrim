df <- readWorksheetFromFile("data.xls", 
                            sheet=1, 
                            startRow = 4,
                            endCol = 2)

#loading data
dataPilg <- read.csv("C:/Users/Uros Randelovic/Documents/R workspace/BUS 111/data.csv")
#explore the data 
head(dataPilg, n=10)
names(dataPilg)
dataPilg
#visualy explore the data in table format
data(dataPilg)
View(dataPilg)

###split the data###
smp_size <- floor(0.75 * nrow(dataPilg))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataPilg)), size = smp_size)
#separate specific sets of data
train <- dataPilg[train_ind, ]
test <- dataPilg[-train_ind, ]
#plot the income and profitability to explore
plot(train[,5], train[,2])
#coorelation plot 
library(corrplot)
M<- cor(train)
corrplot(M,method = "ellipse")
#exploring the possibility to regress the discrete variables
train = within(train, {
  income1 = ifelse(X9Inc == 1, 1, 0)
  income2 = ifelse(X9Inc == 0, 1, 0)
})
View(train)
#tried to regress multiple variables to see the
#intercepts and relevance
fit <- lm(train[2] ~ ., data=train)
head(train)
fit <- lm(formula = `X9Profit` ~ `X9Online`,`X9Inc`, data=train)
summary(fit)
##ploting a matrix of all variables
newData = dataPilg[,c(1:8)]
summary(newData)   
plot(newData, pch=16, col="blue", main="Matrix")
set.seed(1)
mod1 = lm(X9Profit~X9Inc + X9Age, data=newData)
summary(mod1)
newdatacor = cor(newData[1:8])
corrplot(newdatacor, method = "number")
#dropping the N/A values
library(data.table)
DT <- as.data.table(train)
DT[,which(unlist(lapply(DT, function(x)!all(is.na(x))))),with=F]
corrplot(data, method="number")
View
##Second approach
train <- train[,colSums(is.na(train))<nrow(train)]
View(train)

na.omit(train)
train <- as.data.frame(train)

frameTrain <- as.data.frame(train)
#removing NA's by reading in differently 
importData <- read.csv("C:/Users/Uros Randelovic/Documents/R workspace/BUS 111/data.csv",
                       stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
#viewing data 
str(importData)
data <- importData[complete.cases(importData[c("X9Profit","X0Profit","X0Online","X0Billpay","X9Inc","X9Online","X9Age","X9Tenure")]),]
str(data)
#plotting data
corrplot(cor(data), method="ellipse",shade.col=NA, tl.col="black", tl.srt=45)
