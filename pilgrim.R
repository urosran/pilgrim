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
   