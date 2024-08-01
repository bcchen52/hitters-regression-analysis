library(ISLR)

#get info about Hitters
head(Hitters)
?Hitters


#create new data set without NA
ds = na.omit(Hitters) 
str(ds)


#check correlations of each continuous variable relative to salary
cor(ds[,c(-14,-15,-20)])[17,]

hist(ds$Salary)

par(mfrow=c(3,3))

summary(ds)

#univariate plots of significant variables
hist(ds$AtBat)
hist(ds$Hits)
hist(ds$Walks)
hist(ds$CRuns)
hist(ds$CWalks)
hist(ds$PutOuts)

#bivariate plots of significant variables
plot(ds$AtBat, ds$Salary, main="Salary vs AtBat")
plot(ds$Hits, ds$Salary, main="Salary vs Hits")
plot(ds$Walks, ds$Salary, main="Salary vs Walks")
plot(ds$CRuns, ds$Salary, main="Salary vs CRuns")
plot(ds$CWalks, ds$Salary, main="Salary vs CWalks")
plot(ds$PutOuts, ds$Salary, main="Salary vs PutOuts")

dev.off()

plot(ds[,c(1,2,6,11,13,16)])


#Regression analysis 
par(mfrow=c(1,1))

#create an overall linear model to find significant predictors
lr = lm(Salary~AtBat+Hits+HmRun+Runs+RBI+Walks+CAtBat+CHits+CHmRun+CRuns+CRBI+CWalks+PutOuts+Assists+Errors+Years, data=ds)
summary(lr)

#new linear model with significant predictors
lr1 = lm(Salary~AtBat+Hits+Walks+CRuns+CWalks+PutOuts, data=ds)
summary(lr1)
plot(lr1)

#linearity
lr1 = lm(I((Salary))~I(AtBat^2)+(Hits)+I(Walks^2)+I(CRuns^2)+(CWalks)+I(PutOuts^2), data=ds)
summary(lr1)
plot(lr1)

#normality
lr1 = lm(I(sqrt(Salary))~I(AtBat^2)+(Hits)+I(Walks^2)+I(CRuns^2)+(CWalks)+I(PutOuts^2), data=ds)
summary(lr1)
plot(lr1)

#outliers
outliers = c('-Mike Schmidt', '-Pete Rose')
lr1 = lm(I(sqrt(Salary))~I(AtBat^2)+(Hits)+I(Walks^2)+I(CRuns^2)+(CWalks^2)+I(PutOuts^2), data=ds[!(row.names(ds) %in% outliers),])
summary(lr1)
plot(lr1)

