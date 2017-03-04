library(ggplot2)
library(dplyr)
library(car)
# load the data
data(mtcars)
datamtcars <- mutate(mtcars, amvar=as.factor(am))


# clean the data - add new variable - amvar - as factor
dataautocars <- filter(datamtcars, amvar==0)
datamanucars <- filter(datamtcars, amvar==1)
dataautocars$amvar = "auto"
datamanucars$amvar = "manual"
datamtcars <- data.frame()
datamtcars <- rbind(dataautocars, datamanucars)
datamtcars <- select(datamtcars, -am)
# Use explortory analysis to compare the automatic/manual vs mpg
# calculate the mean - boxplot-violin plot
grp1 <- group_by(datamtcars, amvar)
datamean <- summarise (grp1, mean(mpg))
datamean
g1 <- ggplot(data=datamtcars, aes(y=mpg, x=amvar, fill=amvar)) + geom_violin(colour="blue") + xlab("Type Of Auto/Manual") + ylab("Mpg Consumption")
g1
# quantify the difference between automatic cars vs. manual cars for the mpg usage
fit2 <- lm(mpg~amvar, data=datamtcars)
rSingle <- summary(fit2)
rSingle

resi <- resid(fit2)


# Check the multi-variables for the impact to mpg
fitall <- lm(mpg ~ ., datamtcars)
vifResult <- vif(fitall)
par(mfrow=c(2,2))
plot(fitall)
# from the vif result, we pick up the most 4 observations - disp, cyl, wt, hp in addition to the amvar
fit3 <- update(fit2, mpg ~ amvar + disp)
fit4 <- update(fit2, mpg ~ amvar + disp + cyl)
fit5 <- update(fit2, mpg ~ amvar + disp + cyl + wt)
fit6 <- update(fit2, mpg ~ amvar + disp + cyl + wt + hp)

anovavalue <- anova(fit2, fit3, fit4, fit5, fit6)
anovavalue
# after checking the anova value, we would like to pick model 4(mpg ~ amvar + disp + cyl + wt), as the Pvalue is lowest
newMutliModel <- lm(mpg~amvar + disp + cyl + wt, datamtcars)
summaryMulti <- summary(newMutliModel)
summaryMulti
plot(newMutliModel)
# Residual and Diagnostics

# Result Summary


