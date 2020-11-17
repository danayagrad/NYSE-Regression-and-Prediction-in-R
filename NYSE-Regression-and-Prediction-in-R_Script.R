



load("nyse.RData")

# Print first rows of the NYSE data
head(nyse)

#Part1
#1 Correlation coefficient

# empty vector of length 25
corr.coeffs = vector("numeric", length = 25)
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:25){
    corr.coeffs[i] = cor(nyse$GOOGL, nyse[, i + 5])
}
corr.coeffs
library("RColorBrewer")

#Summarize corelation coefficient using bar graph
barplot(corr.coeffs, xlab = "Company", ylab = "Correlation Coefficients", ylim = c(-1,1), main = " Correlation coefficients of Google and other 25 companies", space = 1, las =1, col=brewer.pal(n = 3, name = "Pastel2"))
axis(1, at=seq(1.5,50,2), labels=c("AAPL","AMZN","AZN","BP","C","CDE","DAL","DPZ","F","GIL","JPM","K","KO","M","MSFT","NOK","PG","RBS","SAM","SPGI","T","V","WMT", "WHR","XIN"), las=2, cex = 0.8)
text(x = seq(1.5,50,2), y = c(-0.3, -0.9,  0.50809086, -0.75, -0.2, -1, 0.56874469, -0.75,  0.0455,  0.75887937, -0.7,  0.27974249, 0.33, 0.65175492, -0.85, -0.6,  0.44743752, -0.75, 0.76574429, -0.65, -0.2, -0.4,  0.04607668,  0.59497713, -0.5), label = round(corr.coeffs,3), pos = 3, cex = 0.7, col = "Gray")


#2 Find the top 5 companies with the strongest correlation with google.

names(nyse)
com.name = c(names(nyse))

corr.coeffs.table = data.frame(com.name[6:30], corr.coeffs, abs(corr.coeffs))
corr.coeffs.table
corr.coeffs.table[ order(corr.coeffs.table[,3],decreasing = TRUE),]


cde.lm <- lm( nyse$GOOGL~ nyse$CDE, data = nyse) 
summary(cde.lm)
amzn.lm <- lm( nyse$GOOGL~ nyse$AMZN, data = nyse) 
summary(amzn.lm)
sam.lm <- lm( nyse$GOOGL~ nyse$SAM, data = nyse) 
summary(sam.lm)
gil.lm <- lm( nyse$GOOGL~ nyse$GIL, data = nyse) 
summary(amzn.lm)
msft.lm <- lm( nyse$GOOGL~ nyse$MSFT, data = nyse) 
summary(msft.lm)


par(mfrow = c(1, 1))
plot(nyse$CDE, nyse$GOOGL, xlab = "Scaled CDE Price", ylab = "Google Price", main = "Google and CDE stock price from 2010 to 2015", col= "dimgrey")
abline(cde.lm, lwd = 2, col = "indianred", lty = 2)


plot(nyse$AMZN, nyse$GOOGL, xlab = "Scaled Amazon Price", ylab = "Google Price", main = "Google and Amazon stock price from 2010 to 2015", col= "dimgrey")
abline(amzn.lm, lwd = 2, col = "indianred", lty = 2)

plot(nyse$SAM, nyse$GOOGL, xlab = "Scaled SAM Price", ylab = "Google Price", main = "Google and SAM stock price from 2010 to 2015", col= "dimgrey")
abline(sam.lm, lwd = 2, col = "indianred", lty = 2)


plot(nyse$GIL, nyse$GOOGL, xlab = "Scaled GIL Price", ylab = "Google Price", main = "Google and GIL stock price from 2010 to 2015", col= "dimgrey")
abline(gil.lm, lwd = 2, col = "indianred", lty = 2)

plot(nyse$MSFT, nyse$GOOGL, xlab = "Scaled MSFT Price", ylab = "Google Price", main = "Google and MSFT stock price from 2010 to 2015", col= "dimgrey")
abline(msft.lm, lwd = 2, col = "indianred", lty = 2)


#3 Variable selction and transformation of independent variable

nyse.lm<-lm(GOOGL~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN,data=nyse)
nyse.lm
summary(nyse.lm)

#use leave and bound algorithim to select the 5 best models of variables combinations

library(leaps)
leap.bound <-leaps(y = nyse[, 5],x = nyse[, 6:30] , nbest = 5,
                   method = "adjr2",names = names(nyse[, 6:30]))
leap.bound
leap.bound.df = data.frame(Size = leap.bound$size, AdjR2 = round(leap.bound$adjr2, 3),leap.bound$which, row.names = NULL)

leap.bound.df

par(mfrow = c(1, 1))
plot(leap.bound$size,leap.bound$adjr2, ylab = "Adjusted R^2",xlab = "Number of variables", main = "Leaps and Bounds Result")


# The plot suggests 10 variables
leap.bound.df10 = subset(leap.bound.df, Size == 10)
leap.bound.df10

#Select top 3 AdjR2 of 10 variables.

leap1.lm <- lm(GOOGL ~  AZN + C+ DPZ + GIL +RBS + SAM + T + V + WMT + WHR, data = nyse)
leap2.lm <- lm(GOOGL ~  AZN + C+ DPZ + GIL + M + RBS + SAM + V + WMT, data = nyse)
leap3.lm <- lm(GOOGL ~  AZN + C +DPZ + GIL + K + RBS + SAM + V + WMT, data = nyse)



#Forwards selection
# fit an intercept only model, assume alpha = 0.05 here.

nyse.frwd <- lm(GOOGL ~ 1, data = nyse)
add1(nyse.frwd, scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")

# largest F statistic is for CDE= 6139.0547, p-value is 2.2e-16 < 0.5 
# add CDE to the model
nyse.frwd1 <- lm(GOOGL ~ CDE, data = nyse)
add1(nyse.frwd1,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
#add KO 

nyse.frwd2<- lm(GOOGL ~ CDE+KO, data = nyse)
add1(nyse.frwd2,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add WMT

nyse.frwd3<- lm(GOOGL ~ CDE+KO+WMT, data = nyse)
add1(nyse.frwd3,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add MSFT


nyse.frwd4<- lm(GOOGL ~ CDE+KO+WMT+MSFT, data = nyse)
add1(nyse.frwd4,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add DAL 

nyse.frwd5<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL, data = nyse)
add1(nyse.frwd5,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add XIN

nyse.frwd6<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN, data = nyse)
add1(nyse.frwd6,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add RBS

nyse.frwd7<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS, data = nyse)
add1(nyse.frwd7,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
# add C


nyse.frwd8<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C, data = nyse)
add1(nyse.frwd8,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
#add AZN


nyse.frwd9<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN, data = nyse)
add1(nyse.frwd9,scope = ~ AAPL+AMZN+AZN+BP+C+
         CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+
         PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN, test = "F")
#add DPZ


nyse.frwd10<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ, data = nyse)
add1(nyse.frwd10,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add V


nyse.frwd11<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V, data = nyse)
add1(nyse.frwd11,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add K


nyse.frwd12<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K, data = nyse)
add1(nyse.frwd12,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add GIL


nyse.frwd13<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL, data = nyse)
add1(nyse.frwd13,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add WHR


nyse.frwd14<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR, data = nyse)
add1(nyse.frwd14,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add SAM


nyse.frwd15<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR+SAM, data = nyse)
add1(nyse.frwd15,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add BP

nyse.frwd16<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR+SAM+BP, data = nyse)
add1(nyse.frwd16,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add PG


nyse.frwd17<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR+SAM+BP+PG, data = nyse)
add1(nyse.frwd17,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add T

nyse.frwd18<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR+SAM+BP+PG+T, data = nyse)
add1(nyse.frwd18,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add M


nyse.frwd19<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+
                     WHR+SAM+BP+PG+T+M, data = nyse)
add1(nyse.frwd19,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add SPGI


nyse.frwd20<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+
                     WHR+SAM+BP+PG+T+M+SPGI, data = nyse)
add1(nyse.frwd20,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")
# add AMZN


nyse.frwd21<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+
                     WHR+SAM+BP+PG+T+M+SPGI+AMZN, data = nyse)
add1(nyse.frwd21,scope = ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+
         DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+
         SPGI+T+V+WMT+WHR+XIN, test = "F")

# The largest F statistic is JPM = 3.3112, but its P value is 0.06901 > 0.05 significant level. So stop here. 
# The model from forward selection is nyse.frwdlm21<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+WHR+SAM+BP+PG+T+M+SPGI+AMZN, data = nyse)





#Backwards selection
# fit full model
nyse.back<-lm(GOOGL~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN,data=nyse)

drop1(nyse.back, scope = ~., test = "F")

#smallest F statistic is AAPL = 0.3404, p value = 0.5597118 > 0.05 significant level.               
#drop AAPL

nyse.back1<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                   GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR+XIN,data=nyse)
drop1(nyse.back1, scope = ~., test = "F")
# drop XIN



nyse.back2<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                   GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)
drop1(nyse.back2, scope = ~., test = "F")
# drop KO


nyse.back3<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                   GIL+JPM+K+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)
drop1(nyse.back3, scope = ~., test = "F")
#drop NOK


nyse.back4<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                   GIL+JPM+K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)
drop1(nyse.back4, scope = ~., test = "F")
#drop F



nyse.back5<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+
                   GIL+JPM+K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)
drop1(nyse.back5, scope = ~., test = "F")
# drop JPM


nyse.back6<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+
                   GIL+K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)
drop1(nyse.back6, scope = ~., test = "F")
# smallest F statistics is AMZ = 5.7266, p value = 0.016833 < 0.05, so keep.
# Stop backwared selection, conclude that the best model is nyse.back6<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+GIL+K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)




# STEPWISE SELECTION
# intercept only model
nyse.step<-lm(GOOGL ~ 1, data = nyse)
step(nyse.step,
     scope = ~AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
         GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+
         V+WMT+WHR+XIN, direction = "both")

# Base on the output, the best model gives lowest AIC = 9957.37 which this lm(formula = GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN +  DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


# Summary of selection procedues
# from leaps and bound, the best models are

leap1.lm <- lm(GOOGL ~  AZN + C+ DPZ + GIL +RBS + SAM + T + V + WMT + WHR, data = nyse)
leap2.lm <- lm(GOOGL ~  AZN + C+ DPZ + GIL + M + RBS + SAM + V + WMT, data = nyse)
leap3.lm <- lm(GOOGL ~  AZN + C +DPZ + GIL + K + RBS + SAM + V + WMT, data = nyse)


# from forward selection, the best model is nyse.frwd21<- lm(GOOGL ~ CDE+KO+WMT+MSFT+DAL+XIN+RBS+C+AZN+DPZ+V+K+GIL+ WHR+SAM+BP+PG+T+M+SPGI+AMZN, data = nyse)


# from backward selection, the best model is nyse.back6<-lm(GOOGL~AMZN+AZN+BP+C+CDE+DAL+DPZ+GIL+K+M+MSFT+PG+RBS+SAM+SPGI+T+V+WMT+WHR,data=nyse)


# FROM STEPWISE SELECTION WE CHOSE
nyse.stepw<-lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


# COMPARE Cp AND PRESS FOR EACH MODEL....
full_model <- lm(GOOGL ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                     GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+
                     V+WMT+WHR+XIN, data = nyse)

# empty vectors to store Cp and PRESS values
Cp <- c()
press <- c()

# for leap1
reduced_model <- leap1.lm
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[1]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[1]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for leap2
reduced_model <- leap2.lm
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[2]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[2]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

# for leap3
reduced_model <- leap3.lm
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[3]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[3]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for nyse.fwrd
reduced_model <- nyse.frwd21
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[4]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[4]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)



# for nyse.back
reduced_model <- nyse.back6
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[5]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[5]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)



# for nyse.step
reduced_model <- nyse.stepw
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[6]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[6]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

selection.method<-c("leaps.bounds1", "leaps.bounds2", "leaps.bounds3","forwards","backwards","stepwise")

data.frame(selection.method,Cp,press)

# Stepwise gives lowest CP and press.
# The model is = nyse.stepw<-lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)



# transformation
library(MASS)
nyse.stepw<-lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
summary(nyse.stepw)


# Transformation X

#CDE
par(mfrow = c(1, 1))
plot(nyse$CDE, nyse$GOOGL, xlab = "Scaled CDE
     Price", ylab = "Google Price", main = "Google and CDE stock 
     price from 2010 to 2015", col= "dimgrey", pch = 1)


nyse$CDE.transformed1 = (nyse$CDE)^(0.5)
nyse$CDE.transformed2 = log(nyse$CDE)
nyse$CDE.transformed3 = (nyse$CDE)^(-0.5)

lm1 = lm(GOOGL ~ CDE.transformed1 + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
summary(lm1)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed")

# residuals versus dependent variables
par(mfrow = c(2, 1))
plot(nyse.stepw$residuals ~ nyse$CDE, main = "Orignal")
plot(lm1$residuals ~nyse$CDE.transformed1, main = "Transformed")

#log(CDE) and CDE^(-0.5) give NaN product, accodrinlgy, didn't proceed to fiting 
# based on R^2 and adjusted R^2, appears that using original CDE (CDE^1) is the best.
# based on residuals vs fitted values, it appreas that original CDE model gives evenly and randomly scatter of residuals.
#base on Normal QQ plot, original CDE model lies close to the line of equality while the transformed CDE model departs from the line of equality on both ends.




#WMT
par(mfrow = c(1, 1))
plot(nyse$WMT, nyse$GOOGL, xlab = "Scaled WMT
     Price", ylab = "Google Price", main = "Google and WMT stock 
     price from 2010 to 2015", col= "dimgrey", pch = 1)



nyse$WMT.transformed1 = (nyse$WMT)^(2)
nyse$WMT.transformed2 = (nyse$WMT)^(3)

lm1 = lm(GOOGL ~ CDE +WMT.transformed1 + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE +WMT.transformed2 + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
summary(lm1)
summary(lm2)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$WMT, main = "Orignal")
plot(lm1$residuals ~nyse$WMT.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$WMT.transformed2, main = "Transformed2")


# based on R^2 and adjusted R^2, appears that using original WMT is the best, it gives the higher R^2 and adjusted R^2 than both transformations.
#base on Normal QQ plot, WMT^3 model lies closest to the line of equality while WMT^2 and WMT slightly depart from the line of equaltiy at both ends. 
# based on residuals versus WMT and transfromed WMT , the residual seem to be most evenly distributed on original model. 
# So, will maintain simplicity and not transform WMT.


#MSFT
par(mfrow = c(1, 1))
plot(nyse$MSFT, nyse$GOOGL, xlab = "Scaled MSFT
     Price", ylab = "Google Price", main = "Google and MSFT 
     stock price from 2010 to 2015", pch = 1)

nyse$MSFT.transformed1 = (nyse$MSFT)^(0.5)
nyse$MSFT.transformed2 = log(nyse$MSFT)
nyse$MSFT.transformed3 = (nyse$MSFT)^(-0.5)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT.transformed1 + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT.transformed3 + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$MSFT, main = "Orignal")
plot(lm1$residuals ~nyse$MSFT.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using origianl MSFT (MSFT^1) is the best, it gives higher R^2 and adjusted R^2 than transformed MSFT.
# based on residuals vs fitted values, original MSFT model gives evenly and randomly scatter of residuals. 
#base on Normal QQ plot, original model lies closest to the line of equality.
# So, will maintain simplicity and not transform MSFT.


#DAL
par(mfrow = c(1, 1))
plot(nyse$DAL, nyse$GOOGL, xlab = "Scaled DAL
     Price", ylab = "Google Price", main = "Google and DAL 
     stock price from 2010 to 2015", pch = 1)


nyse$DAL.transformed1 = (nyse$DAL)^(0.5)
nyse$DAL.transformed2 = log(nyse$DAL)
nyse$DAL.transformed3 = (nyse$DAL)^(-0.5)
nyse$DAL.transformed4 = (nyse$DAL)^(2)
nyse$DAL.transformed5 = (nyse$DAL)^(3)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed1 + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed3 + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
lm4 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed4 + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
lm5 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed5 + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)



summary(lm1)
summary(lm3)
summary(lm4)
summary(lm5)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus independent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$DAL, main = "Orignal")
plot(lm1$residuals ~nyse$DAL.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using DAL^0.5 is the best, it gives higher R^2 and adjusted R^2 than original DAL.
# based on residuals vs fitted values,
# based on Normal QQ plot,
# based on residuals vs. transformation, 
# So, will 





#RBS
par(mfrow = c(1, 1))
plot(nyse$RBS, nyse$GOOGL, xlab = "Scaled RBS
     Price", ylab = "Google Price", main = "Google and RBS 
     stock price from 2010 to 2015", pch = 1)



nyse$RBS.transformed1 = (nyse$RBS)^(0.5)
nyse$RBS.transformed2 = log(nyse$RBS)
nyse$RBS.transformed3 = (nyse$RBS)^(-0.5)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS.transformed1 + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS.transformed3 + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$RBS, main = "Orignal")
plot(lm1$residuals ~nyse$RBS.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using original RBS is the best, it gives higher R^2 and adjusted R^2 than RBS^0.5.
# based on residuals vs fitted values,
# based on Normal QQ plot, original RBS model lies closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, 
# So, will 



#C
par(mfrow = c(1, 1))
plot(nyse$C, nyse$GOOGL, xlab = "Scaled C
     Price", ylab = "Google Price", main = "Google and C 
     stock price from 2010 to 2015", pch = 1)


nyse$C.transformed1 = (nyse$C)^(0.5)
nyse$C.transformed2 = log(nyse$C)
nyse$C.transformed3 = (nyse$C)^(-0.5)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C.transformed1 + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C.transformed3 + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$C, main = "Orignal")
plot(lm1$residuals ~nyse$C.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using C^0.5 is the best, it gives higher R^2 and adjusted R^2 than original C.
# based on residuals vs fitted values, original C gives best evenly and randomly distributed residuals.
# based on Normal QQ plot, original c model lies closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, 
# So, will



#AZN
par(mfrow = c(1, 1))
plot(nyse$AZN, nyse$GOOGL, xlab = "Scaled AZN
     Price", ylab = "Google Price", main = "Google and AZN 
     stock price from 2010 to 2015", pch = 1)



nyse$AZN.transformed1 = (nyse$AZN)^(0.5)
nyse$AZN.transformed2 = log(nyse$AZN)
nyse$AZN.transformed3 = (nyse$AZN)^(-0.5)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN.transformed1 + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN.transformed3 + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$AZN, main = "Orignal")
plot(lm1$residuals ~nyse$AZN.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using AZN ^0.5 is the best, it gives higher R^2 and adjusted R^2 than original AZN.
# based on residuals vs fitted values, original AZN gives best evenly and randomly distributed residuals.
# based on Normal QQ plot, original AZN model lies closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, 
# So, wil


#DPZ
par(mfrow = c(1, 1))
plot(nyse$DPZ, nyse$GOOGL, xlab = "Scaled DPZ
     Price", ylab = "Google Price", main = "Google and DPZ 
     stock price from 2010 to 2015", pch = 1)


nyse$DPZ.transformed1 = (nyse$DPZ)^(0.5)
nyse$DPZ.transformed2 = log(nyse$DPZ)
nyse$DPZ.transformed3 = (nyse$DPZ)^(-0.5)
nyse$DPZ.transformed4 = (nyse$DPZ)^(-1)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ.transformed1 + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ.transformed3 + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
lm4 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ.transformed4 + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$DPZ, main = "Orignal")
plot(lm1$residuals ~nyse$DPZ.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using  DPZ^0.5 is the best, it gives higher R^2 and adjusted R^2 than original DPZ.
# based on residuals vs fitted values, original DPZ gives best evenly and randomly distributed residuals.
# based on Normal QQ plot, original DPZ model lies closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, 
# So, wil



#V
par(mfrow = c(1, 1))
plot(nyse$V, nyse$GOOGL, xlab = "Scaled V
     Price", ylab = "Google Price", main = "Google and V 
     stock price from 2010 to 2015", pch = 1)


nyse$V.transformed1 = (nyse$V)^(0.5)
nyse$V.transformed2 = log(nyse$V)
nyse$V.transformed3 = (nyse$V)^(-0.5)
nyse$V.transformed4 = (nyse$V)^(-1)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V.transformed1 + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V.transformed2 + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)
lm4 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V.transformed4 + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(nyse.stepw)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")

plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")


# residuals versus dependent variables
par(mfrow = c(1, 2))
plot(nyse.stepw$residuals ~ nyse$V, main = "Orignal")
plot(lm1$residuals ~nyse$V.transformed1, main = "Transformed1")



# based on R^2 and adjusted R^2, appears that using  V^0.5 is the best, it gives a lot higher R^2 and adjusted R^2 than original DPZ.
# based on residuals vs fitted values, original V gives best evenly and randomly distributed residuals.On the other hand, the residual of transformation V is not, it is divided into 2 groups of data.
# based on Normal QQ plot, original V model lies a lot closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, 
# So, wil


#K
par(mfrow = c(1, 1))
plot(nyse$K, nyse$GOOGL, xlab = "Scaled K
     Price", ylab = "Google Price", main = "Google and K 
     stock price from 2010 to 2015", pch = 1)


nyse$K.transformed1 = (nyse$K)^(2)
nyse$K.transformed2 = (nyse$K)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K.transformed1 + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K.transformed2 + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$K, main = "Orignal")
plot(lm1$residuals ~nyse$K.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$K.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that using  original K is the best, it gives a lot higher R^2 and adjusted R^2 than both transformations.
# based on residuals vs fitted values, K^3 models gives best evenly and randomly distributed residuals.however, others are also quite well evenly and randomly distributed as well.
# based on Normal QQ plot, original V model lies a lot closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, original K gives the best evenly 
# and randomly scattered.
# So, wil



#GIL
par(mfrow = c(1, 1))
plot(nyse$GIL, nyse$GOOGL, xlab = "Scaled GIL
     Price", ylab = "Google Price", main = "Google and GIL 
     stock price from 2010 to 2015", pch = 1)


nyse$GIL.transformed1 = (nyse$GIL)^(2)
nyse$GIL.transformed2 = (nyse$GIL)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL.transformed1 + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL.transformed2 + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$GIL, main = "Orignal")
plot(lm1$residuals ~nyse$GIL.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$GIL.transformed2, main = "Transformed2")



# based on residuals vs fitted values, GIL^3 models gives best evenly and randomly distributed residuals.however, others are also quite well evenly and randomly distributed as well.
# based on Normal QQ plot, GIL^3 model lies a lot closer to the line of equality than transformation model's one.
# based on residuals vs. transformation, origina GIL gives the best evenly 
# and randomly scattered.
# So, wil



#WHR
par(mfrow = c(1, 1))
plot(nyse$WHR, nyse$GOOGL, xlab = "Scaled WHR
     Price", ylab = "Google Price", main = "Google and WHR 
     stock price from 2010 to 2015", pch = 1)


nyse$WHR.transformed1 = (nyse$WHR)^(2)
nyse$WHR.transformed2 = (nyse$WHR)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR.transformed2 + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$WHR, main = "Orignal")
plot(lm1$residuals ~nyse$WHR.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$WHR.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that WHR^2  is the best, it gives a slightly higher R^2 and adjusted R^2 than WHR^3 and original WHR. WHR^3 also gives higher R^2 and adjusted R^2 than origina WHR.
# based on residuals vs fitted values, wHR^3 models gives best evenly and randomly distributed residuals.however, others are also quite well evenly and randomly distributed as well.
# based on Normal QQ plot, WHR^3 model lies a lot closer to the line of equality than other two models.
# based on residuals vs. transformation, origina WHR gives the best evenly and randomly scattered. 
# So, wil



#SAM
par(mfrow = c(1, 1))
plot(nyse$SAM, nyse$GOOGL, xlab = "Scaled SAM
     Price", ylab = "Google Price", main = "Google and SAM 
     stock price from 2010 to 2015", pch = 1)

nyse$SAM.transformed1 = (nyse$SAM)^(2)
nyse$SAM.transformed2 = (nyse$SAM)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM.transformed1 + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM.transformed2 + BP + PG + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$SAM, main = "Orignal")
plot(lm1$residuals ~nyse$SAM.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$SAM.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that original SAM  is the best, it gives a slightly higher R^2 and adjusted R^2 than other models.
# based on residuals vs fitted values, original models gives best evenly and randomly distributed residuals.
# based on Normal QQ plot, original model lies a lot closer to the line of equality than other two models.
# based on residuals vs. transformation, origina SAM gives the best evenly 
# and randomly scattered. 
# So, wil




#BP
par(mfrow = c(1, 1))
plot(nyse$BP, nyse$GOOGL, xlab = "Scaled BP
     Price", ylab = "Google Price", main = "Google and BP 
     stock price from 2010 to 2015", pch = 1)


#PG
par(mfrow = c(1, 1))
plot(nyse$PG, nyse$GOOGL, xlab = "Scaled T
     Price", ylab = "Google Price", main = "Google and PG 
     stock price from 2010 to 2015", pch = 1)

nyse$PG.transformed1 = (nyse$PG)^(2)
nyse$PG.transformed2 = (nyse$PG)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG.transformed1 + T + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG.transformed1 + T + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$PG, main = "Orignal")
plot(lm1$residuals ~nyse$PG.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$PG.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that both transformed models are the best, they give a slightly higher R^2 and adjusted R^2 than other models.
# based on residuals vs fitted values,
# based on Normal QQ plot, transformation models lie closer to the line of equality than the original model.
# based on residuals vs. transformation, origina PG gives the best evenly 
# and randomly scattered. 
# So, wil




#T
par(mfrow = c(1, 1))
plot(nyse$T, nyse$GOOGL, xlab = "Scaled T
     Price", ylab = "Google Price", main = "Google and T 
     stock price from 2010 to 2015", pch = 1)

nyse$T.transformed1 = (nyse$T)^(2)
nyse$T.transformed2 = (nyse$T)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T.transformed1 + M + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG + T.transformed2 + M + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$T, main = "Orignal")
plot(lm1$residuals ~nyse$T.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$T.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that T^3 is the best, it give a slightly higher R^2 and adjusted R^2 than other models.
# based on residuals vs fitted values,
# based on Normal QQ plot, T^3 model lies closer to the line of equality than other models.
# based on residuals vs. transformation, origina T gives the best evenly 
# and randomly scattered. 
# So, wil




#M
par(mfrow = c(1, 1))
plot(nyse$M, nyse$GOOGL, xlab = "Scaled M
     Price", ylab = "Google Price", main = "Google and M 
     stock price from 2010 to 2015", pch = 1)


nyse$M.transformed1 = (nyse$M)^(2)
nyse$M.transformed2 = (nyse$M)^(3)


lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M.transformed1 + SPGI + AMZN + JPM + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M.transformed2 + SPGI + AMZN + JPM + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)



# residual assumption checking plots
par(mfrow = c(2, 3))
plot(nyse.stepw, which =1, ask = F, main = " Original")
plot(lm1,  which = 1, ask = F, main = "Transformed1")
plot(lm2,  which = 1, ask = F, main = "Transformed2")
plot(nyse.stepw, which =2, ask = F, main = "Original")
plot(lm1,  which = 2, ask = F, main = "Transformed1")
plot(lm2,  which = 2, ask = F, main = "Transformed2")

# residuals versus dependent variables
par(mfrow = c(1, 3))
plot(nyse.stepw$residuals ~ nyse$M, main = "Orignal")
plot(lm1$residuals ~nyse$M.transformed1, main = "Transformed1")
plot(lm2$residuals ~nyse$M.transformed2, main = "Transformed2")



# based on R^2 and adjusted R^2, appears that M^3 is the best, it give a slightly higher R^2 and adjusted R^2 than other models.
# based on residuals vs fitted values,
# based on Normal QQ plot, M^3 model lies closer to the line of equality than other models.
# based on residuals vs. transformation, origina M gives the best evenly 
# and randomly scattered. 
# So, wil


#SPGI
par(mfrow = c(1, 1))
plot(nyse$SPGI, nyse$GOOGL, xlab = "Scaled SPGI
     Price", ylab = "Google Price", main = "Google and SPGI 
     stock price from 2010 to 2015", pch = 1)




#AMZN
par(mfrow = c(1, 1))
plot((nyse$AMZN), nyse$GOOGL, xlab = "Scaled sqrt 
     AMZN Price", ylab = "Google Price", main = "Google and 
     AMZN stock price from 2010 to 2015", pch = 1)

nyse$AMZN.transformed5 = (nyse$AMZN)^(3)



#JPM
par(mfrow = c(1, 1))
plot(nyse$JPM, nyse$GOOGL, xlab = "Scaled JPM
     Price", ylab = "Google Price", main = "Google and JPM
     stock price from 2010 to 2015", pch = 1)

nyse$JPM.transformed1 = (nyse$JPM)^(2)
nyse$JPM.transformed2 = (nyse$JPM)^(3)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM.transformed1  + F, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM.transformed2 + F, data = nyse)


summary(lm1)
summary(lm2)
summary(nyse.stepw)




#F
par(mfrow = c(1, 1))
plot(nyse$F, nyse$GOOGL, xlab = "Scaled F
     Price", ylab = "Google Price", main = "Google and F
     stock price from 2010 to 2015", pch = 1)


nyse$F.transformed1 = (nyse$F)^(2)
nyse$F.transformed2 = (nyse$F)^(3)
nyse$F.transformed3 = (nyse$F)^(0.5)
nyse$F.transformed4 = log(nyse$F)

lm1 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM  + F.transformed1, data = nyse)

lm2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F.transformed2, data = nyse)
lm3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN+ DPZ + V + K + GIL + WHR + SAM + BP + PG + T + M + SPGI + AMZN + JPM + F.transformed3, data = nyse)


summary(lm1)
summary(lm2)
summary(lm3)
summary(nyse.stepw)





#### Summary: use WHR^2, T^3, M^3
### improved model is nyse.improved = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN + JPM + F, data = nyse)


#4 Check regression assumptions 

nyse.improved = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN + JPM + F,data = nyse)


summary(nyse.stepw)
summary(nyse.improved)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
shapiro.test(rstudent(nyse.improved))
#p-value = 0.08431 > 0.05
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 


# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved, which = 1, add.smooth = F, main = "Improved Model")


# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved, which = 1, main = "Improved Model")


#Part 2
#1. Prediction

team      <- "Danaya Lorpattanakasem"
token     <- "Eywc5w"    


predict_google <- function(nyse, newdata){
    
    nyse$WHR.transformed1      =  (nyse$WHR)^2
    newdata$WHR.transformed1   = (newdata$WHR)^2
    nyse$T.transformed2        = (nyse$T)^3
    newdata$T.transformed2     = (newdata$T)^3
    nyse$M.transformed2    = (nyse$M)^3
    newdata$M.transformed2     = (newdata$M)^3
    
    google.lm   <- lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS+ C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN + JPM + F,data = nyse) 
    
    predictions <- predict(google.lm, newdata = newdata)
    
    return(predictions)
}

# RSSPE = 12.32



#2 improve prediction accuracy.
# Try different combination of variables

# Transformed DAL5
nyse.improved2 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed5 + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN + JPM + F, data = nyse)

summary(nyse.improved2)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved2, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved2))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved2, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved2, which = 1, main = "Improved Model")

# Fail normality test.


# Add Transformed F and AMZN
nyse.improved3 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F.transformed3, data = nyse)

summary(nyse.improved3)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved3, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved3))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved3, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved3, which = 1, main = "Improved Model")

#Fail normality test.

# Add Transformed F and AMZN, removed transformation of T
nyse.improved4 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F.transformed3, data = nyse)

summary(nyse.improved4)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved4, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved4))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved4, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved4, which = 1, main = "Improved Model")


# Add Transformed F and AMZN, removed transformation of M
nyse.improved5 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M + SPGI + AMZN.transformed5 + JPM + F.transformed3, data = nyse)

summary(nyse.improved5)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved5, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved5))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved5, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved5, which = 1, main = "Improved Model")


# Add Transformed F and AMZN, removed transformation of WHR
nyse.improved6 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F.transformed3, data = nyse)

summary(nyse.improved6)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved6, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved6))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved6, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved6, which = 1, main = "Improved Model")

# Fail normality test.


# Add Transformed AMZN
nyse.improved7 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F, data = nyse)

summary(nyse.improved7)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved7, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved7))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved7, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved7, which = 1, main = "Improved Model")


# Add Transformed DAL
nyse.improved8 = lm(GOOGL ~ CDE + WMT + MSFT + DAL.transformed1 + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F, data = nyse)

summary(nyse.improved8)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved8, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved8))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved8, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved8, which = 1, main = "Improved Model")

# Removed T
nyse.improved9 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F, data = nyse)

summary(nyse.improved9)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved9, which = 2, main = "Improved Model")



# Removed M
nyse.improved10 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M + SPGI + AMZN.transformed5 + JPM + F, data = nyse)

summary(nyse.improved10)

# checking if errors are normally distributed
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 2, main = "Original Model")
plot(nyse.improved10, which = 2, main = "Improved Model")

#testing null hypothesis of normality using the Shapiro-Wilk correlation test
shapiro.test(rstudent(nyse.stepw))
# at alpha = 0.05, we do not reject H0: studentised residuals are normal and conclude that there is no evidence of departure from residdual normality. 
shapiro.test(rstudent(nyse.improved10))

# checking if errors have constant residual variance
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1,add.smooth = F,  main = "Original Model")
plot(nyse.improved10, which = 1, add.smooth = F, main = "Improved Model")

# checking if errors are independent
par(mfrow = c(2, 1))
plot(nyse.stepw, which = 1, main = "Original Model")
plot(nyse.improved10, which = 1, main = "Improved Model")







# Cross validation
# COMPARE Cp,PRESS and Adjusted R^2 FOR EACH MODEL....
full_model <- lm(GOOGL ~ AAPL+AMZN+AZN+BP+C+CDE+DAL+DPZ+F+
                     GIL+JPM+K+KO+M+MSFT+NOK+PG+RBS+SAM+SPGI+T+
                     V+WMT+WHR+XIN, data = nyse)

# empty vectors to store Cp and PRESS values
Cp <- c()
press <- c()

# for stepwise without any transformation
reduced_model <- nyse.stepw
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[1]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[1]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for stepwise with transformation of WHR^2, T^3, M^3
reduced_model <- nyse.improved
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[2]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[2]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

# for stepwise with transformation of WHR^2, M^3, AMZN^3, F^0.5
reduced_model <- nyse.improved4
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[3]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[3]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for stepwise with transformation of WHR^2, T^3, AMZN^3
reduced_model <- nyse.improved5
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[4]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[4]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

# for stepwise with transformation of WHR^2, T^3,M^3,  AMZN^3
reduced_model <- nyse.improved7
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[5]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[5]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)


# for stepwise with transformation of WHR^2, T^3,M^3,  AMZN^3, DAL ^0.5
reduced_model <- nyse.improved8
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[6]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[6]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)

# for stepwise with transformation of WHR^2, T^3, AMZN^3
reduced_model <- nyse.improved10
n          <- length(full_model$fitted.values)
q          <- length(coef(full_model)) - 1
p          <- length(coef(reduced_model)) - 1
rss_full   <- deviance(full_model)
rss_redu   <- deviance(reduced_model)
Cp[7]         <- (rss_redu / (rss_full / (n - q - 1))) + 2 * (p + 1) - n
press[7]      <- sum((resid(reduced_model) / (1 - hatvalues(reduced_model)))^2)



adj.r.squared = c(summary(nyse.stepw)$adj.r.squared, summary(nyse.improved)$adj.r.squared, summary(nyse.improved4)$adj.r.squared, summary(nyse.improved5)$adj.r.squared, summary(nyse.improved7)$adj.r.squared, summary(nyse.improved8)$adj.r.squared, summary(nyse.improved10)$adj.r.squared)



selection.method<-c("Stepwise", "Improved", "Improved4","Improved5", "Improved7", "Improved8", "Improved10")

data.frame(selection.method,Cp,press, adj.r.squared)


# Prediction result, best one is improved2 with RMSPE = 12.11. nyse.improved7 = lm(GOOGL ~ CDE + WMT + MSFT + DAL + RBS + C + AZN + DPZ + V + K + GIL + WHR.transformed1 + SAM + BP + PG + T.transformed2 + M.transformed2 + SPGI + AMZN.transformed5 + JPM + F, data = nyse)
