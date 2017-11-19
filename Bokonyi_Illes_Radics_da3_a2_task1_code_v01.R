# This is a third test



cat("\f")
rm(list=ls())

#setwd("c:/Users/Gabor/Documents/CEU/classes/Data_Analysis_3/Home_Assignment/Assignment_2_team")
setwd("c:/Users/radics.gabor/Documents/ceu/Data_Analysis_3/Assignment_2_team")

# https://cran.r-project.org/web/packages/wbstats/vignettes/Using_the_wbstats_package.html
# install.packages('wbstats')
library(wbstats)
# install.packages("xlsx")
library(xlsx)
library(data.table)
library(ggplot2)
library(lspline)

################################################################################################################################################################
# 1. Download cross-country data on life expectancy and GDP per capita. ?GDP per capita, PPP (constant)? and ?Life expectancy at birth (total)? Delete unnecessary columns and save a csv file with three columns only: country name, life expectancy and GDP per capita. Keep countries with non-missing values for life expectancy and GDP per capita. Document what you do. 
################################################################################################################################################################

GDP <- wb(indicator = "NY.GDP.PCAP.PP.KD", startdate = 2011, enddate = 2011)
setnames(GDP, 'value', 'GDP_per_capita')
setnames(GDP, 'country', 'country_name')

LEX <- wb(indicator = "SP.DYN.LE00.IN", startdate = 2011, enddate = 2011)
setnames(LEX, 'value', 'life_expectancy')

P21 <- data.table(merge(GDP[, c('iso2c', 'country_name', 'GDP_per_capita')], LEX[, c('iso2c', 'life_expectancy')], by = 'iso2c', all.x = TRUE, all.y = TRUE))
P21 <- P21[(GDP_per_capita != 'NA' & life_expectancy != 'NA'),]
write.csv(P21[, c('country_name', 'life_expectancy', 'GDP_per_capita')], 'P21.csv')


################################################################################################################################################################
# 2. Estimate a lowess regression of life expectancy on ln gdp per capita. Estimate a linear regression of life expectancy on GDP per capita that best captures the nonlinearity you found (life expectancy on a piecewise linear spline or a polynomial in the explanatory variable). Argue for your choice. Report the coefficient estimates as well as their confidence interval, interpret and visualize the results. 
################################################################################################################################################################

ggplot(P21, aes(x = life_expectancy)) + geom_histogram(binwidth = 1, colour = "darkgreen", fill ="darkgreen") + ggtitle("life expectancy (ys)")
ggplot(P21, aes(x = GDP_per_capita)) + geom_histogram(binwidth = 1000, colour = "darkgreen", fill ="darkgreen") + ggtitle("GDP per capita")

P21[, `:=`(
  GDP_per_capita_M = GDP_per_capita / 1000, 
  GDP_per_capita_M_sq = (GDP_per_capita / 1000) ^ 2,
  GDP_per_capita_M_cub = (GDP_per_capita / 1000) ^ 3,
  GDP_per_capita_log = log(GDP_per_capita)
),]

# lowess regression of life expectancy on ln gdp per capita
ggplot(P21, aes(GDP_per_capita_log, life_expectancy)) + 
  geom_point(size = 1.5, colour="orange", shape = 4) + 
  geom_smooth(colour = "darkgreen") + 
  labs(x="GDP per capita - log", y="life expectancy (ys)") +
  ggtitle("lowess regression")

# simple linear regression
slr <- lm(life_expectancy ~ GDP_per_capita_M, data = P21)
summary(slr)

ggplot(data = slr, aes(x=GDP_per_capita_M, y=life_expectancy)) +
  geom_point(size=1.5, colour="orange",shape=4) +
  geom_smooth(method="lm", colour="darkgreen", se=FALSE)+
  labs(x="GDP per capita (M$)",y="life expectancy (ys)")+
  ggtitle("simple linear regression")

# level-log linear regression
levellog <- lm(life_expectancy ~ GDP_per_capita_log, P21)
summary(levellog)

ggplot(data = P21, aes(x=GDP_per_capita_log, y=life_expectancy)) +
  geom_point(size=1.5, colour="orange") +
  labs(x="GDP per capita - log",y="life expectancy (ys)")+
  geom_smooth(method="lm", colour="darkgreen", se=F)+
  ggtitle("level-log linear regression")

# lspline: Piecewise linear spline
pls <- lm(life_expectancy ~ lspline(GDP_per_capita_M, c(25)), data=P21)
summary(pls)
P21$pls<-predict(pls)

ggplot(data = P21, aes(x=GDP_per_capita_M, y=life_expectancy)) +
  geom_point(size=1.5, colour="orange",shape=4) +
  labs(x="GDP per capita (M$)", y="life expectancy (ys)")+
  geom_line(data = P21,aes(x=GDP_per_capita_M, y=pls),colour="darkgreen")+
  geom_vline(xintercept=25,colour="red")+
  ggtitle("piecewise linear spline")

# quadratic
quad <- lm(life_expectancy ~ GDP_per_capita_M + GDP_per_capita_M_sq, P21)
summary(quad)
P21$pred_quad<-predict(quad)

ggplot(data = P21, aes(x=GDP_per_capita_M, y=life_expectancy)) +
  geom_point(size=1.5, colour="orange",shape=4) +
  labs(x="GDP per capita (M$)", y="life expectancy (ys)")+
  geom_line(data=P21, aes(x=GDP_per_capita_M,y=pred_quad), colour="darkgreen")+
  ggtitle("quadratic model")

# cubic
cub <- lm(life_expectancy ~ GDP_per_capita_M + GDP_per_capita_M_sq + GDP_per_capita_M_cub, P21)
summary(cub)
P21$pred_cub<-predict(cub)

ggplot(data = P21, aes(x=GDP_per_capita_M, y=life_expectancy)) +
  geom_point(size=1.5, colour="orange",shape=4) +
  labs(x="GDP per capita (M$)", y="life expectancy (ys)")+
  geom_line(data=P21, aes(x=GDP_per_capita_M, y=pred_cub), colour="darkgreen")+
  ggtitle("cubic model")

################################################################################################################################################################
# 3. Estimate a weighted regression (weight=population). Compare results to what we saw in class. 
################################################################################################################################################################

wbsearch(pattern = "Population, total")
# SP.POP.TOTL   Population, total

POP <- wb(indicator = "SP.POP.TOTL", startdate = 2011, enddate = 2011)
setnames(POP, 'value', 'population')
# write.xlsx(POP, 'POP.xls')

PS21w <- data.table(merge(P21, POP[, c('iso2c', 'population')], by = 'iso2c'))
PS21w$population_MM <- PS213$population / 1000000
# write.xlsx(PS21w, 'PS21w.xlsx')

# weighted level-log linear regression
w_levellog <- lm(life_expectancy ~ GDP_per_capita_log, data = PS21w, weights = population_MM)
summary(w_levellog)

ggplot(data = PS21w, aes(x=GDP_per_capita_log, y=life_expectancy, size=population_MM)) +
  geom_point(colour="orange") +
  labs(x="GDP per capita - log",y="life expectancy (ys)")+
  geom_smooth(method="lm", colour="darkgreen", se=F)+
  ggtitle("weighted level-log linear regression")


