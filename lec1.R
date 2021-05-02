setwd("C:/Users/sanke/OneDrive/Desktop/MSBA_fall_2018/Business Analytics with R")


rm(list=ls()) #drop all variables

## Package import
######################################
#only run these commands once (ever) OR use the drop-down install
# install.packages('tidyverse',dep=TRUE)
# install.packages('data.table',dep=TRUE)
#run these commands every time you restart R
#  an import statement like this should be at the start of every R file you 
#  create
library(data.table)
library(tidyverse)

## Data import
######################################
#import commands
churn <- fread('churn.csv') ## make sure your working directory is correct
#churn <- read.csv('churn.csv')

# Short Description		label
# State				      	state
# Account Length			length
# Area Code			    	code
# Phone					      phone
# Int'l Plan				  intl_plan
# VMail Plan		    	vm_plan
# VMail Message	  		vm_mess
# Day Mins			    	day_mins
# Day Calls			    	day_calls
# Day Charge		  		day_charges
# Eve Mins		    		eve_mins
# Eve Calls			     	eve_calls
# Eve Charge		   		eve_charges
# Night Mins			  	night_mins
# Night Calls			  	night_calls
# Night Charge		  	night_charges
# Intl Mins				    intl_mins
# Intl Calls				  intl_calls
# Intl Charge				  intl_charges
# CustServ Calls			cs_calls
# Churn?					    churn

#summary commands
class(churn) #what type of data do we have?
head(churn) #first 6 rows of the data
tail(churn)
summary(churn) #summarize anything in R
colnames(churn) #get the column names of a data set
?summary #get the help files on summary
churn %>% summary  # Piping using the dplyr package equivalent to summary(churn)
summary(churn)

## Basic data manipulation
######################################
#calling elements in a data.table
churn[1] #calls rows
churn[2]
churn[,1] #calls columns
churn[,2]
churn[,churn] #calls columns using variable names
churn[,cs_calls]
churn$churn #short way to call a column
churn$cs_calls

## Check the values in a variable
unique(churn$churn)
unique(churn$cs_calls)
churn %>% select(churn) %>% unique

#discrete variable tabling
table(churn$churn)
table(churn$cs_calls)
table(churn$churn,churn$cs_calls)
table(churn$intl_calls)
table(churn$length)

#vector operations
sum(churn$cs_calls)
mean(churn$cs_calls)
var(churn$cs_calls)
sd(churn$cs_calls)
nrow(churn)
sum(churn$cs_calls)/nrow(churn)
median(churn$cs_calls)

## Variable manipulation
######################################
mean(churn$churn) # throws a non-numeric error
tmp <- churn$churn
table(tmp)
tmp
tmp <- tmp=="True."
table(tmp)
tmp <- as.numeric(tmp) # converts to binary (dummy, indicator) variable
table(tmp)
rm(tmp)

churn$churn <- as.numeric(churn$churn=="True.")
churn$churn
# Note the following is much slower because ifelse is really slow
# churn$churn <- ifelse(churn$churn=="True.",1,0)

sum(churn$churn)
mean(churn$churn)

table(churn$intl_plan)
churn$intl_plan  <- as.numeric(churn$intl_plan=="yes")
churn$vm_plan    <- as.numeric(churn$vm_plan=="yes")
churn$bill       <- churn$day_charges+churn$eve_charges+
churn$night_charges+churn$intl_charges
summary(churn)

## Basic plotting
######################################
plot(churn$length,churn$cs_calls)
plot_basic <- ggplot(churn,aes(x=length,y=cs_calls)) + geom_point()
plot_basic <- churn %>% ggplot(aes(x=length,y=cs_calls)) + geom_point() 
plot_basic
plot_basic <- plot_basic + scale_x_continuous(name="Account length") + 
scale_y_continuous(name="Customer service calls")
plot_basic

## Group by
######################################
#group by cs_calls
#  churn[ROWSTUFF,COLUMNSTUFF,GROUPBY]
churn[,mean(churn),by=cs_calls]
churn[,.N,by=cs_calls]  # Tell me the number in this group
churn[,.(.N,mean(churn)),by=cs_calls]  # Give me two statistics per group (number and mean of churn)
churn[order(cs_calls),.(.N,mean(churn)),by=cs_calls]

#group by others
churn[order(length),.(.N,mean(churn)),by=length]
churn[order(state),.(.N,mean(churn)),by=state]
churn[order(churn),.(.N,mean(length)),by=churn]

#multiple output variables
churn[order(cs_calls),.(.N,mean(length),mean(bill),mean(intl_plan),mean(vm_plan),mean(churn)),by=cs_calls]
outp <- churn[order(cs_calls),.(.N,mean(length),mean(bill),mean(intl_plan),mean(vm_plan),mean(churn)),by=cs_calls]
outp
colnames(outp) <- c('cs_calls','N','length','bill','intl_plan','vm_plan',
                    'churn')
outp # this fixes the column names

#get variance instead
# outp <- churn[order(cs_calls),.(.N,var(length),var(bill),var(intl_plan),
#                                    var(vm_plan),var(churn)),by=cs_calls]
# colnames(outp) <- c('cs_calls','N','length','bill','intl_plan','vm_plan',
#                     'churn')
# outp
rm(outp)

#use cutting to get coarser categories for the length varible
seq(0,250,25)
max(churn$length)
churn$length_bins <- cut(churn$length,seq(0,250,25),include.lowest=TRUE)
table(churn$length_bins)
outp <- churn[order(length_bins),.(.N,mean(bill),mean(cs_calls),
                                      mean(intl_plan),mean(vm_plan),
                                      mean(churn)),by=length_bins]
colnames(outp) <- c('length_bins','N','bill','cs_calls','intl_plan','vm_plan',
                    'churn')
outp
rm(outp)

## Correlations
######################################
cov(churn$length,churn$churn)
var(churn$length)
cor(churn$length,churn$churn)
cor(churn$cs_calls,churn$churn)
cor(churn$bill,churn$churn)

## Correlation Matrix:
churn %>% select(length,bill,intl_plan,vm_plan,churn,cs_calls) %>% cor



library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)

## Read the BWGHT.csv example into the variable churn
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
bwght <- con %>% dbReadTable('bwght') %>% data.table
# bwght <- con %>% dbReadTable('bwght')  # Uses data.frame not data.table
# bwght <- dbReadTable(con,'bwght') 
con %>% dbReadTable('bwght_labels')
con %>% dbDisconnect

## Data description from BWGHT_labels.txt
# variable name   type    format     label      variable label
# faminc          float   %9.0g                 1988 family income, $1000s
# cigtax          float   %9.0g                 cig. tax in home state, 1988
# cigprice        float   %9.0g                 cig. price in home state, 1988
# bwght           int     %8.0g                 birth weight, ounces
# fatheduc        byte    %8.0g                 father's yrs of educ
# motheduc        byte    %8.0g                 mother's yrs of educ
# parity          byte    %8.0g                 birth order of child
# male            byte    %8.0g                 =1 if male child
# white           byte    %8.0g                 =1 if white
# cigs            byte    %8.0g                 cigs smked per day while preg

## Get summary statistics for all variables
head(bwght)
summary(bwght)

## Tables of count data
table(bwght$male)
table(bwght$cigs)
table(bwght$male,bwght$cigs)
table(bwght$fatheduc)

## Basic plotting
plot_basic <- bwght %>% ggplot(aes(x=cigs,y=bwght)) + geom_point() 
plot_basic <- plot_basic + scale_x_continuous(name="Cigarettes smoked during pregnancy per day") + scale_y_continuous(name="Birthweight (oz)")
plot_basic

## Ordinary least-squares the long painful way
y       <- bwght$bwght
x       <- bwght$cigs
my      <- mean(y)
mx      <- mean(x)
diffy   <- y - my
diffx   <- x - mx
num     <- sum(diffy*diffx)
denom   <- sum(diffx^2)
b1      <- num/denom
b0      <- my - b1*mx
c(b0,b1)    # c concatentates inputs into one line

# bwght = 119.8 + -0.514 cigs 

# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.5 decrease in oz of birthweight.
# Every pack per day (20 cigs) is associated with a 10 oz decrease in child size at birth.
# Every pack per day is associated with a 0.64 lb decrease in birthweight.

## Convert to lbs and recompute
y         <- y / 16
my        <- mean(y)
diffy     <- y - my
num       <- sum(diffy*diffx)
b1        <- num/denom
20*b1

## Compute OLS using lm
model1    <- lm(bwght~cigs, data=bwght)
model1
summary(model1)
coef(model1)

## Plotting OLS
plot_ols   <- plot_basic + geom_line(aes(y=predict(model1)),color="red",size=2)
plot_ols

## Multiple least-squares using lm
model2         <- lm(bwght~cigs+faminc, data=bwght)
summary(model2)
# bwght = 119.8 + -0.463 cigs + 0.0928 faminc

# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.46 decrease in oz of birthweight controlling for family income.
model3         <- lm(bwght~cigs+faminc+male+white, data=bwght)
model4         <- lm(bwght~cigs+faminc+male+white+motheduc+fatheduc, data=bwght)
summary(model3)
summary(model4) #this model drops some data because fatheduc is missing

## BWGHT log transform
tmp            <- log(bwght$bwght)
rm("tmp")
bwght$lbwght <- log(bwght$bwght)
model5         <- lm(lbwght~cigs, data=bwght)
model5         <- lm(log(bwght)~cigs, data=bwght)
plot_log   <- plot_ols + geom_line(aes(y=exp(predict(model5))),color="blue",size=2)
plot_log

model5         <- lm(log(bwght)~cigs+faminc+male+white+motheduc+fatheduc, data=bwght)
summary(model5)
coef(model5)
coef(model5)*100
# log(bwght) = 4.7115639 + -0.0051484 cigs + 0.0004177 faminc 
#            + 0.0332363 male + 0.0440468 white + -0.0036268 motheduc
#            + 0.0033968 fatheduc

# Every 1 cigarette smoked by the mother per day during pregancy is associated with a 0.51% decrease in birthweight controlling for family income, etc.
# Every pack per day ==> 10% decrease in bwght

## Quadratic regression
bwght$cigsq  <- bwght$cigs^2
model6         <- lm(bwght~cigs+cigsq, data=bwght)
# model6         <- lm(bwght~cigs+cigs^2, data=bwght) #doesn't work (R looks for interactions)
model6         <- lm(bwght~cigs+I(cigs^2), data=bwght) #this does work because R treats statements inside of the I() as literal
summary(model6)
plot_quad      <- plot_ols + geom_line(aes(y=predict(model6)),color="blue",size=2)
plot_quad
model7         <- lm(log(bwght)~cigs+cigsq+faminc+male+white+motheduc+fatheduc, data=bwght)
summary(model7)

## Subsetting bwght
smokectxt      <- bwght[cigs > 0]
# smokectxt      <- subset(bwght,cigs>0) #data.frame subset for when data.table isn't working
summary(smokectxt)
model8         <- lm(log(bwght)~cigs+male+white, data=smokectxt)
summary(model8)

## Binary variable
bwght$smokes         <- as.numeric(bwght$cigs>0)
# smokes         <- ifelse(bwght$cigs,1,0)
model9        <- lm(log(bwght)~smokes+faminc+male+white+motheduc+fatheduc, data=bwght)
summary(model9)
model9 %>% summary
bwght[1,,]
## Adjusting for heteroskedasticity
library(lmtest)
library(sandwich)
coeftest(model9)
coeftest(model9,vcov=vcovHC)