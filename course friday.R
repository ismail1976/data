###*************************************************************************
###*************************************************************************
###                                                                      ***
###                    machine learning in time series                   ***
###                     1- prediction by regression                      ***
###                                                                      ***
###*************************************************************************
###*************************************************************************

library(readxl)
ph <- read_excel("C:/Users/lenovo/Desktop/tidjani/ph.xls")
View(ph)
unp<-as.vector(ph[[1]])
inf<-as.vector(ph[[2]])
inf<-ts(inf,frequency = 1,start = c(1991))
unp<-ts(unp,frequency = 1,start = c(1991))

###                        Loading Packages
                       ----------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(corrplot))


##===============================================================
##                     Reading in the data                     ==
##===============================================================

library(tidyverse)
df<-ph
str(df)
head(df)
tail(df)
Hmisc::describe(df)
Hmisc::contents(df)
glimpse(df)
car::brief(df)


#################################################################
##                        Joint Graphs                         ##
#################################################################

library(WVPlots)
plot(inf,type ="l",col="red", main="inflation",xlab = "year",ylab = "inf")
plot(unp,type ="l",col="red", main="unemployment",xlab = "year",ylab = "unp")
ggplot() + 
  geom_line(data = df, aes(x = Date, y = inf) + ylab('inflation'))
ggplot() + 
  geom_line(data = df, aes(x = Date, y = unp)) + ylab('unemployment')

# inf VS unp

ScatterHist(df, title = "inf VS unp", 
            xvar = "unp", yvar = "inf", 
            smoothmethod = "lm")

#inf VS unp

ScatterHist(df, title = "inf VS unp", 
            xvar = "unp", yvar = "inf", 
            smoothmethod = "lm", hist_color = "#00ACBB", 
            point_alpha = 0.3, 
            point_color = "#FF00CC")

##***************************************************************
##           Study the correlation (Bivariat analysis)         **
##***************************************************************
args(cor)
cor(df,method =c("pearson"))
psych::lowerCor(x = df)
psych::corr.test(df)$p

# Plotting the ScatterPlotMatrix
# First look at the help, and the arguments 
# I am going to tweak the knobs a little
pairs(df)  

#change the color and get halp matrix
pairs(df, lower.panel = NULL, col= "blue")

# Or if you want only the lower part matrix

pairs(df, upper.panel = NULL, col= "blue")

# Check the documentation for more information
?pairs

# Scatter Matrix with psych package
library(psych)

pairs.panels(df, 
             method = "pearson", # Correlation method
             hist.col = "#11AACC",
             density = TRUE, 
             cex.cor = 1.3, 
             col = "red", 
             lm = TRUE, 
             pch = 25,    # point character
             bg = "cyan") # background

# Scatter Matrix with car package
car::scatterplotMatrix(df, 
                       col = "steelblue", 
                       pch = 21, 
                       upper.panel = NULL)
# Lastly
library(PerformanceAnalytics)
chart.Correlation(df, 
                  histogram=TRUE,
                  pch=19, 
                  col = "blue")


##***************************************************************
##                  correlation plot matrices                  **
##***************************************************************
correl <- cor(df)

psysch::cor.plot(correl)

corrplot::corrplot(correl)

# Tweak the knobs
corrplot(correl, type = "upper", 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 45)

# check ?corrplot

# Heatmap 

heatmap(correl, symm = TRUE, 
        cexRow = 0.7, 
        cexCol = 0.7)

# ggcorrplot
p <- ggcorrplot::ggcorrplot(correl, 
                            method = "circle", 
                            type = "lower", 
                            ggtheme = ggplot2::theme_linedraw, 
                            lab_col = "blue", 
                            lab_size = 3,
                            tl.cex = 10, 
                            lab = TRUE, 
                            pch.cex = 10, 
                            colors = c("#6D9EC1", "white", "#E46726")) 
p
p + guides(scale = "none")

###*************************************************************************
###*************************************************************************
###                                                                      ***
###                          SPLITTING THE DATA                          ***
###                        TRAINING AND TEST SETS                        ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
###*

ind <- createDataPartition(df$inf, 
                           p = 0.7, times = 1, list = FALSE)

train_set <- df[ind, ]
test_set <- df[-ind, ]
nrow(train_set); nrow(test_set)

# Training the model ------------------------------------------------------

lm_fit <- lm(inf ~ . , data = train_set)
summary(lm_fit)
broom::tidy(lm_fit)
broom::glance(lm_fit)

####chick the assumptions / DIAGNOSTIC ####
library(car)
## 1- Linearity in coefficients:
residualPlot(lm_fit)
crPlots(lm_fit)
## 2- Normality:
qqPlot(lm_fit, labels = row.names(df),
       simulate = T, main = "Q-Q Plot", col = "red")
## 3-Independence to errors:
durbinWatsonTest(lm_fit)
## 4-Homoskedasticity (constant variance):
ncvTest(lm_fit)
spreadLevelPlot(lm_fit)
## 6- Outliers :
outlierTest(lm_fit)
## 7- High-leverage point:
influenceIndexPlot(lm_fit, vars = "hat")
abline(h = c(2,3) * mean(hatvalues(lm_fit)), col = c("blue", "red"),
       lty = 2, lwd = 2)
## 8- Influential data plot:
influenceIndexPlot(lm_fit, vars = "Cook")
avPlots(lm_fit, ask = F)
influencePlot(lm_fit, main = "Influence Plot",
              sub = "Circle size is proportional to Cook's distance")
library(gvlma)
gvlma(lm_fit)
###*  *** Prediction ***
#     --------------------

pred <- predict(object = lm_fit, newdata = test_set, type = "response") 

head(pred)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set$inf
mae <- Metrics::mae(actual = actual, predicted = pred)
mse <- Metrics::mse(actual = actual, predicted = pred)
rmse <- Metrics::rmse(actual = actual, predicted = pred)

# Table of results

knitr::kable(cbind(mae, mse, rmse))
