########################################################################
###
### Matthew Salmon
### Homework 12
### myLinearRegression Function
###
########################################################################

#Clears global environment
rm(list=ls())

#reads in myData for default
setwd("C:/Users/notma/OneDrive/Desktop/My classes/Spring 2020/Intro to R/Lecture 12/myLinearPackage")
myData <- read.csv("myData.csv")

#' Perform linear regression and create scatterplots of each pair of covariates.
#'
#' This function takes inputs of a vector as the dependent variable,
#' a matrix as a set of independent variables, and a list of subjects
#' from these sets to perform linear regression. The function
#' outputs the coefficients and p-values from the regression
#' as well as a scatterplot matrix barring that there are more than 5
#' covariates, in which case a warning is given.
#'
#' @param y A vector  of outcomes.
#' @param x A matrix of covariates.
#' @param sub A list of subjects (i.e. a set of integers corresponding to rows in x)
#' @return The coefficients and p-values of a linear regression performed on  \code{y} subject to \code{x} and a scatterplot matrix of each pair of covariates.
#' @export
#' @examples
#' # You can reference the data any way you would like. For example, the
#' # data set myData is running a linear regression of column "Y" and columns
#' # "X1", "X2", and "X3." This data set has 100 rows, but we're using the sub
#' # function to specify that we only want to look at the first 30.
#'
#' myLinearRegression(y = myData[, "Y"], x = myData[ ,c("X1", "X2", "X3")], sub = c(1:30))
#'
#' # Similarly, you can create your own vector to perform linear regression with.
#' # If 5 or more columns are selected as covariates, as is the case here, the
#' # function will not output any scatterplots. This only looks at rows 5 through 20.
#'
#' myLinearRegression(y = c(1:50), x = myData[ ,2:6], sub = c(5:20))

myLinearRegression <- function(y = myData[, 1], x = myData[,2:4], sub = c(1:20)){
  #references library
  library(GGally)
  #limits to subjects
  y <- y[sub]
  x <- x[sub, ]


  #scatterplot of covariates
  if(ncol(x) < 5){
    paired.plots <- ggpairs(x)
  }
  else{
    paired.plots <- NULL
    warning <- warning("Too many variables to plot")
  }

  #runs linear regression
  myModelFit <- lm(y ~ as.matrix(x))

  #return statement
  return(list("paired plots" = paired.plots, "coef" = summary(myModelFit)$coefficients[,"Estimate"],
                     "pvals" = summary(myModelFit)$coefficients[,"Pr(>|t|)"]))
}
