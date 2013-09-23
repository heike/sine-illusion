# Functions to correct actual data in x and y

correctx <- function(x, y, data){
  require(locfit)
  form <- as.formula(paste(x, "~", y))
  model <- locfit(data=data, formula=form)
  deriv <- locfit(data=datasub, formula=form, deriv=1)
  f <- function(z) predict(model, newdata=structure(data.frame(z), names=x))
  fprime <- function(z) predict(deriv, newdata=structure(data.frame(z), names=x))
}