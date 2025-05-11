my_function = function(x, y){
  
  # code goes here
  result = x + y
  return(result)
}

my_function(4, 7)

# install packages
install.packages("ggplot2")
install.packages("gridExtra")

# load packages
library(ggplot2)
library(gridExtra)

# load data set
wasde = read_csv("WASDE.csv")

# plot corn ____ versus over time
g_price = ggplot(wasde, aes(year, corn_price)) +
  geom_line(color = "cadetblue") +
  ggtitle("Corn prices over time") +
  labs(y = "Corn Prices ($)", x = "Year")

g_demand = ggplot(wasde, aes(year, total_use)) +
  geom_line(color = "burlywood") +
  ggtitle("Corn demand over time") +
  labs(y = "Corn Demand (mil bu)", x = "Year")

g_supply = ggplot(wasde, aes(year, total_supply)) +
  geom_line(color = "darkgoldenrod1") +
  ggtitle("Corn supply over time") +
  labs(y = "Corn Supply (mil bu)", x = "Year")

grid.arrange(g_price, g_demand, g_supply, nrow = 3)

# add SUR as a variable
wasde$SUR = (wasde$end_stocks/ wasde$total_use)

ggplot(wasde, aes(SUR, corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "brown") +
  ggtitle("correlation between SUR and corn price ") +
  labs(y = "Corn price ($)", x = "SUR")

# Estimate linear regression model 
reg1 <- lm(corn_price ~ SUR, data = wasde)
summary(reg1)  # Add your model object here

# create a table
install.packages("gtsummary")
library(gtsummary)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Calculate averages
mean_sur <- mean(wasde$SUR)
mean_price <- mean(wasde$corn_price)
elasticity = -3.6 * (mean_sur / mean_price)

# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

# scatterplot of residuals against SUR
ggplot(data=wasde, aes(x=SUR, y=resid(reg1))) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "darksalmon") +
  ggtitle("correlation between SUR residuals ") +
  labs(y = "Corn price ($)", x = "SUR")
#### NO RELATIONSHIP ####

# Create the inverse of stock-to-use ratio, run the regression, and examine the error terms
wasde$SUR_Inv <- 1 / wasde$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=wasde)

summary(reg2)
tbl_regression(reg2, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

non_lin = 0.17 / (mean_sur * mean_price)

# Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=wasde, aes(x=SUR, y=resid(reg2), color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

# Create a character variable denoting the two time periods, create a dummy variable for the post-2006 period, graph a scatterplot of price on SUR with unique colors and regression lines for each period
wasde$period <- ifelse(wasde$year >= 2006, "2006-2019", "1973-2005")
wasde$P2006 <- as.numeric(wasde$year >= 2006)

ggplot(data=wasde, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=wasde)

summary(reg3)
tbl_regression(reg3, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Collect the residuals from the last regression, create a time series of the errors with a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() binds the specified vectors together as columns to create a new data frame

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)
tbl_regression(reg4, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))
