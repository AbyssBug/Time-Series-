library(readxl)
library(TSstudio)
data = read_excel('C:/Users/P/Downloads/_selectedreference_Data Sets.xlsx','the og data souvi sales')
head(data)
ss = data.frame(data)
head(ss)
summary(ss)
str(ss)
plot(ss, type = 'l')
tsss <- ts(data[, 2], start = c(1995, 1), end = c(2001, 12), freq = 12)
plot(tsss/1000, type = 'l')

plot(tsss/1000, ylim = c(0,120), ylab = "Sales (in thousands)", xlab = "Time",
     main = "Souvenir Sales by Month from 1995 to 2001", 
     type = 'b',bty = "l", pch = 18)
box(col = 'black')

plot(log(tsss),ylim = c(6,12), ylab = "Sales (in thousands)", xlab = "Time",
     main = "Souvenir Sales by Month from 1995 to 2001", 
     type = 'b',bty = "l", pch = 18)
box(col = 'black')

acf(log(tsss), lag.max = 100)
acf(tsss)

acf(ts(data[, 2], start = c(1995, 1), end = c(2001, 12), freq = 12),72)
acf(log(ts(data[, 2], start = c(1995, 1), end = c(2001, 12), freq = 12)),12)

pacf(ts(data[, 2], start = c(1995, 1), end = c(2001, 12), freq = 12),36)
pacf(log(ts(data[, 2], start = c(1995, 1), end = c(2001, 12), freq = 12)),36)


x1 = rchisq(48, df = 2)
plot(x1, ylab = 'value', xlab = 'Time', type = 'b', bty = 'l', pch = 18, 
     main = 'Time series plot')
box(lty = 'solid')

x2 = rchisq(48, df = 8)
plot(x2, ylab = 'value', xlab = 'Time', type = 'b', bty = 'l', pch = 18, 
     main = 'Time series plot')
box(lty = 'solid')

plot(decompose(ts(x1, frequency = 12)), type = 'b', bty = 'l', pch = 18)
plot(decompose(ts(x2, frequency = 12)), type = 'b', bty = 'l', pch = 18)

acf(ts(x1, frequency = 12),48)
pacf(ts(x1, frequency = 12),24)

acf(ts(x2, frequency = 12),48)
pacf(ts(x2, frequency = 12),24)


###############################################################################
nValid <- 12
nTrain <- length(tsss) - nValid
train.ts <- window(tsss,
                   start = c(1995, 1), 
                   end = c(1995, nTrain))
valid.ts <- window(tsss,
                   start = c(1995, nTrain + 1),
                   end = c(1995, nTrain + nValid))
head(train.ts)
tail(train.ts)

library(forecast)
# Use regression function for time series tslm() to forecasting model with quadratic curve
lm <- tslm(train.ts ~ trend + I(trend^2))
# Forecasting the validation data h horizon
lm.pred <- forecast(lm, h = nValid, level = 0)

plot(lm.pred,
     xlab = "Year", ylab = "Souvenir Sales", 
     xlim = c(1995, 2003), ylim = c(0, 120000),
     main = "Time Series Analytics of Souvenir Sales from 1995 to 2001",
     bty = "l", xaxt = "n", col = "dark cyan"
)
axis(1, at = seq(1995, 2003, 1), 
     labels = format(seq(1995, 2003, 1), digits = 2))

lines(lm$fitted, lwd = 1.5, col = "black")
lines(valid.ts, lwd = 1.5, col = "coral2")

lines(c(2001, 2001), c(0, 120000), col = "coral3")
lines(c(2002, 2002), c(0, 120000), col = "darkgoldenrod2")
text((length(train.ts)/12)/2 + 1995, 120000, "Training", col = "dark cyan")
text((length(valid.ts)/12)/2 + 2001, 120000, "Validation", col = "coral3")
text((2002 - 2001)/2 + 2002, 120000, "Future", col = "darkgoldenrod2")
arrows(1995 + .03, 115000, 2001 - .03, 115000,
       code = 3, length = 0.1, lwd = 1.5, angle = 30)
arrows(2001 + .03, 115000, 2002 - .03, 115000,
       code = 3, length = 0.1, lwd = 1.5, angle = 30)
arrows(2002 + .03, 115000, 2003 - .03, 115000,
       code = 3, length = 0.1, lwd = 1.5, angle = 30)

legend(x = "left", 
       legend = c("Training Period", "Validation Period"), 
       col = c("dark cyan", "coral2"), lty = c(1, 1), lwd = c(1.5, 1.5), bty = "n")

accuracy(lm.pred,valid.ts)

hist(lm.pred$residuals,
     ylab = "Frequency",
     xlab = "Forecast Error", 
     main = "Regression Forecast of Souvenir Sales",
     bty = "l")

plot(valid.ts, 
     ylim = c(0, 120000),
     xlab = "Months in Year 2001", ylab = "Souvenir Sales",
     main = "Zoom-In Actual Sales aganist Seasonal and Regression Forecasts",
     bty = "l", xaxt = "n", col = "darkgoldenrod3", lwd = 1
)

axis(1, 
     at = seq(2001, (2002-1/12), (1/12)),
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

lines(lm.pred$mean, col = "cyan3", lwd = 1, lty = 2)
lines(valid.ts$mean, col = "coral3", lwd = 1, lty = 2)
legend(x = "topleft", 
       legend = c("Regression Forecast", "Seasonal Forecast", "Actual Sales"), 
       col = c("cyan3", "coral3", "darkgoldenrod3"),
       lty = c(2, 2, 1), bty = "n")

