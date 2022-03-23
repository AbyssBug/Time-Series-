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

