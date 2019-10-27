
# Advanced Plotting With Base R Functions

long.eruptions <- with(faithful, faithful[eruptions > 3, ])
short.eruptions <- with(faithful, faithful[eruptions < 3, ])

plot(faithful, type = 'n', xlab = 'Time of Eruption (min)', ylab = 'Elapsed Time Between Eruptions (min)', main = 'Old Faithful Geyser Eruption Data')

points(short.eruptions, col = "red", pch = 19)
points(long.eruptions, col = "blue", pch = 19)

short.eruptions.model <- lm(waiting~eruptions, data = short.eruptions)
short.eruptions.line <- as.data.frame(c(min(short.eruptions$eruptions), max(short.eruptions$eruptions)))
colnames(short.eruptions.line) <- c("eruptions")
short.eruptions.line$waiting <- predict(short.eruptions.model, newdata = data.frame(x = short.eruptions.line))
lines(x = short.eruptions.line$eruptions, y = short.eruptions.line$waiting, col = "red")

short.eruptions.trendline.equation <- paste('y =', round(coef(short.eruptions.model)[[2]], 2), '* x', '+', round(coef(short.eruptions.model)[[1]], 2))
short.eruptions.r.squared <- as.character(round(summary(short.eruptions.model)$r.squared, 2))
short.eruptions.text <- bquote(atop(textstyle('Short Eruptions'), atop(textstyle(.(short.eruptions.trendline.equation)), textstyle(r^2 == .(short.eruptions.r.squared)))))
text(mean(c(min(short.eruptions$eruptions), max(short.eruptions$eruptions))), mean(c(max(short.eruptions$waiting), (par('usr')[4]))), short.eruptions.text, col = 'red')

long.eruptions.model <- lm(waiting~eruptions, data = long.eruptions)
long.eruptions.line <- as.data.frame(c(min(long.eruptions$eruptions), max(long.eruptions$eruptions)))
colnames(long.eruptions.line) <- c("eruptions")
long.eruptions.line$waiting <- predict(long.eruptions.model, newdata = data.frame(x = long.eruptions.line))
lines(x = long.eruptions.line$eruptions, y = long.eruptions.line$waiting, col = "blue")

long.eruptions.trendline.equation <- as.character(paste('y =', round(coef(long.eruptions.model)[[2]], 2), '* x', '+', round(coef(long.eruptions.model)[[1]], 2)))
long.eruptions.r.squared <- as.character(round(summary(long.eruptions.model)$r.squared, 2))
long.eruptions.text <- bquote(atop(textstyle('Long Eruptions'), atop(textstyle(.(long.eruptions.trendline.equation)), textstyle(r^2 == .(long.eruptions.r.squared)))))
text(mean(c(min(long.eruptions$eruptions), max(long.eruptions$eruptions))), mean(c(min(long.eruptions$waiting), (par('usr')[3]))), long.eruptions.text, col = 'blue')

#mean(c(min(short.eruptions$eruptions), max(short.eruptions$eruptions))) # x-coordinate of red text
#mean(c(min(long.eruptions$eruptions), max(long.eruptions$eruptions))) # x-coordinate of blue text
#mean(c(max(short.eruptions$waiting), (par('usr')[4]))) # y-coordinate of red text
#mean(c(min(long.eruptions$waiting), (par('usr')[3]))) # y-coordinate of blue text
