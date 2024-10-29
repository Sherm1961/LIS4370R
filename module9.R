#install.packages("lattice")
library(ggplot2)
library(lattice)

data<-read.csv("Electricity1970.csv")

#basic plotting with no packages
cost <- data$cost
plot(cost)

plot(cost, type="o", col="blue")
title(main="Electricity Cost", col.main="red", font.main = 3)


#lattice technique
tplot <- xyplot(cost~output, data=data, pch=".",
                panel = function(x, y, ...) {
                  panel.xyplot(x, y, cex = 2.5,...)
                  panel.lmline(x, y, col = "red", lwd = 2)  # Adds linear trend line
                })
print(tplot)

#ggplot technique 
ggplot(data, aes(x = cost, y = output, color = 2)) +
  geom_point(size = 2, show.legend = TRUE) +
  geom_smooth(method = "loess", show.legend = FALSE) +
  labs(title = "Electricity Output with Legend", x = "Cost", y = "Electricity Output")




























