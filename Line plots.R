library(ggplot2)
library(reshape2)
library(ggrepel)

# Electricity access rate
AccessDF <- data.frame(AccessDF1)
AccessDF
AccessMelt <- melt(AccessDF,id="years")
AccessMelt
E <- ggplot(data = AccessMelt, aes(x = years, y = value, color = variable)) + geom_line(size=4) + xlab("Years") + ylab("Electricity access rate (%)")
E
E <- E + labs(title = "Electricity access rate")
E
E <- E + theme_bw()

theme_text <- theme(plot.title = element_text(lineheight=.8, face="bold",size = 28)) + theme(text = element_text(size=25))

E <- E + theme_text
E
g <- E + theme(axis.text.x = element_text(face="bold", color="black", 
                                          size=20, angle=0),
               axis.text.y = element_text(face="bold", color="black", 
                                          size=20, angle=0))
k <- E + theme_classic()
k <- k + theme_text1
k

