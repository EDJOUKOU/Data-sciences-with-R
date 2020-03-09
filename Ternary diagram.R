library(ggplot2)
library(ggtern)
library(ggthemes)
library(reshape2)

TernaryDF <- data.frame(Ternarys)
TernaryDF
T <- ggtern(data = Ternarys, aes(x = Fossil.F, y = Water, z = Biofuels)) + geom_point(aes(fill=c("2017","2020",'2030')),
    size = 7, shape=21) +
  labs(title = "Type of fuels used for Electricity Generation", fill = "Years")+
  theme_rgbw()
T
Tf = T + guides(fill=FALSE)
Tf
Tf + annotate(geom  = 'text',
              x     = Ternarys$Fossil.F,
              y     = Ternarys$Water,
              z     = Ternarys$Biofuels,
              angle = c(-20,-20,-20),
              vjust = c(1.5,0.5,-0.5),
              label = paste("",c("2017","2020","2030")),
              color = c("darkblue","darkred",'goldenrod'))







