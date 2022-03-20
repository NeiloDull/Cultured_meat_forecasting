#Clear R
install.packages("ggplot2")
install.packages("ggbeeswarm")

rm(list=ls())
library(readstata13)

CM <- read.dta13("~/Downloads/swarm.dta")
table(metric_tons)
CM$metric_tons<-as.factor(CM$metric_tons)
print(levels(metric_tons))
CM$metric_tons<- factor(CM$metric_tons, levels = c(">100,000",  ">1M"  ,  ">10M" ,     ">50M"  ))
CM$metric_tons


table(metric_tons)
print(levels(metric_tons))
attach(CM)

# filter dataframe to get data to be highligheted

library(ggplot2)

library(ggbeeswarm)
library(scales)
library(stringr)
# Basic beeswarm plot in ggplot2
my_title = "Probabilities for metric tons of cultured meat that will be sold at any price within a continuous 12-month span before the end of the year"

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p<-ggplot(CM, aes(x = as.factor(year), y = probability,color = metric_tons)) +
  geom_beeswarm(priority='density', cex=2, size =3,dodge.width = .5)+
  theme_classic(base_size = 22) +
  scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1),
                     label = scales::label_percent(accuracy = 1L) ) +
  labs( y = "Probability", x = "")+
  scale_colour_manual("Metric Tons", values=cbbPalette)+
  labs(title = str_wrap(my_title, 60))+
  theme(legend.position = c(0.13, 0.8), legend.background = element_rect(
                                         size=0.5, linetype="solid", 
                                         colour ="black"))
p



