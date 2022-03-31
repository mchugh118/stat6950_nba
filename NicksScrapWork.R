library(corrplot) #for visualization of correlation
library(lattice) #for visualization
library(ggplot2) #for visualization
library(patchwork) #for gridding ggplots
library(caTools) #for splittind data into testing and training data
library(dplyr) #manipulating dataframe
library(plotly) #converting ggplot to plotly

attach(data)

str(data)
summary <- summary(data)
num.medians <- apply(data[sapply(data, is.numeric)],2,median, na.rm = TRUE)
num.means <- colMeans(data[sapply(data, is.numeric)], na.rm = TRUE) 
skews <- abs(num.means-num.medians)/abs(num.means)
skews>0.1

p1 <- ggplot(data, aes(x=G)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p2 <- ggplot(data, aes(x=GS)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p3 <- ggplot(data, aes(x=FTr)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p4 <- ggplot(data, aes(x=ORB.)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p5 <- ggplot(data, aes(x=TRB.)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p6 <- ggplot(data, aes(x=AST.)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p7 <- ggplot(data, aes(x=BLK.)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p8 <- ggplot(data, aes(x=OWS)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p9 <- ggplot(data, aes(x=DWS)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p10 <- ggplot(data, aes(x=WS)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p11 <- ggplot(data, aes(x=BPM)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p12 <- ggplot(data, aes(x=VORP)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p13 <- ggplot(data, aes(x=SALARY)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)
p14 <- ggplot(data, aes(x=PTS)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=4)

boxplots14 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + plot_layout(ncol = 4)

p1 <- ggplot(data, aes(x=G)) + 
  geom_histogram(color="blue", fill="blue", binwidth=1)
p2 <- ggplot(data, aes(x=GS)) + 
  geom_histogram(color="blue", fill="blue", binwidth=1)
p3 <- ggplot(data, aes(x=FTr)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.02)
p4 <- ggplot(data, aes(x=ORB.)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.25)
p5 <- ggplot(data, aes(x=TRB.)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.5)
p6 <- ggplot(data, aes(x=AST.)) + 
  geom_histogram(color="blue", fill="blue", binwidth=1)
p7 <- ggplot(data, aes(x=BLK.)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.2)
p8 <- ggplot(data, aes(x=OWS)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.25)
p9 <- ggplot(data, aes(x=DWS)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.1)
p10 <- ggplot(data, aes(x=WS)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.5)
p11 <- ggplot(data, aes(x=BPM)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.5)
p12 <- ggplot(data, aes(x=VORP)) + 
  geom_histogram(color="blue", fill="blue", binwidth=0.25)
p13 <- ggplot(data, aes(x=SALARY)) + 
  geom_histogram(color="black", fill="blue", binwidth=250000)
p14 <- ggplot(data, aes(x=PTS)) + 
  geom_histogram(color="black", fill="blue", binwidth=50)

histograms14 <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 + plot_layout(ncol = 4)

corrplot(cor(data[sapply(data, is.numeric)]))


      