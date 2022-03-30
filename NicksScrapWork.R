library(ggplot2)
library(patchwork)
attach(data)

hist.data.frame(data[,4:12])
hist.data.frame(data[,13:21])
hist.data.frame(data[,c(22:27,31,34)])
