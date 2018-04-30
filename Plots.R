library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

start <- as.Date("2007-01-01")
end <- as.Date("2018-04-13")

getSymbols("AMZN", src = "yahoo", from = start, to = end)
AMZN<-data.frame(as.matrix(AMZN))
getSymbols("FB", src = "yahoo", from = start, to = end)
FB<-data.frame(as.matrix(FB))
getSymbols("GOOGL", src = "yahoo", from = start, to = end)
GOOGL<-data.frame(as.matrix(GOOGL))

library(data.table)
AMZN<-setDT(AMZN, keep.rownames = TRUE)[]
AMZN$Date<-as.Date(AMZN$rn)
AMZN$rn<-NULL
FB<-setDT(FB, keep.rownames = TRUE)[]
FB$Date<-as.Date(FB$rn)
FB$rn<-NULL
GOOGL<-setDT(GOOGL, keep.rownames = TRUE)[]
GOOGL$Date<-as.Date(GOOGL$rn)
GOOGL$rn<-NULL
names(AMZN) <- sub(".*\\.", "", names(AMZN))
names(GOOGL) <- sub(".*\\.", "", names(GOOGL))
names(FB) <- sub(".*\\.", "", names(FB))

library(sqldf)
union<-sqldf("select a.Date,a.close as Amazon,b.close as Facebook,c.close as Google from AMZN a left join FB b on a.Date=b.Date left join GOOGL c on a.Date=c.Date")

library(tidyr)
library(dplyr)
df <- union %>%
  select(Date, Amazon, Facebook,Google) %>%
  gather(key = "Stock_name", value = "Stock_price", -Date)
head(df, 3)

df1<-df[complete.cases(df),]

colourCount = length(unique(df1$variable))
getPalette = colorRampPalette(brewer.pal(3, "Set1"))

p<-ggplot(df1[df1$Stock_name=="Amazon",], aes(x = Date, y = Stock_price)) + 
  geom_area(aes(color = Stock_name, fill = Stock_name), 
            alpha = 0.8, position = position_dodge(0.8)) +
  scale_color_manual(values =c("#00AFBB")) +
  scale_fill_manual(values =c("#00AFBB")) + ggtitle("Plot of Amazon stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.justification=c(0.1,.9), legend.position=c(0.1,.9)
)

p<-ggplot(df1[df1$Stock_name=="Facebook",], aes(x = Date, y = Stock_price)) + 
  geom_area(aes(color = Stock_name, fill = Stock_name), 
            alpha = 0.8, position = position_dodge(0.8)) +
  scale_color_manual(values =c("#E7B800")) +
  scale_fill_manual(values =c("#E7B800")) + ggtitle("Plot of Facebook stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.justification=c(0.1,.9), legend.position=c(0.1,.9)
)

df1$year<-as.character(year(df1$Date))
p<-ggplot(df1[df1$Stock_name=="Google",], aes(x = Date, y = Stock_price)) + 
  geom_area(aes(color = Stock_name, fill = Stock_name), 
            alpha = 0.8, position = position_dodge(0.8)) +
  scale_color_manual(values =c("#60FF60")) +
  scale_fill_manual(values =c("#60FF60")) + ggtitle("Plot of Google stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.justification=c(0.1,.9), legend.position=c(0.1,.9)
)




p<-ggplot(df1[df1$Stock_name=="Google",], aes(x=year, y=Stock_price, color=year)) +
  geom_boxplot()+ ggtitle("Boxplot of Google stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.position="none"
)

p<-ggplot(df1[df1$Stock_name=="Facebook",], aes(x=year, y=Stock_price, color=year)) +
  geom_boxplot()+ ggtitle("Boxplot of Facebook stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.position="none"
)


p<-ggplot(df1[df1$Stock_name=="Amazon",], aes(x=year, y=Stock_price, color=year)) +
  geom_boxplot()+ ggtitle("Boxplot of Amazon stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.position="none"
)


# Multiple line plot
p<-ggplot(df1, aes(x = Date, y = Stock_price)) + 
  geom_line(aes(color = Stock_name), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#60FF60")) +
  theme_minimal()+ggtitle("Curve of all stock price") +
  xlab("Year") + ylab("Stock Price")

p + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  legend.title = element_text(colour="black", size=12, face="plain"),
  legend.text = element_text(colour="black", size = 12, face = "plain"),
  legend.background = element_rect(fill="white", size=.5, linetype="dotted"),
  legend.justification=c(0.1,.9), legend.position=c(0.1,.9)
)

