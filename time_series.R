View(sections)

#filter only from 2014 and later

mydata <- filter(sections, Month > "2014-01-01") %>% droplevels

mydata <- filter(mydata, Section == "Thrombophilia") %>% droplevels
mydata <- select(mydata, 3)

ts.mydata <- ts(data=mydata, start = 2014, frequency = 12)
plot(ts.mydata)
dec.mydata <- decompose(ts.mydata, "additive")
plot(dec.mydata)

# adjust graph to remove the trend element
plot(dec.mydata$trend, xlab = "Year", ylab = "Workload")

adj.mydata <- mydata - dec.mydata$trend
