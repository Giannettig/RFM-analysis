#http://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/
#https://www.r-bloggers.com/rfm-customer-analysis-with-r-language/

install.packages("StatMeasures")

library(tidyverse)
library(lubridate)
library(StatMeasures)
library(scales)


# Data normalization ------------------------------------------------------

# The scope of this section is to ensure that I can change the data from different sources using the same script. 

# Datová struktura - vstup
#   Pozadavky na data:
#   eshop
# 1. ID online zakaznika = hash emailu
# 2. ID objednavky
# 3. Datum objednavky
# 4. Hodnota objednavky bez DPH
# 5. Status objednavky (zda byla reklamovana ci ne)
# 
# TPOMM (jen transkace zakaniku u kterych evidujeme email)
# 1. ID zakznika = has emailu
# 2. ID uctenky
# 3. Datum uctenky
# 4. Hodnota uctenky bez DPH
# 5. Pripadne pokud lze status uctenky (zda byla reklamovana)

# Datová struktura - vystup
# 1) ID zákazníka (custId)
# 2) ID objednávky (orderId)
# 3) datum objednaávky (dateSold)
# 4) hodnota objednávky (monetary)
# 5) status objednávky (status)
# 6) kanál objednávky (salesChannel)
# 7) dny od konce casove osy (recency)
# 8) dny od prvniho nakupu (timeline)

input<-sample%>%
       group_by(`Order ID`,`Order Date`,`Customer Name`,`Ship Mode`)%>%
       summarise(monetary=sum(Sales))%>%
       rename(orderId=`Order ID`, dateSold=`Order Date`, custId=`Customer Name`,channel=`Ship Mode`)%>%
       mutate(status=channel)%>%ungroup
       

#Odsud by se vsechno melo pocitat dynamicky

endDate<-input$dateSold%>%ymd%>%max

data<-input%>%
      group_by(custId)%>%
      mutate(timeline=as.numeric(ymd(dateSold)-min(ymd(dateSold))),
             recency=as.numeric(endDate-ymd(dateSold)))%>%
      arrange(custId,desc(dateSold))%>%
      mutate(lag=as_date(lag(dateSold))-as_date(dateSold))

#Podívám se na distribuci dat, abych se ujistil že tam není žádný problém. 
library(ggplot2)
library(gridExtra)
library(ggpubr)

  


tml<-ggplot(data, aes(x=timeline, fill=status))+
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightgray")+  geom_density(alpha=.5, fill="lightblue")+ ylab("Počet objednávek") + xlab("Dny od prvni objednavky")
rec<-ggplot(data, aes(x=recency))+
  geom_histogram(aes(y=..density..),color="darkblue", fill="#E69F00")+ ylab("Počet objednávek") + xlab("Dny od posledni objednavky")+ geom_density(alpha=.5, fill="lightblue")
lag<-ggplot(data, aes(x=lag))+
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightgreen")+ ylab("Počet objednávek") + xlab("Interval mezi objednavkami")+ geom_density(alpha=.5, fill="lightblue")

ggarrange(tml,rec,lag, labels=c("Timeline","Recency"),ncol=3,nrow=1)

rm(tml,rec)

#koukneme na seasonalitu v datech 

ds<-data%>%group_by(dateSold)%>%tally

dsw<-data%>%group_by(week(dateSold),channel)%>%tally

ggplot(dsw, aes(`week(dateSold)`, n, colour=channel)) + geom_line() + ggtitle("Seasonality")

monthOrder <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
ds$Month <- factor(format(ds$dateSold, "%B"))
ggplot(ds, aes(Month, n)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + ggtitle("Sezónní výkyvy v Prodejích")

rm(ds,dsw)

# Distribuce monetary values u zákazníků - ujistím se že je stejná jak v prodejně tak v eshopu. 

#kumulativní distribuce

cust<-data%>%group_by(custId)%>%summarize(OrderSum=sum(monetary),Recency=min(recency, na.rm= TRUE),Orders=n())%>%ungroup

ggplot(cust, aes(x=OrderSum))+
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue", bins=50)+ ylab("Počet zákazníků") + xlab("Celková hodnota objednávek")+geom_density(alpha=.5, fill="yellow")


# Automatická kalkulace segmentů podle decilů -----------------------------


cust$Monetary<-rescale(cust$OrderSum,c(0,100))
cust$Recency<-rescale(cust$Recency,c(100,0))
cust$Frequency<-rescale(cust$Orders,c(0,100))

library(plotly)

plot_ly(cust, x = ~Monetary, y = ~Recency, z = ~Recency, color = ~Orders, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Monetary'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Recency')))


# Lifegrid ----------------------------------------------------------------





