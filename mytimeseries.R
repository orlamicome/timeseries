#loading the library
library(tidyverse)
library(forecast)
library(tseries)
library(xts)

cereal_prod <- read_csv('cereal_data.csv')
cereal_prod %>% head()

cereal_prod %>% spec()
glimpse(cereal_prod)




#Variable Selection
cereal_prod<-
  cereal_prod %>%
  select(Time, `Cereal production (metric tons) [AG.PRD.CREL.MT]`) %>%
  rename(cereal_production = `Cereal production (metric tons) [AG.PRD.CREL.MT]`)

cereal_prod %>% head()

as.numeric(cereal_prod$cereal_production)

cereal_prod<-
  cereal_prod %>%
  mutate_at('cereal_production', as.numeric) %>%
  na.omit()

str(cereal_prod)

## Convert Data to Time Series Object
date <- seq(from = as.Date('1961/1/1'),
            to = as.Date('2020/1/1'),
            by = 'years')
cereal.xts <- xts(cereal_prod$cereal_production, date)
autoplot(cereal.xts)



###TIME SERIES COMPONENT
#Trend
#Sesonaloity
#Cyclic
#Random Error

autoplot(cereal.xts) +
  geom_line(color='blue') +
  labs(x='Years', y='Cereal Production (MT)', title = 'Time Series Plot')+
  theme_bw() +
  theme(axis.title.x = element_text(size=20, face='bold'),
        axis.text.x = element_text(size=16, face='bold', color = 'black'),
        axis.title.y = element_text(size=13, face='bold'),
        axis.text.y = element_text(size=11, face='bold', color = 'black'),
        plot.title = element_text(hjust = 0.5))


#ARIMA

ndiffs(cereal.xts)
dcereals<-diff(cereal.xts)

autoplot(dcereals)

adf.test(cereal.xts)

adf.test(na.omit(dcereals))
autoplot(Acf(dcereals))

autoplot(Pacf(dcereals))

fit<-auto.arima(cereal.xts)
fit

forecast(fit, 5)
accuracy(fit)



checkresiduals(fit) #Goodness of fit of ARIMA Model

jarque.bera.test(fit$residuals)

autoplot(forecast(fit, 5)) + labs(x='Year', y='Cereal Production')

