install.packages("fpp3")
install.packages("readxl")
# Importarea librariilor 
install.packages("tsibble")
library(tsibble) #furnizeaza functionalitati avansate pentru manipularea, vizualizarea si analiza acestor serii de timp
library(readxl) #permite citirea fisierelor Excel direct in mediu R
# Librariile ffp fac parte dintr-un pachet mai mare numit "Forecasting: Principles and Practice"
# Acest pachet contine un set bogat de functii si metode pentru modelarea, evaluarea si realizarea de prognoze in R
library(fpp3)
library(fpp2)

install.packages("forecast")
library(forecast)
library(fable)
library(fabletools)
library(feasts)
library(tseries)
library(dplyr)
library(tidyr)

date_init <- read_excel("C:/Users/Cristina/Downloads/Rata inflatiei.xlsx")
date_init

d_i <- ts(date_init, start=2002, frequency = 4)

plot(d_i, ylab = "Rata Inflatiei")

install.packages("ggplot2")
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
windows()
autoplot(d_i) +
  labs(title = "Plot Rata Inflatiei",
       y = "Rata inflatiei trimestriala")
#datele au sezonalitate deoarece au aceste cresteri si descresteri

#grafic de sezonalitate

library(forecast)
windows()
ggsubseriesplot(d_i) +
  ylab("%") +
  ggtitle("Grafic sezonalitate") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

#graficul prezinta asadar sezonalitate

install.packages("seasonal")
library(seasonal)
#grafic ajustare sezoniera o serie temporală folosind metoda X-11
d_i_adj <- d_i %>% seas(x11="") 

windows()
autoplot(d_i_adj) +
  ggtitle("X11 descompunerea serii de timp") + theme_bw()

windows()
autoplot(d_i, series="Data") +
  autolayer(trendcycle(d_i_adj), series="Trend") +
  autolayer(seasadj(d_i_adj), series="Seasonally Adjusted") +
  xlab("Trimestre") + ylab("%") +
  ggtitle("Grafic serie desezonalizata") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend")) +
  theme_bw()
 
windows()
d_i_adj %>% seasonal() %>% 
  ggsubseriesplot() + 
  ggtitle("Graficul de sezonalitate pentru componenta sezoniera a metodei X11") +
  ylab("Seasonal") + 
  theme_bw()

# Graficul de sezonalitate a seriei ajustate sezonier
windows()
seasadj(d_i_adj)%>% ggsubseriesplot() +
  ggtitle("Graficul de sezonalitate a seriei ajustate sezonier") +
  ylab("Seasonal") + 
  theme_bw()

# Time series plots
windows()
autoplot(seasadj(d_i_adj)) +
  ggtitle("Evolutia seriei ajustate") +
  xlab("Trimestru") +
  ylab("%") +
  theme_bw()

training <- window(seasadj(d_i_adj), start=2002, end=c(2019,4))
test <- tail(seasadj(d_i_adj), 4*4)

# Corelograma seriei ajustate sezonier (ACF si PACF)
windows()
ggtsdisplay(seasadj(d_i_adj))

#holt liniar simplu
fit <- holt(training,h=12)
summary(fit)
forecast::accuracy(fit,test)
autoplot(fit)

#holt cu trend
fit_trend <- holt(training, h = 12, beta = TRUE)
summary(fit_trend)
forecast::accuracy(fit_trend, test)
autoplot(fit_trend)


#simple exponential smoothing SES
d_i_ses <- window(training, start=2002)
fc <- ses(d_i_ses, h=12)
forecast::accuracy(fc, test)
autoplot(fc)
# Summary forecast
summary(fc)
# Seria valorilor fitted - valorile estimate
fc$model$fitted

# Seria reziduurilor 
fc$model$residuals

# Acuratetea modelului
round(accuracy(fc),2)
# se alege modelul cu cele mai mic erori 
# ME: Mean Error - Eroarea medie
# RMSE: Root Mean Squared Error - eroarea medie patratica
# MAE: Mean Absolute Error - eroarea medie absoluta
# MPE: Mean Percentage Error - eroarea medie procentuala
# MAPE: Mean Absolute Percentage Error - eroarea medie procentuala absoluta
# MASE: Mean Absolute Scaled Error - eroarea medie absoluta scalata

# Prognoza prin SES
fc <- ses(training,h=12)
summary(fc)
forecast::accuracy(fc,test)
autoplot(fc)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Rata inflatiei") + xlab("Trimestru") +
  theme_bw() 

# Corelograma seriei ajustate sezonier
training %>% ggtsdisplay()

## Testarea stationaritatii prin ADF pentru seria nediferentiata

# Trend si intercept
install.packages("magrittr") 
install.packages("urca")
library(urca) 
library(magrittr)
adf_ur <- training %>%
  ur.df(., type='trend', selectlags=c("AIC"))

summary(adf_ur) #nestationara la 99%, stationara la 95%

#intercept
adf_ur <- training %>%
  ur.df(., type='drift', selectlags=c("AIC"))

summary(adf_ur) #nestationara la 99%, stationara la 95%

# Elemente deterministe
adf_ur <- training %>%
  ur.df(., type="none", selectlags=c("AIC"))

summary(adf_ur) # 

# Testarea stationaritatii prin KPSS pentru seria nediferentiata
training %>% ur.kpss() %>% summary() # valoarea testului 1.30002 > toate valorile critice
# seria este nestationara pt ca test cristic> ca toate valorile cristice accept ip alternativa


# Testarea stationaritatii prin Philips-Perron pentru seria nediferentiata
PP.test(training) # p = 0.01 < 0.1 serie stationara

# Testul Phillips-Perron
# H0: seria admite o radacina unitate
# H1: seria nu admite o radacina unitate

windows()
autoplot(training) +
  ggtitle('Rata inflatiei trimestriala pe 21 ani') +
  theme_bw() 

#windows()
#ggAcf(training, lag.max = 100)
#windows()
#ggAcf(diff(training))

#testarea ADF pe seria diferentiata

adf_ur <- diff(training) %>%
  ur.df(., type='trend', selectlags=c("AIC"))

summary(adf_ur) # testul statistic> val critice
#stationara pe toate intervalele

adf_ur <- diff(training) %>%
  ur.df(., type='drift', selectlags=c("AIC"))

summary(adf_ur) # testul statistic> val critice
#stationara pe toate intervalele

adf_ur <- diff(training) %>%
  ur.df(., type='none', selectlags=c("AIC"))

summary(adf_ur) # testul statistic> val critice
#stationara pe toate intervalele

# Testarea stationaritatii prin KPSS pentru seria diferentiata
diff(training) %>% ur.kpss() %>% summary() #daca val testului < val critice
#seria este stationara

# Testarea stationaritatii prin Philips-Perron pentru seria nediferentiata
PP.test(diff(training)) # p = 0.01 < 0.1 serie stationara

ndiffs(training) #ne arata ca seria are nevoie doar de 1 diferentiere
nsdiffs(training) #ne arata ca seria noastra nu are nevoie de diferentiere sezoniera

# Testul Quandt Likelihood Ratio (QLR)

# Pas 1 - cream un obiect tibble care va contine variabila originala (lag0)
# si variabila originala cu un decalar (lag1)
library(tibble)
data_qlr <- tibble(ylag0 = training,
                   ylag1 = lag(as.numeric(training))) 

# Pas 2 - aplicam o regresie falsa in care variabila dependenta este variabila
# cu lagul 0 (variabila originala) si variabila independenta este variabila
# cu 1 decalaj (lag1)
library(fpp2)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
qlr <- Fstats(ylag0 ~ ylag1, data = data_qlr)

# Pasul 3 - estimam punctul in care avem structural change
breakpoints(qlr)

# Pasul 4 - testam semnificatia punctului
# daca p < 0.1 inseamna ca avem structural change semnificativ
sctest(qlr, type = "supF")

# Pasul 5 - reprezentam grafic 
autoplot(training) +
  #geom_vline(xintercept = 164, colour = 'red') + # identificam initial punctul in grafic
  # si aproximam valoarea pentru a trasa linia orizontala 
  geom_hline(yintercept = 1, colour = 'red')+
  ggtitle("Structural break for inflation rate") +
  ylab("Rata inflatiei %") +
  theme_bw()

### Testarea radacinii unitare atunci cand avem structural break ###

# Testul Zivot & Andrews se foloseste atunci cand avem o ruptura endogena in
# structura seriei
# H0: Serie are o radacina unitate cu un structural break
# H1: Serie nu are o radacina unitate cu un structural break

## Testul Zivot-Andrews pentru testarea radacinii unitare atunci cand avem structural break

# Trend si intercept
za_ur <- training %>%
  ur.za(., model="both", lag = 1)

summary(za_ur) # p < 0.1 seria este nestationara cu structural break in trend si intercept
plot(za_ur) # testul ne confirma ca indiferent daca eliminam structural break
# seria tot ramane nestationara 

# Trend
za_ur <- training %>%
  ur.za(., model="trend", lag = 1)

summary(za_ur) # p < 0.1 seria este nestationara cu structural break in trend 
windows()
plot(za_ur) 


# Intercept 
za_ur <- training %>%
  ur.za(., model="intercept", lag = 1)

summary(za_ur) # p < 0.1 seria este nestationara cu structural break in intercept 
plot(za_ur) 

# Putem identifica lagurile maximale pentru AR si MA acum pe baza corelogramei 
# deoarece am stabilit prin testele de stationaritate ca seria primei diferentei 
# este stationara 
windows()
training %>% diff(lag=1) %>% ggtsdisplay()
#Pe PACF identificăm primul lag semnificativ - ordinul maximal AR este 1
#numaram primele n bete care ies in afara liniei punctate pana la primul 
#care se afla inauntrul liniilor
#Pe ACF identificăm primul lag semnificativ - ordinul maximal MA este 1(la fel)

library(lmtest)
fit1 <- Arima(training, order=c(1,1,0), include.constant =TRUE)
coeftest(fit1) # coeficienti neseminificativi
summary(fit1)

fit2 <- Arima(training, order=c(0,1,1),include.constant =TRUE)
coeftest(fit2) # coeficienti neseminificativi
summary(fit2)

fit3 <- Arima(training, order=c(1,1,1), include.constant =TRUE)
coeftest(fit3) # AR(1) nesemnificativ
summary(fit3)

fit4 <- Arima(training, order=c(1,1,2), include.constant =TRUE)
coeftest(fit4) # toti coeficientii semnificativi
summary(fit4)

fit5 <- Arima(training, order=c(2,1,1), include.constant =TRUE)
coeftest(fit5) # coef nesemnificativ, dar potential model
summary(fit5)

fit6 <- Arima(training, order=c(2,1,2), include.constant =TRUE)
coeftest(fit6) # coef nesemnificativ
summary(fit6)

summary(fit4) #AICc = 167.05, BIC = 177.44, RMSE = 0.7129 - ARIMA(1,1,2)
summary(fit5) #AICc = 165.33, BIC 176.64, RMSE = 0.7157 - ARIMA(2,1,1)

#vom alege ARIMA(1,1,2) deoarece are toti coeficientii semnificativi si
#eroarea standard mai mica 

fit1 <- Arima(training, order=c(1,1,2), include.constant =TRUE)
coeftest(fit1)
summary(fit1)

auto.arima(training)

# Testam acuratetea si pe zona de test
fit1_acc<- fit1%>% forecast(h=12) 
summary(fit1_acc)
forecast::accuracy(fit1_acc, test)
#modelul ARIMA(1,1,2) nu este eficient(Theil's U>1, erori reziduale mari)
#nu este eficient si este slab performant in comparatie cu un model naiv
#fapt care ne sugereaza sa alegem un alt model ARIMA
fit1 <- Arima(training, order=c(2,1,1), include.constant =TRUE)
coeftest(fit1)
summary(fit1)

fit1_acc<- fit1%>% forecast(h=12) 
summary(fit1_acc)
forecast::accuracy(fit1_acc, test)
#in urma testarii, modelul ARIMA(2,1,1) are o acuratete mai mare pe datele
#de testare, fiind usor mai bun decat un model naiv

# Prognoza modelului ARIMA(2,1,1)
windows()
fit1 %>% forecast(h=12) %>% autoplot()

# Testarea ipotezelor pe reziduuri
windows()
checkresiduals(fit1)
# Normalitate
library (tseries)
jarque.bera.test(residuals(fit1)) # reziduurile nu sunt distribuite normal
# deoarece p-value < 0.1

# Autocorelarea reziduurilor prin Ljung-Box
#daca batul(lag-ul) iese din afara liniei punctate, inseammna ca avem autocorelare
Box.test(residuals(fit1), lag=1, type = 'Lj') #nu avem autocorelare pt ca p > 0.1
Box.test(residuals(fit1), lag=2, type = 'Lj') #nu avem pt ca p> 0.05
Box.test(residuals(fit1), lag=3, type = 'Lj') #nu avem autocorelare
Box.test(residuals(fit1), lag=4, type = 'Lj') #nu avem autocorelare
Box.test(residuals(fit1), lag=5, type = 'Lj') 
Box.test(residuals(fit1), lag=12, type = 'Lj')
Box.test(residuals(fit1), lag=24, type = 'Lj') 

install.package("FinTS")
library (FinTS)
ArchTest(residuals(fit1), lags = 1) #Nu avem nevoie de arch garch
# reziduurile sunt homoschedastice

#Modelul identificat ARIMA(2,1,1) incalca ipotezele de normalitate, prezinta non-autocorelare si homoschedasticitate
#Modelul este optim

# Pentru a putea compara acuratetea prognozelor vom folosi testul Diebold Mariano
# fit - Holt liniar simplu
# fit1 - model optim identificat cu ajutorul ARIMA(2,1,1)

dm.test(fit1$residuals,fit$residuals, h=1) # pe zona de training, nu exista diferente semnificative
# intre prognozele modelelor deoarece p > 0.1
# acceptam H0
# modelul HW multiplicativ si ETS nu prezinta prognoze cu acuratete diferita
dm.test(fit1_acc$mean,fit$mean, h=1) # pe zona de test, exista diferente semnificative
# intre prognoze deoarece p < 0.1, prin urmare ambele modele pot fi luate in considerare


# Graficul final al prognozei
library(forecast)
library(ggplot2)
library(dplyr)

windows()
autoplot(d_i) +
  autolayer(fit, series="Holt", PI=FALSE) +
  autolayer(fit1_acc, series="ARIMA", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast")) +
  xlab("Time") + ylab(" dolari ") +
  ggtitle("Prognoza finală a modelului ales") +
  theme_bw()

