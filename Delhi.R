#using specific library functions
install.packages("data.table") #installing package
library(data.table) #calling library function

#read data
Delhi = read.csv("Delhi.csv")

#create new column for date
install.packages("lubridate")
library(lubridate)
Delhi$date <- seq(ym('2015-01'), ym('2020-02'), by = "1 month")
Delhi$days <- wday(Delhi$date)
Delhi$foam <- Delhi$Temp*Delhi$Wind*((Delhi$P*0.1)+ (Delhi$NH3*0.34) + (Delhi$SO4*0.33) + (Delhi$PM2.5*0.1))

#basic correlation
Delhi_numeric <- subset(Delhi, select = -c(ï..Date, date)) #subseting to remove non-numeric column
cor(Delhi_numeric)
plot(Delhi_numeric)

lm_raw <- lm(Delhi$TDS ~ Delhi$foam + Delhi$Temp + Delhi$Wind)
summary(lm_raw)
plot(lm_raw)

lm_raw_foam <- lm(Delhi$foam ~ Delhi$Temp + Delhi$PM2.5 + Delhi$P + Delhi$SO4 + Delhi$NH3 + Delhi$Wind)
summary(lm_raw_foam)

lm_BOD <- lm(Delhi$BOD ~ Delhi$NH3 + Delhi$TDS + Delhi$foam + Delhi$SO4 + Delhi$P + Delhi$PM2.5)
summary(lm_BOD)



#kz filter
install.packages("kza")
library(kza)

#longterm
Delhi$kz_PM_annual <- kz(Delhi$PM2.5, m = 13, k = 3)
Delhi$kz_wind_annual <- kz(Delhi$Wind, m = 13, k = 3)
Delhi$kz_temp_annual <- kz(Delhi$Temp, m = 13, k = 3)
Delhi$kz_BOD_annual <- kz(Delhi$BOD, m = 13, k = 3)
Delhi$kz_P_annual <- kz(Delhi$P, m = 13, k = 3)
Delhi$kz_NH3_annual <- kz(Delhi$NH3, m = 13, k = 3)
Delhi$kz_SO4_annual <- kz(Delhi$SO4, m = 13, k = 3)
Delhi$kz_TDS_annual <- kz(Delhi$TDS, m = 13, k = 3)
Delhi$kz_foam_annual <- kz(Delhi$foam, m = 13, k = 3)

#Annual_relationships
Delhi_annual <- subset(Delhi, select = c(kz_PM_annual, kz_wind_annual, kz_BOD_annual, kz_P_annual, 
                                         kz_NH3_annual, kz_SO4_annual, kz_TDS_annual, kz_temp_annual,
                                         kz_foam_annual))
cor(Delhi_annual)
plot(Delhi_annual)

lm_annual <- lm(Delhi$kz_TDS_annual ~ Delhi$kz_foam_annual + Delhi$Temp + Delhi$Wind)
summary(lm_annual)
plot(lm_annual)

lm_annual_foam <- lm(Delhi$kz_foam_annual ~ Delhi$kz_temp_annual + Delhi$kz_PM_annual + 
                             Delhi$kz_P_annual + Delhi$kz_SO4_annual + Delhi$kz_NH3_annual + 
                             Delhi$kz_wind_annual)
summary(lm_annual_foam)

lm_BOD <- lm(Delhi$kz_BOD_annual ~ Delhi$kz_NH3_annual + Delhi$kz_TDS_annual + Delhi$kz_foam_annual + Delhi$kz_SO4_annual
             + Delhi$kz_P_annual + Delhi$kz_PM_annual)
summary(lm_BOD)



#longterm, remaining
Delhi$PM_LT_rem <- Delhi$PM2.5 - Delhi$kz_PM_annual
Delhi$wind_LT_rem <- Delhi$Wind - Delhi$kz_wind_annual
Delhi$temp_LT_rem <- Delhi$Temp - Delhi$kz_temp_annual
Delhi$BOD_LT_rem <- Delhi$BOD - Delhi$kz_BOD_annual
Delhi$P_LT_rem <- Delhi$P - Delhi$kz_P_annual
Delhi$NH3_LT_rem <- Delhi$NH3 - Delhi$kz_NH3_annual
Delhi$SO4_LT_rem <- Delhi$SO4 - Delhi$kz_SO4_annual
Delhi$TDS_LT_rem <- Delhi$TDS - Delhi$kz_TDS_annual
Delhi$foam_LT_rem <- Delhi$foam - Delhi$kz_foam_annual

#seasonal
Delhi$PM_seasonal <- kz(Delhi$PM_LT_rem, m = 3, k = 1)
Delhi$wind_seasonal <- kz(Delhi$wind_LT_rem, m = 3, k = 1)
Delhi$temp_seasonal <- kz(Delhi$temp_LT_rem, m = 3, k = 1)
Delhi$BOD_seasonal <- kz(Delhi$BOD_LT_rem, m = 3, k = 1)
Delhi$P_seasonal <- kz(Delhi$P_LT_rem, m = 3, k = 1)
Delhi$NH3_seasonal <- kz(Delhi$NH3_LT_rem, m = 3, k = 1)
Delhi$SO4_seasonal <- kz(Delhi$SO4_LT_rem, m = 3, k = 1)
Delhi$TDS_seasonal <- kz(Delhi$TDS_LT_rem, m = 3, k = 1)
Delhi$foam_seasonal <- kz(Delhi$foam_LT_rem, m = 3, k = 1)

#seasonal_relationships
Delhi_seasonal <- subset(Delhi, select = c(PM_seasonal, wind_seasonal, BOD_seasonal, P_seasonal, 
                                           NH3_seasonal, SO4_seasonal, TDS_seasonal, temp_seasonal,
                                           foam_seasonal))
cor(Delhi_seasonal)
plot(Delhi_seasonal)



lm_seasonal <- lm(Delhi$TDS_seasonal ~ Delhi$foam_seasonal + Delhi$Temp + Delhi$Wind)
summary(lm_seasonal)
plot(lm_seasonal)

lm_seasonal_foam <- lm(Delhi$foam_seasonal ~ Delhi$temp_seasonal + Delhi$PM_seasonal + 
                               Delhi$P_seasonal + Delhi$SO4_seasonal + Delhi$NH3_seasonal + 
                               Delhi$wind_seasonal)
summary(lm_seasonal_foam)

lm_BOD <- lm(Delhi$BOD_seasonal ~ Delhi$NH3_seasonal + Delhi$TDS_seasonal + Delhi$foam_seasonal
              +Delhi$PM_seasonal + Delhi$P_seasonal + Delhi$SO4_seasonal)
summary(lm_BOD)



#short-term
Delhi$PM_STM <- Delhi$PM_LT_rem - Delhi$PM_seasonal
Delhi$wind_STM <- Delhi$wind_LT_rem - Delhi$wind_seasonal
Delhi$temp_STM <- Delhi$temp_LT_rem - Delhi$temp_seasonal
Delhi$BOD_STM <- Delhi$BOD_LT_rem - Delhi$BOD_seasonal
Delhi$P_STM <- Delhi$P_LT_rem - Delhi$P_seasonal
Delhi$NH3_STM <- Delhi$NH3_LT_rem - Delhi$NH3_seasonal
Delhi$SO4_STM <- Delhi$SO4_LT_rem - Delhi$SO4_seasonal
Delhi$TDS_STM <- Delhi$TDS_LT_rem - Delhi$TDS_seasonal
Delhi$foam_STM <- Delhi$foam_LT_rem - Delhi$foam_seasonal

#Short-term_relationships
Delhi_STM <- subset(Delhi, select = c(PM_STM, wind_STM, BOD_STM, P_STM, 
                                      NH3_STM, SO4_STM, TDS_STM, temp_STM,
                                      foam_STM))
cor(Delhi_STM)
plot(Delhi_STM)

lm_STM <- lm(Delhi$TDS_STM ~ Delhi$foam_STM + Delhi$Temp + Delhi$Wind +Delhi$days)
summary(lm_STM)
plot(lm_STM)

lm_STM_foam <- lm(Delhi$foam_STM ~ Delhi$temp_STM + Delhi$PM_STM + 
                          Delhi$P_STM + Delhi$SO4_STM + Delhi$NH3_STM + 
                          Delhi$wind_STM)
summary(lm_STM_foam)

lm_BOD <- lm(Delhi$BOD_STM ~ Delhi$NH3_STM + Delhi$TDS_STM + Delhi$foam_STM + Delhi$P_STM + Delhi$SO4_STM + Delhi$PM_STM)
summary(lm_BOD)

#correlation model
lm1 <- lm(Delhi$P_STM ~ Delhi$PM_STM + Delhi$temp_STM + Delhi$wind_STM)
summary(lm1)
plot(lm1)

lm11 <- lm(Delhi$P_STM ~ Delhi$temp_STM + Delhi$wind_STM)
summary(lm11)
plot(lm11)

lm2 <- lm(Delhi$P_seasonal ~ Delhi$PM_seasonal + Delhi$temp_seasonal + Delhi$wind_seasonal)
summary(lm2)
plot(lm2)

lm21 <- lm(Delhi$P_seasonal ~ Delhi$temp_seasonal + Delhi$wind_seasonal)
summary(lm21)
plot(lm21)


lm3 <- lm(Delhi$NH3_STM ~ Delhi$PM_STM + Delhi$temp_STM + Delhi$wind_STM)
summary(lm3)
plot(lm3)

lm4 <- lm(Delhi$SO4_STM ~ Delhi$PM_STM + Delhi$temp_STM + Delhi$wind_STM)
summary(lm4)
plot(lm4)

lm5 <- lm(Delhi$TDS_STM ~ Delhi$PM_STM + Delhi$temp_STM + Delhi$wind_STM)
summary(lm5)
plot(lm5)

lm6 <- lm(Delhi$PM_STM ~ Delhi$temp_STM + Delhi$wind_STM)
summary(lm6)
plot(lm6)

lm7 <- lm(Delhi$P ~ Delhi$PM2.5 + Delhi$Temp + Delhi$Wind)
summary(lm7)
plot(lm7)


#plots

plot(x = Delhi$date, y = Delhi$P,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of total Phosphorous (mg/L) over Delhi (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi$date, y = Delhi$P_annual,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi (2015-20) after applying long-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid", las = 1)

plot(x = Delhi$date, y = Delhi$P_seasonal,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi (2015-20)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)

plot(x = Delhi$date, y = Delhi$P_STM,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi (2015-20) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)



plot(x = Delhi$date, y = Delhi$PM_seasonal,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc",
     main = "Yearly concentration of PM2.5 over Delhi (2015-20)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)


plot(x = Delhi$date, y = Delhi$PM_STM,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Delhi (2015-20) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)


plot(x = Delhi$date, y = Delhi$PM2.5,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Delhi (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi$date, y = Delhi$SO4,
     xlab = "Time (years)", ylab = "SO4 Avg daily conc (ug/m3)",
     main = "Yearly concentration of SO4 over Delhi (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi$date, y = Delhi$TDS,
     xlab = "Time (years)", ylab = "TDS Avg daily conc",
     main = "Yearly concentration of TDS over Delhi (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi$date, y = Delhi$NH3,
     xlab = "Time (years)", ylab = "NH3 Avg daily conc",
     main = "Yearly concentration of NH3 over Delhi (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)


cor(Delhi$P, Delhi$PM2.5)



