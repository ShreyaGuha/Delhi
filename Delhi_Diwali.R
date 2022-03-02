#using specific library functions
install.packages("data.table") #installing package
library(data.table) #calling library function

#read data
Delhi_Diwali = read.csv("Delhi_Diwali.csv")

#create new column for date
install.packages("lubridate")
library(lubridate)
Delhi_Diwali$date <- seq(ym('2019-11'), ym('2020-02'), by = "1 month")
Delhi_Diwali$days <- wday(Delhi_Diwali$ï..Date)

#kz filter
install.packages("kza")
library(kza)

#longterm
Delhi_Diwali$kz_PM_annual <- kz(Delhi_Diwali$PM2.5, m = 4, k = 3)
Delhi_Diwali$kz_wind_annual <- kz(Delhi_Diwali$Wind, m = 4, k = 3)
Delhi_Diwali$kz_temp_annual <- kz(Delhi_Diwali$Temp, m = 4, k = 3)
Delhi_Diwali$kz_BOD_annual <- kz(Delhi_Diwali$BOD, m = 4, k = 3)
Delhi_Diwali$kz_P_annual <- kz(Delhi_Diwali$p, m = 4, k = 3)
Delhi_Diwali$kz_NH3_annual <- kz(Delhi_Diwali$NH3, m = 4, k = 3)
Delhi_Diwali$kz_SO4_annual <- kz(Delhi_Diwali$SO4, m = 4, k = 3)
Delhi_Diwali$kz_TDS_annual <- kz(Delhi_Diwali$TDS, m = 4, k = 3)

#longterm, remaining
Delhi_Diwali$PM_LT_rem <- Delhi_Diwali$PM2.5 - Delhi_Diwali$kz_PM_annual
Delhi_Diwali$wind_LT_rem <- Delhi_Diwali$Wind - Delhi_Diwali$kz_wind_annual
Delhi_Diwali$temp_LT_rem <- Delhi_Diwali$Temp - Delhi_Diwali$kz_temp_annual
Delhi_Diwali$BOD_LT_rem <- Delhi_Diwali$BOD - Delhi_Diwali$kz_BOD_annual
Delhi_Diwali$P_LT_rem <- Delhi_Diwali$p - Delhi_Diwali$kz_P_annual
Delhi_Diwali$NH3_LT_rem <- Delhi_Diwali$NH3 - Delhi_Diwali$kz_NH3_annual
Delhi_Diwali$SO4_LT_rem <- Delhi_Diwali$SO4 - Delhi_Diwali$kz_SO4_annual
Delhi_Diwali$TDS_LT_rem <- Delhi_Diwali$TDS - Delhi_Diwali$kz_TDS_annual

#seasonal
Delhi_Diwali$PM_seasonal <- kz(Delhi_Diwali$PM_LT_rem, m = 1, k = 5)
Delhi_Diwali$wind_seasonal <- kz(Delhi_Diwali$wind_LT_rem, m = 1, k = 5)
Delhi_Diwali$temp_seasonal <- kz(Delhi_Diwali$temp_LT_rem, m = 1, k = 5)
Delhi_Diwali$BOD_seasonal <- kz(Delhi_Diwali$BOD_LT_rem, m = 1, k = 5)
Delhi_Diwali$P_seasonal <- kz(Delhi_Diwali$P_LT_rem, m = 1, k = 5)
Delhi_Diwali$NH3_seasonal <- kz(Delhi_Diwali$NH3_LT_rem, m = 1, k = 5)
Delhi_Diwali$SO4_seasonal <- kz(Delhi_Diwali$SO4_LT_rem, m = 1, k = 5)
Delhi_Diwali$TDS_seasonal <- kz(Delhi_Diwali$TDS_LT_rem, m = 1, k = 5)

#short-term
Delhi_Diwali$PM_STM <- Delhi_Diwali$PM_LT_rem - Delhi_Diwali$PM_seasonal
Delhi_Diwali$wind_STM <- Delhi_Diwali$wind_LT_rem - Delhi_Diwali$wind_seasonal
Delhi_Diwali$temp_STM <- Delhi_Diwali$temp_LT_rem - Delhi_Diwali$temp_seasonal
Delhi_Diwali$BOD_STM <- Delhi_Diwali$BOD_LT_rem - Delhi_Diwali$BOD_seasonal
Delhi_Diwali$P_STM <- Delhi_Diwali$P_LT_rem - Delhi_Diwali$P_seasonal
Delhi_Diwali$NH3_STM <- Delhi_Diwali$NH3_LT_rem - Delhi_Diwali$NH3_seasonal
Delhi_Diwali$SO4_STM <- Delhi_Diwali$SO4_LT_rem - Delhi_Diwali$SO4_seasonal
Delhi_Diwali$TDS_STM <- Delhi_Diwali$TDS_LT_rem - Delhi_Diwali$TDS_seasonal

#correlation model
lm1 <- lm(Delhi_Diwali$P_STM ~ Delhi_Diwali$PM_STM + Delhi_Diwali$temp_STM + Delhi_Diwali$wind_STM)
summary(lm1)
plot(lm1)

lm2 <- lm(Delhi_Diwali$P_seasonal ~ Delhi_Diwali$PM_seasonal + Delhi_Diwali$temp_seasonal + Delhi_Diwali$wind_seasonal)
summary(lm2)
plot(lm2)


lm3 <- lm(Delhi_Diwali$NH3_STM ~ Delhi_Diwali$PM_STM + Delhi_Diwali$temp_STM + Delhi_Diwali$wind_STM)
summary(lm3)
plot(lm3)

lm4 <- lm4(Delhi_Diwali$SO4_STM ~ Delhi_Diwali$PM_STM + Delhi_Diwali$temp_STM + Delhi_Diwali$wind_STM)
summary(lm4)
plot(lm4)

lm5 <- lm(Delhi_Diwali$TDS_STM ~ Delhi_Diwali$PM_STM + Delhi_Diwali$temp_STM + Delhi_Diwali$wind_STM)
summary(lm5)
plot(lm5)

#plots

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$P,
     xlab = "Time (months)", ylab = "Phosphorous Avg daily conc",
     main = "Monthly concentration of P (mg/L) over Delhi_Diwali (2019 Nov-2020 Feb)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$P_annual,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi_Diwali (2015-20) after applying long-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$P_seasonal,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi_Diwali (2015-20)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$P_STM,
     xlab = "Time (years)", ylab = "P Avg daily conc",
     main = "Yearly concentration of P over Delhi_Diwali (2015-20) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)




plot(x = Delhi_Diwali$date, y = Delhi_Diwali$PM2.5,
     xlab = "Time (months)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Monthly concentration of PM2.5 over Delhi_Diwali (2019 Nov-2020 Feb)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$P,
     xlab = "Phosphorous Avg daily conc (mg/L)", ylab = "Time (months)",
     main = "Monthly concentration of PM2.5 over Delhi_Diwali (2019 Nov-2020 Feb)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$SO4,
     xlab = "Time (years)", ylab = "SO4 Avg daily conc (ug/m3)",
     main = "Yearly concentration of SO4 over Delhi_Diwali (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$TDS,
     xlab = "Time (years)", ylab = "TDS Avg daily conc",
     main = "Yearly concentration of TDS over Delhi_Diwali (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Delhi_Diwali$date, y = Delhi_Diwali$NH3,
     xlab = "Time (years)", ylab = "NH3 Avg daily conc",
     main = "Yearly concentration of NH3 over Delhi_Diwali (2015-2020)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)
