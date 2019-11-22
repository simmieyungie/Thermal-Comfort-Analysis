#Trend analysis
#Temperature
#Filter and summarise for monthly data
T_decade_month <- project %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y"))%>% 
  group_by(site, month) %>% 
  select(-year) %>% 
  summarise_each(funs(mean))

#sumarise data for trend in variables over the years
Yearly_average <- project %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(site, year) %>% 
  summarise_each(funs(mean))


#Plot trend for temperature monthly
qplot(data = T_decade_month, x = month, y = temp,
      color = site, 
      main = "Mean Monthly Temperature (1989 - 2018)") + 
  geom_line(aes(group = site)) + 
  xlab("Time(Month)") +
  ylab("Temperature(°C)") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))




#plot temperature trend yearly
qplot(data = Yearly_average, x = year, y = temp,
      color = site,
      main = "Mean Yearly Temperature (1989 - 2018)")+
  geom_line(aes(group = site)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Year") +
  ylab("Temperature(°C)") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))


#For relative Humidity
qplot(data = Yearly_average, x = year, y = RH,
      color = site,
      main = "Yearly Mean Relative Humidity (%) (1989  2018)") +
  geom_line(aes(group = site)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Year") +
  ylab("RH(%)") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))



#Plot humidity trend monthly
qplot(data = T_decade_month, x = month, y = RH,
      color = site, 
      main = "Monthly Mean RH (%) (1989 - 2018)") + 
  geom_line(aes(group = site)) + 
  xlab("Time(Month)") +
  ylab("RH(%)") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))


#THI trend yearly
qplot(data = Yearly_average, x = year, y = comfort,
      color = site,
      main = "Yearly Mean THI (°C) (1989 - 2018)")+
  geom_line(aes(group = site)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Year") +
  ylab("THI(°C)")+
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))


#THI monthly
#plot comfort montly
qplot(data = T_decade_month, x = month, y = comfort,
      color = site, 
      main = "Monthly Mean THI (°C) (1989 - 2018)") + 
  geom_line(aes(group = site)) + 
  xlab("Time(Month)") +
  ylab("THI(°C)") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))


#Discomfort and heating days i.e THI >= 27
#monthly peaks over the years 
project_Peak_monthly <- project %>% 
  filter(comfort >= 27) %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  group_by(site, month) %>% 
  summarise(n = n())

#plot of monthly peaks over the years
peak_monthly <- ggplot(data = project_Peak_monthly, aes(month, n)) +
  geom_bar(aes(fill = site), position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  xlab("Time(Month)") +
  ylab("Days") +
  ggtitle("Months with the Highest Discomfort Days") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold"))

peak_monthly


#Yearly discomfort trend
##Filter for extreme discomfort events with THI >= 27
project_Peak <- project %>% 
  filter(comfort >= 27) %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  group_by(site, year) %>% 
  summarise(n = n()) 


#Plot the project peak
ggplot(data = project_Peak, aes(year, n, color = site)) + geom_point(size = 2) +
  geom_line(size = 1, aes(group = site)) + geom_abline(color = "#032535", size = 1) + xlab("Year") +
  ylab("Heating Days") + ggtitle("Heating Days (THI >= 27°C)") +
  theme(axis.text.x = element_text(angle = 30),legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))




#Comfort correlation trends
project %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(site, year) %>% 
  summarise_each(funs(mean)) %>% 
  group_by(site) %>% 
  mutate(Year = as.numeric(year)) %>% 
  select(-year) %>% 
  summarise(correla = cor(Year, comfort, method = "spearman"))


#Humidity correlation trend  
project %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(site, year) %>% 
  summarise_each(funs(mean)) %>% 
  group_by(site) %>% 
  mutate(Year = as.numeric(year)) %>% 
  select(site, Year, RH) %>% 
  group_by(site) %>% 
  summarise(correla = cor(Year, RH, method = "spearman"))

#Temperature correlation trend
project %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(site, year) %>% 
  summarise_each(funs(mean)) %>% 
  group_by(site) %>% 
  mutate(Year = as.numeric(year)) %>% 
  select(-year)%>% 
  summarise(correla = cor(Year, temp, method = "spearman"))

#Heating days >= 27 trend
#examine the trend in the data by group
#convert year column to numeric
project_Peak$year <- as.numeric(project_Peak$year)
#check  fofr class
class(project_Peak$year)

#now examnine the trend for yearly strength for days >27 heating level
project_Peak %>% 
  group_by(site) %>% 
  summarise(correla = cor(x = year, y = n, method = "spearman"))


#Quantile ranges of data 
#Maximum temperature
quantile_range <- cbind.data.frame(Ikeja = quantile(Ikeja$T.MAX.Ikeja, seq(0, 1, by=.1)),
                                   LagosIsland = quantile(LI$T.MAX_LI,seq(0, 1, by=.1)),
                                   Ikorodu = quantile(IKR$TMAX.IKO, seq(0, 1, by=.1)))

#Maximum RH
quantile_range2 <- cbind.data.frame(Ikeja = quantile(Ikeja$RH.Ikeja, seq(0, 1, by=.1)),
                                    LagosIsland = quantile(LI$RH.LI,seq(0, 1, by=.1)),
                                    Ikorodu = quantile(IKR$RH2M.IKO, seq(0, 1, by=.1)))

#Write csv files of quantile ranges
write.csv(quantile_range, file = "quantile.csv")
write.csv(quantile_range2, file = "quantile.csv")


#Seasonal Mann kendall trend tests
#A function the converts results of seasonal mannkendall to a dataframe easily
library(trend)

mannKendall <- function(x){
  time <- ts(x, start = 1989, frequency = 12)
  manken <- smk.test(time)
  cbind(season ,S = manken$Sg, varSg = round(manken$varSg),
        tau = round(manken$taug, 4), Z = round(manken$Zg, 4),
        "Pr(>|z|)" = round(manken$pvalg, 4), Signifi = ifelse(manken$pvalg < 0.05, "*", " "))
}


#Ikeja
Ikeja_kendall <-project %>% 
  select(Date, site, comfort) %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  group_by(month, year, site) %>% 
  summarise_all(funs(mean)) %>% 
  arrange(year, site) %>% 
  ungroup(month, year,site) %>% 
  filter(site == "Ikeja") %>% 
  select(comfort) %>% 
  mannKendall()


#Ikorodu
Ikorodu_kendall <- project %>% 
  select(Date, site, comfort) %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  group_by(month, year, site) %>% 
  summarise_all(funs(mean)) %>% 
  arrange(year, site) %>% 
  ungroup(month, year,site) %>% 
  filter(site == "Ikorodu") %>% 
  select(comfort) %>% 
  mannKendall()



#lagos Island
LagosIsland_kendall <- project %>% 
  select(Date, site, comfort) %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  group_by(month, year, site) %>% 
  summarise_all(funs(mean)) %>% 
  arrange(year, site) %>% 
  ungroup(month, year,site) %>% 
  filter(site == "Lagos Island") %>% 
  select(comfort) %>% 
  mannKendall()


#Write csv values
write.csv(LagosIsland_kendall, file = "Island_k.csv")
write.csv(Ikorodu_kendall, file = "Ikorodu_k.csv")
write.csv(Ikeja_kendall, file = "Ikeja_k.csv")



#correlation between THI values and contributing variables
#THI vs Temperature
library(DEGreport) #for the geom_cor package
project %>% 
ggplot(.,aes(comfort, temp, col = site)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  facet_wrap(~site) +
  xlab("THI") +
  ylab("Temperature") +
  ggtitle("THI vs Temperature") +
  geom_cor(method = "pearson", ypos = 1e5, size = 2.7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


#THI vs Relative Humidity
project %>% 
  ggplot(.,aes(comfort, RH, col = site)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  facet_wrap(~site) +
  xlab("THI") +
  ylab("RH") +
  ggtitle("THI vs RH") +
  geom_cor(method = "pearson", ypos = 1e5, size = 3.0)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))



#Wavelet transform
library(WaveletComp)
library(tidyverse)


#filter for Lagos Island
pp <- project %>% 
  filter(site == "Lagos Island") %>% 
  select(Date, comfort) %>% 
  rename(date = Date)


#Build wavelet for comfort Lagos Island
my.w <- analyze.wavelet(pp, "comfort",
                        loess.span = 0,
                        dt = 1, dj = 1/50,
                        make.pval = TRUE, n.sim = 10, lowerPeriod = 32, upperPeriod = 1024)



max.power <- max(my.w$Power)
#construct Image
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "Period (Days)", show.date = T, date.format = "%F", timelab = "",
         main = "Seasonality (Lagos Island)", 
         maximum.level = 1.001 * max.power)#pg 38, 46!!!



#For ikeja
pp2 <- project %>% 
  filter(site == "Ikeja") %>% 
  select(Date, comfort) %>% 
  rename(date = Date)

#Build wavelet for comfort Ikeja
my.w2 <- analyze.wavelet(pp2, "comfort",
                         loess.span = 0,
                         dt = 1, dj = 1/50,
                         make.pval = TRUE, n.sim = 10, lowerPeriod = 32, upperPeriod = 1024)

#construct Image
wt.image(my.w2, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (Days)", show.date = T, date.format = "%F", timelab = "",
         main = "Seasonality (Ikeja)",
         maximum.level = 1.001 * max.power)#pg 38, 46!!!


#For ikorodu
pp3 <- project %>% 
  filter(site == "Ikorodu") %>% 
  select(Date, comfort) %>% 
  rename(date = Date)

#Build wavelet for comfort Ikorodu
my.w3 <- analyze.wavelet(pp3, "comfort",
                         loess.span = 0,
                         dt = 1, dj = 1/50,
                         make.pval = TRUE, n.sim = 10, lowerPeriod = 32, upperPeriod = 1024)

#construct Image
wt.image(my.w3, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (Days)", show.date = T, date.format = "%F", timelab = "",
         main = "Seasonality (Ikorodu)",
         maximum.level = 1.001 * max.power)#pg 38, 46 of the wavelet comp book

