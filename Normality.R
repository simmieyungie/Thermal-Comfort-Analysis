#The first procedure is the Test for normality
#Plot overall yearly distribution for temperature
ggplot(project, aes(temp, fill = site)) + geom_density(aes(group = site, alpha = .1),
                                                       position = "identity") +
  ggtitle("Density Distribution of Temperature (1989-2018)") +
  xlab("Temperature(°C)")+
  ylab("Density") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold")) +
  scale_alpha(guide = "none")


#For relative Humidity
##Plot overall yearly distrubution for relative humidity
ggplot(project, aes(RH, fill = site)) + geom_density(aes(group = site, alpha = .1),
                                                     position = "identity") +
  ggtitle("Density Distribution of Relative Humididty (1989 - 2018)") +
  xlab("Relative Humidity (%)")+
  ylab("Density") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold")) +
  scale_alpha(guide = "none")




###Using wilcox test for normality
#convert site to a factor

#WIlcoxon test for temperature
# both groups have significance differences in the mean values of that variable.
wilcox.test(ikeja$temp, ikorodu$temp, paired = T, alternative = "two.sided")

wilcox.test(ikeja$temp, Lagos_Island$temp, paired = T, alternative = "two.sided")

wilcox.test(Lagos_Island$temp, ikorodu$temp, paired = T, alternative = "two.sided")


#WIlcoxon test for Relative Humidity
wilcox.test(ikeja$RH, ikorodu$RH, paired = T, alternative = "two.sided")

wilcox.test(ikeja$RH, Lagos_Island$RH, paired = T, alternative = "two.sided")

wilcox.test(Lagos_Island$RH, ikorodu$RH, paired = T, alternative = "two.sided")



#maximum temperature
proj_max_temp <- cbind.data.frame(date, Ikeja = Ikeja$T.MAX.Ikeja, 
                                  Ikorodu = IKR$TMAX.IKO,
                                  LagosIsland = LI$T.MAX_LI)
#Max RH
proj_max_rh <- cbind.data.frame(date ,Ikeja = Ikeja$RH.Ikeja,
                                Ikorodu = IKR$RH2M.IKO,
                                LagosIsland = LI$RH.LI)
#convert to date
proj_max_temp$date <- as.Date(proj_max_temp$date)
proj_max_rh$date <- as.Date(proj_max_rh$date)


#Density distibution of maximum temperature
proj_max_temp %>% 
  gather(2:4, key = "Location", value = "TMAX") %>% 
  ggplot(., aes(TMAX, fill = Location)) +
  geom_density(aes(group = Location,alpha = .1),
               position = "identity" ) +
  ggtitle("Density Distribution of Maximum Temperature (1989 - 2018)") +
  xlab("Maximum Temp (C)")+
  ylab("Density") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold")) +
  scale_alpha(guide = "none")

#Density distribution of maximum Relative Humidity
proj_max_rh %>% 
  gather(2:4, key = "Location", value = "RH") %>% 
  ggplot(., aes(RH, fill = Location)) +
  geom_density(aes(group = Location,alpha = .1),
               position = "identity" ) +
  ggtitle("Density Distribution of Maximum RH (1989 - 2018)") +
  xlab("Maximum RH (%)")+
  ylab("Density") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"), plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold")) +
  scale_alpha(guide = "none")

#For minimum temperature
cbind.data.frame(Ikeja = Ikeja$T.MIN.Ikeja,
                 Ikorodu = IKR$TMIN.IKO,
                 LagosIsland = LI$T.MIN_LI,
                 date = proj_max_rh$date) %>% 
  gather(1:3, key = "Location", value = "TMin") %>% 
  ggplot(., aes(TMin, fill = Location)) +
  geom_density(aes(group = Location,alpha = .1),
               position = "identity" ) +
  ggtitle("Density Distribution of Minimum Temperature (1989 - 2018)") +
  xlab("Minimum Temperature")+
  ylab("Density") +
  theme(legend.background = element_rect(fill = "darkgray"),
        legend.key.size = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(size = 0.5, linetype = 'solid',
                                    colour = "black", fill = NA),
        legend.position = "bottom", legend.box = 
          "horizontal", legend.title = element_text(size = 12, face = "bold")) +
  scale_alpha(guide = "none")
