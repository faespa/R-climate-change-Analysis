library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

dim(temp_carbon)
#head(historic_co2)


#first year with register off temp carbon
first_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

#Last year with register off temp carbon
last_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

#temp carbon of the first year register
year1 <-temp_carbon  %>% 
  filter( year == 1751) %>% 
  .$carbon_emissions
#temp carbon of the last year register
year2 <- temp_carbon$carbon_emissions[temp_carbon$year==2014]

#relation into fist and last year temp_carbon
comp <-year2/year1


#first year with register off temp anomaly
first_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  min()

#last year with register off temp anomaly
last_year <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  max()

#diference into last year and fisrt year of temp anomaly
diff_tempAnormaly <- temp_carbon$temp_anomaly[temp_carbon$year == last_year] - 
  temp_carbon$temp_anomaly[temp_carbon$year == first_year]



#first year with a temp anomaly upper to mean
year_upperMean <- temp_carbon %>% filter(!is.na(temp_anomaly) & temp_anomaly >= 0) %>%
  .$year %>%
  min()

#last year with a temp anomaly lower to mean
lastYear_lowerMean <- temp_carbon %>% filter(!is.na(temp_anomaly) & temp_anomaly <= 0) %>%
  .$year %>%
  max()

#fisrt year with a temp anomaly upper to 0.5
year_supp05 <- temp_carbon %>% filter(!is.na(temp_anomaly) & temp_anomaly >= 0.5) %>%
  .$year %>%
  min()


#plot line for time line of termperature Anomaly and lines for means 
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line(col = "black") + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 1900, y = 0.05, label = "20th century mean"), col = "blue") +
  geom_vline(aes(xintercept=1939),col="red") +
  geom_vline(aes(xintercept=1976),col="orange") +
  geom_vline(aes(xintercept=1997),col="green") +
  geom_text(aes(x=1939,y=0.05,label="1939"),col="red") +
  geom_text(aes(x=1976,y=0.05,label="1976"),col="orange") +
  geom_text(aes(x=1997,y=0.5,label="1997"),col="green")




p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line(col = "black") + 
  geom_line(aes(year,ocean_anomaly),col="blue", alpha = 0.4) +
  geom_line(aes(year,land_anomaly), col="brown", , alpha = 0.4) +
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") +
    xlim(c(1900, 2018)) +
  ggtitle("Temperature anomaly on land and ocean") +
  geom_text(aes(x = 1920, y = 0.05, label = "20th century mean"), col = "blue")




#########################################
#ghrapich with REgion defnined
#########################################
# set colorblind-friendly color palette
colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")

p <- temp_carbon %>%
  select(Year = year, Global = temp_anomaly, Land = land_anomaly, Ocean = ocean_anomaly) %>%
  gather(Region, Temp_anomaly, Global:Ocean) %>%
  ggplot(aes(Year, Temp_anomaly, col = Region)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0), col = colorblind_palette[8], lty = 2) +
  geom_label(aes(x = 2005, y = -.08), col = colorblind_palette[8],label = "20th century mean", size = 4) +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  scale_color_manual(values = colorblind_palette) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")


#########################################
#Green House
#########################################
head(greenhouse_gases)
p <- greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  geom_hline(aes(yintercept = 275), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#comparation with temp carbon 
p <- temp_carbon %>%
  ggplot(aes(year,carbon_emissions)) +
  geom_line()



head(historic_co2 )

co2_time  <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line()


co2_time_recent  <- historic_co2 %>%
  filter(year > 1500) %>%
  ggplot(aes(year, co2, color = source)) +
  geom_line()

co2_time + xlim(-800000, -775000)
co2_time + xlim(-375000, -330000)
co2_time + xlim(-140000, -120000)
co2_time + xlim(-3000, 2018)




