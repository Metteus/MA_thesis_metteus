#### LIBRARY ####
packages <- c("dplyr", "tidyverse", "tidyr", "haven", 
              "readxl", "knitr", "ggplot2", "sf", "raster",
              "spData", "tmap", "leaflet", "gganimate",
              "stargazer", "foreign", "readr","rnaturalearth",
              "rnaturalearthdata",
              "sandwich", "gghighlight", "psy",
              "MASS", "xtable", "texreg", "psych",
              "multiwayvcov", "countrycode", "moments") ## Vector of needed packages
sapply(packages, function(x) library(x, character.only = TRUE))

#### WD ####
setwd("C:/Users/mette/OneDrive/Dokumenter/R/Masterdatasett/Raw data")

#### LOAD DATA ####
thesisdata <- read_csv("thesis_data.csv")
oecddata <- thesisdata %>%
  dplyr::filter((CC %in% c("AUT", "AUS", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST",
                           "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
                           "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
                           "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR",
                           "USA")))

#### New WD ####
setwd("C:/Users/mette/Dropbox/Apper/Overleaf/MA_thesis_Metteus2021/R_figures")

#### DESCRIPTIVE PLOTS ####
#### LAWS ####
#histogram of total laws (y axis is count of occurences, so number of observations with the said amount of laws?)
ggplot(oecddata, aes(x = Year, y = law_count))+
  geom_col(aes(), colour="black", fill="white") +
  ylab("Climate laws")
#  geom_density(alpha=.2, fill="#FF6666") #if this is to be used remember geom_histogram(aes(y = ...density...))
ggsave("total_laws.pdf")

#histogram of proper count 
ggplot(oecddata) + geom_histogram(aes(x = count1), bins = 10, colour = "black", fill ="white")+
  ylab("") + xlab("Yearly adopted climate policies")
ggsave("total_count.pdf")

#new laws per country, year trend
oecddata %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = count1))+
  geom_line(aes(x = Year, y = count1))+
  facet_wrap(~Country) +
  ylab("Newly adopted laws")
ggsave("facetcount.pdf", units = "in", height = 8.5, width = 11)

#frequence of laws
freq <- table(oecddata$count1)
xtable::xtable(freq)

## Make sum variables ##
oecddata <- oecddata %>%
  group_by(Year) %>%
  dplyr::mutate(countyear = sum(count1))

oecddata <- oecddata %>%
  group_by(Year) %>%
  dplyr::mutate(countobs = sum(count1 > 0))

### trend plots ###
oecddata %>%
  ggplot() +
  geom_line(aes(x = Year, y = countyear))+
  geom_line(aes(x = Year, y = countobs), colour= "darkgrey") +
  ylab("") + xlab("Year") 
ggsave("trendplot.pdf")

oecddata %>%
  ggplot() +
  geom_point(aes(x = count1, y = UD)) +
  xlab("Yearly count") + ylab("Union density")
ggsave("UD_count_pointplot.pdf")

oecddata %>%
  ggplot() +
  geom_point(aes(x = count1, y = CorpAll)) +
  xlab("Yearly count") + ylab("Corporatism")
ggsave("Corp_count_pointplot.pdf")

oecddata %>%
  ggplot() +
  geom_point(aes(x = count1, y = Industry))+
  xlab("Yearly count") + ylab("Industry")
ggsave("Ind_count_pointplot.pdf")

oecddata %>%
  dplyr::filter(Year == 1990) %>%
  ggplot() +
  geom_point(aes(x = count1, y = UD))+
  xlab("Yearly count") + ylab("Union density")
ggsave("plot_90.pdf")

oecddata %>%
  dplyr::filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = count1, y = UD))+
  xlab("Yearly count") + ylab("Union density")
ggsave("plot_00.pdf")

oecddata %>%
  dplyr::filter(Year == 2010) %>%
  ggplot()+
  geom_point(aes(x=count1, y = UD))+
  xlab("Yearly count") + ylab("Union density")
ggsave("plot_10.pdf")


#### KEY VARIABLES PER COUNTRY ####
#Corporatism per country, year trend
oecddata %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = CorpAll))+
  geom_line(aes(x = Year, y = CorpAll))+
  facet_wrap(~Country)+
  ylab("Corporatism")
ggsave("facetcorp.pdf", units = "in", height = 8.5, width = 11)

#UD per country, year trend
oecddata %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = UD))+
  geom_line(aes(x = Year, y = UD))+
  facet_wrap(~Country)+
  ylab("Union density")
ggsave("facetud.pdf", units = "in", height = 8.5, width = 11)

##Histogram corporatism
#countries with high union density
highcorp <- oecddata %>%
  filter(CorpAll > -0.015)
resthigh_corp <- oecddata

My_Theme = theme(axis.title.x = element_text(size = 12),
                 axis.title.y = element_text(size = 12),
                 axis.text = element_text(size = 11),
                 legend.position = "top",
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 11))

highcorp$veg <- 'sub'
resthigh_corp$veg <- 'whole'
highcorp_length <- rbind(resthigh_corp, highcorp)

highcorp_length <- highcorp_length %>%
  group_by(Country)

both <- ggplot(highcorp_length, aes(count1, fill = veg)) +
  geom_histogram(alpha = 0.6, aes(), position = 'identity', bins = 70) +
  theme_bw() +
  scale_y_continuous() +
  scale_fill_manual(name = "", values = c("red", "darkgrey"), labels=c("Corporatism index above mean","All"))

both +  My_Theme + xlab("Count of newly adopted laws per country") + 
  ylab("Frequency") 
ggsave("Corp_histandcount.pdf")

#countries with low corporatism
lowcorp <- oecddata %>%
  filter(CorpAll < -0.015)
restlow_corp <- oecddata

My_Theme = theme(axis.title.x = element_text(size = 12),
                 axis.title.y = element_text(size = 12),
                 axis.text = element_text(size = 11),
                 legend.position = "top",
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 11))

lowcorp$veg <- 'sub'
restlow_corp$veg <- 'whole'
lowcorp_length <- rbind(restlow_corp, lowcorp)

lowcorp_length <- lowcorp_length %>%
  group_by(Country)

both <- ggplot(lowcorp_length, aes(count1, fill = veg)) +
  geom_histogram(alpha = 0.6, aes(), position = 'identity', bins = 70) +
  theme_bw() +
  scale_y_continuous() +
  scale_fill_manual(name = "", values = c("red", "darkgrey"), labels=c("Corporatism index below mean","All"))

both +  My_Theme + xlab("Count of newly adopted laws per country") + 
  ylab("Frequency") 
ggsave("Corp_histandcountlow.pdf")

## Histogram
# countries with high union density
highUD_data <- oecddata %>%
  filter(UD > 50)
resthigh_data <- oecddata

My_Theme = theme(axis.title.x = element_text(size = 12),
                 axis.title.y = element_text(size = 12),
                 axis.text = element_text(size = 11),
                 legend.position = "top",
                 legend.title = element_text(size = 12),
                 legend.text = element_text(size = 11))

highUD_data$veg <- 'sub'
resthigh_data$veg <- 'whole'
highUD_length <- rbind(resthigh_data, highUD_data)

highUD_length <- highUD_length %>%
  group_by(Country)

both <- ggplot(highUD_length, aes(count1, fill = veg)) +
  geom_histogram(alpha = 0.6, aes(), position = 'identity', bins = 70) +
  theme_bw() +
  scale_y_continuous() +
  scale_fill_manual(name = "", values = c("red", "darkgrey"), labels=c("Union density above 50%","All"))

both +  My_Theme + xlab("Count of newly adopted laws per country") + 
  ylab("Frequency") 
ggsave("UD_histandcount.pdf")

#countries with low union density
lowUD_data <- oecddata %>%
  filter(UD <= 30)
restlow_data <- oecddata

lowUD_data$veg <- 'sub'
restlow_data$veg <- 'whole'
lowUD_length <- rbind(restlow_data, lowUD_data)

lowUD_length <- lowUD_length %>%
  group_by(Country)

both2 <- ggplot(lowUD_length, aes(count1, fill = veg)) +
  geom_histogram(alpha = 0.6, aes(), position = 'identity', bins = 70) +
  theme_bw() +
  scale_y_continuous() +
  scale_fill_manual(name = "", values = c("red", "darkgrey"), labels=c("Union density under or equal to 30%","All"))

both2 + My_Theme + xlab("Count of newly adopted laws per country") + 
  ylab("Frequency") 
ggsave("UD_histandcountlow.pdf")

#### FIGURE OF keys ALONG COUNTS ####
mean(oecddata$UD, na.rm = TRUE)
oecddata$UnionDensity_mean <- ifelse(oecddata$UD >= 32.06923, "Above or equal", "Below")

ggplot(oecddata, aes(factor(count1))) + 
  geom_bar(aes(fill = UnionDensity_mean), position = "fill") + 
  geom_hline(yintercept = 0.5) +
  ylab("Density") + xlab("Count of laws adopted") + 
  ggtitle("Density of adopted laws in a year by level of union density")
ggsave("densityoflaws_UD.pdf", height = 8.5, width = 11, units = "in")

mean(oecddata$CorpAll, na.rm = TRUE)
oecddata$Corporatism_mean <- ifelse(oecddata$CorpAll >= -0.09354894, "Above or equal", "Below")

ggplot(oecddata, aes(factor(count1))) + 
  geom_bar(aes(fill = Corporatism_mean), position = "fill") + 
  geom_hline(yintercept = 0.5) +
  ylab("Density") + xlab("Count of laws adopted") +
  ggtitle("Density of adopted laws in a year by level of corporatism")
ggsave("densityoflaws_corp.pdf", height = 8.5, width = 11, units = "in")

oecddata$Year_2010 <- ifelse(oecddata$Year <= 2010, "Before or equal", "After")

ggplot(oecddata, aes(factor(count1))) +
  geom_bar(aes(fill = Year_2010), position = "fill") +
  geom_hline(yintercept = 0.5)
ggsave("density of laws_year2010.pdf", height = 8.5, width = 11, units = "in")

