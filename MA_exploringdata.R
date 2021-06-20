#### LIBRARY ####
packages <- c("dplyr", "tidyverse", "tidyr", "haven", 
              "readxl", "knitr", "ggplot2", "ggpubr",
              "stargazer", "foreign", "readr", "pastecs",
              "sandwich", "gghighlight", "psy",
              "MASS", "xtable", "texreg", "psych",
              "multiwayvcov", "countrycode", "moments") ## Vector of needed packages
sapply(packages, function(x) library(x, character.only = TRUE))

#### WD ####
setwd("C:/Users/mette/Dropbox/Apper/Overleaf/MA_thesis_Metteus2021/R_figures")

#### LOAD DATA ####
thesisdata <- read.csv("thesis_data.csv")
oecd_data <- read.csv("oecd_data.csv")
UD_data <- read.csv("UDdata.csv")

#### Exploring the data ####
str(thesisdata)
sample_n(thesisdata, 20)

names(thesisdata)
class(thesisdata)

#1 row per country per year
table(thesisdata$Country, thesisdata$Year)

#Look at Union Density
min(thesisdata$UD, na.rm = T)  # 4.3
max(thesisdata$UD, na.rm = T)  # 100
mean(thesisdata$UD, na.rm = T) # 36.10005
median(thesisdata$UD, na.rm = T)  # 31.4
sd(thesisdata$UD, na.rm = T)      # 21.27793
var(thesisdata$UD, na.rm = T)     # 452.7502
quantile(thesisdata$UD, na.rm = T) # 25% = 18.7, 75% = 50.7

skewness(thesisdata$UD, na.rm = T) # 0.7402012
kurtosis(thesisdata$UD, na.rm = T) # 2.74354

#Look at count of laws
min(thesisdata$count1, na.rm = T)  # 0
max(thesisdata$count1, na.rm = T)  # 8
mean(thesisdata$count1, na.rm = T) # 0.2456086
median(thesisdata$count1, na.rm = T)  # 0
sd(thesisdata$count1, na.rm = T)      # 0.6955136
var(thesisdata$count1, na.rm = T)     # 0.4837392
quantile(thesisdata$count1, na.rm = T) # 25% = 0, 75% = 0

skewness(thesisdata$count1, na.rm = T) # 4.025284
kurtosis(thesisdata$count1, na.rm = T) # 24.55971

#Dichotomous
min(thesisdata$New_law, na.rm = T)  # 0
max(thesisdata$New_law, na.rm = T)  # 1
mean(thesisdata$New_law, na.rm = T) # 0.1550077
median(thesisdata$New_law, na.rm =T )  # 0
sd(thesisdata$New_law, na.rm = T)      # 0.361978
var(thesisdata$New_law, na.rm = T)     # 0.1310207
quantile(thesisdata$New_law, na.rm = T) # 25% = 0, 75% = 0

skewness(thesisdata$New_law, na.rm = T) # 1.906498
kurtosis(thesisdata$New_law, na.rm = T) # 4.634735

nrow(thesisdata) ##3245 
sum(is.na(thesisdata)) ##45024 missing observations (many are due to merging I believe)

length(unique(thesisdata$CC)) ## 55 countries (OECD + EU members,
## but there might be some that don't have values in all datasets I merged 

range(thesisdata$Year) ##1960-2018 (but same note as above)

#correlation between UD and Corporatism
cor.test(thesisdata$CorpAll, thesisdata$UD,  method = "pearson", use = "complete.obs")
cor.test(thesisdata$CorpAll, thesisdata$count1, method = "pearson", use = "complete.obs")
cor.test(thesisdata$UD, thesisdata$count1, method = "pearson", use = "complete.obs")

#### Look at missing ####
table(complete.cases(thesisdata)) # 48 rows complete for all variables..

#dependent variable
table(is.na(thesisdata$count1)) # no missing

#different independent variables
table(is.na(thesisdata$UD)) #1914 observations with values, 1331 without

table(is.na(thesisdata$GDPperhead)) #1807 observations with values, 1438 without
table(is.na(thesisdata$Indep)) #3245 with value
table(is.na(thesisdata$indepyears)) #3245 with value
table(is.na(thesisdata$Industry)) #1861 with value, 1384 without
table(is.na(thesisdata$fossilexport)) # 3245 with value
table(is.na(thesisdata$fossilproducer)) # 3245 with value

table(is.na(thesisdata$logemissions)) #1458 with value, 1787 without

table(is.na(thesisdata$CorpAll)) #1963, 1282
table(is.na(thesisdata$CS_part)) #1856, 1389
table(is.na(thesisdata$Corr)) #1856, 1389
table(is.na(thesisdata$Dem)) #1856, 1389

#### Decriptive tables ####
Work <- oecddata %>%
  dplyr::select(UD, CorpAll, Industry, environshare, environvote, kyoto_member, EUmember,
                logemissions, Dem, Indep, trade, logGDP, fossilproducer, fossilexport) %>%
  dplyr::rename("Union density" = UD,
                "Corporatism" = CorpAll,
                "Industry" = Industry,
                "Green party in gov." = environshare,
                "Green party vote" = environvote,
                "Kyoto protocol membership" = kyoto_member,
                "EU membership" = EUmember,
                "Emissions per capita (log)" = logemissions,
                "Democracy" = Dem,
                "Independent years" = Indep,
                "Trade openness" = trade,
                "GDP per capita (log)"= logGDP,
                "Fossil fuel producer" = fossilproducer,
                "Fossil fuel exporter" = fossilexport)

stargazer(as.data.frame(Work), type = "latex")


corpdata <- oecddata %>%
  dplyr::select(CENT, WC_STRUCT, WC_RIGHTS, Govint, Level, RI, Coord, EXT, Sector)%>%
  dplyr::rename("Centralization of bargaining" = CENT,
                "Work council structure" = WC_STRUCT,
                "Work council rights" = WC_RIGHTS,
                "Government intervention" = Govint,
                "Level of wage bargaining" = Level,
                "Involvement of labor organizations" = RI,
                "Wage bargaining coordination" = Coord,
                "Extention of bargaining" = EXT,
                "Sectoral organization" = Sector)

stargazer(as.data.frame(corpdata), type = "latex")

## descriptive union density table
oecddata <- oecddata %>%
  dplyr::mutate(UD_levels = ifelse(UD > 0 & UD < 0.3, low, UD))

oecddata$UDlevel <- ifelse(oecddata$UD <= 30, "Low", oecddata$UD)
oecddata$UDlevel <- ifelse(oecddata$UD > 30, "Medium", oecddata$UDlevel)
oecddata$UDlevel <- ifelse(oecddata$UD >50, "High", oecddata$UDlevel)

table(oecddata$UDlevel, oecddata$count1)

#### Descriptive figures####
## UNION DENSITY
unique(thesisdata$Region)

thesisdata$OECD <- ifelse(thesisdata$Region == "NonOECD", 0, 1)

ggboxplot(thesisdata, x = "Region", y = "UD", width = 0.5) + 
  ggtitle("Boxplot: union density") +
  ylab("Union density") + xlab("")

gghistogram(thesisdata, x = "UD", add = "mean", bins = 10) + 
  ggtitle("Histogram: union density")

ggqqplot(thesisdata, x = "UD") +
  ggtitle("Quantile-quantile plot: Union density")

UD_percountry <- group_by(thesisdata, Country) %>% 
  summarise(count = n(), 
    mean = mean(UD, na.rm = TRUE),
    sd = sd(UD, na.rm = TRUE))

xtable(UD_percountry)

ggstripchart(thesisdata, x = "Region", y = "UD",
             color = "Region")

## CORPORATISM
ggboxplot(thesisdata, x = "Region", y = "CorpAll", width = 0.5) +
  ggtitle("Boxplot: corporatism") +
  ylab("Corporatism") + xlab("")

gghistogram(thesisdata, x = "CorpAll", bins = 10,
            add = "mean") +
  ggtitle("Histogram: corporatism")

ggqqplot(thesisdata, x = "CorpAll")+
  ggtitle("Quantile-quantile plot: Corporatism")

corp_percountry <- group_by(thesisdata, Country) %>% 
  summarise(count = n(), 
            mean = mean(CorpAll, na.rm = TRUE),
            sd = sd(CorpAll, na.rm = TRUE))

xtable(corp_percountry)

ggstripchart(thesisdata, x = "Region", y = "CorpAll",
             color = "Region",
             add = "mean_sd")

## LAWS 
gghistogram(thesisdata, x = "count1", add = "mean") + 
  ggtitle("Historgram: laws") +
  xlab("Laws adopted")

laws_percountry <- group_by(thesisdata, Country) %>%
  summarise(count = n(),
            median = median(count1, na.rm = TRUE),
            sd = sd(count1, na.rm = TRUE))

xtable(laws_percountry)


#### Correlation ####
korrelasjon <- oecddata %>%
  dplyr::select(UD, CorpAll, Industry, environshare, environvote, kyoto_member, EUmember,
                logemissions, Dem, Indep, trade, logGDP, fossilproducer, fossilexport) %>%
  dplyr::rename("Union density" = UD,
                "Corporatism" = CorpAll,
                "Industry" = Industry,
                "Green party in gov." = environshare,
                "Green party vote" = environvote,
                "Kyoto protocol membership" = kyoto_member,
                "EU membership" = EUmember,
                "Emissions per capita (log)" = logemissions,
                "Democracy" = Dem,
                "Independent years" = Indep,
                "Trade openness" = trade,
                "GDP per capita (log)"= logGDP,
                "Fossil fuel producer" = fossilproducer,
                "Fossil fuel exporter" = fossilexport) %>%
  cor(, use = "pairwise.complete.obs")

stargazer(korrelasjon) 

#### Different union density descriptives ####
UD_data <- UD_data %>%
  dplyr::filter(Year != 2019)

UD_data <- UD_data %>%
group_by(Country) %>%
arrange(Year, .by_group = TRUE) %>%
dplyr::mutate(peakUD = max(UD, na.rm = T),
              change = (UD/dplyr::lag(UD) - 1) * 100)

UD_data <-UD_data %>%
group_by(Country) %>% 
summarise(lastUD = last(na.omit(UD))) %>% 
left_join(UD_data, ., by = 'Country')
               
UD_data <-UD_data %>%
group_by(Country) %>% 
dplyr::mutate(peakchange = (lastUD/peakUD - 1) *100)
                

dataframe <- UD_data %>%
  dplyr::select(Country, Year, UD, peakchange, peakUD, lastUD) %>%
  dplyr::mutate(Year = ifelse(UD == peakUD | UD == lastUD, Year, NA))

dataframe <- dataframe %>%
  na.omit()
dataframe <- dataframe[- c(12, 13, 20, 25, 40:43, 70, 81), ]

UD_data <- UD_data %>%
  group_by(Country) %>%
  summarise(lastPublic = last(na.omit(UD_public)),
            lastPrivate = last(na.omit(UD_private)),
            lastAgr = last(na.omit(UD_agr)),
            lastInd = last(na.omit(UD_ind)),
            lastServ = last(na.omit(UD_serv)),
            lastUtil = last(na.omit(UD_util)),
            lastTransport = last(na.omit(UD_transport)),
            firstPublic = first(na.omit(UD_public)),
            firstPrivate = first(na.omit(UD_private)),
            firstAgr = first(na.omit(UD_agr)),
            firstInd = first(na.omit(UD_ind)),
            firstServ = first(na.omit(UD_serv)),
            firstUtil = first(na.omit(UD_util)),
            firstTransport = first(na.omit(UD_transport))) %>%
  left_join(UD_data, ., by = 'Country')

UD_data <-UD_data %>%
  group_by(Country) %>% 
  dplyr::mutate(pubchange = (lastPublic/firstPublic - 1) *100,
                privchange = (lastPrivate/firstPrivate -1)*100,
                agrchange = (lastAgr/firstAgr -1)*100,
                indchange = (lastInd/firstInd-1)*100,
                servchange = (lastServ/firstServ-1)*100)


dataframe2 <- UD_data %>%
  dplyr::select(Country, Year, UD_public, firstPublic, lastPublic, pubchange,
                UD_private, firstPrivate, lastPrivate, privchange,
                UD_agr, firstAgr, lastAgr, agrchange,
                UD_ind, firstInd, lastInd, indchange,
                UD_serv, firstServ, lastServ, servchange)%>%
  dplyr::mutate(Yearpub = ifelse(UD_public == firstPublic | UD_public == lastPublic, Year, NA),
                Yearpriv = ifelse(UD_private == firstPrivate | UD_private == lastPrivate, Year, NA),
                Yearagr = ifelse(UD_agr == firstAgr | UD_agr == lastAgr, Year, NA),
                Yearind = ifelse(UD_ind == firstInd | UD_ind == lastInd, Year, NA),
                Yearserv = ifelse(UD_serv == firstServ | UD_serv == lastServ, Year, NA))

dataframe <- dataframe %>%
  na.omit()
dataframe <- dataframe[- c(12, 13, 20, 25, 40:43, 70, 81), ]