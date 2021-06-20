#### LIBRARY####
packages <- c("dplyr", "tidyverse", "tidyr", "haven", 
              "readxl", "knitr", "ggplot2", "WriteXLS", 
              "stargazer", "foreign", "readr",
              "sandwich", "gghighlight", "psy",
              "MASS", "xtable", "texreg", "psych",
              "multiwayvcov", "countrycode", "moments") ## Vector of needed packages
sapply(packages, function(x) library(x, character.only = TRUE))

#### WD ####
setwd("C:/Users/mette/OneDrive/Dokumenter/R/Masterdatasett/Raw Data")

#### DATA ####
#### Laws data ####
laws_data <- read_xlsx("laws_and_policies_29102020.xlsx")

#subset variables
laws_data <- laws_data %>%
  dplyr::select(Country = "Geography", 
                CC = "Geography ISO", 
                Year = "Year") %>%
  dplyr::mutate(New_law = ifelse(Year, 1, 0))

#subset countries
laws_data <- laws_data[laws_data$Country %in% c("Australia", "Austria", "Belgium",
                                               "Canada", "Chile", "Colombia", "Czech Republic",
                                               "Denmark", "Estonia", "Finland", "France",
                                               "Germany", "Greece", "Hungary", "Iceland",
                                               "Ireland", "Israel", "Italy", "Japan", "South Korea",
                                               "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                               "Netherlands", "New Zealand", "Norway", "Poland",
                                               "Portugal", "Slovakia","Slovenia", "Spain", 
                                               "Sweden", "Switzerland", "Turkey", 
                                               "United Kingdom", "United States of America",
                                               "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                               "Romania", "Argentina", "Brazil", "China",
                                               "India", "Indonesia", "Russia", "South Africa",
                                               "Hong Kong, China", "Malaysia", "Philippines", 
                                               "Singapore", "Taiwan"), ]

#fix CC
laws_data$CC <- countrycode(laws_data$Country, 
                                 origin = "country.name",
                                 destination = "iso3c")

#fix countryname
laws_data$Country <- countrycode(laws_data$CC, 
                                 origin = "iso3c",
                                 destination = "country.name")

#count
laws_data <- laws_data %>%
  group_by(Country, Year) %>%
  mutate(count1 = cumsum(New_law==1))


#subset, i.e. remove years so I only have one year per country to make proper count later
laws_data <- laws_data[- c(2, 5, 7, 10, 13, 17, 22, 24, 25, 27, 28, 
                           30, 33, 34, 39, 42, 43, 46, 58, 66, 68, 
                           70, 74:77, 79, 82:84, 86, 88:90, 99, 100,
                           110, 112, 118:121, 126:130, 133, 134, 136:138,
                           140, 141, 143, 145, 146, 148, 156, 163, 165, 168, 
                           170, 172:176, 179:181, 183, 189, 191, 195, 197, 
                           199:203, 205:208, 213, 217, 219, 221, 223, 234:238,
                           243, 251, 252, 255, 270, 271, 273, 275:278, 280, 281, 
                           283:288, 295, 297, 300, 306, 307, 309, 311, 313, 314,
                           322:324, 328, 330, 335, 336, 338, 340:342, 346, 348, 350,
                           353, 358, 360, 363, 366, 368, 369, 375, 377, 379, 380, 
                           382:385, 388, 389, 391, 393, 394, 396, 397, 399, 401,
                           409, 415, 417, 421, 422, 425, 430, 431, 433:436, 439, 
                           441, 444, 445, 448, 451, 454, 456, 459, 461, 462, 464, 
                           466, 468, 470, 473:475, 489, 492, 494:497, 500:502, 
                           508, 513, 514, 516, 518, 520, 522, 525, 530, 534, 536, 
                           538, 541, 551, 556, 558, 563, 571, 574:576, 581, 582, 
                           587, 588, 593, 604:607, 609, 613, 616, 617, 
                           619:621, 623, 624, 629, 631, 637, 638, 645, 648, 649, 
                           652, 654, 655, 657:659, 661, 668, 669, 671, 678, 680,
                           682, 683, 691:694, 696, 701, 702, 705, 715, 718, 720:723,
                           725, 734, 740, 741, 749, 750, 754, 758, 759, 762, 765, 
                           767, 769, 770, 773, 777, 780, 786, 790, 792:794, 796:802,
                           804, 805, 807, 809:811, 813, 815, 817, 818, 820, 821, 
                           825, 827, 829, 832, 840, 841, 847, 852, 854, 858, 860, 867, 
                           869:871, 873, 874, 878, 879, 881, 885, 888, 895, 897, 
                           901), ]

#count
laws_data <- laws_data %>%
  group_by(Country) %>%
  mutate(count2 = cumsum(count1))

#total count
laws_data <- laws_data %>%
  ungroup() %>%
  mutate(totcount = cumsum(count1))


#### ICTWSS data####
ICTWSS_data <- read.csv("ICTWSS_6_1_Dataset_csv_release (2).csv")

#subset variables
ICTWSS_data <- ICTWSS_data %>%
  dplyr::select(Country = "ï..country",
                CC = "iso2c",
                Year= "year",
                UD, CENT, WC_STRUCT, WC_RIGHTS, Govint, Level, RI, 
                Coord, EXT)
#sector data
ICTWSS_data <- ICTWSS_data %>%
  dplyr::select(Country = "ï..country",
                CC = "iso2c",
                Year = "year",
                Sector)

ICTWSS_data <- ICTWSS_data %>%
  dplyr::select(Country = "ï..country",
                CC = "iso2c",
                Year = "year",
                ED)

ICTWSS_data <- ICTWSS_data %>%
  dplyr::select(Country = "ï..country",
                CC = "iso2c",
                Year = "year",
                UD_manual, UD_nonmanual, UD_agr,UD_ind,UD_serv)

#CORPORATISM INDEX:
# CENT, WC_STRUCT, WC_RIGHTS, GOVINT, Level, RI, Coord, Ext 

#subset countries
ICTWSS_data <- ICTWSS_data[ICTWSS_data$Country %in% c("Australia", "Austria", "Belgium",
                                                      "Canada", "Chile", "Colombia", "Czech Republic",
                                                      "Denmark", "Estonia", "Finland", "France",
                                                      "Germany", "Greece", "Hungary", "Iceland",
                                                      "Ireland", "Israel", "Italy", "Japan", 
                                                      "Korea, Republic of",
                                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                                      "Portugal", "Slovak Republic","Slovenia", "Spain", 
                                                      "Sweden", "Switzerland", "Turkey", 
                                                      "United Kingdom", "United States of America",
                                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                                      "Romania", "Argentina", "Brazil", "China",
                                                      "India", "Indonesia", "Russian Federation", 
                                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                                      "Singapore", "Taiwan, China"), ]
              
#fix cc
ICTWSS_data$CC <- countrycode(ICTWSS_data$Country, 
                            origin = "country.name",
                            destination = "iso3c")
#fiX countryname
ICTWSS_data$Country <- countrycode(ICTWSS_data$CC,
                                   origin = "iso3c",
                                   destination = "country.name")

#### Labor force data  ####
labor <- read.csv("STLABOUR_24022021132244184.csv")

#subset variables
labor <- labor %>%
  dplyr::select(Country,
                CC = "ï..LOCATION",
                Year = "Time",
                Subject,
                laborforce = "Value")

#filter
labor <- labor %>%
  dplyr::filter(Subject == "Employment rate, Aged 15-64, All persons")

#subset countries
labor <- labor[labor$Country %in% c("Australia", "Austria", "Belgium",
                                                      "Canada", "Chile", "Colombia", "Czech Republic",
                                                      "Denmark", "Estonia", "Finland", "France",
                                                      "Germany", "Greece", "Hungary", "Iceland",
                                                      "Ireland", "Israel", "Italy", "Japan", 
                                                      "Korea, Republic of",
                                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                                      "Portugal", "Slovak Republic","Slovenia", "Spain", 
                                                      "Sweden", "Switzerland", "Turkey", 
                                                      "United Kingdom", "United States of America",
                                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                                      "Romania", "Argentina", "Brazil", "China",
                                                      "India", "Indonesia", "Russian Federation", 
                                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                                      "Singapore", "Taiwan, China"), ]

#fix cc
labor$CC <- countrycode(labor$Country, 
                              origin = "country.name",
                              destination = "iso3c")

#fix countryname
labor$Country <- countrycode(labor$CC,
                             origin = "iso3c",
                             destination = "country.name")

#remove unwanted variable
labor$Subject <- NULL

#### Other OECD data ####
OECDLABOR3 <- read.csv("OECDLABOR3.csv")

#Subset variables
OECDLABOR3 <- OECDLABOR3 %>%
  dplyr::select(Subject,
                CC = "ï..LOCATION",
                Country,
                Measure,
                Year = "Time",
                Value)

#subset countries
OECDLABOR3 <- OECDLABOR3 %>%
  filter(Country != "European Union (28 countries)",
         Country != "G20",
         Country != "G7",
         Country != "OECD - Europe",
         Country != "OECD - Total",
         Country != "BRIICS economies - Brazil, Russia, India, Indonesia, China and South Africa",
         Country != "Euro area (19 countries)")

#Fix CC 
OECDLABOR3$CC <- countrycode(OECDLABOR3$Country, 
                             origin = "country.name",
                             destination = "iso3c") ##43 countries

OECDLABOR3$Country <- countrycode(OECDLABOR3$CC,
                             origin = "iso3c",
                             destination = "country.name")

#filter
GDPperhead <- OECDLABOR3 %>%
  filter(Subject == "GDP per head of population",
         Measure == "USD, current prices, current PPPs")
GDPperhead <- GDPperhead %>%
  dplyr::rename(GDPperhead = Value)


GDPperemployed <- OECDLABOR3 %>%
  filter(Subject == "GDP per person employed",
         Measure == "USD, current prices, current PPPs")
GDPperemployed <- GDPperemployed %>%
  dplyr::rename(GDPperemployed = Value)


#merge back
OECDLABOR3 <- left_join(GDPperhead, GDPperemployed, 
                    by = c("Country", "CC", "Year"))


#remove unwanted/empty variables
OECDLABOR3$Subject.y <- NULL
OECDLABOR3$Measure.x <- NULL
OECDLABOR3$Measure.y <- NULL

#### Average wage data ####
OECDaveragewage <- read.csv("OECDAVERAGEWAGES.csv")

#Subset variables
OECDaveragewage<- OECDaveragewage %>%
  dplyr::select(CC = "ï..COUNTRY",
                Country,
                Year = "Time",
                Averagewage = "Value")

#Fix cc
OECDaveragewage$CC <- countrycode(OECDaveragewage$Country, 
                             origin = "country.name",
                             destination = "iso3c") ##35 countries
#Fix countryname
OECDaveragewage$Country <- countrycode(OECDaveragewage$CC,
                             origin = "iso3c",
                             destination = "country.name")

#### Emissions data ####
#load and change to "numeric"
emissions <-read_csv("historical_emissions.csv", 
                     col_types = cols(`1990` = col_number(), 
                                      `1991` = col_number(), `1992` = col_number(), 
                                      `1993` = col_number(), `1994` = col_number(), 
                                      `1995` = col_number(), `1996` = col_number(), 
                                      `1997` = col_number(), `1998` = col_number(), 
                                      `1999` = col_number(), `2000` = col_number(), 
                                      `2001` = col_number(), `2002` = col_number(), 
                                      `2003` = col_number(), `2004` = col_number(), 
                                      `2005` = col_number(), `2006` = col_number(), 
                                      `2007` = col_number(), `2008` = col_number(), 
                                      `2009` = col_number(), `2010` = col_number(), 
                                      `2011` = col_number(), `2012` = col_number(), 
                                      `2013` = col_number(), `2014` = col_number(), 
                                      `2015` = col_number(), `2016` = col_number(), 
                                      `2017` = col_number()))


#Filter
emissions <- emissions %>%
  filter(Sector == "Total including LUCF")

#Subset countries
emissions <- emissions[emissions$Country %in% c("Australia", "Austria", "Belgium",
                                                "Canada", "Chile", "Colombia", "Czech Republic",
                                                "Denmark", "Estonia", "Finland", "France",
                                                "Germany", "Greece", "Hungary", "Iceland",
                                                "Ireland", "Israel", "Italy", "Japan", 
                                                "South Korea",
                                                "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                                "Netherlands", "New Zealand", "Norway", "Poland",
                                                "Portugal", "Slovakia","Slovenia", "Spain", 
                                                "Sweden", "Switzerland", "Turkey", 
                                                "United Kingdom", "United States",
                                                "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                                "Romania", "Argentina", "Brazil", "China",
                                                "India", "Indonesia", "Russia", 
                                                "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                                "Singapore", "Taiwan, China"), ]

#fix so that x is in front of years, easier to change format
emissions <- emissions %>%
  dplyr::rename(x2017 = "2017", x2016 = "2016", x2015 = "2015",
                x2014 = "2014", x2013 = "2013", x2012 = "2012",
                x2011 = "2011", x2010 = "2010", x2009 = "2009",
                x2008 = "2008", x2007 = "2007", x2006 = "2006",
                x2005 = "2005", x2004 = "2004", x2003 = "2003",
                x2002 = "2002", x2001 = "2001", x2000 = "2000",
                x1999 = "1999", x1998 = "1998", x1997 = "1997",
                x1996 = "1996", x1995 = "1995", x1994 = "1994",
                x1993 = "1993", x1992 = "1992", x1991 = "1991",
                x1991 = "1991", x1990 = "1990")

#from wide to long data
emissions <- pivot_longer(emissions, cols = starts_with("x"),
                          names_to = "Year", values_to = "emissions")

#change back to normal year without x
Year2 <- c("2017", "2016","2015",
           "2014",  "2013","2012",
           "2011", "2010", "2009",
           "2008", "2007", "2006",
           "2005", "2004", "2003",
           "2002", "2001", "2000",
           "1999", "1998", "1997",
           "1996", "1995", "1994",
           "1993", "1992", "1991", "1990")

emissions[1:28, "Year"] <- Year2
emissions[29:56, "Year"] <- Year2
emissions[57:84, "Year"] <- Year2
emissions[85:112, "Year"] <- Year2
emissions[113:140, "Year"]<- Year2
emissions[141:168, "Year"]<- Year2
emissions[169:196, "Year"]<- Year2
emissions[197:224, "Year"]<- Year2
emissions[225:252, "Year"]<- Year2
emissions[253:280, "Year"]<- Year2
emissions[281:308, "Year"]<- Year2
emissions[309:336, "Year"]<- Year2
emissions[337:364, "Year"]<- Year2
emissions[365:392, "Year"]<- Year2
emissions[393:420, "Year"]<- Year2
emissions[421:448, "Year"]<- Year2
emissions[449:476, "Year"]<- Year2
emissions[477:504, "Year"]<- Year2
emissions[505:532, "Year"]<- Year2
emissions[533:560, "Year"]<- Year2
emissions[561:588, "Year"]<- Year2
emissions[589:616, "Year"]<- Year2
emissions[617:644, "Year"]<- Year2
emissions[645:672, "Year"]<- Year2
emissions[673:700, "Year"]<- Year2
emissions[701:728, "Year"]<- Year2
emissions[729:756, "Year"]<- Year2
emissions[757:784, "Year"]<- Year2
emissions[785:812, "Year"]<- Year2
emissions[813:840, "Year"]<- Year2
emissions[841:868, "Year"]<- Year2
emissions[869:896, "Year"]<- Year2
emissions[897:924, "Year"]<- Year2
emissions[925:952, "Year"]<- Year2
emissions[953:980, "Year"]<- Year2
emissions[981:1008, "Year"]<- Year2
emissions[1009:1036, "Year"]<- Year2
emissions[1037:1064, "Year"]<- Year2
emissions[1065:1092, "Year"]<- Year2
emissions[1093:1120, "Year"]<- Year2
emissions[1121:1148, "Year"]<- Year2
emissions[1149:1176, "Year"]<- Year2
emissions[1177:1204, "Year"]<- Year2
emissions[1205:1232, "Year"]<- Year2
emissions[1233:1260, "Year"]<- Year2
emissions[1261:1288, "Year"]<- Year2
emissions[1289:1316, "Year"]<- Year2
emissions[1317:1344, "Year"]<- Year2
emissions[1345:1372, "Year"]<- Year2
emissions[1373:1400, "Year"]<- Year2
emissions[1401:1428, "Year"]<- Year2
emissions[1429:1456, "Year"]<- Year2
emissions[1457:1484, "Year"]<- Year2

#fix cc
emissions$CC <- countrycode(emissions$Country, 
                           origin = "country.name",
                           destination = "iso3c") ##53 countries
#fix countryname
emissions$Country <- countrycode(emissions$CC,
                             origin = "iso3c",
                             destination = "country.name")

emissions <- emissions %>%
  dplyr::select(Country,
                CC,
                Year,
                emissions)

#### Corporatism ####
corporatism_data <- read_dta("corporatism_3-0.dta")

#fix cc
corporatism_data$CC <- countrycode(corporatism_data$iso, 
                                   origin = "iso3n",
                                   destination = "iso3c") 

#fix countryname
corporatism_data$Country <- countrycode(corporatism_data$CC,
                                        origin = "iso3c",
                                        destination = "country.name")

corporatism_data$iso <- NULL

#rename
corporatism_data <- corporatism_data %>%
  dplyr::rename(Year = year)

#### v-dem ####
Vdem <- readRDS("V-Dem-CY-Full+others-v11.rds")

Vdem <- Vdem[Vdem$country_name %in% c("Australia", "Austria", "Belgium",
                                      "Canada", "Chile", "Colombia", "Czech Republic",
                                      "Denmark", "Estonia", "Finland", "France",
                                      "Germany", "Greece", "Hungary", "Iceland",
                                      "Ireland", "Israel", "Italy", "Japan", 
                                      "Korea, South",
                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                      "Portugal", "Slovakia","Slovenia", "Spain", 
                                      "Sweden", "Switzerland", "Turkey", 
                                      "United Kingdom", "United States",
                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                      "Romania", "Argentina", "Brazil", "China",
                                      "India", "Indonesia", "Russia", 
                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                      "Singapore", "Taiwan, China"), ]



Vdem <- Vdem %>%
  dplyr::select(CC = "country_text_id",
                Country = "country_name",
                Year = "year",
                CS_part = "v2x_cspart",
                Corr = "v2x_corr",
                Dem = "v2x_polyarchy",
                Indep = "v2svindep")

#fix cc
Vdem$CC <- countrycode(Vdem$Country, 
                       origin = "country.name",
                       destination = "iso3c") ##53 countries
#fix countryname
Vdem$Country <- countrycode(Vdem$CC,
                            origin = "iso3c",
                            destination = "country.name")

#### Fossil fuel production ####
fossil <- read_csv("fossil-fuel-production.csv")

fossil <- fossil[fossil$Entity %in% c("Australia", "Austria", "Belgium",
                                      "Canada", "Chile", "Colombia", "Czechia",
                                      "Denmark", "Estonia", "Finland", "France",
                                      "Germany", "Greece", "Hungary", "Iceland",
                                      "Ireland", "Israel", "Italy", "Japan", 
                                      "South Korea",
                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                      "Portugal", "Slovakia","Slovenia", "Spain", 
                                      "Sweden", "Switzerland", "Turkey", 
                                      "United Kingdom", "United States",
                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                      "Romania", "Argentina", "Brazil", "China",
                                      "India", "Indonesia", "Russia", 
                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                      "Singapore", "Taiwan, China"), ]

fossil <- fossil %>%
  dplyr::rename(Country = "Entity",
                CC = "Code",
                Year = "Year",
                coalprod = "Coal Production - EJ",
                oilprod = "Oil Production - Tonnes",
                gasprod = "Gas Production - EJ")

#fix cc
fossil$CC <- countrycode(fossil$Country, 
                       origin = "country.name",
                       destination = "iso3c") ##53 countries
#fix countryname
fossil$Country <- countrycode(fossil$CC,
                            origin = "iso3c",
                            destination = "country.name")

#### BIG MERGE ####
#This is to be merged, therefore changing to same class (numeric didn't work for some reason): 
ICTWSS_data$Year <- as.character(ICTWSS_data$Year)
ICTWSS_data$CC <- as.character(ICTWSS_data$CC)
ICTWSS_data$Country <- as.character(ICTWSS_data$Country)
labor$Year <- as.character(labor$Year) 
labor$CC <- as.character(labor$CC)
labor$Country <- as.character(labor$Country)
OECDLABOR3$Year <- as.character(OECDLABOR3$Year)
OECDLABOR3$CC <- as.character(OECDLABOR3$CC)
OECDLABOR3$Country <- as.character(OECDLABOR3$Country)
OECDaveragewage$Year <- as.character(OECDaveragewage$Year)
OECDaveragewage$CC <- as.character(OECDaveragewage$CC)
OECDaveragewage$Country <- as.character(OECDaveragewage$Country)
emissions$Year <- as.character(emissions$Year)
emissions$CC <- as.character(emissions$CC)
emissions$Country <- as.character(emissions$Country)
laws_data$Year <- as.character(laws_data$Year)
laws_data$CC <- as.character(laws_data$CC)
laws_data$Country <- as.character(laws_data$Country)
corporatism_data$Year <- as.character(corporatism_data$Year)
corporatism_data$CC <- as.character(corporatism_data$CC)
corporatism_data$Country <- as.character(corporatism_data$Country)
Vdem$Year <- as.character(Vdem$Year)
Vdem$CC <- as.character(Vdem$CC)
Vdem$Country <- as.character(Vdem$Country)
fossil$Year <- as.character(fossil$Year)
fossil$CC <- as.character(fossil$CC)
fossil$Country <- as.character(fossil$Country)

#ICTWSS + labor
work_data <- left_join(ICTWSS_data, labor,
                            by = c("Country", "CC","Year"))

work_data$Year <- as.character(work_data$Year)


# OECD + Average wage
OECDLABORwage <- left_join(OECDLABOR3, OECDaveragewage,
                           by = c("Country", "CC", "Year"))

OECDLABORwage$Year <- as.character(OECDLABORwage$Year)


#ICTWSS, labor + OECD1, OECD2, OECDaveragewage
labor_OECD <- left_join(work_data, OECDLABORwage,
                        by = c("Country", "CC", "Year"))

labor_OECD$Year <- as.character(labor_OECD$Year)

#Corporatism + QOG
corpfoss <- left_join(corporatism_data, fossil,
                   by = c("Country", "CC", "Year"))

corpfoss$Year <- as.character(corpfoss$Year)

#corporatism, QOG + vdem
almost <- left_join(corpfoss, Vdem, 
                    by = c("Country", "CC", "Year"))

almost$Year <- as.character(almost$Year)

#ICTWSS, labor, OECD, OECDaveragewage + CPI, emissions
final_data <- left_join(labor_OECD, emissions,
                        by = c("Country", "CC", "Year"))

final_data$Year <- as.character(final_data$Year)

final_data1 <- left_join(final_data, laws_data,
                        by = c("Country", "CC", "Year"))

final_data1$Year <- as.character(final_data1$Year)

#### FINAL MERGE ####
final_data2 <- left_join(final_data1, almost,
                         by = c("Country", "CC", "Year"))

final_data2 <- final_data2 %>%
  filter(Year != 2019) #only Denmark and Netherlands have observations on this anyways.....

#change back to numeric!
final_data2$Year <- as.numeric(final_data2$Year)

#### Make count variable ####
#make NA 0 for proper count
final_data2$count1[is.na(final_data2$count1)] <- 0

#MAKE na 0 for dichotomous 
final_data2$New_law[is.na(final_data2$New_law)] <- 0

#count per country
final_data2 <- final_data2 %>%
  group_by(Country)%>%
  mutate(law_count = cumsum(count1))

#total laws for whole dataset
final_data2 <- final_data2 %>%
  ungroup() %>%
  mutate(totlaw_count = cumsum(count1))

#checking that I got it right
# subfinal <- final_data2 %>%
#   dplyr::select(Country, CC, Year, New_law, count1, totlaw_count, law_count) 

#### Fix before saving ####
final_data2$CorpCORE<- NULL
final_data2$Corp_fEUplus<- NULL
final_data2$Corp_fCORE<- NULL
final_data2$CorpCOREsm<- NULL
final_data2$CorpEUplus<- NULL
final_data2$CorpEUplussm<- NULL
final_data2$Corpo_fEUplussm <- NULL
final_data2$Corpo_fCOREsm <- NULL
final_data2$Subject.x <- NULL

#### Make region variable ####
final_data2$Area <- countrycode(sourcevar = final_data2$Country, origin = "country.name",destination = "region")

regionlist <- list(
  Nordic = c("NOR", "SWE", "DNK", "ISL", "FIN"),
  Central = c("AUT", "BEL", "CZE", "DEU", "IRL", "LUX", "NLD", "CHE"),
  Liberal = c("AUS","NZL", "CAN", "GBR", "USA"),
  East = c("EST", "HUN", "POL", "SVK", "SVN", "LVA", "LTU"),
  South = c("FRA", "GRC", "ITA", "ESP"),
  NonEurope = c("ISR", "JPN", "KOR", "MEX", "CHL", "COL", "PRT", "TUR"),
  NonOECD = c("ARG", "BGR", "BRA", "CHN", "CRI", "CYP", "HKG", "HRV", "IDN", 
              "IND", "MLT", "MYS", "PHL", "ROU", "RUS", "SGP", "TWN", "ZAF"))

regiondf <- tibble::enframe(regionlist, name="Region", value="CC") %>% tidyr::unnest()

final_data2 <- final_data2 %>%
  left_join(regiondf, by = "CC")
  
  
#### KYOTO ####
Kyoto_data <- final_data2 %>%
  dplyr::filter(Year >= 1997)

Country <- Kyoto_data$Country
CC <- Kyoto_data$CC
Year <- 1997:2018
Kyoto <- c(0, 1, 1, 1, 1, 2, 2, 2, 3, 3,
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0, 
           
           0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 0, 0, 0, 0, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 2, 2, 2, 2, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA,
           
           0, 0, 0, 0, 0, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 0, 0, 0, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 0, 0, 0, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 1, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 1, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 2, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 2, 2, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 1, 1, 1, 1, 1, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 0, 0, 0, 0, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA,
           
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
           0, 0, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 0,
           0, 0,
           
           0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
           0, 0)

Kyoto_data <- data.frame(CC, Country, Year, Kyoto)


#### Other memebership ####
setwd("C:/Users/mette/OneDrive/Dokumenter/R/Masterdatasett")

pa_data2 <- readxl::read_xlsx("Paris_agreement.xlsx")
unfccc_data <- readxl::read_xlsx("UNFCCC_membership.xlsx")

unfccc_data <- unfccc_data[unfccc_data$Country %in% c("Australia", "Austria", "Belgium",
                                                      "Canada", "Chile", "Colombia", "Czechia",
                                                      "Denmark", "Estonia", "Finland", "France",
                                                      "Germany", "Greece", "Hungary", "Iceland",
                                                      "Ireland", "Israel", "Italy", "Japan", 
                                                      "Korea, Republic of",
                                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                                      "Portugal", "Slovakia","Slovenia", "Spain", 
                                                      "Sweden", "Switzerland", "Turkey", 
                                                      "United Kingdom", "United States",
                                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                                      "Romania", "Argentina", "Brazil", "China",
                                                      "India", "Indonesia", "Russian Federation", 
                                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                                      "Singapore", "Taiwan, China"), ]

#fix cc
unfccc_data$CC <- countrycode(unfccc_data$Country, 
                            origin = "country.name",
                            destination = "iso3c") ##53 countries
#fix countryname
unfccc_data$Country <- countrycode(unfccc_data$CC,
                                 origin = "iso3c",
                                 destination = "country.name")


pa_data2 <- pa_data2[pa_data2$Country %in% c("Australia", "Austria", "Belgium",
                                                      "Canada", "Chile", "Colombia", "Czechia",
                                                      "Denmark", "Estonia", "Finland", "France",
                                                      "Germany", "Greece", "Hungary", "Iceland",
                                                      "Ireland", "Israel", "Italy", "Japan", 
                                                      "Korea, Republic of",
                                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                                      "Portugal", "Slovakia","Slovenia", "Spain", 
                                                      "Sweden", "Switzerland", "Turkey", 
                                                      "United Kingdom", "United States",
                                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                                      "Romania", "Argentina", "Brazil", "China",
                                                      "India", "Indonesia", "Russian Federation", 
                                                      "South Africa","Hong Kong, China", "Malaysia", "Philippines", 
                                                      "Singapore", "Taiwan, China"), ]

#fix cc
pa_data2$CC <- countrycode(pa_data2$Country, 
                              origin = "country.name",
                              destination = "iso3c") ##53 countries
#fix countryname
pa_data2$Country <- countrycode(pa_data2$CC,
                                   origin = "iso3c",
                                   destination = "country.name")


membership <- left_join(unfccc_data, pa_data2, 
                        by= c("CC", "Country", "Year"))

membership <- membership %>%
  dplyr::rename(pa_member = "Membership Status (Paris",
                unfccc_member = "Membership Status (UNFCCC)")

membership$pa_member[is.na(membership$pa_member)] <- -1

#### MERGE AGAIN ####
final_data2 <- left_join(final_data2, membership, 
                         by = c("CC", "Country", "Year"))

final_data2 <- left_join(final_data2, Kyoto_data,
                         by = c("CC", "Country", "Year"))

#### FIX KYOTO AND MEMBERSHIP ####
final_data2$Kyoto[is.na(final_data2$Kyoto)] <- 0

final_data2[1:32, c("pa_member", "unfccc_member")] <- -1
final_data2[60:91, c("pa_member", "unfccc_member")] <- -1
final_data2[119:150, c("pa_member", "unfccc_member")] <- -1
final_data2[178:209, c("pa_member", "unfccc_member")] <- -1
final_data2[237:268, c("pa_member", "unfccc_member")] <- -1
final_data2[296:327, c("pa_member", "unfccc_member")] <- -1
final_data2[355:386, c("pa_member", "unfccc_member")] <- -1
final_data2[414:445, c("pa_member", "unfccc_member")] <- -1
final_data2[473:504, c("pa_member", "unfccc_member")] <- -1
final_data2[532:563, c("pa_member", "unfccc_member")] <- -1
final_data2[591:622, c("pa_member", "unfccc_member")] <- -1
final_data2[650:681, c("pa_member", "unfccc_member")] <- -1
final_data2[709:740, c("pa_member", "unfccc_member")] <- -1
final_data2[768:799, c("pa_member", "unfccc_member")] <- -1
final_data2[827:858, c("pa_member", "unfccc_member")] <- -1
final_data2[886:917, c("pa_member", "unfccc_member")] <- -1
final_data2[945:976, c("pa_member", "unfccc_member")] <- -1
final_data2[1004:1035, c("pa_member", "unfccc_member")] <- -1
final_data2[1063:1094, c("pa_member", "unfccc_member")] <- -1
final_data2[1122:1153, c("pa_member", "unfccc_member")] <- -1
final_data2[1181:1212, c("pa_member", "unfccc_member")] <- -1
final_data2[1240:1271, c("pa_member", "unfccc_member")] <- -1
final_data2[1299:1330, c("pa_member", "unfccc_member")] <- -1
final_data2[1358:1389, c("pa_member", "unfccc_member")] <- -1
final_data2[1417:1448, c("pa_member", "unfccc_member")] <- -1
final_data2[1476:1507, c("pa_member", "unfccc_member")] <- -1
final_data2[1535:1566, c("pa_member", "unfccc_member")] <- -1
final_data2[1594:1625, c("pa_member", "unfccc_member")] <- -1
final_data2[1653:1684, c("pa_member", "unfccc_member")] <- -1
final_data2[1712:1743, c("pa_member", "unfccc_member")] <- -1
final_data2[1771:1802, c("pa_member", "unfccc_member")] <- -1
final_data2[1830:1861, c("pa_member", "unfccc_member")] <- -1
final_data2[1889:1920, c("pa_member", "unfccc_member")] <- -1
final_data2[1948:1979, c("pa_member", "unfccc_member")] <- -1
final_data2[2007:2038, c("pa_member", "unfccc_member")] <- -1
final_data2[2066:2097, c("pa_member", "unfccc_member")] <- -1
final_data2[2125:2156, c("pa_member", "unfccc_member")] <- -1
final_data2[2184:2215, c("pa_member", "unfccc_member")] <- -1
final_data2[2243:2274, c("pa_member", "unfccc_member")] <- -1
final_data2[2302:2333, c("pa_member", "unfccc_member")] <- -1
final_data2[2361:2392, c("pa_member", "unfccc_member")] <- -1
final_data2[2420:2451, c("pa_member", "unfccc_member")] <- -1
final_data2[2479:2510, c("pa_member", "unfccc_member")] <- -1
final_data2[2538:2569, c("pa_member", "unfccc_member")] <- -1
final_data2[2597:2628, c("pa_member", "unfccc_member")] <- -1
final_data2[2656:2687, c("pa_member", "unfccc_member")] <- -1
final_data2[2715:2746, c("pa_member", "unfccc_member")] <- -1
final_data2[2774:2805, c("pa_member", "unfccc_member")] <- -1
final_data2[2833:2864, c("pa_member", "unfccc_member")] <- -1
final_data2[2892:2923, c("pa_member", "unfccc_member")] <- -1
final_data2[2951:2982, c("pa_member", "unfccc_member")] <- -1
final_data2[3010:3041, c("pa_member", "unfccc_member")] <- -1
final_data2[3069:3100, c("pa_member", "unfccc_member")] <- -1
final_data2[3128:3159, c("pa_member", "unfccc_member")] <- -1
final_data2[3187:3218, c("pa_member", "unfccc_member")] <- -1

final_data2$pa_member[final_data2$pa_member == 2] <- 4
final_data2$pa_member[final_data2$pa_member == 1] <- 3
final_data2$pa_member[final_data2$pa_member == 0] <- 2
final_data2$pa_member[final_data2$pa_member == -1] <- 1
unique(final_data2$pa_member)

final_data2$unfccc_member[final_data2$unfccc_member == 2] <- 4
final_data2$unfccc_member[final_data2$unfccc_member == 1] <- 3
final_data2$unfccc_member[final_data2$unfccc_member == 0] <- 2
final_data2$unfccc_member[final_data2$unfccc_member == -1] <- 1
unique(final_data2$unfccc_member)

final_data2$Kyoto[final_data2$Kyoto == 3] <- 4
final_data2$Kyoto[final_data2$Kyoto == 2] <- 3
final_data2$Kyoto[final_data2$Kyoto == 1] <- 2
final_data2$Kyoto[final_data2$Kyoto == 0] <- 1
unique(final_data2$unfccc_member)

#### Make time trend variable ####
final_data2 <- final_data2 %>%
  group_by(Country) %>%
  mutate(timetrend = 0:58)

#### Remove some variables ####
final_data2$count2 <- NULL
final_data2$totcount <- NULL

#### Filter OECD countries ####
oecd_data <- final_data2 %>%
  dplyr::filter(Country %in% c("Australia", "Austria", "Belgium", "Canada",
                               "Chile", "Colombia", "Czechia", "Denmark", "Estonia",
                               "Finland", "France", "Germany", "Greece", "Hungary",
                               "Iceland", "Ireland", "Israel", "Italy", "Japan",
                               "South Korea", "Latvia", "Lithuania", "Luxembourg",
                               "Mexico", "Netherlands", "New Zealand", "Norway",
                               "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                               "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"))

#### RENAME VARIABLE ####
final_data2 <- final_data2 %>%
  dplyr::rename(kyoto_member = Kyoto)

oecd_data <- oecd_data %>%
  dplyr::rename(kyoto_member = Kyoto)


#### SAVE DATA ####
#as csv
write.csv(final_data2, file = "thesis_data.csv")
write.csv(oecd_data, file = "oecd_data.csv")

#for stata
write.dta(final_data2, "thesisdata.dta")
write.dta(oecd_data, file = "oecd_data.dta")

#### Fix GDP ####
gdptest <- read.csv("data_csv.csv")

gdptest <- gdptest[gdptest$Country.Name %in% c("Australia", "Austria", "Belgium",
                                             "Canada", "Chile", "Colombia", "Czech Republic",
                                             "Denmark", "Estonia", "Finland", "France",
                                             "Germany", "Greece", "Hungary", "Iceland",
                                             "Ireland", "Israel", "Italy", "Japan", 
                                             "Korea, Rep.",
                                             "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                             "Netherlands", "New Zealand", "Norway", "Poland",
                                             "Portugal", "Slovakia","Slovenia", "Spain", 
                                             "Sweden", "Switzerland", "Turkey", 
                                             "United Kingdom", "United States",
                                             "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                             "Romania", "Argentina", "Brazil", "China",
                                             "India", "Indonesia", "Russian Federation", 
                                             "South Africa","Hong Kong SAR, China", "Malaysia", "Philippines", 
                                             "Singapore", "Taiwan, China"), ]


gdptest <- gdptest %>%
  dplyr::rename(Country = Country.Name,
                CC = Country.Code)

#fix cc
gdptest$CC <- countrycode(gdptest$Country, 
                           origin = "country.name",
                           destination = "iso3c") ##53 countries
#fix countryname
gdptest$Country <- countrycode(gdptest$CC,
                                origin = "iso3c",
                                destination = "country.name")


#### Merge GDP ####
thesisdata <- read_csv("thesis_data.csv")
thesisdata <- left_join(thesisdata, gdptest,
                        by = c("CC", "Country", "Year"))

thesisdata <- thesisdata %>%
  dplyr::rename(GDPpercapita = Value)

thesisdata$GDPperemployed <- NULL
thesisdata$GDPperhead <- NULL
thesisdata$totlaw_count <- NULL
thesisdata$Subject.x <- NULL

#### SAVE AGAIN ####
write.csv(thesisdata, file = "thesis_data.csv")
#write.csv(oecd_data, file = "oecd_data.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
#write.dta(oecd_data, file = "oecd_data.dta")

#### Industry as GDP ####
Industry <- read.csv("InudstrypercentGDP.csv")

Industry <- Industry[Industry$Country.Name %in% c("Australia", "Austria", "Belgium",
                                               "Canada", "Chile", "Colombia", "Czech Republic",
                                               "Denmark", "Estonia", "Finland", "France",
                                               "Germany", "Greece", "Hungary", "Iceland",
                                               "Ireland", "Israel", "Italy", "Japan", 
                                               "Korea, Rep.",
                                               "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                               "Netherlands", "New Zealand", "Norway", "Poland",
                                               "Portugal", "Slovakia","Slovenia", "Spain", 
                                               "Sweden", "Switzerland", "Turkey", 
                                               "United Kingdom", "United States",
                                               "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                               "Romania", "Argentina", "Brazil", "China",
                                               "India", "Indonesia", "Russian Federation", 
                                               "South Africa","Hong Kong SAR, China", "Malaysia", "Philippines", 
                                               "Singapore", "Taiwan, China"), ]

Industry <- Industry %>%
  dplyr::rename(CC = Country.Code,
                Country = Country.Name)

#fix cc
Industry$CC <- countrycode(Industry$Country, 
                          origin = "country.name",
                          destination = "iso3c") ##53 countries
#fix countryname
Industry$Country <- countrycode(Industry$CC,
                               origin = "iso3c",
                               destination = "country.name")


#### Merge again ####
thesisdata <- left_join(thesisdata, Industry,
                        by = c("CC", "Country", "Year"))

thesisdata <- thesisdata %>%
  dplyr::rename(Industry = Value)

#### SAVE AGAIN ####
write.csv(thesisdata, file = "thesis_data.csv")
#write.csv(oecd_data, file = "oecd_data.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
#write.dta(oecd_data, file = "oecd_data.dta")

#### Party information ####
vparty <- read_csv("V-Dem-CPD-Party-V1.csv")

vparty <- vparty[vparty$country_name %in% c("Australia", "Austria", "Belgium",
                                      "Canada", "Chile", "Colombia", "Czech Republic",
                                      "Denmark", "Estonia", "Finland", "France",
                                      "Germany", "Greece", "Hungary", "Iceland",
                                      "Ireland", "Israel", "Italy", "Japan", 
                                      "South Korea",
                                      "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                      "Netherlands", "New Zealand", "Norway", "Poland",
                                      "Portugal", "Slovakia","Slovenia", "Spain", 
                                      "Sweden", "Switzerland", "Turkey", 
                                      "United Kingdom", "United States of America",
                                      "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                      "Romania", "Argentina", "Brazil", "China",
                                      "India", "Indonesia", "Russia", 
                                      "South Africa","Hong Kong", "Malaysia", "Philippines", 
                                      "Singapore", "Taiwan"), ]

vparty <- vparty %>%
  dplyr::select(country_name, country_text_id, v2paid, year, v2pasalie_12, v2paseatshare, v2pavote)

vparty <- vparty %>%
  dplyr::filter(year >= 1960)

vparty <- vparty %>%
  dplyr::rename(Country = country_name,
                CC = country_text_id,
                Year = year,
                Party = v2paid,
                environment = v2pasalie_12,
                seatshare = v2paseatshare,
                voteshare = v2pavote)

#fix cc
vparty$CC <- countrycode(vparty$Country, 
                           origin = "country.name",
                           destination = "iso3c") ##53 countries
#fix countryname
vparty$Country <- countrycode(vparty$CC,
                                origin = "iso3c",
                                destination = "country.name")


vparty <- vparty %>%
  dplyr::filter(environment >= 0.5)

vparty <- vparty %>%
  group_by(Country, Year) %>%
  mutate(environshare = cumsum(environment>= 0 & seatshare != 0))

vparty <- vparty %>%
  group_by(Country, Year) %>%
  mutate(environvote = cumsum(environment>= 0 & voteshare != 0 ))

vparty <- vparty[- c(103, 93, 91, 56, 45, 21:13), ]

table(vparty$Country, vparty$Year)

#### merge again ####
thesisdata <- left_join(thesisdata, vparty,
                        by = c("Country", "CC", "Year"))


#### Fix green party variable ####
thesisdata <- thesisdata %>%
  dplyr::group_by(Country) %>%
  fill(environvote, .direction = "down")

thesisdata <- thesisdata %>%
  dplyr::group_by(Country) %>%
  fill(environshare, .direction = "down")

thesisdata$environshare[is.na(thesisdata$environshare)] <- 0
thesisdata$environvote[is.na(thesisdata$environvote)] <- 0


#### check alternative fossil ####
fossil <- read_csv("Ross-Mahdavi Oil and Gas 1932-2014.csv")

fossil <- fossil %>%
  dplyr::filter(year >= 1960)

fossil <- fossil %>%
  dplyr::select(cty_name, id, year, oil_prod32_14, gas_prod55_14, oil_exports, gas_exports)

fossil <- fossil %>%
  dplyr::rename(Country = cty_name,
                CC = id,
                Year = year,
                Oilprod = oil_prod32_14,
                Gasprod = gas_prod55_14,
                Oilexport = oil_exports,
                Gasexport = gas_exports)

#### merge again ####
thesisdata <- left_join(thesisdata, fossil, 
                        by = c("Country", "CC", "Year"))

#### lag fossil variables ####
thesisdata$Oilprod <- lag(thesisdata$Oilprod, 4)
thesisdata$Gasprod <- lag(thesisdata$Gasprod, 4)
thesisdata$Gasexport <- lag(thesisdata$Gasexport, 4)
thesisdata$Oilexport <- lag(thesisdata$Oilexport, 4)

thesisdata <- thesisdata %>%
  group_by(Country, Year) %>%
  fill(Oilprod, .direction = "down")

thesisdata <- thesisdata %>%
  group_by(Country, Year) %>%
  fill(Gasprod, .direction = "down")

thesisdata <- thesisdata %>%
  group_by(Country, Year) %>%
  fill(Oilexport, .direction = "down")

thesisdata <- thesisdata %>%
  group_by(Country, Year) %>%
  fill(Gasexport, .direction = "down")

thesisdata$Oilprod[is.na(thesisdata$Oilprod)] <- 0
thesisdata$Gasprod[is.na(thesisdata$Gasprod)] <- 0
thesisdata$Oilexport[is.na(thesisdata$Oilexport)] <- 0
thesisdata$Gasexport[is.na(thesisdata$Gasexport)] <- 0

#### Remove old fossil ####
thesisdata$oilprod <- NULL
thesisdata$gasprod <- NULL
thesisdata$coalprod <- NULL

#### drop early years ####
thesisdata_90 <- thesisdata %>%
  dplyr::filter(Year >= 1990)

#### SAVE AGAIN ####
write.csv(thesisdata, file = "thesis_data.csv")
write.csv(thesisdata_90, file = "thesis_data90.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
write.dta(thesisdata_90, file = "thesisdata_90.dta")




#### producer variable ####
thesisdata <- thesisdata %>%
  group_by(Country, Year) %>%
  mutate(fossilproducer = ifelse(Oilprod > 0 | Gasprod > 0, 1, 0),
         fossilexport = ifelse(Oilexport > 0 | Gasexport > 0, 1, 0))

thesisdata_90 <- thesisdata %>%
  dplyr::filter(Year >= 1990)

#### Save again ####
write.csv(thesisdata, file = "thesis_data.csv")
write.csv(thesisdata_90, file = "thesis_data90.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
write.dta(thesisdata_90, file = "thesisdata_90.dta")


#### Make independent years variable ####
#Fill in missing

thesisdata[1:23, c("Indep")] <- 1
thesisdata[237:268, c("Indep")] <- 1
thesisdata[296:327, c("Indep")] <- 0
thesisdata[414:444, c("Indep")] <- 1
thesisdata[473:531, c("Indep")] <- 1
thesisdata[532:590, c("Indep")] <- 1
thesisdata[591:649, c("Indep")] <- 1
thesisdata[650:680, c("Indep")] <- 0
thesisdata[681:689, c("Indep")] <- 1
thesisdata[709:738, c("Indep")] <- 1
thesisdata[768:800, c("Indep")] <- 0
thesisdata[886:918, c("Indep")] <- 0
thesisdata[1122:1136, c("Indep")] <- 1
thesisdata[1181:1239, c("Indep")] <- 0
thesisdata[1240:1269, c("Indep")] <- 0
thesisdata[1299:1357, c("Indep")] <- 1
thesisdata[1358:1416, c("Indep")] <- 1
thesisdata[1417:1475, c("Indep")] <- 1
thesisdata[1712:1770, c("Indep")] <- 1
thesisdata[1771:1803, c("Indep")] <- 0
thesisdata[1830:1862, c("Indep")] <- 0
thesisdata[1948:1950, c("Indep")] <- 0
thesisdata[1951:2006, c("Indep")] <- 1
thesisdata[2007:2010, c("Indep")] <- 0
thesisdata[2011:2036, c("Indep")] <- 1
thesisdata[2066:2093, c("Indep")] <- 1
thesisdata[2302:2360, c("Indep")] <- 1
thesisdata[2361:2390, c("Indep")] <- 0
thesisdata[2420:2435, c("Indep")] <- 1
thesisdata[2479:2511, c("Indep")] <- 1
thesisdata[2538:2596, c("Indep")] <- 1
thesisdata[2656:2688, c("Indep")] <- 0
thesisdata[2715:2744, c("Indep")] <- 0
thesisdata[2774:2807, c("Indep")] <- 1
thesisdata[2833:2850, c("Indep")] <- 0
thesisdata[3010:3068, c("Indep")] <- 0
thesisdata[3069:3075, c("Indep")] <- 1
thesisdata[3187:3245, c("Indep")] <- 1

### Count
thesisdata <- thesisdata %>%
  group_by(Country)%>%
  mutate(indepyears = cumsum(Indep))

#### Save again ####
write.csv(thesisdata, file = "thesis_data.csv")
write.csv(thesisdata_90, file = "thesis_data90.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
write.dta(thesisdata_90, file = "thesisdata_90.dta")



#### Trade openness ####
trade <- read_xls("Tradedata.xls")
trade = trade[-1,]
trade=trade[-1,]

trade$...3 <- NULL
trade$...4 <- NULL

names(trade) <- as.matrix(trade[1, ])
trade <- trade[-1, ]
trade[] <- lapply(trade, function(x) type.convert(as.character(x)))

trade <- trade %>%
  dplyr::rename(Country = 'Country Name',
                CC = 'Country Code')

trade <- trade[trade$Country %in% c("Australia", "Austria", "Belgium",
                                    "Canada", "Chile", "Colombia", "Czech Republic",
                                    "Denmark", "Estonia", "Finland", "France",
                                    "Germany", "Greece", "Hungary", "Iceland",
                                    "Ireland", "Israel", "Italy", "Japan", 
                                    "Korea, Rep.",
                                    "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                    "Netherlands", "New Zealand", "Norway", "Poland",
                                    "Portugal", "Slovak Republic","Slovenia", "Spain", 
                                    "Sweden", "Switzerland", "Turkey", 
                                    "United Kingdom", "United States",
                                    "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                    "Romania", "Argentina", "Brazil", "China",
                                    "India", "Indonesia", "Russian Federation", 
                                    "South Africa","Hong Kong SAR, China", "Malaysia", "Philippines", 
                                    "Singapore", "Taiwan, China"), ]


trade <- pivot_longer(trade, cols = -c("Country", "CC"),
                          names_to = "Year", values_to = "trade")

#fix cc
trade$CC <- countrycode(trade$Country, 
                            origin = "country.name",
                            destination = "iso3c") ##53 countries
#fix countryname
trade$Country <- countrycode(trade$CC,
                                 origin = "iso3c",
                                 destination = "country.name")

#### MERGE AGAIN ####
trade$Year <- as.character(trade$Year)
thesisdata$Year <- as.character(thesisdata$Year)
thesisdata <- left_join(thesisdata, trade, 
                        by = c("Country", "CC", "Year"))

thesisdata_90 <- thesisdata %>%
  dplyr::filter(Year >= 1990)

#### save again ####
write.csv(thesisdata, file = "thesis_data.csv")
write.csv(thesisdata_90, file = "thesis_data90.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
write.dta(thesisdata_90, file = "thesisdata_90.dta")




#### Fix more vote ####
thesisdata <- thesisdata %>%
  dplyr::group_by(Country) %>%
  fill(voteshare, .direction = "down")

thesisdata <- thesisdata %>%
  dplyr::group_by(Country) %>%
  fill(seatshare, .direction = "down")

thesisdata$voteshare[is.na(thesisdata$voteshare)] <- 0
thesisdata$seatshare[is.na(thesisdata$seatshare)] <- 0





#### save again ####
thesisdata$Party <- NULL

thesisdata_90 <- thesisdata %>%
  dplyr::filter(Year >= 1990)

write.csv(thesisdata, file = "thesis_data.csv")
write.csv(thesisdata_90, file = "thesis_data90.csv")

#for stata
write.dta(thesisdata, "thesisdata.dta")
write.dta(thesisdata_90, file = "thesisdata_90.dta")



#### Make EU membership variable ####
thesisdata <- thesisdata %>%
  mutate(EUmember = ifelse(Year >= 1960 & Country %in% c("Belgium", "France", "Germany", "Italy", "Luxembourg",
                                                         "Netherlands") |
                             Year >= 1973 & Country %in% c("Denmark", "Ireland", "United Kingdom") |
                             Year >= 1981 & Country %in% c("Greece") |
                             Year >= 1986 & Country %in% c("Portugal", "Spain") |
                             Year >= 1995 & Country %in% c("Austria", "Finland", "Sweden") |
                             Year >= 2004 & Country %in% c("Cyprus", "Czhech Republic", "Estonia", "Hungary",
                                                           "Latvia", "Lithuania", "Malta", "Poland", 
                                                           "Slovak Republic", "Slovenia") |
                             Year >= 2007 & Country %in% c("Bulgaria", "Romania") |
                             Year >= 2013 & Country %in% c("Croatioa"), 1, 0))


thesisdata <- thesisdata %>%
  group_by(Country) %>%
  dplyr::mutate(EUdic = cumsum(EUmember))

#### Other emissions data ####
# emissionspercapita <- read_csv("co-emissions-per-capita (1).csv")
# 
# # emissionspercapita <- emissionspercapita[emissionspercapita$Entity %in% c("Australia", "Austria", "Belgium",
# #                                     "Canada", "Chile", "Colombia", "Czechia",
# #                                     "Denmark", "Estonia", "Finland", "France",
# #                                     "Germany", "Greece", "Hungary", "Iceland",
# #                                     "Ireland", "Israel", "Italy", "Japan", 
# #                                     "South Korea",
# #                                     "Latvia", "Lithuania", "Luxembourg", "Mexico",
# #                                     "Netherlands", "New Zealand", "Norway", "Poland",
# #                                     "Portugal", "Slovakia","Slovenia", "Spain", 
# #                                     "Sweden", "Switzerland", "Turkey", 
# #                                     "United Kingdom", "United States",
# #                                     "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
# #                                     "Romania", "Argentina", "Brazil", "China",
# #                                     "India", "Indonesia", "Russia", 
# #                                     "South Africa","Hong Kong", "Malaysia", "Philippines", 
# #                                     "Singapore", "Taiwan, China"), ]
# 
# emissionspercapita <- emissionspercapita %>%
#   dplyr::rename(Country = Entity,
#                 CC = Code)
# 
# 
# #fix cc
# emissionspercapita$CC <- countrycode(emissionspercapita$Country, 
#                         origin = "country.name",
#                         destination = "iso3c") ##53 countries
# #fix countryname
# emissionspercapita$Country <- countrycode(emissionspercapita$CC,
#                              origin = "iso3c",
#                              destination = "country.name")
# 
# emissionspercapita$Year <- as.character(emissionspercapita$Year)
# thesisdata$Year <- as.character(thesisdata$Year)
# 
# test <- left_join(thesisdata, emissionspercapita, 
#                   by = c("CC", "Country", "Year"))

#### Population to make emissions per capita ####
population <- read_xlsx("populationdata.xlsx")

population = population[-1,]
population=population[-1,]

population$...3 <- NULL
population$...4 <- NULL

names(population) <- as.matrix(population[1, ])
population <- population[-1, ]
population[] <- lapply(population, function(x) type.convert(as.character(x)))

population <- population %>%
  dplyr::rename(Country = 'Country Name',
                CC = 'Country Code')

population <- population[population$Country %in% c("Australia", "Austria", "Belgium",
                                    "Canada", "Chile", "Colombia", "Czech Republic",
                                    "Denmark", "Estonia", "Finland", "France",
                                    "Germany", "Greece", "Hungary", "Iceland",
                                    "Ireland", "Israel", "Italy", "Japan", 
                                    "Korea, Rep.",
                                    "Latvia", "Lithuania", "Luxembourg", "Mexico",
                                    "Netherlands", "New Zealand", "Norway", "Poland",
                                    "Portugal", "Slovak Republic","Slovenia", "Spain", 
                                    "Sweden", "Switzerland", "Turkey", 
                                    "United Kingdom", "United States",
                                    "Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Malta",
                                    "Romania", "Argentina", "Brazil", "China",
                                    "India", "Indonesia", "Russian Federation", 
                                    "South Africa","Hong Kong SAR, China", "Malaysia", "Philippines", 
                                    "Singapore", "Taiwan, China"), ]


population <- pivot_longer(population, cols = -c("Country", "CC"),
                      names_to = "Year", values_to = "population")

#fix cc
population$CC <- countrycode(population$Country, 
                        origin = "country.name",
                        destination = "iso3c") ##53 countries
#fix countryname
population$Country <- countrycode(population$CC,
                             origin = "iso3c",
                             destination = "country.name")


#### merge again ####
thesisdata <- left_join(thesisdata, population, 
                        by = c("CC", "Country", "Year"))

#### make emissions per capita ####
thesisdata <- thesisdata %>%
  dplyr::mutate(emissionsPC = (emissions/population))

#### recode GDP ####
thesisdata$logGDP <- log(thesisdata$GDPpercapita)
thesisdata$logemissions <- log(thesisdata$emissionsPC)

thesisdata_90$logGDP <- log(as.numeric(thesisdata_90$GDPpercapita))
thesisdata_90$logemissions <- log(as.numeric(thesisdata_90$emissionsPC))

#### MORE UNION DENSITY ####
UD2 <- read.csv("UD2.csv")

UD2$note_indicator.label <- NULL
UD2$indicator.label <- NULL
UD2$note_source.label <- NULL
UD2$source.label <- NULL

UD2 <- UD2 %>%
  dplyr::rename(Country = "ï..ref_area.label",
                Year = "time",
                UD2 = "obs_value")

#fix cc
UD2$CC <- countrycode(UD2$Country, 
                             origin = "country.name",
                             destination = "iso3c") 
#fix countryname
UD2$Country <- countrycode(UD2$CC,
                                  origin = "iso3c",
                                  destination = "country.name")

oecddata <- left_join(oecddata, UD2, 
                      by = c("Country", "CC", "Year"))


#### fix ud variable ####
oecddata <- oecddata %>%
  dplyr::mutate(UD = ifelse(is.na(UD), UD2, UD))

#### LAG independent variables ####
oecddata <- oecddata %>%
  group_by(Country) %>%
  dplyr::mutate(lagUD = dplyr::lag(UD, 3),
         lagCorp = dplyr::lag(CorpAll, 3),
         lagIndustry = dplyr::lag(Industry, 3),
         laglogGDP = dplyr::lag(logGDP, 3),
         lagDem = dplyr::lag(Dem, 3),
         laglogemssions = dplyr::lag(logemissions, 3),
         lagtrade = dplyr::lag(trade, 3),
         lagenvironshare = dplyr::lag(environshare, 3),
         lagEU = dplyr::lag(EUmember, 3))

data.frame(oecddata$UD, oecddata$lagUD)

oecddata_LAG <- oecddata %>%
  dplyr::select(Country, Year, count1, UD, lagUD)


#### Save again ####
write.csv(oecddata, file = "thesis_data.csv")

#for stata
write.dta(oecddata, "thesisdata.dta")

#### union density data - for descriptives ####
## Other UD variables
# UD_public,UD_private,UD_manual, UD_nonmanual, UD_agr,UD_ind,UD_serv,UD_mining,
# UD_manuf,UD_metal,UD_util,UD_constr,UD_trade,UD_hotels,UD_transport,UD_finance,UD_business,
# UD_commercial,UD_socialserv,UD_publadmin,UD_educ, UD_health,UD_otherserv,

UD_data <- ICTWSS_data %>%
  dplyr::select(Country = "ï..country",
                CC = "iso2c",
                Year = "year",
                UD, UD_public, UD_private, UD_manual, UD_nonmanual, UD_agr,UD_ind,UD_serv,UD_mining,
                UD_manuf,UD_metal,UD_util,UD_constr,UD_trade,UD_hotels,UD_transport,UD_finance,UD_business,
                UD_commercial,UD_socialserv,UD_publadmin,UD_educ, UD_health,UD_otherserv)


#fix cc
UD_data$CC <- countrycode(UD_data$Country, 
                          origin = "country.name",
                          destination = "iso3c")
#fiX countryname
UD_data$Country <- countrycode(UD_data$CC,
                               origin = "iso3c",
                               destination = "country.name")
#subset countries
#only OECD
UD_data <- UD_data[UD_data$CC %in% c("AUT", "AUS", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST",
                                     "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
                                     "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
                                     "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR","USA"), ]

UD_data <- UD_data %>%
  dplyr::filter(Year >= 1990)

##save
write.csv(UD_data, file = "UDdata.csv")

#for stata
write.dta(UD_data, "UDdata.dta")


