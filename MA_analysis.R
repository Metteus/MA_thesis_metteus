#### LIBRARY ####
packages <- c("dplyr", "tidyverse", "tidyr", "haven", "margins",
              "readxl", "knitr", "ggplot2", "msm", "plm", "interplot",
              "stargazer", "foreign", "readr", "panelr", "ggseas",
              "sandwich", "gghighlight", "psy","gmm", "zoo", "effects",
              "MASS", "xtable", "texreg", "psych", "separationplot",
              "multiwayvcov", "countrycode", "moments", "jtools", "broom", "ggstance",
              "lmtest","MASS", "broom",  "DescTools", "forecast",
              "cowplot", "plotROC", "pscl", "ordinal", "data.table",
              "nnet", "countreg", "car", "sjPlot", "sjlabelled", "sjmisc") ## Vector of needed packages

sapply(packages, function(x) library(x, character.only = TRUE))
 
#### WD ####
setwd("C:/Users/mette/OneDrive/Dokumenter/R/Masterdatasett/Raw data")

#### LOAD DATA ####
thesisdata <- read.csv("thesis_data.csv")
oecddata <- thesisdata %>%
  dplyr::filter(CC %in% c("AUT", "AUS", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST",
                          "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA",
                          "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR",
                         "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR",
                                 "USA"))
#### New WD ####
setwd("C:/Users/mette/Dropbox/Apper/Overleaf/MA_thesis_Metteus2021/R_figures")
####################################
#            BASIC MODELS           #
####################################
#### Base Poisson ####
poisson_base <- glm(count1 ~ UD + as.factor(Country),
                    oecddata,
                    family=poisson)
poisson_base0 <- glm(count1 ~ CorpAll + as.factor(Country),
                     oecddata,
                     family=poisson)

#Model 1 (country fixed effects)
poisson_base1 <- glm(count1 ~ UD + CorpAll + 
                    as.factor(Country),
                  oecddata,
                  family = poisson)

#Model 2 (both fixed effects)
poisson_base2 <- glm(count1 ~ UD + CorpAll + 
                    as.factor(Country) + as.factor(Year),
                  oecddata,
                  family = poisson)


###Model 3 (lagged dependent as independent, country fixed effects)
poisson_base3 <- glm(count1 ~ UD + CorpAll + lag(count1) + as.factor(Country), 
                  oecddata,
                   family = poisson)

#Model 4 (timetred)
poisson_base4 <- glm(count1 ~ UD + CorpAll + as.factor(Country) + timetrend,
                   oecddata, 
                   family = poisson)


#Results with clustered SE
writeLines(capture.output(stargazer(poisson_base, poisson_base0, poisson_base1, poisson_base2, poisson_base3, poisson_base4, 
          omit = c("Country", "Year"),
          se = list(sqrt(diag(vcovCL(poisson_base, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(poisson_base0, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(poisson_base1, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(poisson_base2, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(poisson_base3, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(poisson_base4, cluster = oecddata$Country)))),
          add.lines=list(c('Country fixed effects', 'Yes','Yes', 'Yes','Yes', 'Yes', 'Yes'),
                         c('Year fixed effects', 'No','No','No', 'Yes', 'No', 'No'),
                         c('Standard errors clustered on country', 'Yes', 'Yes','Yes', 'Yes', 'Yes', 'Yes')),
          title = "Base models: Poisson", 
          dep.var.caption = "Yearly adopted climate policies",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          covariate.labels = 
            c("Union density", "Corporatism", "Lagged dependent", "Timetrend"))), "basepoisson.tex")

#### Base negative binomial ####
negbin <- glm.nb(poisson_base$formula, 
                 data = poisson_base$data)

negbin0 <- glm.nb(poisson_base0$formula,
                  data = poisson_base0$data)

negbin1 <- glm.nb(poisson_base1$formula, 
                 data = poisson_base1$data)

negbin2 <- glm.nb(poisson_base2$formula,
                  data= poisson_base2$data)

negbin3 <- glm.nb(poisson_base3$formula,
                  data = poisson_base3$data)

negbin4 <- glm.nb(poisson_base4$formula,
                  data = poisson_base4$data)


writeLines(capture.output(stargazer(negbin, negbin0, negbin1, negbin2, negbin3, negbin4, 
          omit = c("Country", "Year"),
          se = list(sqrt(diag(vcovCL(negbin, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin0, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin1, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin2, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin3, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin4, cluster = oecddata$Country)))),
          add.lines=list(c('Country fixed effects', 'Yes', 'Yes','Yes', 'Yes','Yes', 'Yes'),
                         c('Year fixed effects', 'No','No','No', 'Yes', 'No', 'No'),
                         c('Standard errors clustered on country', 'Yes', 'Yes','Yes', 'Yes', 'Yes', 'Yes')),
          title = "Base models: Negative Binomial", 
          object.names = FALSE,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          dep.var.caption = "Yearly adopted climate policies",
          covariate.labels = 
            c("Union density", "Corporatism", "Lagged dependent", "Timetrend"))), "basenegbin.tex")


#########################################
#         With control variables        #
#########################################
#### NEGATIVE BINOMIAL: controls and offsets compared (appendix) ####
negbin_mod1 <- glm.nb(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry +
                         trade + environshare + EUmember  + as.factor(Country) + timetrend,
                      data = oecddata)

negbin_mod12 <- glm.nb(count1 ~ UD + CorpAll + logGDP +  Dem + logemissions + Industry +
                        trade + environshare + EUmember  + as.factor(Country) + timetrend +
                         offset(log(indepyears)),
                      data = oecddata)

negbin_mod2 <- glm.nb(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry +
                         trade + environshare + EUmember  + as.factor(Country) + as.factor(Year),
                      data = oecddata)

negbin_mod22 <- glm.nb(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember  + as.factor(Country) + as.factor(Year) + 
                         offset(log(indepyears)),
                      data = oecddata)

negbin_mod3 <- glm.nb(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry + trade + 
                        environshare + EUmember + as.factor(Country) + lag(count1), 
                      data = oecddata)

negbin_mod32 <- glm.nb(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry + trade + 
                        environshare + EUmember + as.factor(Country) + lag(count1) + 
                        offset(log(indepyears)), 
                      data = oecddata)

writeLines(capture.output(stargazer(negbin_mod1, negbin_mod12, negbin_mod2, 
          negbin_mod22, negbin_mod3, negbin_mod32,
          omit = c("Year", "Country"),
          se = list(sqrt(diag(vcovCL(negbin_mod1, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod12, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod2, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod22, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod3, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod32, cluster = oecddata$Country)))),
          add.lines=list(c('Country fixed effects','Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
                         c('Year fixed effects', 'No', 'No', 'Yes', 'Yes', 'No', 'No'),
                         c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')),
          title = "Negative binomial, comparing with and without offset specification", 
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes.align = "l",
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          dep.var.labels = c("Yearly adopted climate policies"),
          covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                               "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                               "EU member", "Timetrend", "Lagged dependent"),
          column.labels = c("", "Offset: independent years", "", "Offset: independent years","", "Offset: independent years"))), 
"negbinoffset.tex")

#### MAIN MODELS: no interaction, negative binomial ####
writeLines(capture.output(stargazer(negbin_mod12, negbin_mod22, negbin_mod32, 
          omit = c("Country", "Year"), 
          se = list(sqrt(diag(vcovCL(negbin_mod12, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod22, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_mod32, cluster = oecddata$Country)))),
          add.lines=list(c('Country fixed effects','Yes', 'Yes', 'Yes'),
                         c('Year fixed effects', 'No', 'Yes', 'No'),
                         c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes'),
                         c('Offset specification', 'Independent years (log)', 'Independent years (log)', 'Independent years (log)')),
          title = "Main models: negative binomial models", 
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes.align = "l",
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          dep.var.labels = c("Yearly adopted climate policies"),
          covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                               "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                               "EU member", "Timetrend", "Lagged dependent"),
          column.labels = c("Timetrend model", "FE", "AR(1)"))), 
"mainmodels.tex")

#### MAIN MODELS: only control variables ####
negbin_base1 <- glm.nb(count1 ~ logGDP + Dem + logemissions + Industry +
                         trade + environshare + EUmember  + as.factor(Country) + timetrend +
                         offset(log(indepyears)),
                       data = oecddata)

negbin_base2 <- glm.nb(count1 ~ logGDP + Dem + logemissions + Industry +
                         trade + environshare + EUmember  + as.factor(Country) + as.factor(Year) + 
                         offset(log(indepyears)),
                       data = oecddata)

negbin_base3 <- glm.nb(count1 ~ logGDP + Dem + logemissions + Industry + trade + 
                         environshare + EUmember + as.factor(Country) + lag(count1) + 
                         offset(log(indepyears)), 
                       data = oecddata)


writeLines(capture.output(stargazer(negbin_base1, negbin_base2, negbin_base3, 
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_base1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_base2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_base3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Country fixed effects','Yes', 'Yes', 'Yes'),
                                                   c('Year fixed effects', 'No', 'Yes', 'No'),
                                                   c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes'),
                                                   c('Offset specification', 'Independent years (log)', 'Independent years (log)', 'Independent years (log)')),
                                    title = "Negative binomial: only control variables", 
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member", "Timetrend", "Lagged dependent"),
                                    column.labels = c("Timetrend model", "FE", "AR(1)"))), 
           "controlvariablemodels.tex")

#### SCENARIOS: union density ####
##MODEL 1##
#scenario 1
set.seed(567890)

simb <-mvrnorm(n= 1000,
               mu= negbin_mod1$coefficients,
               Sigma= vcovCL(negbin_mod1, cluster = oecddata$Country))


#Sequence from min to max 
ourRange <- seq(min(oecddata$UD, na.rm = TRUE),
                max(oecddata$UD, na.rm = TRUE)) 

negbin_mod1$coefficients
ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0, 
              0,0,
              0) #timetrend

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + 
  labs(title = "Scenario 1, model 1")
ggsave("Scenario1model1.pdf", units = "in", height = 8.5, width = 11)


#scenario 2
set.seed(567890)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              min(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,60) +
  labs(title = "Scenario 2, model 1", caption = "Minimun level of corporatism")
ggsave("Scenario2model1.pdf", units = "in", height = 8.5, width = 11)


#scenario 3
set.seed(900)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              max(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 3, model 1", caption = "Max level of corporatism")
ggsave("Scenario3model1.pdf", units = "in", height = 8.5, width = 11)


#scenario 4
set.seed(999)

negbin_mod32$coefficients
ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              min(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 4, model 1", caption = "Minimum level of industry")
ggsave("Scenario4model1.pdf", units = "in", height = 8.5, width = 11)

#scenario 5
set.seed(997)

negbin_mod32$coefficients
ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              max(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 5, model 1", caption = "Max level of industry")
ggsave("Scenario5model1.pdf", units = "in", height = 8.5, width = 11)


##MODEL 2##
#scenario 1
set.seed(56293)

simb <-mvrnorm(n= 1000,
               mu= negbin_mod2$coefficients,
               Sigma= vcovCL(negbin_mod2, cluster = oecddata$Country))


#Sequence from min to max 
ourRange <- seq(min(oecddata$UD, na.rm = TRUE),
                max(oecddata$UD, na.rm = TRUE)) 


negbin_mod2$coefficients

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #countries and years.

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + 
  labs(title = "Scenario 1, model 2")
ggsave("Scenario1model2.pdf", units = "in", height = 8.5, width = 11)


#scenario 2
set.seed(56777)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              min(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,60) +
  labs(title = "Scenario 2, model 2", caption = "Minimun level of corporatism")
ggsave("Scenario2model2.pdf", units = "in", height = 8.5, width = 11)


#scenario 3
set.seed(9020)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              max(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 3, model 2", caption = "Max level of corporatism")
ggsave("Scenario3model2.pdf", units = "in", height = 8.5, width = 11)


#scenario 4
set.seed(9929)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              min(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 4, model 2", caption = "Minimum level of industry")
ggsave("Scenario4model2.pdf", units = "in", height = 8.5, width = 11)

#scenario 5
set.seed(9927)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              max(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 5, model 2", caption = "Max level of industry")
ggsave("Scenario5model2.pdf", units = "in", height = 8.5, width = 11)

##MODEL 3##
#scenario 1
set.seed(56780)

simb <-mvrnorm(n= 1000,
               mu= negbin_mod1$coefficients,
               Sigma= vcovCL(negbin_mod1, cluster = oecddata$Country))


#Sequence from min to max 
ourRange <- seq(min(oecddata$UD, na.rm = TRUE),
                max(oecddata$UD, na.rm = TRUE)) 

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0, 
              0,0,
              0) #the lagged dep.

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + 
  labs(title = "Scenario 1, model 3")
ggsave("Scenario1.pdf", units = "in", height = 8.5, width = 11)


#scenario 2
set.seed(567890)

ourXmin <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              min(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourXmin) # it is true!

xbetasmin <- ourXmin %*% t(simb)
expYmin <- exp(xbetasmin)
quantileValuesmin <- apply(X = expYmin,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPointsmin <- as.data.frame(cbind(ourRange, t(quantileValuesmin)))

ggplot(plotPointsmin,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,60) +
  labs(title = "Scenario 2, model 3")
ggsave("Scenario2.pdf", units = "in", height = 8.5, width = 11)


#scenario 3
set.seed(900)

ourXmax <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              max(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              0) #

ncol(simb) == ncol(ourXmax) # it is true!

xbetasmax <- ourXmax %*% t(simb)
expYmax <- exp(xbetasmax)
quantileValuesmax <- apply(X = expYmax,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPointsmax <- as.data.frame(cbind(ourRange, t(quantileValuesmax)))

ggplot(plotPointsmax,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 3, model 3")
ggsave("Scenario3.pdf", units = "in", height = 8.5, width = 11)


## BOTH MINIMUM AND MAXIMUM 
plotPointsmin <- plotPointsmin %>%
  dplyr::rename(estimate = `50%`,
                lower = `2.5%`,
                upper = `97.5%`)

plotPointsmax <- plotPointsmax %>%
  dplyr::rename(estimatemax = `50%`,
                lowermax = `2.5%`,
                uppermax = `97.5%`)
plotPointscomb <- left_join(plotPointsmin, plotPointsmax)

plotPointscomb %>%
  ggplot() +
  geom_line(aes(x = ourRange, y = estimatemax), color = "coral")+
  geom_ribbon(aes(x = ourRange,
                  y = estimatemax,
                  ymin = lowermax, 
                  ymax = uppermax),
              alpha = 0.3, fill = "coral")+
  geom_line(aes(x = ourRange, y = estimate), color = "skyblue") +
  geom_ribbon(aes(x = ourRange,
                  y = estimate,
                  ymin = lower, 
                  ymax = upper),
              alpha = 0.3, fill = "skyblue")+
  ylab("Predicted count") + xlab("Union density") +
  labs(title = "Scenario 3, model 1")
  #scale_fill_manual(values = c("skyblue", "coral"), aesthetics = c("color", "fill"))
  # ylab(c("Predicted count", "Predicted count")) +
  # xlab(c("Union density", "Union density")) + 
  #labs(title = "Scenario 3, model 3")

#scenario 4
set.seed(999)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              1) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 4, model 3", caption = "One climate policy adopted in the previous year")
ggsave("Scenario4.pdf", units = "in", height = 8.5, width = 11)

#scenario 5
set.seed(997)

ourX <- cbind(1, # intercept,
              ourRange, # our range of union density
              mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,
              2) #

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Union density") + ylim(0,2) + xlim(0,80) +
  labs(title = "Scenario 5, model 3", caption = "Two climate policies adopted in the previous year")
ggsave("Scenario5.pdf", units = "in", height = 8.5, width = 11)

#### SCENARIOS: other significant results ####
##MODEL 1##
#scenario 1 CORPORATISM
set.seed(567840)

simb <-mvrnorm(n= 1000,
               mu= negbin_mod1$coefficients,
               Sigma= vcovCL(negbin_mod1, cluster = oecddata$Country))

#Sequence from min to max 
ourRangeCorp <- seq(min(oecddata$CorpAll, na.rm = TRUE),
                max(oecddata$CorpAll, na.rm = TRUE)) 

ourXCorp<- cbind(1, # intercept,
              mean(oecddata$UD, na.rm=TRUE), #mean union density
              ourRangeCorp, # our range of corporatism
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0, 
              0,0,
              0) #timetrend

ncol(simb) == ncol(ourXCorp) # it is true!

xbetasCorp <- ourXCorp %*% t(simb)

expYUD <- exp(xbetasCorp)
quantileValuesUD <- apply(X = expYUD,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRangeCorp, t(quantileValuesUD)))

ggplot(plotPoints,
       aes(x = ourRangeCorp,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Corporatism") + 
  labs(title = "Model 1")
ggsave("Scenario1model1_corp.pdf", units = "in", height = 8.5, width = 11)

#scenario 2 Logged GDP
#Sequence from min to max 
ourRangeGDP <- seq(min(oecddata$logGDP, na.rm = TRUE),
                    max(oecddata$logGDP, na.rm = TRUE)) 

negbin_mod1$coefficients
ourXGDP <- cbind(1, # intercept,
                 mean(oecddata$UD, na.rm=TRUE), #mean union density
                 mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
                 ourRangeGDP, #mean GDP
                 mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
                 mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
                 mean(oecddata$Industry, na.rm = TRUE), #mean Industry
                 mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
                 0, #has green party in government
                 0, #is not EU member
                 #countries below
                 0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,1,0,
                 0,0,0,
                 0,0,0, 
                 0,0,
                 0) #timetrend

ncol(simb) == ncol(ourXGDP) # it is true!

xbetasGDP <- ourXGDP %*% t(simb)

expYGDP <- exp(xbetasGDP)
quantileValuesGDP <- apply(X = expYGDP,
                          MARGIN = 1,
                          FUN = quantile,
                          probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRangeGDP, t(quantileValuesGDP)))

ggplot(plotPoints,
       aes(x = ourRangeGDP,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Logged GDP") + 
  labs(title = "Model 1")
ggsave("Scenario2model1_GDP.pdf", units = "in", height = 8.5, width = 11)

#Scenario 3 INDUSTRY
ourRangeInd <- seq(min(oecddata$Industry, na.rm = TRUE),
                   max(oecddata$Industry, na.rm = TRUE)) 

ourXInd <- cbind(1, # intercept,
                 mean(oecddata$UD, na.rm=TRUE), #mean union density
                 mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
                 mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
                 mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
                 mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
                 ourRangeInd, #mean Industry
                 mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
                 0, #has green party in government
                 0, #is not EU member
                 #countries below
                 0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,1,0,
                 0,0,0,
                 0,0,0, 
                 0,0,
                 0) #timetrend

ncol(simb) == ncol(ourXInd) # it is true!

xbetasInd <- ourXInd %*% t(simb)

expYInd <- exp(xbetasInd)
quantileValuesInd <- apply(X = expYInd,
                           MARGIN = 1,
                           FUN = quantile,
                           probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRangeInd, t(quantileValuesInd)))

ggplot(plotPoints,
       aes(x = ourRangeInd,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Industry") + 
  labs(title = "Model 1")
ggsave("Scenario2model1_Industry.pdf", units = "in", height = 8.5, width = 11)

#scenario 4, Trade openness
ourRangetrade <- seq(min(oecddata$trade, na.rm = TRUE),
                   max(oecddata$trade, na.rm = TRUE)) 

ourXtrade <- cbind(1, # intercept,
                 mean(oecddata$UD, na.rm=TRUE), #mean union density
                 mean(oecddata$CorpAll, na.rm=TRUE), #mean corporatism
                 mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
                 mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
                 mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
                 mean(oecddata$Industry, na.rm = TRUE), #mean Industry
                 ourRangetrade, #mean trade opennes
                 0, #has green party in government
                 0, #is not EU member
                 #countries below
                 0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,0,0,
                 0,1,0,
                 0,0,0,
                 0,0,0, 
                 0,0,
                 0) #timetrend

ncol(simb) == ncol(ourXtrade) # it is true!

xbetastrade <- ourXtrade %*% t(simb)

expYtrade <- exp(xbetastrade)
quantileValuestrade <- apply(X = expYtrade,
                           MARGIN = 1,
                           FUN = quantile,
                           probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRangetrade, t(quantileValuestrade)))

ggplot(plotPoints,
       aes(x = ourRangetrade,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Trade openness") + 
  labs(title = "Model 1")
ggsave("Scenario2model1_trade.pdf", units = "in", height = 8.5, width = 11)


##MODEL 2##
#scenario 1
set.seed(56293)

simb <-mvrnorm(n= 1000,
               mu= negbin_mod2$coefficients,
               Sigma= vcovCL(negbin_mod2, cluster = oecddata$Country))


#Sequence from min to max 
ourRange <- seq(min(oecddata$CorpAll, na.rm = TRUE),
                max(oecddata$CorpAll, na.rm = TRUE)) 


negbin_mod2$coefficients

ourX <- cbind(1, # intercept,
              mean(oecddata$UD, na.rm=TRUE), #mean corporatism
              ourRange, # our range of union density
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              mean(oecddata$trade, na.rm = TRUE), #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #countries and years.

ncol(simb) == ncol(ourX) # it is true!

xbetas <- ourX %*% t(simb)
expY <- exp(xbetas)
quantileValues <- apply(X = expY,
                        MARGIN = 1,
                        FUN = quantile,
                        probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

ggplot(plotPoints,
       aes(x = ourRange,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Corporatism") + 
  labs(title = "Model 2")
ggsave("Scenario1model2_corp.pdf", units = "in", height = 8.5, width = 11)

#scenario 2 TRADE OPENNESS
ourRangetrade <- seq(min(oecddata$trade, na.rm = TRUE),
                   max(oecddata$trade, na.rm = TRUE)) 

ourXtrade <- cbind(1, # intercept,
              mean(oecddata$UD, na.rm=TRUE), #mean corporatism
              mean(oecddata$CorpAll, na.rm=T), # our range of union density
              mean(oecddata$logGDP, na.rm=TRUE), #mean GDP
              mean(oecddata$Dem, na.rm = TRUE), #mean level of democracy
              mean(oecddata$logemissions, na.rm = TRUE), #mean emissions level
              mean(oecddata$Industry, na.rm = TRUE), #mean Industry
              ourRangetrade, #mean trade opennes
              0, #has green party in government
              0, #is not EU member
              #countries below
              0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,1,0,
              0,0,0,
              0,0,0,
              0,0,0, 
              0,0,
              0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              0,0,0,
              1) #countries and years.

ncol(simb) == ncol(ourXtrade) # it is true!

xbetastrade <- ourXtrade %*% t(simb)

expYtrade <- exp(xbetastrade)
quantileValuestrade <- apply(X = expYtrade,
                           MARGIN = 1,
                           FUN = quantile,
                           probs = c(.025,.5,.975))

plotPoints <- as.data.frame(cbind(ourRangetrade, t(quantileValuestrade)))

ggplot(plotPoints,
       aes(x = ourRangetrade,
           y = `50%`,
           ymin = `2.5%`,
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line(aes(col=`50%`)) +
  ylab("Predicted count") +
  xlab("Trade openness") + 
  labs(title = "Model 2")
ggsave("Scenario2model2_trade.pdf", units = "in", height = 8.5, width = 11)

#### MARGINAL EFFECTS: coefficient plot ####
#Model 1
negbin_mod4Summary <- tidy(negbin_mod12) %>%
  dplyr::select(term, estimate) %>%
  mutate(clusteredSE = sqrt(diag(vcovCL(negbin_mod12, cluster = oecddata$Country)))) %>%
  filter(grepl("as.factor", term) == FALSE,
         term != "(Intercept)")  %>%
  mutate(labels = c("Union density",
                    "Corporatism",
                    "GDP per capita (log)",
                    "Democracy",
                    "Emissions per capita (log)",
                    "Industry",
                    "Trade openness",
                    "Green party in gov.",
                    "EU member",
                    "Timetrend"))

ggplot(negbin_mod4Summary, aes(y = labels, x = estimate,
                            xmin = estimate - 1.96 * abs(clusteredSE),
                            xmax = estimate + 1.96 * abs(clusteredSE))) +
  geom_errorbarh(height = .2, color = "red") +
  geom_point() +
  geom_vline(aes(xintercept = 0), color = "lightgrey") +
  ylab("") +
  xlab("") +
  theme_classic()
ggsave("model1marginal.pdf")

#Model 2
negbin_mod22Summary <- tidy(negbin_mod22) %>%
  dplyr::select(term, estimate) %>%
  mutate(clusteredSE = sqrt(diag(vcovCL(negbin_mod22, cluster = oecddata$Country)))) %>%
  filter(grepl("as.factor", term) == FALSE,
         term != "(Intercept)")  %>%
  mutate(labels = c("Union density",
                    "Corporatism",
                    "GDP per capita (log)",
                    "Democracy",
                    "Emissions per capita (log)",
                    "Industry",
                    "Trade openness",
                    "Green party in gov.",
                    "EU member"))

ggplot(negbin_mod22Summary, aes(y = labels, x = estimate,
                                xmin = estimate - 1.96 * abs(clusteredSE),
                                xmax = estimate + 1.96 * abs(clusteredSE))) +
  geom_errorbarh(height = .2, color = "red") +
  geom_point() +
  geom_vline(aes(xintercept = 0), color = "lightgrey") +
  ylab("") +
  xlab("") +
  theme_classic()
ggsave("model2marginal.pdf")

#Model 3
negbin_mod32Summary <- tidy(negbin_mod32) %>%
  dplyr::select(term, estimate) %>%
  mutate(clusteredSE = sqrt(diag(vcovCL(negbin_mod32, cluster = oecddata$Country)))) %>%
  filter(grepl("as.factor", term) == FALSE,
         term != "(Intercept)")  %>%
  mutate(labels = c("Union density",
                    "Corporatism",
                    "GDP per capita (log)",
                    "Democracy",
                    "Emissions per capita (log)",
                    "Industry",
                    "Trade openness",
                    "Green party in gov.",
                    "EU member",
                    "Lagged dependent"))

ggplot(negbin_mod32Summary, aes(y = labels, x = estimate,
                                xmin = estimate - 1.96 * abs(clusteredSE),
                                xmax = estimate + 1.96 * abs(clusteredSE))) +
  geom_errorbarh(height = .2, color = "red") +
  geom_point() +
  geom_vline(aes(xintercept = 0), color = "lightgrey") +
  ylab("") +
  xlab("") +
  theme_classic()
ggsave("model3marginal.pdf")



############################
#    INTERACTION MODELS    #
############################
#### Interaction effect: Negative binomial models ####
negbin_int1 <- glm.nb(count1 ~ UD*CorpAll + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata)

negbin_int2 <- glm.nb(count1 ~ UD*CorpAll + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata)

negbin_int3 <- glm.nb(count1 ~ UD*CorpAll + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata)

writeLines(capture.output(stargazer(negbin_int1, negbin_int2, negbin_int3,
          omit = c("Country", "Year"), 
          se = list(sqrt(diag(vcovCL(negbin_int1, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_int2, cluster = oecddata$Country))),
                    sqrt(diag(vcovCL(negbin_int3, cluster = oecddata$Country)))),
          add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                           'Country fixed effect', 'Yes', 'Yes', 'Yes')),
          column.labels = c("Timetrend", "FE", "AR(1)"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = FALSE,
          title = "Main models with interaction effects",
          dep.var.labels = "Yearly adopted climate policies",
          covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                               "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                               "Interaction: union density and corporatism"))), "negbininteract.tex")


#### Interaction effect plot ####
# standard errors #
#model 1
clusteredVCOVnegbin_inter1 <- cluster.vcov(negbin_int1, 
                                           cluster= oecddata$Country)
negbin_int1$clustervcov <- clusteredVCOVnegbin_inter1

#model 2
clusteredVCOVnegbin_inter2 <- cluster.vcov(negbin_int2,
                                           cluster = oecddata$Country)
negbin_int2$clustervcov <- clusteredVCOVnegbin_inter2

#model 3
clusteredVCOVnegbin_inter3 <- cluster.vcov(negbin_int3,
                                          cluster = oecddata$Country)
negbin_int3$clustervcov <- clusteredVCOVnegbin_inter3

#plot 1
inter_nvp <- interactions::johnson_neyman(negbin_int1, pred = UD, modx = CorpAll, 
                                          vmat = vcovCL(negbin_int1, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = TRUE,
                                          alpha = 0.1)
inter_nvp$plot + xlab("Corporatism") + ylab("Slope of union density") + ggtitle("Model 1")


#plot 2
inter_nvp <- interactions::johnson_neyman(negbin_int2, pred = UD, modx = CorpAll, 
                                          vmat = vcovCL(negbin_int2, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = TRUE,
                                          alpha = 0.1)
inter_nvp$plot + xlab("Corporatism") + ylab("Slope of union density") + ggtitle("Model 2")

#plot 3
inter_nvp <- interactions::johnson_neyman(negbin_int3, pred = UD, modx = CorpAll, 
                            vmat = vcovCL(negbin_int3, cluster = oecddata$Country), 
                            sig.color = "cyan4", insig.color = "coral", title = NULL,
                            control.fdr = TRUE,
                            alpha = 0.1)
inter_nvp$plot + xlab("Corporatism") + ylab("Slope of union density") + ggtitle("Model 3")

#interactions::interact_plot(negbin_int2, pred = UD, modx = CorpAll)

## other plots
plot3val <- interplot(negbin_int2, "UD", "CorpAll", predPro = TRUE, var2vals = c(min(oecddata$CorpAll), max(oecddata$CorpAll)))

interactions::interact_plot(negbin_int3, pred=UD, modx=CorpAll)

exp(0.022)
(1.022244 - 1)*100 #2.2244 = When variable a (UD) increases, the effect of B decreases by 2.22 %

###########################
#    ADDITIONAL TESTING   #
###########################
#### Poisson models with and without interactions ####
poisson_int1 <- glm(count1 ~ UD*CorpAll+Industry + logGDP + Dem + logemissions +
                      trade + environshare + EUmember + as.factor(Country) + timetrend+ offset(log(indepyears)), 
                    family = poisson, data = oecddata)

poisson_int2 <- glm(count1 ~ UD*CorpAll+Industry + logGDP + Dem + logemissions  +
                      trade + environshare + EUmember + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                    family = poisson, data = oecddata)

poisson_int3 <- glm(count1 ~ UD*CorpAll+Industry + logGDP + Dem + logemissions  +
                      trade + environshare + EUmember +  as.factor(Country) + lag(count1)+ offset(log(indepyears)),
                    family = poisson, data = oecddata)

poisson1 <- glm(count1 ~ UD+CorpAll+Industry + logGDP + Dem + logemissions +
                  trade + environshare + EUmember + as.factor(Country) + timetrend+ offset(log(indepyears)), 
                family = poisson, data = oecddata)

poisson2 <- glm(count1 ~ UD+CorpAll+Industry + logGDP + Dem + logemissions  +
                  trade + environshare + EUmember + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                family = poisson, data = oecddata)

poisson3 <- glm(count1 ~ UD+CorpAll+Industry + logGDP + Dem + logemissions  +
                  trade + environshare + EUmember +  as.factor(Country) + lag(count1)+ offset(log(indepyears)),
                family = poisson, data = oecddata)

writeLines(capture.output(stargazer(poisson1, poisson2, poisson3, poisson_int1, poisson_int2, poisson_int3, 
                                    omit = c("Country", "Year"),
                                    se = list(sqrt(diag(vcovCL(poisson1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_int1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_int2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_int3, cluster = oecddata$Country)))),
                                    column.labels = c("Timetrend", "FE", "AR(1)", "Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001",
                                              "Country fixed effects and country-clustered standard errors on all models"),
                                    notes.append = FALSE,
                                    title = "Poisson models with interaction effects",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "poissoninteract.tex")

#### Poisson models: offset ####
###offset 
poisson_mod1 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                      trade + environshare + EUmember + as.factor(Country) + timetrend, 
                    family = poisson, data = oecddata)
poisson_offsetin1 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                           trade + environshare + EUmember + as.factor(Country) + timetrend, 
                         offset = log(indepyears),
                         family = poisson, data = oecddata)
poisson_offsetout1 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry + trade + 
                            environshare + EUmember + as.factor(Country) + timetrend + log(indepyears), 
                          family = poisson, data = oecddata)

poisson_mod2 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                      trade + environshare + EUmember + as.factor(Country) + as.factor(Year),
                    family = poisson, data = oecddata)
poisson_offsetin2 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                           trade + environshare + EUmember + as.factor(Country) + as.factor(Year),
                         offset = log(indepyears),
                         family = poisson, data = oecddata)
poisson_offsetout2 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                            trade + environshare + EUmember + as.factor(Country) + as.factor(Year) + log(indepyears), 
                          family = poisson, data = oecddata)

poisson_mod3 <- glm(count1 ~ UD + CorpAll + logGDP + Dem + logemissions + Industry +
                      trade + environshare + EUmember +  as.factor(Country) + lag(count1) + UD*CorpAll,
                    family = poisson, data = oecddata)
poisson_offsetin3 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                           trade + environshare + EUmember +  as.factor(Country) + lag(count1), offset = log(indepyears),
                         family = poisson, data = oecddata)
poisson_offsetout3 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                            trade + environshare + EUmember +  as.factor(Country) + lag(count1) + log(indepyears),
                          family = poisson, data = oecddata)

## TABLE ##
writeLines(capture.output(stargazer(poisson_mod1, poisson_offsetin1, poisson_offsetout1,
                                    poisson_mod2, poisson_offsetin2, poisson_offsetout2,
                                    poisson_mod3, poisson_offsetin2, poisson_offsetout3,
                                    se = list(sqrt(diag(vcovCL(poisson_mod1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetin1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetout1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_mod2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetin2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetout2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_mod3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetin3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(poisson_offsetout3, cluster = oecddata$Country)))),
                                    omit = c("Year", "Country"), title = "Poisson models with different offsets", 
                                    star.char = c("+", "*", "**", "***"),star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
                                    notes.align = "l",
                                    notes.append = FALSE,
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = 
                                      c("Union density", "Corporatism", "Logged GDP", "Democracy",
                                        "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                        "EU member", "Timetrend", "Offset: independent years, logged", "Lagged dependent"),
                                    column.labels = c("Ignorable offset", "Offset in equation", "Offset outside of equation",
                                                      "Ignorable offset", "Offset in equation", "Offset outside of equation",
                                                      "Ignorable offset", "Offset in equation", "Offset outside of equation"),
                                    add.lines = list(c('Country fixed effects', 'Yes','Yes','Yes','Yes','Yes','Yes','Yes','Yes','Yes'),
                                                     c('Year fixed effects', 'No', 'No','No','Yes', 'Yes', 'Yes', 'No','No','No'),
                                                     c('Standard errors clustered by country', 'Yes','Yes','Yes','Yes','Yes','Yes','Yes','Yes','Yes')))), 
           "poissonoffsets.tex")

#dispersion!
ods <- poisson_offsetout3$deviance/poisson_mod3$df.residual
ods

#### Quasi Poisson models ####
poisson_quasi1 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember + as.factor(Country) + timetrend,
                      family = quasipoisson, data = oecddata)

poisson_quasi2 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember + as.factor(Country) + as.factor(Year),
                      family = quasipoisson, data = oecddata)

poisson_quasi3 <- glm(count1 ~ UD*CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember + as.factor(Country) + lag(count1),
                      family = quasipoisson, data = oecddata)

## TABLE ##
writeLines(capture.output(stargazer(poisson_mod1, poisson_quasi1, poisson_mod2, poisson_quasi2, 
                                    poisson_mod3, poisson_quasi3, 
                                    omit = c("Year", "Country"), 
                                    title = "Poisson and quasi-poisson models", 
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes.align = "l",
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member", "Timetrend", "Lagged dependent"),
                                    column.labels = c("Poisson", "Quasi-poisson", "Poisson", "Quasi-poisson","Poisson", "Quasi-poisson"),
                                    add.lines = list(c('Country fixed effects', 'Yes','Yes','Yes','Yes','Yes','Yes'),
                                                     c('Year fixed effects', 'No', 'No','Yes', 'Yes', 'No','No')))), "quasipoisson.tex")


#### BASE POISSON + NEGBIN ROOTOGRAMS ####
par(mfrow=c(2,2))
rootogram(negbin)
rootogram(poisson_base)
rootogram(negbin0)
rootogram(poisson_base0)
par(mfrow=c(2,2))
rootogram(negbin1)
rootogram(poisson_base1)
rootogram(negbin2)
rootogram(poisson_base2)
par(mfrow=c(2,2))
rootogram(negbin3)
rootogram(poisson_base3)
rootogram(negbin4)
rootogram(poisson_base4)



#### Compare model predictions ####
m1 <- lm(count1 ~ UD*CorpAll +  logGDP + Dem +
           logemissions + Industry + trade  + 
           environshare + EUmember + 
           as.factor(Country) + lag(count1),
         data = oecddata)

po.p <- predprob(poisson3) %>% colMeans
po.nb <- predprob(negbin_mod32) %>% colMeans
po.zinb <- predprob(hurdle3) %>% colMeans

df <- data.frame(x = 0:max(oecddata$count1), Poisson = po.p, 
                 NegBin = po.nb, Zinb = po.zinb)

obs <- table(oecddata$count1) %>% prop.table() %>% data.frame
names(obs) <- c("x", 'Observed')

p1 <- predict(m1) %>% round() %>% table %>% prop.table %>% data.frame
names(p1) <- c('x', 'OLS')
p1 <- p1[-1, ] #there is a negative value

tmp <- merge(p1, obs, by = 'x', all = T)
tmp$x <- as.numeric(as.character(tmp$x))

comb <- merge(tmp, df, by = 'x', all = T)
comb[is.na(comb)] <- 0
mm <- melt(comb, id.vars = 'x', value.name = 'prob', variable.name = 'Model')

ggplot(mm, aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 1) +
  theme_bw() +
  labs(x = "Number of yearly adopted policies", y = 'Probability',
       title = "Comparison of models") +
  scale_color_manual(values = c('brown', 'black', 'blue', 'green', 'red')) +
  scale_linetype_manual(values = c('dotted', 'solid', 'dotted', 'dotted', 'dashed')) +
  theme(legend.position=c(.65, .65), legend.background = element_rect(), 
        axis.title.y = element_text(angle = 0))
ggsave("Comparisons of count models.pdf", units = "in", width = 8.5, height = 11)

#### Diagnostics ####
#equidispersion?
var(thesisdata$count1)
mean(thesisdata$count1)
#variance is greater than the mean, indicating overdispersion

### Multicollinearity? ###
VIF(negbin_mod12) # remove UD
vif(negbin_mod22) # remove UD and trade
vif(negbin_mod32) # remove UD and industry

negbin_vif1 <- glm.nb(count1 ~ CorpAll +Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + timetrend,
                         data = oecddata)

negbin_vif2 <- glm.nb(count1 ~ CorpAll +Industry + logGDP + Dem + logemissions +
                         environshare + EUmember  + as.factor(Country) + as.factor(Year),
                      data = oecddata)

negbin_vif3 <- glm.nb(count1 ~ CorpAll + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + lag(count1),
                         data = oecddata_uten)

writeLines(capture.output(stargazer(negbin_vif1, negbin_vif2, negbin_vif3, 
                                   omit=c("Country", "Year"),
                                   star.char = c("+", "*", "**", "***"),
                                   star.cutoffs = c(.1, .05, .01, .001),
                                   notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                   notes.append = FALSE,
                                   add.lines = c('Robust standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                 'Country fixed effects', 'Yes', 'Yes', 'Yes'),
          title = "Main models without variables with problematic VIF values",
          dep.var.labels = "Newly adopted climate policies",
          column.labels = c("Timetrend", "FE", "AR(1)"),
          covariate.labels = c("Corporatism", "Industry", "GDP (log)", "Democracy", "Emissions per capita (log)",
                               "Trade openness", "Green party in gov.", "EU member", "Time trend", "Lagged dependent"))), "negbin3VIFadjust.tex")

### Hausman test ###
random_test <- plm(count1 ~ UD + CorpAll+ Industry + logGDP + Dem + logemissions  +
                     trade + environshare + EUmember, 
                   data = oecddata, 
                   effect = "twoways", 
                   model = "random", 
                   index = c("Country", "Year"))

fixed_test <- plm(count1 ~ UD + CorpAll + Industry + logGDP + Dem + logemissions  +
                    trade + environshare + EUmember,
                  data = thesisdata,
                  model = "within",
                  effect = "twoways",
                  index = c("Country", "Year"))

phtest(random_test, fixed_test)


#### Rootograms ####
par(mfrow=c(1,3))
rootogram(poisson_mod1)
rootogram(poisson_mod2)
rootogram(poisson_mod3)

par(mfrow=c(1,3))
rootogram(negbin_mod12)
rootogram(negbin_mod22)
rootogram(negbin_mod32)

par(mfrow=c(1,3))
rootogram(negbin_int1)
rootogram(negbin_int2)
rootogram(negbin_int3)

par(mfrow=c(1,3))
rootogram(negbin_int_ut1)
rootogram(negbin_int_ut2)
rootogram(negbin_int_ut3)

#### Main models: without outliers ####
out <- boxplot.stats(oecddata$count1)$out
out_ind <- which(oecddata$count1 %in% c(out))
out_ind

oecddata_uten <- oecddata[-c(116,135,140,172,174,223,319,369,399,
                             468,514,548,
                             740,829,833,864,921,923,926,1028), ]


negbin_ut1 <- glm.nb(count1 ~ UD+CorpAll +Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata_uten)

negbin_ut2 <- glm.nb(count1 ~ UD+CorpAll + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata_uten)

negbin_ut3 <- glm.nb(count1 ~ UD+CorpAll + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata_uten)

writeLines(capture.output(stargazer(negbin_ut1, negbin_ut2, negbin_ut3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_ut1, cluster = oecddata_uten$Country))),
                                              sqrt(diag(vcovCL(negbin_ut2, cluster = oecddata_uten$Country))),
                                              sqrt(diag(vcovCL(negbin_ut3, cluster = oecddata_uten$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = "Main models with interaction effects: without outliers",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent"))), "negbinnoSpain.tex")

negbin_int_ut1 <- glm.nb(count1 ~ UD*CorpAll +Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata_uten)

negbin_int_ut2 <- glm.nb(count1 ~ UD*CorpAll + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata_uten)

negbin_int_ut3 <- glm.nb(count1 ~ UD*CorpAll + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata_uten)



writeLines(capture.output(stargazer(negbin_int_ut1, negbin_int_ut2, negbin_int_ut3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_int_ut1, cluster = oecddata_uten$Country))),
                                              sqrt(diag(vcovCL(negbin_int_ut2, cluster = oecddata_uten$Country))),
                                              sqrt(diag(vcovCL(negbin_int_ut3, cluster = oecddata_uten$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = "Main models with interaction effects: without outliers",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "negbininteractnoSpain.tex")



#### DIFFERENT STANDARD ERRORS####
#Model 1
writeLines(capture.output(stargazer(negbin_int1, negbin_int1, negbin_int1, negbin_int1,
                                    omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(negbin_int1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_int1, cluster = oecddata$Year))),
                                              sqrt(diag(vcovHC(negbin_int1)))),
                                    title = c("Different standard error specification: timetrend variable"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member", "Timetrend"),
                                    column.labels = c("Country cluster", "Year cluster", "HAC", "Normal"))), "negbinSEmod1.tex")

#Model 2
writeLines(capture.output(stargazer(negbin_int2, negbin_int2, negbin_int2, negbin_int2,
                                    omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(negbin_int2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_int2, cluster = oecddata$Year))),
                                              sqrt(diag(vcovHC(negbin_int2)))),
                                    title = c("Different standard error specification: FE"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member"),
                                    column.labels = c("Country cluster", "Year cluster", "HAC", "Normal"))), "negbinSEmod2.tex")

#Model 3
writeLines(capture.output(stargazer(negbin_int3, negbin_int3, negbin_int3, negbin_int3,
                                    omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(negbin_int3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_int3, cluster = oecddata$Year))),
                                              sqrt(diag(vcovHC(negbin_int3)))),
                                    title = c("Different standard error specification: AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Corporatism", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member", "Lagged dependent"),
                                    column.labels = c("Country cluster", "Year cluster", "HAC", "Normal"))), "negbinSEmod3.tex")

#### Trying different operationalizations of control variables ####
negbin_moddif <- glm.nb(count1 ~ UD*CorpAll+fossilproducer + logGDP + Dem +  
                          logemissions + trade + environvote + EUmember +
                          as.factor(Country) + timetrend,
                        data = oecddata)

negbin_moddif0 <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                           Dem +  environvote + EUmember + as.factor(Country)+ timetrend,
                         data = oecddata)

negbin_0moddif <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                           Dem +  environshare + EUmember + as.factor(Country)+ timetrend,
                         data = oecddata)

negbin_moddif2 <- glm.nb(count1 ~ UD*CorpAll+fossilproducer + logGDP + 
                           logemissions +   Dem +  
                           trade + environvote + EUmember + 
                           as.factor(Country) + as.factor(Year),
                         data = oecddata)

negbin_moddif02 <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                            Dem + trade + environvote + EUmember + as.factor(Country)+ as.factor(Year),
                          data = oecddata)

negbin_2moddif <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                           Dem +  environshare + EUmember + as.factor(Country)+ as.factor(Year),
                         data = oecddata)

negbin_moddif3 <- glm.nb(count1 ~ UD*CorpAll+fossilproducer +  logGDP + Dem + 
                           logemissions  + trade +  
                           environvote + EUmember + Year +
                           as.factor(Country) + lag(count1),
                         data = oecddata)

negbin_moddif03 <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                            Dem + trade + environvote + EUmember + as.factor(Country) + lag(count1),
                          data = oecddata)

negbin_3moddif <- glm.nb(count1~ UD*CorpAll+fossilexport + logGDP + logemissions + 
                           Dem +  environshare + EUmember + as.factor(Country)+ lag(count1),
                         data = oecddata)

writeLines(capture.output(stargazer(negbin_moddif, negbin_moddif0, negbin_0moddif, 
                                    negbin_moddif2, negbin_moddif02, negbin_2moddif,
                                    negbin_moddif3, negbin_moddif03, negbin_3moddif,
                                    omit = c("Year", "Country"),
                                    add.lines = c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                  'Country fixed effects', 'Yes', 'Yes', 'Yes'),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    dep.var.labels = "Yearly adopted climate policies",
                                    se = list(sqrt(diag(vcovHC(negbin_moddif, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_moddif0, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_0moddif, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_moddif2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_moddif02, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_2moddif, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_moddif3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_moddif03, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(negbin_3moddif, cluster = oecddata$Country)))),
                                    title = "Different covariates",
                                    covariate.labels = c("Union density", "Corporatism", "Fossil fuel producer", "Fossil fuel exporter",
                                                         "GDP per capita (log)", "Democracy", "Green party in gov.", "Emissions per capita (log)",
                                                         "Trade openness", "Green party vote", "EU member", "Timetrend", "Lagged independent",
                                                         "Interaction: union density and corporatism"))), "negbincovariates.tex") 
#### employers####
negbin_emp <- glm.nb(count1 ~ ED+CorpAll + logGDP + Dem + logemissions + Industry +
                       trade + environshare + EUmember  + as.factor(Country) + timetrend +
                       offset(log(indepyears)),
                     data = oecddata)

negbin_emp2 <- glm.nb(count1 ~ ED+CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+
                        offset(log(indepyears)),
                      data = oecddata)

negbin_emp3 <- glm.nb(count1 ~ ED + CorpAll + logGDP + Dem + logemissions + Industry +
                        trade + environshare + EUmember  + as.factor(Country) + lag(count1)+
                        offset(log(indepyears)),
                      data = oecddata)

negbin_emp4 <- glm.nb(count1 ~ ED*CorpAll*Industry + logGDP + Dem + logemissions + trade + 
                        environshare + EUmember + as.factor(Country) + lag(count1)+
                        offset(log(indepyears)), 
                      data = oecddata)

writeLines(capture.output(stargazer(negbin_emp, negbin_emp2, negbin_emp3, negbin_emp4, omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(negbin_emp, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_emp2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_emp3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_emp4, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Exploring the limited data on employers organizations"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Employers organization density", "Corporatism", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Industry", "Trade openness", "Green party in gov.", 
                                                         "EU member", "Timetrend", "Lagged dependent", "Interaction: employers and corporatism",
                                                         "Interaction: employers and industry", "Interaction: corporatism and industry",
                                                         "Interaction: employers, corporatism and industry"),
                                    column.labels = c("Timetred", "FE", "AR(1)", "AR(1): Interaction"))), "employers.tex")

# checking coutnries
thesisdata.xemp <- na.omit(oecddata[ , c("Country", "Year",
                                        all.vars(formula(negbin_emp4)))])
data.frame(thesisdata.xemp$Year, thesisdata.xemp$Country)

#### civil society ####
negbin_civ  <- glm.nb(count1~ CS_part+Industry + logGDP + Dem + logemissions+
                        trade + environshare + EUmember + as.factor(Country) + timetrend,
                      data = oecddata)

negbin_civ2 <- glm.nb(count1 ~ CS_part+Industry + logGDP + Dem + logemissions + 
                        trade + environshare + EUmember + as.factor(Country) + as.factor(Year),
                      data= oecddata)
negbin_civ3 <- glm.nb(count1~ CS_part+Industry + logGDP + Dem + logemissions + 
                        trade + environshare + EUmember + as.factor(Country) + lag(count1),
                      data = oecddata)

writeLines(capture.output(stargazer(negbin_civ, negbin_civ2, negbin_civ3,  
                                    omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(negbin_civ, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_civ2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_civ3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Country fixed effects','Yes', 'Yes', 'Yes', 'Yes'),
                                                   c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes')),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Civil society participation"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Civil society participation", "Industry", "Logged GDP", "Democracy",
                                                         "Logged emissions per capita", "Trade openness", "Green party in gov.", 
                                                         "EU member","Timetrend", "Lagged dependent"),
                                    column.labels = c("Timetrend", "FE", "AR(1)"))), "civilsociety.tex")

#### Zero inflated ####
zero_mod1 <- zeroinfl(count1 ~ UD+CorpAll + logGDP + Dem + 
                        logemissions + Industry + 
                        trade + environshare + EUmember +
                        as.factor(Country) + timetrend + offset(log(indepyears))|
                        ## model of excess zeroes:
                        UD+CorpAll + logGDP + Dem + 
                        logemissions + Industry + 
                        trade + environshare + EUmember +
                        as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata,
                      dist = "negbin", link = "logit")

zero_mod2 <- zeroinfl(count1 ~ UD+CorpAll + logGDP
                      + logemissions + Industry + EUmember + Dem +
                        trade + environshare + +as.factor(Country) + as.factor(Year) + offset(log(indepyears))|
                        ## model of excess zeroes:
                        UD+CorpAll +  logGDP
                      + logemissions + Industry + EUmember + Dem +
                        trade + environshare + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata, 
                      dist = "negbin", link = "logit")

zero_mod3 <- zeroinfl(count1 ~ UD+CorpAll +  logGDP
                      + logemissions + Industry + EUmember + Dem+
                        trade + environshare + as.factor(Country) + lag(count1)+ offset(log(indepyears)) |
                        ## model of excess zeroes:
                        UD+CorpAll +  logGDP
                      + logemissions + Industry + EUmember + Dem+
                        trade + environshare +as.factor(Country) + lag(count1)+ offset(log(indepyears)),
                      data = oecddata, 
                      dist = "negbin", link = "logit")


thesisdata.x <- na.omit(thesisdata[ , c("Country", "Year",
                                        all.vars(formula(zero_mod2)))]) ## to check which countries and years are included in the model (and also for clustered SE)

writeLines(capture.output(stargazer(zero_mod1, zero_mod2, zero_mod3, omit = c("Year", "Country"),
                                    se = list(sqrt(diag(vcovCL(zero_mod1, cluster = thesisdata.x$Country))),
                                              sqrt(diag(vcovCL(zero_mod2, cluster = thesisdata.x$Country))),
                                              sqrt(diag(vcovCL(zero_mod3, cluster = thesisdata.x$Country)))),
                                    title = c("Zero inflated negative binomial models"),
                                    notes = c("Uncertainty is not reported due to coding errors"),
                                    add.lines=list(c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    dep.var.labels = c("Newly adopted climate laws"),
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy",
                                                         "Emissions per capita (log)", "Industry", "Trade openess",
                                                         "Green party in gov.", "EU member", "Timetrend",
                                                         "Lagged dependent"))), "zeromodels.tex") 

stargazer(zero_mod3, type="text")
#### Hurdle ####
hurdle1 <- pscl::hurdle(count1 ~ UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Country) + timetrend|
                          ## model of excess zeroes:
                          UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Country) + timetrend,
                        data = oecddata,
                        dist = "negbin", zero.dist = "binomial", link = "logit")


hurdle2 <- pscl::hurdle(count1 ~ UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Year) + as.factor(Country)|
                          ## model of excess zeroes:
                          UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Year) + as.factor(Country),
                        data = oecddata,
                        dist = "negbin", zero.dist = "binomial", link = "logit")

hurdle3 <- pscl::hurdle(count1 ~ UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Country) + lag(count1)|
                          ## model of excess zeroes:
                          UD + CorpAll + logGDP + logemissions + Industry +  
                          trade + environshare +  EUmember + as.factor(Country) + lag(count1),
                        data = oecddata,
                        dist = "negbin", zero.dist = "binomial", link = "logit")

texreg(list(hurdle1, hurdle2, hurdle3), omit.coef = c("Year|Country"), 
       stars = c(0.001, 0.01, 0.05, 0.1), custom.model.names = c("Timetrend", "FE", "AR(1)"),
       custom.coef.names = c("Count model: (Intercept)", "Count model: Union density", 
                             "Count model: Corporatism", "Count model: GDP (log)", 
                             "Count model: Emissions per capita (log)", "Count model: Industry",
                             "Count model: Trade openess", "Count model: Green party in gov.",
                             "Count model: EU member", "Count model: Timetrend", "Count model: Theta (log)",
                             "Zero model: (Intercept)", "Zero model: Union density", "Zero model: Corporatism",
                             "Zero model: GDP (log)", "Zero model: Emissions per capita (log)", "Zero model: Industry",
                             "Zero model: Trade openess", "Zero model: Green party in gov.", "Zero model: Eu member",
                             "Zero model: Timetrend", "Count model: Lagged dependent", "Zero model: Lagged dependent"),
       file = "hurdle.tex")

rootogram(hurdle1)
rootogram(hurdle2)
rootogram(hurdle3)

#### Different corporatism ####
corpreg <- glm.nb(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                    Govint + Level + RI + 
                    Coord + EXT + Sector +
                    as.factor(Country) + timetrend,
                  data = thesisdata)

corpreg2 <- glm.nb(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                     Govint + Level + RI + 
                     Coord + EXT + Sector +
                     as.factor(Country) + as.factor(Year),
                   data = thesisdata)

corpreg3 <- glm.nb(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                     Govint + Level + RI + 
                     Coord + EXT + Sector +
                     as.factor(Country) + lag(count1),
                   data = thesisdata)

corppoisson <- glm(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                     Govint + Level + RI + 
                     Coord + EXT + Sector +
                     as.factor(Country) + timetrend ,
                   family = "poisson", 
                   data = thesisdata)

corppoisson2 <- glm(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                      Govint + Level + RI + 
                      Coord + EXT + Sector +
                      as.factor(Country) + as.factor(Year) ,
                    family = "poisson", 
                    data = thesisdata)

corppoisson3 <- glm(count1 ~ UD + CENT + WC_STRUCT + WC_RIGHTS + 
                      Govint + Level + RI + 
                      Coord + EXT + Sector +
                      as.factor(Country) + lag(count1),
                    family = "poisson", 
                    data = thesisdata)


writeLines(capture.output(stargazer(corpreg, corpreg2, corpreg3, corppoisson, corppoisson2, corppoisson3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovHC(corpreg, cluster = thesisdata$Country))),
                                              sqrt(diag(vcovHC(corpreg2, cluster = thesisdata$Country))),
                                              sqrt(diag(vcovHC(corpreg3, cluster = thesisdata$Country))),
                                              sqrt(diag(vcovHC(corppoisson, cluster = thesisdata$Country))),
                                              sqrt(diag(vcovHC(corppoisson2, cluster = thesisdata$Country))),
                                              sqrt(diag(vcovHC(corppoisson3, cluster = thesisdata$Country)))),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Association with different components of corporatism index"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)", "Timetred", "FE", "AR(1)"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Centralization of bargaining", "Work council structure", 
                                                         "Work council rights", "Government intervention", "Level of wage bargaining", 
                                                         "Involvement of labor organizations", "Wage bargaining coordination", "Extention of bargaining", 
                                                         "Sectoral organization", "Timetrend", "Lagged dependent"))), "Corporatism.tex")

#### Each corporatism variable in main model ####
# CENT + WC_STRUCT + WC_RIGHTS + 
#   Govint + Level + RI + 
#   Coord + EXT + Sector +

negbin_CENT1 <- glm.nb(count1 ~ UD*CENT +Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata)

negbin_CENT2 <- glm.nb(count1 ~ UD*CENT + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata)

negbin_CENT3 <- glm.nb(count1 ~ UD*CENT + Industry + logGDP + Dem + logemissions +
                        trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata)

negbin_STRUCT1 <- glm.nb(count1 ~ UD*WC_STRUCT +Industry + logGDP + Dem + logemissions +
                         trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                       data = oecddata)

negbin_STRUCT2 <- glm.nb(count1 ~ UD*WC_STRUCT + Industry + logGDP + Dem + logemissions +
                         trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                       data = oecddata)

negbin_STRUCT3 <- glm.nb(count1 ~ UD*WC_STRUCT + Industry + logGDP + Dem + logemissions +
                         trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                       data = oecddata)

writeLines(capture.output(stargazer(negbin_CENT1, negbin_CENT2, negbin_CENT3, 
                                    negbin_STRUCT1, negbin_STRUCT2, negbin_STRUCT3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_CENT1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_CENT2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_CENT3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_STRUCT1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_STRUCT2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_STRUCT3, cluster = oecddata$Country)))),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Negative binomial models: different corporatism components 1"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)", "Timetrend", "FE", "AR(1)"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Centralization of bargaining", "Work council strucutre", "GDP (log)", 
                                                         "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and centralization", "Interaction: union density and work council structure"))), "centandstructnegbin.tex")


negbin_RIGHTS1 <- glm.nb(count1 ~ UD*WC_RIGHTS +Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_RIGHTS2 <- glm.nb(count1 ~ UD*WC_RIGHTS + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_RIGHTS3 <- glm.nb(count1 ~ UD*WC_RIGHTS + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

negbin_govint1 <- glm.nb(count1 ~ UD*Govint +Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_govint2 <- glm.nb(count1 ~ UD*Govint + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_govint3 <- glm.nb(count1 ~ UD*Govint + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

writeLines(capture.output(stargazer(negbin_RIGHTS1, negbin_RIGHTS2, negbin_RIGHTS3,
                                    negbin_govint1, negbin_govint2, negbin_govint3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_RIGHTS1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_RIGHTS2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_RIGHTS3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_govint1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_govint2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_govint3, cluster = oecddata$Country)))),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Negative binomial models: different corporatism components 2"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)", "Timetrend", "FE", "AR(1)"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Work council rights", "Government intervention", "GDP (log)", 
                                                         "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and work council rights", "Interaction: union density and government intervention"))), "rightsandgovintnegbin.tex")

negbin_level1 <- glm.nb(count1 ~ UD*Level +Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_level2 <- glm.nb(count1 ~ UD*Level + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_level3 <- glm.nb(count1 ~ UD*Level + Industry + logGDP + Dem + logemissions +
                           trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

negbin_ri1 <- glm.nb(count1 ~ UD*RI +Industry + logGDP + Dem + logemissions +
                          trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                        data = oecddata)

negbin_ri2 <- glm.nb(count1 ~ UD*RI + Industry + logGDP + Dem + logemissions +
                          trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                        data = oecddata)

negbin_ri3 <- glm.nb(count1 ~ UD*RI + Industry + logGDP + Dem + logemissions +
                          trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                        data = oecddata)
writeLines(capture.output(stargazer(negbin_level1, negbin_level2, negbin_level3,
                                    negbin_ri2, negbin_ri2, negbin_ri3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_level1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_level2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_level3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ri1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ri2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ri3, cluster = oecddata$Country)))),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Negative binomial models: different corporatism components 3"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)", "Timetrend", "FE", "AR(1)"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Level of wage bargaining", "Involvement of labor organizations", "GDP (log)", 
                                                         "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and level of wage bargaining", "Interaction: union density and involvement of labor organizations"))), "levelandrinegbin.tex")

negbin_coord1 <- glm.nb(count1 ~ UD*Coord +Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                     data = oecddata)

negbin_coord2 <- glm.nb(count1 ~ UD*Coord + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                     data = oecddata)

negbin_coord3 <- glm.nb(count1 ~ UD*Coord + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                     data = oecddata)

negbin_ext1 <- glm.nb(count1 ~ UD*EXT +Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                     data = oecddata)

negbin_ext2 <- glm.nb(count1 ~ UD*EXT + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                     data = oecddata)

negbin_ext3 <- glm.nb(count1 ~ UD*EXT + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                     data = oecddata)

negbin_sector1 <- glm.nb(count1 ~ UD*Sector +Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                     data = oecddata)

negbin_sector2 <- glm.nb(count1 ~ UD*Sector + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                     data = oecddata)

negbin_sector3 <- glm.nb(count1 ~ UD*Sector + Industry + logGDP + Dem + logemissions +
                       trade + environshare + EUmember  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                     data = oecddata)

writeLines(capture.output(stargazer(negbin_coord1, negbin_coord2, negbin_coord3,
                                    negbin_ext1, negbin_ext2, negbin_ext3,
                                    negbin_sector1, negbin_sector2, negbin_sector3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_coord1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_coord2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_coord3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ext1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ext2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_ext3, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_sector1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_sector2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_sector3, cluster = oecddata$Country)))),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = c("Negative binomial models: different corporatism components 4"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)", "Timetrend", "FE", "AR(1)", "Timetrend", "FE", "AR(1)"),
                                    dep.var.labels = c("Yearly adopted climate policies"),
                                    covariate.labels = c("Union density", "Wage bargaining coordination", "Extention of bargaining","Sectoral organization",
                                                         "GDP (log)", "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and wage bargaining coordination", "Interaction: union density and extention of bargaining",
                                                         "Interaction: union density and sectoral organization"))), "coordandextandsectornegbin.tex")


#### interaction effect plot: different corporatism ####
#CENT
inter_nvp <- interactions::johnson_neyman(negbin_CENT1, pred = UD, modx = CENT, 
                                          vmat = vcovCL(negbin_CENT1, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Centralization of bargaining") + ylab("Slope of union density") + ggtitle("Model 1")

inter_nvp <- interactions::johnson_neyman(negbin_CENT2, pred = UD, modx = CENT, 
                                          vmat = vcovCL(negbin_CENT2, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Centralization of bargaining") + ylab("Slope of union density") + ggtitle("Model 2")

inter_nvp <- interactions::johnson_neyman(negbin_CENT3, pred = UD, modx = CENT, 
                                          vmat = vcovCL(negbin_CENT3, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Centralization of bargaining") + ylab("Slope of union density") + ggtitle("Model 3")


##level of wage bargaining
inter_nvp <- interactions::johnson_neyman(negbin_level3, pred = UD, modx = Level, 
                                          vmat = vcovCL(negbin_level3, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Level of wage bargaining") + ylab("Slope of union density") + ggtitle("Model 3")


##Wage bargaining coordination
inter_nvp <- interactions::johnson_neyman(negbin_coord3, pred = UD, modx = Coord, 
                                          vmat = vcovCL(negbin_coord3, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Wage bargaining coordination") + ylab("Slope of union density") + ggtitle("Model 3")


##Extention of bargaining
inter_nvp <- interactions::johnson_neyman(negbin_ext1, pred = UD, modx = EXT, 
                                          vmat = vcovCL(negbin_ext1, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Extention of bargaining") + ylab("Slope of union density") + ggtitle("Model 1")

inter_nvp <- interactions::johnson_neyman(negbin_ext2, pred = UD, modx = EXT, 
                                          vmat = vcovCL(negbin_ext2, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Extention of bargaining") + ylab("Slope of union density") + ggtitle("Model 2")

inter_nvp <- interactions::johnson_neyman(negbin_ext3, pred = UD, modx = EXT, 
                                          vmat = vcovCL(negbin_ext3, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Extention of bargaining") + ylab("Slope of union density") + ggtitle("Model 3")


##sectoral organization
inter_nvp <- interactions::johnson_neyman(negbin_sector1, pred = UD, modx = Sector, 
                                          vmat = vcovCL(negbin_sector1, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Sectoral organization") + ylab("Slope of union density") + ggtitle("Model 1")

inter_nvp <- interactions::johnson_neyman(negbin_sector2, pred = UD, modx = Sector, 
                                          vmat = vcovCL(negbin_sector2, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Sectoral organization") + ylab("Slope of union density") + ggtitle("Model 2")

inter_nvp <- interactions::johnson_neyman(negbin_sector3, pred = UD, modx = Sector, 
                                          vmat = vcovCL(negbin_sector3, cluster = oecddata$Country), 
                                          sig.color = "cyan4", insig.color = "coral", title = NULL,
                                          control.fdr = FALSE,
                                          alpha = 0.05)
inter_nvp$plot + xlab("Sectoral organization") + ylab("Slope of union density") + ggtitle("Model 3")



#### Different dependent ####
oecddata <- oecddata %>%
  mutate(kyoto_dic = ifelse(kyoto_member <= 1, 0, 1))

data.frame(oecddata$kyoto_dic, oecddata$kyoto_member)

glmkyoto <- glm(kyoto_dic ~ UD*CorpAll +  logGDP + Dem +
                  logemissions + Industry + trade  + environshare + EUmember +
                  as.factor(Country) + timetrend,
                family=binomial(link='logit'), data = oecddata)

negbinkyoto <- glm.nb(kyoto_member ~ UD*CorpAll +  logGDP + Dem +
                        logemissions + Industry + trade  + environshare + EUmember +
                        as.factor(Country) + timetrend,
                      data = oecddata)

# negbinols <- lm(kyoto_member ~ UD+CorpAll + logGDP+ Dem + logemissions + 
#                   Industry + trade + environshare + EUmember + as.factor(Country) + timetrend,
#                 data = oecddata)

glmkyoto2 <- glm(kyoto_dic ~ UD*CorpAll +  logGDP + Dem +
                  logemissions + Industry + trade  + environshare + EUmember +
                  as.factor(Country) + as.factor(Year),
                 family=binomial(link='logit'), data = oecddata)


negbinkyoto2 <- glm.nb(kyoto_member ~ UD*CorpAll +  logGDP + Dem +
                         logemissions + Industry + trade + environshare + EUmember +
                         as.factor(Country) + as.factor(Year),
                       data = oecddata)

# negbinols2 <- lm(kyoto_member ~ UD + CorpAll + logGDP + Dem + logemissions + Industry + 
#                    trade + environshare + EUmember + as.factor(Country) + as.factor(Year),
#                  data = oecddata)

glmkyoto3 <- glm(kyoto_dic ~ UD*CorpAll +  logGDP + Dem +
                  logemissions + Industry + trade  + environshare + EUmember +
                  as.factor(Country) + lag(count1),
                 family=binomial(link='logit'), data = oecddata)


negbinkyoto3 <- glm.nb(kyoto_member ~ UD*CorpAll +  logGDP + Dem +
                         logemissions + Industry + trade + environshare + EUmember +
                         as.factor(Country) + lag(count1),
                       data = oecddata)

# negbinols3 <- lm(kyoto_member ~ UD*CorpAll + logGDP + Dem + logemissions + Industry + trade +
#                    environshare + EUmember + as.factor(Country) + lag(count1),
#                  data = oecddata)

writeLines(capture.output(stargazer(negbinkyoto, negbinkyoto2, negbinkyoto3, 
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbinkyoto, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbinkyoto2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbinkyoto3, cluster = oecddata$Country)))),
                                    title = c("Negative binomial models: membership in international environmental agreement"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)"),
                                    dep.var.labels = c("Kyoto protocol membership"),
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", 
                                                         "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "kyotomember.tex")


#### Logistic regressions ####
glm_mod <- glm(New_law ~ UD*CorpAll + logGDP + Dem + 
                 logemissions + Industry +  
                 trade + environshare + EUmember +
                 as.factor(Country) + timetrend, 
               data = oecddata,
               family = "binomial"(link= "logit"))

glm_mod2 <- glm(New_law ~ UD*CorpAll + logGDP + 
                  logemissions + Industry + Dem + 
                  trade + environshare + EUmember + 
                  as.factor(Country) + as.factor(Year),
                data = oecddata, 
                family = "binomial"(link = "logit"))

glm_mod3 <- glm(New_law ~ UD*CorpAll +  logGDP + Dem +
                  logemissions + Industry + trade  + 
                  environshare + EUmember + 
                  as.factor(Country) + lag(count1),
                data = oecddata, 
                family = "binomial"(link = "logit"))

writeLines(capture.output(stargazer(glm_mod, glm_mod2, glm_mod3, omit = c("Country","Year"), 
                                    se = list(sqrt(diag(vcovHC(glm_mod, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(glm_mod2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovHC(glm_mod3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on Country', 'Yes', 'Yes', 'Yes')),
                                    title = c("Logistic regression"),
                                    column.labels = c("Timetrend" ,"FE", "AR(1)"),
                                    dep.var.labels = c("Adopted climate policy"),
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", 
                                                         "Democracy", "Emissions per capita (log)", "Industry", 
                                                         "Trade openess", "Green party in gov.", "EU member", "Timetrend",
                                                         "Lagged dependent variable", "Interaction: union density and corporatism"))), "logistic.tex")

























#### Lagged independent variables ####
oecddata <- oecddata %>%
  group_by(Country) %>%
  dplyr::mutate(lagUD = dplyr::lag(UD, 1),
                lagCorp = dplyr::lag(CorpAll, 1),
                lagIndustry = dplyr::lag(Industry, 1),
                laglogGDP = dplyr::lag(logGDP, 1),
                lagDem = dplyr::lag(Dem, 1),
                laglogemssions = dplyr::lag(logemissions, 1),
                lagtrade = dplyr::lag(trade, 1),
                lagenvironshare = dplyr::lag(environshare, 1),
                lagEU = dplyr::lag(EUmember, 1))

negbin_lag1 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag2 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag3 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata)

##with interactions##
negbin_lagint1 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint2 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint3 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

writeLines(capture.output(stargazer(negbin_lag1, negbin_lag2, negbin_lag3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lag1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = "Main models, lagged independent variables (1 year)",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent"))), "negbinlagged1.tex")

writeLines(capture.output(stargazer(negbin_lagint1, negbin_lagint2, negbin_lagint3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lagint1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    notes = c("Country fixed effects on all models"),
                                    title = "Main models with interaction effects, lagged independent variables (1 year)",
                                    dep.var.labels = "Newly adopted climate laws",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "negbinlaggedinteract1.tex")

#### next lag
oecddata <- oecddata %>%
  group_by(Country) %>%
  dplyr::mutate(lagUD = dplyr::lag(UD, 2),
                lagCorp = dplyr::lag(CorpAll, 2),
                lagIndustry = dplyr::lag(Industry, 2),
                laglogGDP = dplyr::lag(logGDP, 2),
                lagDem = dplyr::lag(Dem, 2),
                laglogemssions = dplyr::lag(logemissions, 2),
                lagtrade = dplyr::lag(trade, 2),
                lagenvironshare = dplyr::lag(environshare, 2),
                lagEU = dplyr::lag(EUmember, 2))

negbin_lag1 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag2 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag3 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata)

##with interactions##
negbin_lagint1 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint2 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint3 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

writeLines(capture.output(stargazer(negbin_lag1, negbin_lag2, negbin_lag3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lag1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = "Main models, lagged independent variables (2 years)",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent"))), "negbinlagged2.tex")

writeLines(capture.output(stargazer(negbin_lagint1, negbin_lagint2, negbin_lagint3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lagint1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    notes = c("Country fixed effects on all models"),
                                    title = "Main models with interaction effects, lagged independent variables (2 years)",
                                    dep.var.labels = "Newly adopted climate laws",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "negbinlaggedinteract2.tex")


#### next lag
oecddata <- oecddata %>%
  group_by(Country) %>%
  dplyr::mutate(lagUD = dplyr::lag(UD, 1),
                lagCorp = dplyr::lag(CorpAll, 1),
                lagIndustry = dplyr::lag(Industry, 1),
                laglogGDP = dplyr::lag(logGDP, 1),
                lagDem = dplyr::lag(Dem, 1),
                laglogemssions = dplyr::lag(logemissions, 1),
                lagtrade = dplyr::lag(trade, 1),
                lagenvironshare = dplyr::lag(environshare, 1),
                lagEU = dplyr::lag(EUmember, 1))

negbin_lag1 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag2 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                      data = oecddata)

negbin_lag3 <- glm.nb(count1 ~ lagUD+lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                        lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                      data = oecddata)

##with interactions##
negbin_lagint1 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + timetrend+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint2 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + as.factor(Year)+ offset(log(indepyears)),
                         data = oecddata)

negbin_lagint3 <- glm.nb(count1 ~ lagUD*lagCorp + lagIndustry + laglogGDP + lagDem + laglogemssions +
                           lagtrade + lagenvironshare + lagEU  + as.factor(Country) + lag(count1) + offset(log(indepyears)),
                         data = oecddata)

writeLines(capture.output(stargazer(negbin_lag1, negbin_lag2, negbin_lag3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lag1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lag3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes',
                                                     'Country fixed effects', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    star.char = c("+", "*", "**", "***"),
                                    star.cutoffs = c(.1, .05, .01, .001),
                                    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
                                    notes.append = FALSE,
                                    title = "Main models, lagged independent variables (3 years)",
                                    dep.var.labels = "Yearly adopted climate policies",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent"))), "negbinlagged3.tex")

writeLines(capture.output(stargazer(negbin_lagint1, negbin_lagint2, negbin_lagint3,
                                    omit = c("Country", "Year"), 
                                    se = list(sqrt(diag(vcovCL(negbin_lagint1, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint2, cluster = oecddata$Country))),
                                              sqrt(diag(vcovCL(negbin_lagint3, cluster = oecddata$Country)))),
                                    add.lines=list(c('Standard errors clustered on country', 'Yes', 'Yes', 'Yes')),
                                    column.labels = c("Timetrend", "FE", "AR(1)"),
                                    notes = c("Country fixed effects on all models"),
                                    title = "Main models with interaction effects, lagged independent variables (3 years)",
                                    dep.var.labels = "Newly adopted climate laws",
                                    covariate.labels = c("Union density", "Corporatism", "GDP (log)", "Democracy", "Emissions per capita (log)",
                                                         "Industry", "Trade openess", "Green party in gov.", "EU member", "Timetrend", "Lagged dependent",
                                                         "Interaction: union density and corporatism"))), "negbinlaggedinteract3.tex")










