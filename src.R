### R code associated with opioid crisis analysis

setwd("~/Documents/programming/GitHub/opioid-crisis/NSDUH-2019")
load("~/Documents/programming/GitHub/opioid-crisis/NSDUH-2019/NSDUH_2019.RData")
NSDUH_2019 <- PUF2019_100920
library(tidyverse)
glimpse(NSDUH_2019)

NSDUH_2019 <- select(NSDUH_2019, herever, pnranyflag, AGE2, service, 
                     irsex, IREDUHIGHST2, NEWRACE2, income, COUTYP4)


## ## CLEAN VARIABLES

## ever used heroin
NSDUH_2019$herever <- recode(NSDUH_2019$herever, "1" = 1, "2" = 0)
NSDUH_2019 <- subset(NSDUH_2019, !is.na(NSDUH_2019$herever))

## service
NSDUH_2019$service <- recode(NSDUH_2019$service, "1" = 1, "2" = 0)
NSDUH_2019 <- subset(NSDUH_2019, !is.na(NSDUH_2019$service))

## sex
NSDUH_2019$irsex <- factor(NSDUH_2019$irsex)

## race/ethnicity
NSDUH_2019$NEWRACE2 <- factor(NSDUH_2019$NEWRACE2)


## ## Descriptive statistics
summary(NSDUH_2019)
#install.packages("Hmisc")
library(Hmisc)
describe(NSDUH_2019)
library(stargazer)
stargazer(NSDUH_2019)

## ## VISUALIZATIONS

## dependent variable: ever used heroin (herever)

herever_table <- table(NSDUH_2019$herever)
barplot(herever_table, main = 'Responses to "Have you ever, even once, used heroin?"',
        xlab = "Response",
        ylim = c(0, 47500),
        names.arg = c("No", "Yes"))
text(x = .7, y = 45500, sum(NSDUH_2019$herever == 0), col = "black")
text(x = 1.9, y = 2500, sum(NSDUH_2019$herever == 1), col = "black")


## independent variable: ever used prescription pain relievers

pnranyflag_table <- table(NSDUH_2019$pnranyflag)
barplot(pnranyflag_table, main = 'Respondent Use of Prescription Pain Relievers',
        xlab = "Response",
        ylim = c(0, 30000),
        names.arg = c("Never used", "Used at least once"))
text(x = .7, y = 20000, sum(NSDUH_2019$pnranyflag == 0), col = "black")
text(x = 1.9, y = 27000, sum(NSDUH_2019$pnranyflag == 1), col = "black")


## independent x dependent

NSDUH_2019$factor_herever <- factor(NSDUH_2019$herever)
NSDUH_2019$factor_herever <- fct_recode(NSDUH_2019$factor_herever,
                                        "Never used heroin" = "0",
                                        "Used heroin at least once" = "1")

NSDUH_2019$factor_pnranyflag <- factor(NSDUH_2019$pnranyflag)
NSDUH_2019$factor_pnranyflag <- fct_recode(NSDUH_2019$factor_pnranyflag,
                                           "Never used prescription pain relievers" = "0",
                                           "Used prescription pain relievers at least once" = "1")

ggplot(NSDUH_2019, aes(x = jitter(as.numeric(factor_pnranyflag)), y = jitter(as.numeric(factor_herever)))) +
  geom_point(alpha=0.5) +
  #stat_smooth(method="lm")
  labs(x = "Prescription Pain Reliever Use", y = "Heroin Use")+
  scale_y_continuous(breaks=c(1,2), 
                     labels=c("Never used heroin", 
                              "Used heroin at least once"))+
  scale_x_continuous(breaks=c(1,2),
                     labels=c("Never used prescription pain relievers", 
                              "Used prescription pain relievers at least once"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())


## ## REGRESSION ANALYSIS

## bivariate regression

summary(lm(herever*100 ~ pnranyflag, data = NSDUH_2019))

## multivariate regression

# sex indicator variables

NSDUH_2019$male <- ifelse(NSDUH_2019$irsex == "1", 1, 0)
NSDUH_2019$female <- ifelse(NSDUH_2019$irsex == "2", 1, 0)

# race/ethnicity indicator variables

NSDUH_2019$white <- ifelse(NSDUH_2019$NEWRACE2 == "1", 1, 0)
NSDUH_2019$black <- ifelse(NSDUH_2019$NEWRACE2 == "2", 1, 0)
NSDUH_2019$native <- ifelse(NSDUH_2019$NEWRACE2 == "3", 1, 0)
NSDUH_2019$pacisl <- ifelse(NSDUH_2019$NEWRACE2 == "4", 1, 0)
NSDUH_2019$asian <- ifelse(NSDUH_2019$NEWRACE2 == "5", 1, 0)
NSDUH_2019$multi <- ifelse(NSDUH_2019$NEWRACE2 == "6", 1, 0)
NSDUH_2019$hispanic <- ifelse(NSDUH_2019$NEWRACE2 == "7", 1, 0)

summary(lm(herever*100 ~ pnranyflag + female + AGE2 +  
             black + native + pacisl + asian + multi + hispanic +
             service + IREDUHIGHST2 + income + COUTYP4, 
           data = NSDUH_2019))


## ## hypothesis testing

rx_users <- NSDUH_2019[NSDUH_2019$pnranyflag == 1,]
never_used_rx <- NSDUH_2019[NSDUH_2019$pnranyflag == 0,]

t.test(rx_users$herever, never_used_rx$herever)


