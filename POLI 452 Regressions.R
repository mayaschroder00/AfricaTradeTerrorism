################################################
# Running Regressions for Terrorism and Trade #
################################################

rm(list=ls())
library(tidyverse)
library(readxl)
library(texreg)
library(here)
setwd(here("/Users/mayaschroder/Desktop/POLI 452"))

# load in terrorism data and group to make dataset necessary
GTD.POLI452 = read_excel("globalterrorismdb_0919dist.xlsx") # error message, but necessary data still loads
GTD.POLI452 = GTD.POLI452 %>%
  group_by(iyear, country_txt)
GTD.POLI452=GTD.POLI452 %>%
  summarise(n=n())
GTD.POLI452 = GTD.POLI452 %>%
  rename(Year=iyear, Country=country_txt, terrorist_attack=n)

# load in land/labor/capital data and make necessary changes
land_data=read_excel("Land data.xls")
capital_data=read_excel("FebPwtExport10202020 Capital Stock.xlsx")
labor_data= read_excel("FebPwtExport10202020 Labor.xlsx")
step.1=left_join(capital_data, land_data)
factor_endowments=left_join(step.1, labor_data)
factor_endowments$Year=as.numeric(factor_endowments$Year)
factor_endowments$`Capital Stock Per Capita`=as.numeric(factor_endowments$`Capital Stock Per Capita`)
factor_endowments$`Hectares of Arable Land per Capita`=as.numeric(factor_endowments$`Hectares of Arable Land per Capita`)
factor_endowments$`Human Capital`=as.numeric(factor_endowments$`Human Capital`)
info=factor_endowments %>%
  group_by(Year) %>%
  summarise(avglabor=mean(`Human Capital`, na.rm = T), medlabor=median(`Human Capital`, na.rm=T), avgcapital=mean(`Capital Stock Per Capita`), medcapital=median(`Capital Stock Per Capita`, na.rm=T), avgland=mean(`Hectares of Arable Land per Capita`, na.rm = T), medland=median(`Hectares of Arable Land per Capita`, na.rm=T)) %>%
  ungroup()


# mutate data to determine abundance
factor_endowments <- factor_endowments %>%
  group_by(Year) %>% 
  mutate(capital_abundant=as.numeric(`Capital Stock Per Capita`>quantile(`Capital Stock Per Capita`, prob=0.75)))

factor_endowments <- factor_endowments %>%
  group_by(Year) %>% 
  mutate(labor_abundant=as.numeric(`Human Capital`>quantile(`Human Capital`, prob=0.75, na.rm=T)))

factor_endowments <- factor_endowments %>%
  group_by(Year) %>% 
  mutate(land_abundant=as.numeric(`Hectares of Arable Land per Capita`>quantile(`Hectares of Arable Land per Capita`, prob=0.75, na.rm=T)))

capital_countries <- factor_endowments %>%
  filter(capital_abundant==1)
unique(capital_countries$Country)

# combining all data
as.numeric(GTD.POLI452$Year)
capital.terrorism <- left_join(capital_countries, GTD.POLI452)

test <- na.omit(capital.terrorism)

south.africa <- test %>%
  filter(Country=="South Africa")

ggplot(data=south.africa) + geom_line(mapping=aes(x=Year, y=terrorist_attack)) + ylab("Terrorist Attacks")

capital.terrorism <- capital.terrorism %>%
  mutate(n=coalesce(terrorist_attack, 0))

# load in trade data and combine
WtoData = read_excel("WtoData_20201029104506.xlsx")
WtoData <- WtoData %>% gather("Year", "count", c(`1980`:`2019`), convert=T)
WtoData <- WtoData %>%
  rename(Country=`Reporting Economy`)
sapply(capital.terrorism, class)
as.numeric(WtoData$Year)
sapply(WtoData, class)
test.1<- left_join(capital.terrorism, WtoData)

# creating models
test.1 <- test.1[!grepl("Algeria", test.1$Country),]
test.1 <- test.1[!grepl("Morocco", test.1$Country),]
test.capital.total <- test.1 %>%
  filter(`Product/Sector`=="SI3_AGG - TO - Total merchandise")
Model1=lm(count~n, data=test.capital.total)
summary(Model1)

test.capital.c <- test.1 %>%
  filter(`Product/Sector`=="SI3_AGG - MA - Manufactures"| `Product/Sector`=="SI3_AGG - MAMT - Machinery and transport equipment
" | `Product/Sector`=="SI3_AGG - MAMTOTTL - Telecommunications equipment
" | `Product/Sector`=="SI3_AGG - MAMTAU - Automotive products
" | `Product/Sector`=="SI3_AGG - MAMTOTEP - Electronic data processing and office equipment"
  )
Model2=lm(count~n, data=test.capital.c)
summary(Model2)

# more joining for further models
test.2ish <- left_join(factor_endowments, WtoData)
test.2 <- left_join(test.2ish, GTD.POLI452)

test.2 <- test.2[!grepl("Algeria", test.2$Country),] # dropping North African countries
test.2 <- test.2[!grepl("Morocco", test.2$Country),]
test.2 <- test.2[!grepl("Egypt", test.2$Country),]
test.2 <- test.2[!grepl("Tunisia", test.2$Country),]
test.2 <- test.2 %>% mutate(n=coalesce(terrorist_attack, 0))
test.all.total <- test.2 %>%
  filter(`Product/Sector`=="SI3_AGG - TO - Total merchandise")
Model3=lm(count~n, data=test.all.total)
summary(Model3)


# final model
test.capital.all <- test.2 %>%
  filter(`Product/Sector`=="SI3_AGG - MA - Manufactures"| `Product/Sector`=="SI3_AGG - MAMT - Machinery and transport equipment
" | `Product/Sector`=="SI3_AGG - MAMTOTTL - Telecommunications equipment
" | `Product/Sector`=="SI3_AGG - MAMTAU - Automotive products
" | `Product/Sector`=="SI3_AGG - MAMTOTEP - Electronic data processing and office equipment"
  )
Model4=lm(count~n, data=test.capital.all)
summary(Model4)

# creating regression table (included in the paper)
htmlreg(list(Model1, Model2, Model3, Model4),
        file = "Terrorism and Trade Models.html",
        caption = "Effect of Terrorist Attacks on Exports", 
        caption.above = TRUE,
        custom.header = list("Capital-Abundant Countries"= 1:2, "All Countries" = 3:4),
        custom.coef.names = c("Intercept", "Exports"),
        custom.note = "%stars. All models estimated using ordinary least squares.",
        stars = c(0.001, 0.01, 0.05))

texreg(list(Model1, Model2, Model3, Model4),
        file = "terrorism_trade.tex",
        caption = "Effect of Terrorist Attacks on Exports", 
        caption.above = TRUE,
        custom.header = list("Capital-Abundant Countries"= 1:2, "All Countries" = 3:4),
        custom.coef.names = c("Intercept", "Exports"),
        custom.note = "%stars. All models estimated using ordinary least squares.",
        stars = c(0.001, 0.01, 0.05))
