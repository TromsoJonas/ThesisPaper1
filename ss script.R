rm(list=ls())
getwd()
setwd("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Artikkel striking similarities")
library(lme4)
library(sandwich)
library(lmtest)
library(car)
library(tidyverse)
library(nlme)
library(haven)
library(forecast)
library(stargazer)
ss <- read_dta("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Artikkel striking similarities/ss.dta")



#1. Data management----
#Creating variable Sweden and Norway 
d <- ss
glimpse(d)
d$country <- ifelse(d$riksomraade == "Övra Norrland", c("Sweden"), c("Norway"))
d$norway <- ifelse(d$country == "Norway", 1, 0)
table(d$norway)
d$nr <- d$lannr

d <- subset(d, kommune!="Longyearbyen") #removes Longyearbyen

#Creating change variable innby
d <- d %>%
  group_by(kommune) %>%
  mutate(demchange = innby - lag(innby, 1))

#Creating growth variable innby
d <- d %>%
  group_by(kommune) %>%
  mutate(demgrowth = ((innby/lag(innby, 1))-1)*100)   

#Creating change variable innbypros
d <- d %>%
  group_by(kommune) %>%
  mutate(changepros = innbypros - lag(innbypros, 1))

#Creating growth variable innbypros
d <- d %>%
  group_by(kommune) %>%
  mutate(growthpros = ((innbypros/lag(innbypros, 1))-1)*100) 

#Creating growth variable trend
d <- d %>%
  group_by(kommune) %>%
  mutate(trend = year-1952)

#Creating growth variable 
d <- d %>%
  group_by(kommune) %>%
  mutate(intact = year-1975)

d$intact[d$intact <0] <- 0 #velger alle observasjoner som er under 0 og gjør dem til 0

#Creating higher education variables
d$uni <- ifelse(d$kommune=="Tromsø" & d$year>1967 | d$kommune== "Umeå" & d$year > 1964, 1,0)
table(d$uni, d$kommune)

d$college <- ifelse(d$kommune=="Bodø" & d$year>1970, 1,0)

d$college <- ifelse(d$kommune=="Bodø" & d$year > 1970 | d$kommune == "Luleå" & d$year > 1970  | d$kommune == "Harstad" & d$year > 1993  | d$kommune == "Kautokeino" & d$year > 1988 | d$kommune == "Alta" & d$year > 1972 | d$kommune== "Narvik" & d$year>1993|  d$kommune == "Nesna", 1,0)
table(d$college)

#Classifying municipalities in 1970
e <- d %>% filter(year==1975)
e <- e %>% arrange(desc(innby))
e[1,"kommune"] 
df <- e %>% select("kommune", "country", "innby", "innbypros")
print(df, n=116)
e$innbycat <- cut(e$innby, c(0,5000,10000,25000,150000), labels=c("Smallest (<5000)", "Small (5-10000)", "Medium (10-25000)", "Large (>25000)"))
table(e$innbycat, e$country)
e$loginnby <- log(e$innby)
summary(e$loginnby)
e <- e %>% select(kommune, innbycat, loginnby)
e$innbycat <- relevel(e$innbycat, ref= 4)
d <- merge(d, e, by="kommune")

library(plm)
d3 <-pdata.frame(d, index=c("kommune", "year"))
glimpse(d3)
d2 <- d %>% filter(year>1969)
d <- d %>% filter(year>1951)
d4 <- pdata.frame(d2, index=c("kommune", "year"))

# 2. Panel data analyis----
library(nlme)

rho0 <- lme(changepros ~ 1, random= ~1|kommune, data=d)
rho0 <- lme(changepros ~ 1+trend, random= ~1|kommune, data=d)
rho1 <- lme(changepros ~ trend+intact+norway+intact:norway, random= ~trend |kommune, data = d, method="ML")
rho2 <- lme(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway, random= ~trend |kommune, data = d, method="ML")
rho3 <- lme(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway + uni+college, random= ~trend |kommune, data = d, method="ML")
rho5 <- lme(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway + uni+college, random= ~trend|kommune, data = d, method="ML")
rho6 <- lme(changepros ~ trend+intact+norway+intact:norway+ loginnby+loginnby:norway + uni+college, random= ~trend |kommune, data = d, method="ML")

library(sjstats)
library(lme4)
model1 <- lmer(changepros ~ 1 + (1 | kommune), data=d)
summary(model1)
icc(model1)

summary(rho6)
anova(rho3, rho6)
stargazer(rho0, rho1, rho2,rho3,rho6, type="text")

stargazer(rho1, rho2, rho3, type = "html", digits =2, 
          out = "table1.html",
          title="Table 1: Regression analysis 1970-2015") 

rho0.ar1 <- update(rho0, correlation = corAR1())
rho1.ar1 <- update(rho1, correlation = corAR1())
rho2.ar1 <- update(rho2, correlation = corAR1())
rho3.ar1 <- update(rho3, correlation = corAR1())
rho5.ar1 <- update(rho5, correlation = corAR1())
  
Acf(residuals(rho3, type="normalized"))
Acf(residuals(rho3.ar1, type="normalized"))

summary(rho3.ar1)
stargazer(rho0.ar1, rho1.ar1, rho2.ar1, rho3.ar1, rho5.ar1, type = "text")
stargazer(rho0.ar1, rho1.ar1, rho2.ar1, rho3.ar1, type = "html", digits =3, 
          out = "table4.html",
          title="Table 4: Multilevel Regression Analysis 1952-2015", 
          covariate.labels = c("Year", "Year after 1975", "Norway", "Municipality < 5000", "Municipality 5000-10000",
                               "Municipality 10000-25000", "University City", "University College City",
                               "Interaction Norway*Year after 1975", "Muncipality < 5000*Norway", "Municipality 5-10000*Norway",
                               "Municpality10-25000*Norway"),
          column.labels = c("Model 0", "Model 1", "Model 2", "Model 3"), model.numbers = FALSE, notes = "Standard errors in parentheses", dep.var.labels = "Yearly change") 


# 3. Graphs ----
library(ggplot2)
figure1 <- ggplot(data = d, aes(x=year, y=innbypros)) + geom_smooth() + scale_color_manual(name= c("Countries"), values=c("orangered","cornflowerblue")) + aes(colour=country) + facet_wrap(~innbycat, scales = "free") + theme_bw() + theme(legend.background = element_rect(colour = "black"), legend.position = c(.90,.90)) + labs(list(title= "Figure 1 - Percentage of national population 1952-2015", x="Year", y="Percentage of national population", caption="Source: Statistics Norway and Statistics Sweden")) + geom_vline(xintercept=1975) + scale_x_continuous(breaks=c(1955,1965,1975,1985,1995,2005,2015))

figure1

ggsave("figure1.jpg", plot=figure1, dpi=600)

city <- d %>% filter(kommune == "Tromsø" | kommune == "Bodø" | kommune=="Rana" | kommune == "Umeå" | kommune=="Luleå" | kommune=="Skellefteå")

figure2 <- ggplot(city, aes(x=year, y=innbypros, colour=kommune, linetype=kommune),
) + geom_line(fullrange = TRUE, se=FALSE) +
  theme_bw() + scale_color_manual(name= "Municipalities", 
                                  labels=c("Bodø (University College)", "Luleå (University College)", "Rana (Industrial city)", 
                                           "Skellefteå (Industrial city)", "Tromsø (University)","Umeå (University)"), 
                                  values=c("darkblue", "cornflowerblue",
                                           "darkred", "coral", "darkgreen",
                                           "darkolivegreen2"))+
  theme(legend.background = element_rect(colour = "black"), legend.position = c(.17,.83)) + 
  labs(list(title= "Figure 2: Percentage of national population  1952-2015", x="Year", y="Percentage of national population", caption="Source: Statistics Norway and Statistics Sweden", subtitle="The Three Largest Municipalities in Northern Norway and Northern Sweden")) +
  xlim(1950,2015) + ylim(0.4,1.5) + scale_y_continuous(breaks=c(0.4,0.6,0.8,1,1.2,1.4)) + scale_x_continuous(breaks=c(1955,1965,1975,1985,1995,2005,2015)) +
  scale_linetype_manual(values=c("solid", "dashed","solid", "dashed", "solid","dashed"), guide=FALSE) + geom_vline(xintercept=1975)

figure2

ggsave("figure2.jpg", plot=figure2, dpi=600)

# 4. appendix ----
d6 <- d %>% select(innbypros, innby, norway, changepros, trend, intact, uni, college, innbycat)
stargazer(d6, type = "html", title = "Appendix: Descriptive statistics of variables",
          align = TRUE, digits=2, out ="appendix.html", 
          covariate.labels = c("Percentage of national population","Inhabitants", "Norway", "Change", "Year", "Year after 1975", "University City", "University College City"))           
          
# 5. Testing with varying slopes
rho3 <- lme(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway + uni+college, random= ~1 |kommune, data = d)
summary(rho3)

rho3slop <- lme(changepros ~ trend+intact+norway+intact:norway + innbycat+innbycat:norway+ uni+college, 
                random= ~ innbycat |kommune , data = d)
rho3sloptrend <- lme(changepros ~ trend+intact+norway+intact:norway + innbycat+innbycat:norway+ uni+college, 
            random= ~ trend|kommune , data = d, method="ML")

anova(rho3sloptrend, rho5)

rho3sloptrend.ar1 <- update(rho3sloptrend, correlation = corAR1())
anova(rho3sloptrend, rho3slop)
summary(rho3)
rho3.ar2 <- update(rho3slop, correlation = corAR1())
summary(rho3.ar2)

stargazer(rho0.ar1, rho1.ar1, rho2.ar1, rho3.ar1, rho5.ar1, rho3sloptrend.ar1, type = "text")



library(plm)
plmpooled <- plm(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway + uni+college, data = d, model = "pooling", index = c("kommune","year"))
summary(plmpooled)
plmfixed <- plm(changepros ~ trend+intact+norway+intact:norway+ innbycat+innbycat:norway + uni+college, data = d, model = "within", effect="individual", index = c("kommune","year"))
summary(plmfixed)
stargazer(plmpooled, plmfixed, type="text")

anova(rho3, plmfixed)



