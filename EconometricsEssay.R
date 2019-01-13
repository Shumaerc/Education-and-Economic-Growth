# Load necessary packages
library(here)
library(tidyverse)
library(haven)
library(gridExtra)
library(forcats)
library(stargazer)
library(ggpubr)
library(corrplot)

# Load data of GDP growth from WDI
data <- read_dta("/Users/ShuLFO/Desktop/Data Analysis/GDP_Growth.dta")
View(data)
head(data)

# tidy up the dataset
data$time <- as.numeric(data$ïtime)
data <- data[-c(1:4),]
data <- data[-1,]
data <- data[,-c(1:2)]
data$gdp <- data$gdpconstant2010usnygdpmktpkd
data <- rename(data, c(gdpconstant2010usnygdpmktpkd = "GDP",
               gdpgrowthannualnygdpmktpkdzg = "GDP_growth",
               gdppercapitaconstant2010usnygdpp = "GDP_per_capita",
               barroleeaverageyearsoftertiarysc = "YOSTer",
               barroleeaverageyearsofsecondarys = "YOSSec",
               barroleeaverageyearsofprimarysch = "YOSPri",
               barroleeaverageyearsoftotalschoo = "YOSTotal",
               gdppercapitagrowthannualnygdppca = "GDP_per_capita_growth"))

data$GDP <- as.numeric(data$GDP)
data$GDP_growth <- as.numeric(data$GDP_growth)
data$GDP_per_capita <- as.numeric(data$GDP_per_capita)
data$GDP_per_capita_growth <- as.numeric(data$GDP_per_capita_growth)
data$YOSTer <- as.numeric(data$YOSTer)
data$YOSSec <- as.numeric(data$YOSSec)
data$YOSPri <- as.numeric(data$YOSPri)
data$YOSTotal <- as.numeric(data$YOSTotal)

# Create a subset of the data1 with variables of interest
data2 <- data %>% select(countrycode, countryname, time, GDP, GDP_per_capita, GDP_growth, GDP_per_capita_growth, YOSPri, YOSSec, YOSTer, YOSTotal)
View(data2)

# Calculate averages of each vars
data3 <- data2 %>% group_by(countryname, countrycode) %>% 
        summarise(AVG_Growth = mean(GDP_growth, na.rm = TRUE),
                  AVG_percapita_growth = mean(GDP_per_capita_growth, na.rm = TRUE),
                  AVG_YOS_Pri = mean(YOSPri, na.rm = TRUE),
                  AVG_YOS_Sec = mean(YOSSec, na.rm = TRUE),
                  AVG_YOS_Ter = mean(YOSTer, na.rm = TRUE),
                  AVG_YOS_Total = mean(YOSTotal, na.rm = TRUE)) %>% 
        arrange(desc(AVG_Growth)) 
head(data3)
View(data3)

# export it as a csv file 
write_csv(data3, path = "/Users/ShuLFO/Desktop/R/Econometrics Essay/data3.csv")

# import a new merged dataset which contains Hanushek data as well as the original data3
newdata <- read_dta("/Users/ShuLFO/Desktop/Data Analysis/Hanushek Data/hawo2012tabs.dta")
View(newdata)
glimpse(newdata)

# recode some vars as numeric
newdata$avg_yos_pri <- as.numeric(newdata$avg_yos_pri)
newdata$avg_yos_sec <- as.numeric(newdata$avg_yos_sec)
newdata$avg_yos_ter <- as.numeric(newdata$avg_yos_ter)
newdata$avg_yos_total <- as.numeric(newdata$avg_yos_total)

# create a subset and tidying up
newdata2 <- newdata %>% select(countrycode, country, reg, avg_growth, avg_percapita_growth, ypc60,avg_yos_pri,avg_yos_sec,avg_yos_ter,avg_yos_total,tmeanmsagay, open, exprop, tfr_avg, tropical, oecd)
View(newdata2)
newdata2[42, 3:8] <- newdata2[53,3:8]
subdata <- newdata2[-c(48, 53),]
subdata$reg[6] <- "SSAFR"
subdata$reg[35] <- "SSAFR"
subdata$reg[42] <- "S-EUR"
subdata$oecd <- as.factor(subdata$oecd)
View(subdata)

#descriptive statistics
stargazer(as.data.frame(subdata[,-c(4, 16)]),
          type = "text",
          title = "Descriptive Statistics",
          digits = 1,
          covariate.labels = c("Average GDP per capita growth", "Log GDP per capita in 1960", "Average years of primary schooling",
                               "Average years of secondary schooling", "Average years of tertiary schooling",
                               "Average years of total schooling", "Average test score","Openness", "Security of property rights",
                               "Fertility rate", "Tropical climate"),
          nobs = FALSE,
          align = TRUE,
          style = "aer",
          summary.stat = c("mean", "median","sd", "min", "max"),
          out = "desc_table1.html")

# Distribution

ggplot(subdata, aes(reorder(reg, avg_yos_total), avg_yos_total)) +
        geom_boxplot() +
        coord_flip() +
        labs(y = "Years of schooling",
             x = "Regions") +
        theme_classic();

box1 <- ggboxplot(subdata, x = "reg", y = "avg_yos_total",
          add = "jitter", shape = "reg") +
        labs(x = "",
             y = "Average years of schooling") +
        theme_classic(base_family = "Times New Roman")

box1_1 <- ggpar(box1, legend = "none")


box2 <- ggboxplot(subdata, x = "reg", y = "avg_percapita_growth",
                  add = "jitter", shape = "reg") +
        labs(x = "",
             y = "Average GDP per capita growth") +
        theme_classic(base_family = "Times New Roman")

box2_1 <- ggpar(box2, legend = "none")

box3 <- ggboxplot(subdata, x = "reg", y = "tmeanmsagay",
                  add = "jitter", shape = "reg") +
        labs(x = "Regions",
             y = "Test Score") +
        theme_classic(base_family = "Times New Roman")

box3_1 <- ggpar(box3, legend = "none")

grid.arrange(box2_1,box1_1 ,box3_1)


# Average GDP per capita growth by country
ggplot(subdata, aes(reorder(country, avg_percapita_growth), avg_percapita_growth)) +
        geom_point(col = "red", size = 3) +
        geom_segment(aes(x = country,
                         xend = country,
                         y = min(avg_percapita_growth),
                         yend = max(avg_percapita_growth)),
                     linetype = "dashed",
                     size = .1) +
        labs(y = "Average per capita growth (%)",
             x = "Country") +
        coord_flip() +
        theme_classic(base_family = "Times New Roman", base_size = 13)

# Figure 3
plot1 <- ggplot(subdata, aes(avg_yos_total, avg_percapita_growth)) +
        geom_label(label = subdata$countrycode) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Average GDP Per Capita Growth and Average Years of Total Schooling",
             x = "Average Years of Total Schooling",
             y = "Average GDP Per Capita Growth (%)") +
        theme_classic(base_family = "Times New Roman")

plot2 <- ggplot(subdata, aes(tmeanmsagay, avg_percapita_growth)) +
        geom_label(label = subdata$countrycode) +
        geom_smooth(method = "lm", se = F) +
        labs(title = "Average GDP Per Capita Growth and Test Score",
             x = "Test Score",
             y = "Average GDP Per Capita Growth (%)") +
        theme_classic(base_family = "Times New Roman")
grid.arrange(plot1, plot2)


# Relationship between Total YOS and Per capita growth
YOS_Growth <- ggplot(subdata, aes(avg_yos_total, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Average Years of Total Schooling") +
        theme_classic(base_family = "Times New Roman")

# Higher the initial GDP, the lower growth
GDP60vsGrowth <- ggplot(subdata, aes(ypc60, avg_percapita_growth)) +
        geom_label(label = subdata$countrycode) +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Log GDP Per Capita in 1960",
             y = "Average GDP Per Capita Growth (%)") + 
        theme_classic(base_family = "Times New Roman")

## Others 
OpennessvsGrowth <- ggplot(subdata, aes(open, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Openness",
             y = "") +
        theme_classic(base_family = "Times New Roman")

PropvsGrowth <- ggplot(subdata, aes(exprop, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Security of Property Rights",
             y = "") +
        theme_classic(base_family = "Times New Roman")

FertvsGrowth <- ggplot(subdata, aes(tfr_avg, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Fertility rate",
             y = "") +
        theme_classic(base_family = "Times New Roman")

TropvsGrowth <- ggplot(subdata, aes(tropical, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Tropical location",
             y = "") +
        theme_classic(base_family = "Times New Roman")

YOSvsGrowth <- ggplot(subdata, aes(avg_yos_total, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Years of schooling",
             y = "") +
        theme_classic(base_family = "Times New Roman")

Test_Growth <- ggplot(subdata, aes(tmeanmsagay, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        labs(x = "Test Score",
             y = "") +
        theme_classic(base_family = "Times New Roman")

grid.arrange(YOSvsGrowth, 
             Test_Growth, 
             OpennessvsGrowth, 
             PropvsGrowth,
             FertvsGrowth, 
             TropvsGrowth,
             left = "Average annual GDP per capita growth rate",
             ncol = 1)

# Relationship btw Cognitive skills and Growth by Region
ggplot(subdata, aes(tmeanmsagay, avg_percapita_growth)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        facet_wrap(~ reg, ncol = 2) +
        theme_bw()

# Create a subset of the data 
GrowthRegion <- subdata %>% group_by(reg) %>% 
        summarise(mean_growth_reg = mean(avg_percapita_growth),
                  mean_total_yos = mean(avg_yos_total, na.rm = TRUE),
                  mean_score = mean(tmeanmsagay, na.rm = TRUE)) %>% 
        arrange(desc(mean_growth_reg)) 

# Mean growth rate by region        
ggplot(GrowthRegion, aes(reorder(reg, mean_growth_reg), mean_growth_reg)) +
        geom_bar(stat = "identity") +
        labs(title = "Average GDP Per Capita Growth by Region between 1960 and 2000",
             x = "Region",
             y = "Average GDP Per Capita Growth (%)") +
        theme_classic() 

# Mean Total YOS by region
ggplot(GrowthRegion, aes(reorder(reg, mean_total_yos), mean_total_yos)) +
        geom_bar(stat = "identity") +
        theme_classic()

# Mean Test Score by region 
ggplot(GrowthRegion, aes(reorder(reg, mean_score), mean_score)) +
        geom_bar(stat = "identity") +
        theme_classic()

# Correlation matrix using corrplot

S <- subdata %>% select(-c(countrycode, country, reg, avg_growth, avg_yos_pri, avg_yos_sec, avg_yos_ter, oecd))
glimpse(S)
S <- S %>% rename(c(avg_percapita_growth = "Growth",
               ypc60 = "GDP 1960",
               avg_yos_total = "Years of schooling",
               tmeanmsagay = "Test Score",
               open = "Openness",
               exprop = "Property rights",
               tfr_avg = "Fertility",
               tropical = "Tropical location")) %>%
        cor(use = "complete.obs")

corrplot(S, method = "number", col = "black", cl.pos = "n", type = "lower")
corrplot.mixed(S)


# Regression Analysis 

m1 <- lm(avg_percapita_growth ~ avg_yos_total, data = subdata)

m2 <- lm(avg_percapita_growth ~ avg_yos_total + ypc60, data = subdata)

m3 <- lm(avg_percapita_growth ~ avg_yos_total + ypc60 + tmeanmsagay, data = subdata)

m4 <- lm(avg_percapita_growth ~ avg_yos_total + ypc60 + tmeanmsagay + open + exprop, data = subdata)

m5 <- lm(avg_percapita_growth ~ avg_yos_total + ypc60 + tmeanmsagay + open + exprop + tfr_avg + tropical, data = subdata)

m6 <- lm(avg_percapita_growth ~ avg_yos_total + ypc60 + tmeanmsagay + open + exprop + tfr_avg + tropical + oecd, data = subdata)

## Regression table output
stargazer(m1, m2, m3, m4, m5, m6,
          dep.var.labels = "Average Annual GDP Per Capita Growth Rate, 1960-2000",
          covariate.labels = c("Average years of total schooling",
                               "GDP per capita in 1960",
                               "Cognitive skills",
                               "Openness",
                               "Property rights",
                               "Fertility",
                               "Tropical location",
                               "OECD"),
          type = "text",
          align = TRUE,
          omit.stat = c("n", "ser", "f"),
          out = "model_ver1.html")

