# spring22-1gizemg
spring22-1gizemg created by GitHub Classroom
library(ggplot2)
install.packages("ggplot2")
install.packages("data.table")
library(data.table)
install.packages("knitr")
library(knitr)
install.packages("zoo")
library(zoo)
install.packages("readxl")
library(readxl)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("lubridate")
library(lubridate)
install.packages("GGally")
library(GGally)


# 1.Introduction
#Turkey produces very little of its own oil and natural gas needs.Turkey generally uses natural gas and oil to produce electricity.The automotive industry is considered to be the locomotive of the economy . The reason for this effect of the sector in the economy is its very close relationship with other branches of industry and other sectors of the economy such as fuel oil industry and energy industry.As the number of automobile produced increases, the need for fuel will also increase.As the amount of natural gas and fuel oil produced increases, the amount of imported electricity decreases.
#I have monthly observations for our 3 sets of data The Number Of Car Produced,Industrial Oil and Fuel Oil Production Index and Amount of Imported Electricity. The data is collected between 2016 and 2019 by the Central Bank of the Republic of Turkey. I will be using data until 2019 I won't be analyzing the effects of COVID-19 on the Turkey. On the following paragraphs I will be analyze and question assumed correlation between the different groups of data in hand.
#I searched the words related to the data I examined in Google Trend.s it is useful to check if search volume of a certain keyword has some relation to a measure. 
#The aim of this homework is try to find the answer of “Are  the amount of electricity import, production of natural gas and fuel oil index and the number of car produced are related?”.

# 2.Data Manipulation and Visualization

### a.Data Manipulation

#The datasets with which I will try to find correlation relationships are taken from [EVDS](https://evds2.tcmb.gov.tr/).

#Datasets are manipulated so that their dates coincide
Production_automobile = read_excel("C:\\Users\\GİZEM\\Downloads\\EVDS (10).xlsx", range = "A2:B38", col_names = c("Date", "Number_of_automobile"))

Industrial_Production_Index = read_excel("C:\\Users\\GİZEM\\Downloads\\EVDS (9).xlsx", range = "A2:B38", col_names = c("Date", "Index"))

Electricity_Import = read_excel("C:\\Users\\GİZEM\\Downloads\\EVDS (8).xlsx", range = "A2:B38", col_names = c("Date", "Electricity_Import"))

#Datasets are merged in one data table

dt = data.table(Date = Production_automobile$'Date', Production_Number_Automobile = Production_automobile$'Number_of_automobile', Production_Index = Industrial_Production_Index$'Index', Electricity_Import_Rate = Electricity_Import$'Electricity_Import')
head(dt)

#Manipulate date column to make it time object by adding a day
dt$Date<-parse_date_time(dt[,Date], "Ym")
dt[,Date := as.Date(Date, format='%Y-%m-%d')]

head(dt)


### b.Data Visualization
# After those manipulations, it would be useful to visualize the measures in histogram to check for any assumption related to distribution or other statistics.

ggplot(dt, aes(x = Production_Number_Automobile) )+
  geom_histogram(aes(y = ..density..),bins=30, fill = "light blue") +
  geom_density(alpha = 0.2)+
  theme_minimal() + facet_wrap(~year(Date)) + labs(title = "Histograms of Monthly Production Number of Automobile in Turkey over 2016-2019",x = "Monthly Production Number Automobile ")
#By looking at the yearly histograms of of Monthly Production Number of Automobile in Turkey over 2016-2019, it can assumed that the samples collected in a monthly manner cannot be fitted in to a normal distribution or other distributions . 

ggplot(dt, aes(x = Date, y =Production_Number_Automobile )) + geom_line(color = "dark blue") + theme_minimal() + labs(title = "Line Graph of Production Number of Automobile in Turkey over 2016-2019", y = "Production Number Automobile")
#The number of automobile produced line graphs shows us no apparent trend but  but seasonality is appeared..

AutomobileTrend <- read.csv("C:\\Users\\GİZEM\\Downloads\\multiTimeline (1).csv",header = T, skip = 1)
names(AutomobileTrend)[2] <-"Popularity"
plot(AutomobileTrend$Popularity, type = "l", xlab = "Time(Months)", ylab = "Popularity", main = "Popularity of the Keyword")
#Automobile in Turkey seems to gain popularity over years and lose some at the end. .
#I chose “automobile” as google trends search keyword.

ggplot(dt, aes(x = Production_Index) )+
  geom_histogram(aes(y = ..density..),bins = 30, fill = "light blue") +
  geom_density(alpha = 0.2)+
  theme_minimal() + facet_wrap(~year(Date)) + labs(title = "Histograms of Monthly Industrial Production Index in Turkey over 2016-2019",x = "Monthly Production Index")
#The Histograms of Monthly Industrial Production Index in Turkey over 2016-2019  can nearly be represented via a normal distribution, except this year 2019. However, the mean values are subject to a relatively increase.
ggplot(dt, aes(x = Date, y =Production_Index )) + geom_line(color = "dark blue") + theme_minimal() + labs(title = "Line Graph of Industrial Production Index in Turkey over 2016-2019", y = "Production Index")
##The industrial production index line graphs shows us  apparent trend after 2017 and seasonality.

OilProductionTrend <- read.csv("C:\\Users\\GİZEM\\Downloads\\multiTimeline (2).csv",header = T, skip = 1)
names(OilProductionTrend)[2] <-"Popularity"
plot(OilProductionTrend$Popularity, type = "l", xlab = "Time(Months)", ylab = "Popularity", main = "Popularity of the Keyword")
#Oil Production in Turkey  tend to have a yearly cyclic behavior..
#I chose “Oil Production” as google trends search keyword.


ggplot(dt, aes(x = Electricity_Import_Rate) )+
  geom_histogram(aes(y = ..density..),bins=30, fill = "light blue") +
  geom_density(alpha = 0.2)+
  theme_minimal() + facet_wrap(~year(Date)) + labs(title = "Histograms of Monthly Electricity Import in Turkey over 2016-2019",x = "Monthly Electricity Import ")
#By looking at the yearly Histograms of Monthly Electricity Import in Turkey over 2016-2019, it can assumed that the samples collected in a monthly manner cannot be fitted in to a normal distribution or other distributions 
ggplot(dt, aes(x = Date, y =Electricity_Import_Rate )) + geom_line(color = "dark blue") + theme_minimal() + labs(title = "Line Graph of Electricity Import in Turkey over 2016-2019", y = "Electricity Import")
#Electricity_Import_Rate line graphs shows us  apparent no trend and seasonality.After 2017 it seems to be randomly fluctuate.At the end of 2016 sudden decrease.
ElectricityImportTrend <- read.csv("C:\\Users\\GİZEM\\Downloads\\multiTimeline (3).csv",header = T, skip = 1)
names(ElectricityImportTrend)[2] <-"Popularity"
plot(ElectricityImportTrend$Popularity, type = "l", xlab = "Time(Months)", ylab = "Popularity", main = "Popularity of the Keyword")
## Electricity Import Trend in Turkey  tend to have a yearly cyclic behavior..
###I chose “Electricity Import” as google trends search keyword.


ggplot(dt, aes(x = Date))+geom_boxplot(aes(y = Production_Number_Automobile, group = year(Date),fill="Production_Number_Automobile")) +geom_boxplot(aes(y = Production_Index*1000, group = year(Date),fill= "Production_Index")) + geom_boxplot(aes(y =Electricity_Import_Rate, group = year(Date),fill= "")) + theme_minimal() + labs(title = "Boxplots of Monthly Production Number of Automobile, Production Index and Electricity Import", y = "") + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
#Above boxplot of Production_Number_Automobile shows the  no trends  as similar as the histograms do.The production index shows the  upward trends more visible than the histograms do. Electricity_Import_Rate shows the  no trends  as similar as the histograms do
ggplot(dt, aes(x = Date)) + geom_line(aes(y = Production_Number_Automobile, color = "Production Number Automobile")) + geom_line(aes(y = Production_Index*1000, color = "Production Index")) + geom_line(aes(y = Electricity_Import_Rate, color = "Electricity Import")) + theme_minimal() + labs(title = "Line Graphs of Monthly Production Number of Automobile, Production Index and Electricity Import", y = NULL, color = "Legend") + scale_color_manual(values = c("dark blue", "dark red", "dark green"))
#Before moving to correlation analysis, figure above shows all three datasets together.I use y = Production_Index*1000 value to see the production index effect . While there is no noticeable trend followed by production number automobile and electricity import rate.Trend is visible that oil and natural gas industrial production index is upward.



# 3.Correlation Analysis

ggpairs(dt[,c(2:4)]) + labs(title = "Paired Correlation Graphs")

cor(dt[, c(2:4)])
#Correlation matrix gives us the correlation values clearly. Notice that the matrix is a symmetric matrix with diagonals equal to 1 because its rows and columns contain the same datasets and the correlation of a dataset to itself is equal to 1 by the definition of correlation. From the correlation graphs and the correlation matrix, it is safe to say that production of automobile has no statistically significant correlation with neither production index nor electricity import rate. On the other hand, production index and electricity import rate seems to be  negatively correlated, which will be explained in more detail.

### Correlation between Production Index  and Electricity Import

cor.test(dt$ Electricity_Import_Rate, dt$Production_Index)

#I can say that there is a strong *negative* correlation between the amount of imported electricity  and the oil and natural gas industrial production index in. P- value is significantly small that is 0.03534, so we reJeCt the null hypothesis that is . The correlation coefficient is *-0.3470446 *
#Apart from the correlation value found in the above analysis, this correlation test includes a p value, which is important because it is used while testing hypothesis in confidence intervals. Notice p value has a very small value, meaning that the null hypothesis that production index and electricity import rate are not correlated can be rejected. So, we can say that production index and electricity import rate have a negative relationship. The reason behind this relationship may be that as the production of oil and natural gas increase, the demand of imported electricity decrease.

# 4.Conclusion

#In conclusion, aim of this study was to find whether or not there is a correlation between Production Number Automobile , production index and electricity import in Turkey. To investigate it, monthly data of this measures from 2016 to 2019 are taken and manipulated. Correlation analysis showed that while there was no visible correlation with regards to the number of production automobile and the other two measures, production index and electricity import rate were strongly correlated. The study shows there exists a negative correlation between production oil,natural gas index and electricity import rate. In addition, although there is no direct and strong relationship between google trends and studied data, google trends data helps the visual prediction and some insights about the data.    



#### References
#[EVDS](https://evds2.tcmb.gov.tr/)
#Dorukhan Kılınç Homework 2 and 3.
#Google Trends 

