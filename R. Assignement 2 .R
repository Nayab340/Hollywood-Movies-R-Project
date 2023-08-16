#1 - import dataset 

#2- import library 
library(tidyverse)

#3check data types
str(hollywood_df)

#checking the rows 
dim(hollywood_df)

#check for missing values 
colSums(is.na(hollywood_df))

#dropping the missing values 
hollywood_df <- na.omit(hollywood_df)
dim(hollywood_df)

#checking for duplicates and removing them 
hollywood_df <- hollywood_df[!duplicated(hollywood_df$Film), ]
dim(hollywood_df)

#Rounding off values to 2d.p
hollywood_df$Profitability <- round(hollywood_df$Profitability,digit=2)
hollywood_df$Worldwide.Gross <- round(hollywood_df$Worldwide.Gross,digit=2)

#step 2 

#checking for outliers using a boxplot 
library(ggplot2)

ggplot(hollywood_df,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))

#Remove outliers in "Profitability"

Q1 <- quantile(hollywood_df$Profitability, .25)
Q3 <- quantile(hollywood_df$Profitability, .75)
IQR <- IQR(hollywood_df$Profitability)

no_outliers <- subset(hollywood_df, hollywood_df$Profitability> (Q1 - 1.5*IQR) & hollywood_df$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)


#Remove the outliers in 'Worldwide.Gross'

Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1)

#Step 3

#Do a Summary Statistics/Univariate Analysis using appropriate code
summary(df1)

#bivariate analysis

#Scatter Plot

ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))


#bar chart
ggplot(df1, aes(x=Year)) + geom_bar()

#Additional plots

ggplot(df1, aes(x = Genre, y = Audience..score..)) + geom_bar(stat = "identity", fill = "#CDA7DF") + labs(title = "Genre vs. Audience Score", x = "Genre", y = "Audience Score")

ggplot(df1, aes(x=Profitability, y=Worldwide.Gross)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 400))

ggplot(df1, aes(x=Year, y=Profitability)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 10))

ggplot(df1, aes(x = Year, y = Profitability)) + geom_bar(stat = "identity", fill = "#8BAD82") + scale_y_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 100)) + labs(title = "Profitability Over Years", x = "Year", y = "Profitability")

#piechart --> additional work
counts <- table(df1$Genre)
pie(counts, main="Pie Chart - Genres", labels=names(counts), col = rainbow(length(counts)))

#Histogram --> additionalwork
hist(df1$Audience..score.., main="Histogram", xlab="Audience Score", ylab="Frequency", col="lightblue")

#Step 4 
#Write a line of code to export the clean data
write.csv(df1, "clean_df1.csv")

