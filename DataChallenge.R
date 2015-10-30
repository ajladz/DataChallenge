

# Data Challenge

unzip('Data_Challenge.zip', exdir = 'Data_Challenge')

setwd('C:/Users/ajla/Documents/GitHub/Data_Challenge')

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_60")
library(rJava)
library(xlsx)
# DataChallenge <- read.xlsx('Question_2_data.xlsx', sheetIndex = 1, header = TRUE)

data <- read.xlsx('Question_2_data.xlsx', sheetIndex = 1, colIndex = c(1:3), header = TRUE)
head(data)


# 1. Distribution of sightings per year

# base plotting package
hist(data$Year, breaks = length(unique(data$Year)), main = 'Distribution of Sightings per Year', xlab = 'Year',col = 'green', las = 1)



library(ggplot2)
ggplot(data, aes(x = Year)) + geom_histogram(binwidth = 1, aes(fill = ..count..)) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Frequency')

ggplot(data, aes(x = Year)) + geom_histogram(aes(y = ..density..), binwidth = 1, col = 'red', fill = 'green', alpha = 0.5) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Density') + geom_density()


# Correlation between shape and city

levels(data$City)
levels(data$Shape)

data$Shape <- as.character(data$Shape)
data$Shape[grep('changed|changing', data$Shape, ignore.case = TRUE)] <- 'Changing'
data$Shape[grepl('triangle', data$Shape, ignore.case = TRUE)] <- 'Triangle'
data$Shape[grepl('egg|eggs', data$Shape, ignore.case = TRUE)] <- 'Egg'
levels(as.factor(data$Shape))
# Since, I'm not sure of 'Other' and 'Unknown' are the same thing here, I'm going to leave them as they are.

data$City <- gsub('Canon city', 'Canon City', as.character(data$City))
data$City <- gsub('LIttleton', 'Littleton', as.character(data$City))

levels(as.factor(data$City))

data$City <- as.factor(data$City)
data$Shape <- as.factor(data$Shape)

# Contingency table/table of counts to show distribution of different shapes in cities
table <- table(data$City, data$Shape)
table


# To get a better overview I'll change the format from long to wide:
library(reshape2)
wide_data <- dcast(data, City ~ Shape, value.var = 'Year')




# After consulting this http://www.ats.ucla.edu/stat/mult_pkg/whatstat/choosestat.html
# To test independence of two categorical variables I'll try chi-sqare test of independence
expected <- chisq.test(table)$expected
expected

# Many of the expected counts are less than 5 => accuracy of any p-value calculated from the chi-squared 
# distribution maybe doubtful

chisq.test(table)

# Alternative: p-value calculated by Monte Carlo simulation
chisq.test(table, simulate.p.value = TRUE)   
# Conclusion: p-value < 0.05 => We would reject the null hypothesis that the shape is indepnendent of the city


#I'll try to show the relationship between shape and city graphically


ggplot(data, aes(City, ..count..)) + geom_bar(aes(fill = Shape)) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black'))

ggplot(data, aes(City, ..count..)) + geom_bar(aes(fill = Shape), position = 'dodge') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) 

ggplot(data, aes(Shape)) + geom_bar(binwidth = 1, aes(fill = Shape)) + facet_wrap(~City, nrow = 4) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, colour ='black')) +
        labs(title = 'Correlation Between Shape and City', y = 'Frequency')


# Since percents are more useful than counts for describing how two categorical variables are related and 
# since counts are difficult to interpret, especially with unequal numbers of observations in the rows (and columns):

ggplot(data, aes(x= Shape, group = City)) + 
        geom_bar(aes(y = ..density.., fill = factor(..x..))) + 
        facet_wrap(~City, nrow = 4) +
        scale_y_continuous(labels=percent) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, colour ='black')) +
        labs(title = 'Correlation Between Shape and City', y = 'Density')

# As we can see from the graph percentages of observations of certain shapes of objects are much higher in
# certain towns than in others. If variables shape and city were not correlated we would expect to see 
# approximately uniform distribution of percentages of observed shapes. 




# Since the chi-square test of independence is not applicable in this case, I'll use conditional percents 
# as evidence of a relationship

# Percent of different shapes of observed UFO's per City
row_percents <- prop.table(table, 1)
row_percents <- as.data.frame.matrix(row_percents) 

# # From http://sites.stat.psu.edu/~ajw13/stat200_notes/12_assoc/10_assoc_print.htm :
# Conditional Percents as evidence of a relationship
# Definition : Two categorical variables are related in the sample if at least two rows noticeably differ 
# in the pattern of row percents


# adding 'average' column with mean percentage of observations of different shapes
row_percents$average <- rowMeans(row_percents)  
row_percents <- as.data.frame(row_percents)


# rows/Citys with percent of observed shapes 5 times average 
sub <- subset(row_percents, select = Changing:Unknown)
row.names(sub) = NULL
m <- row_percents[apply(sub, MARGIN = 1, function(x) any(x > (rowMeans(row_percents))*5)),] 
apply(m, 1, max)
# There are 10 rows that noticeably differ in the pattern of row percents, which can also be seen from the 
# graph


