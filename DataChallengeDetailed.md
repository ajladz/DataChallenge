
# Data Challenge - Detailed Version


```r
unzip('Data_Challenge.zip', exdir = 'Data_Challenge')

setwd('C:/Users/ajla/Documents/GitHub/Data_Challenge')

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_60")
library(rJava)
library(xlsx)
```




Reading the data into R and looking through the first 10 rows of the data set: 

```r
data <- read.xlsx('Question_2_data.xlsx', sheetIndex = 1, colIndex = c(1:3), header = TRUE)
head(data, 10)
```

```
##                City    Shape Year
## 1  Colorado Springs     Disk 1953
## 2  Colorado Springs Fireball 1954
## 3            Denver    Light 1957
## 4  Colorado Springs Fireball 1957
## 5            Aurora    Light 1962
## 6            Denver     Disk 1962
## 7  Colorado Springs     Disk 1963
## 8      Fort Collins    Cigar 1963
## 9  Colorado Springs Triangle 1967
## 10 Colorado Springs   Sphere 1968
```

## 1. Distribution of sightings per year


```r
library(ggplot2)
ggplot(data, aes(x = Year)) + geom_histogram(binwidth = 1, aes(fill = ..count..)) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Frequency')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Histogram with both density and counts on y-axis:


```r
ggplot(data, aes(x = Year)) + geom_histogram(aes(y = ..density..), binwidth = 1, col = 'red', fill = 'green', alpha = 0.5) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Density') + geom_density()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

## 2.  Correlation between shape and city


```r
levels(data$City)
```

```
##  [1] "Aurora"           "Boulder"          "Broomfield"      
##  [4] "Canon city"       "Canon City"       "Colorado Springs"
##  [7] "Denver"           "Englewood"        "Fort Collins"    
## [10] "Golden"           "Greeley"          "Highlands Ranch" 
## [13] "Littleton"        "LIttleton"        "Longmont"        
## [16] "Parker"           "Pueblo"           "Thornton"
```

```r
levels(data$Shape)
```

```
##  [1] "changed"   "Changing"  "Chevron"   "Cigar"     "Circle"   
##  [6] "Cross"     "Cylinder"  "Diamond"   "Disk"      "Egg"      
## [11] "Eggs"      "Fireball"  "Flash"     "Formation" "Light"    
## [16] "Other"     "Oval"      "Rectangle" "Sphere"    "Teardrop" 
## [21] "triangle"  "Triangle"  "Unknown"
```

As we can see there are number of misspelled names. In the following chunk I'm going to make a few corrections.


```r
data$Shape <- as.character(data$Shape)
data$Shape[grep('changed|changing', data$Shape, ignore.case = TRUE)] <- 'Changing'
data$Shape[grepl('triangle', data$Shape, ignore.case = TRUE)] <- 'Triangle'
data$Shape[grepl('egg|eggs', data$Shape, ignore.case = TRUE)] <- 'Egg'

levels(as.factor(data$Shape))
```


```r
data$City <- gsub('Canon city', 'Canon City', as.character(data$City))
data$City <- gsub('LIttleton', 'Littleton', as.character(data$City))

levels(as.factor(data$City))
```

```
##  [1] "Aurora"           "Boulder"          "Broomfield"      
##  [4] "Canon City"       "Colorado Springs" "Denver"          
##  [7] "Englewood"        "Fort Collins"     "Golden"          
## [10] "Greeley"          "Highlands Ranch"  "Littleton"       
## [13] "Longmont"         "Parker"           "Pueblo"          
## [16] "Thornton"
```


```r
data$City <- as.factor(data$City)
data$Shape <- as.factor(data$Shape)
```


Contingency table/table of counts:

```r
table <- table(data$City, data$Shape)
```

### Chi-Square Test of Independence


```r
chisq.test(table)$expected
```

```
##                   
##                     Changing   Chevron     Cigar     Circle      Cross
##   Aurora           1.5737010 1.0678685 2.3043478  3.4284199 0.05620361
##   Boulder          2.2863203 1.5514316 3.3478261  4.9809120 0.08165429
##   Broomfield       0.4156946 0.2820785 0.6086957  0.9056204 0.01484624
##   Canon City       0.4453871 0.3022269 0.6521739  0.9703075 0.01590668
##   Colorado Springs 6.6511135 4.5132556 9.7391304 14.4899258 0.23753977
##   Denver           6.3541888 4.3117709 9.3043478 13.8430541 0.22693531
##   Englewood        0.7423118 0.5037116 1.0869565  1.6171792 0.02651113
##   Fort Collins     2.0784730 1.4103924 3.0434783  4.5281018 0.07423118
##   Golden           0.5344645 0.3626723 0.7826087  1.1643690 0.01908802
##   Greeley          0.5938494 0.4029692 0.8695652  1.2937434 0.02120891
##   Highlands Ranch  1.0392365 0.7051962 1.5217391  2.2640509 0.03711559
##   Littleton        1.6330859 1.1081654 2.3913043  3.5577943 0.05832450
##   Longmont         1.6924708 1.1484624 2.4782609  3.6871686 0.06044539
##   Parker           0.4453871 0.3022269 0.6521739  0.9703075 0.01590668
##   Pueblo           0.9798515 0.6648993 1.4347826  2.1346766 0.03499470
##   Thornton         0.5344645 0.3626723 0.7826087  1.1643690 0.01908802
##                   
##                     Cylinder   Diamond      Disk       Egg   Fireball
##   Aurora           1.1240721 0.5058324  3.878049 1.0678685  3.2598091
##   Boulder          1.6330859 0.7348887  5.634146 1.5514316  4.7359491
##   Broomfield       0.2969247 0.1336161  1.024390 0.2820785  0.8610817
##   Canon City       0.3181336 0.1431601  1.097561 0.3022269  0.9225875
##   Colorado Springs 4.7507953 2.1378579 16.390244 4.5132556 13.7773065
##   Denver           4.5387063 2.0424178 15.658537 4.3117709 13.1622481
##   Englewood        0.5302227 0.2386002  1.829268 0.5037116  1.5376458
##   Fort Collins     1.4846235 0.6680806  5.121951 1.4103924  4.3054083
##   Golden           0.3817603 0.1717922  1.317073 0.3626723  1.1071050
##   Greeley          0.4241782 0.1908802  1.463415 0.4029692  1.2301166
##   Highlands Ranch  0.7423118 0.3340403  2.560976 0.7051962  2.1527041
##   Littleton        1.1664899 0.5249205  4.024390 1.1081654  3.3828208
##   Longmont         1.2089077 0.5440085  4.170732 1.1484624  3.5058324
##   Parker           0.3181336 0.1431601  1.097561 0.3022269  0.9225875
##   Pueblo           0.6998940 0.3149523  2.414634 0.6648993  2.0296925
##   Thornton         0.3817603 0.1717922  1.317073 0.3626723  1.1071050
##                   
##                        Flash Formation     Light     Other       Oval
##   Aurora           1.0116649 2.1357370  9.442206  4.496288  2.4167550
##   Boulder          1.4697773 3.1028632 13.717922  6.532344  3.5111347
##   Broomfield       0.2672322 0.5641569  2.494168  1.187699  0.6383881
##   Canon City       0.2863203 0.6044539  2.672322  1.272534  0.6839873
##   Colorado Springs 4.2757158 9.0265111 39.906681 19.003181 10.2142100
##   Denver           4.0848356 8.6235419 38.125133 18.154825  9.7582185
##   Englewood        0.4772004 1.0074231  4.453871  2.120891  1.1399788
##   Fort Collins     1.3361612 2.8207847 12.470838  5.938494  3.1919406
##   Golden           0.3435843 0.7253446  3.206787  1.527041  0.8207847
##   Greeley          0.3817603 0.8059385  3.563097  1.696713  0.9119830
##   Highlands Ranch  0.6680806 1.4103924  6.235419  2.969247  1.5959703
##   Littleton        1.0498409 2.2163309  9.798515  4.665960  2.5079533
##   Longmont         1.0880170 2.2969247 10.154825  4.835631  2.5991516
##   Parker           0.2863203 0.6044539  2.672322  1.272534  0.6839873
##   Pueblo           0.6299046 1.3297985  5.879109  2.799576  1.5047720
##   Thornton         0.3435843 0.7253446  3.206787  1.527041  0.8207847
##                   
##                    Rectangle    Sphere  Teardrop  Triangle    Unknown
##   Aurora           0.8430541  3.934252 0.5620361  6.969247  2.9225875
##   Boulder          1.2248144  5.715801 0.8165429 10.125133  4.2460233
##   Broomfield       0.2226935  1.039236 0.1484624  1.840933  0.7720042
##   Canon City       0.2386002  1.113468 0.1590668  1.972428  0.8271474
##   Colorado Springs 3.5630965 16.627784 2.3753977 29.454931 12.3520679
##   Denver           3.4040297 15.885472 2.2693531 28.139979 11.8006363
##   Englewood        0.3976670  1.855779 0.2651113  3.287381  1.3785790
##   Fort Collins     1.1134677  5.196182 0.7423118  9.204666  3.8600212
##   Golden           0.2863203  1.336161 0.1908802  2.366914  0.9925769
##   Greeley          0.3181336  1.484624 0.2120891  2.629905  1.1028632
##   Highlands Ranch  0.5567338  2.598091 0.3711559  4.602333  1.9300106
##   Littleton        0.8748674  4.082715 0.5832450  7.232238  3.0328738
##   Longmont         0.9066808  4.231177 0.6044539  7.495228  3.1431601
##   Parker           0.2386002  1.113468 0.1590668  1.972428  0.8271474
##   Pueblo           0.5249205  2.449629 0.3499470  4.339343  1.8197243
##   Thornton         0.2863203  1.336161 0.1908802  2.366914  0.9925769
```

Many of the expected counts are less than 5 => accuracy of any p-value calculated from the chi-squared
distribution may be doubtful.

```r
chisq.test(table)
```

```
## Warning in chisq.test(table): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table
## X-squared = 758.6177, df = 285, p-value < 2.2e-16
```

Alternative: p-value calculated by Monte Carlo simulation

```r
chisq.test(table, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  table
## X-squared = 758.6177, df = NA, p-value = 0.0004998
```

**Conclusion**

p-value < 0.05  =>  We would reject the null hypothesis that the shape is indepnendent of the city

### Graphical Representation of Data

Since I'm not sure whether chi-square test of independence is applicable in this case, I'll try and show the relationship between shape and city graphically.

```r
ggplot(data, aes(Shape)) + geom_bar(binwidth = 1, aes(fill = Shape)) + facet_wrap(~City, nrow = 4) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, colour ='black')) +
        labs(title = 'Correlation Between Shape and City', y = 'Frequency')
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Percents are more useful than counts for describing how two categorical variables are related and since counts are difficult to interpret, especially with unequal numbers of observations in the rows 
(and columns):

```r
library(scales)
ggplot(data, aes(x= Shape, group = City)) + 
        geom_bar(aes(y = ..density.., fill = factor(..x..))) + 
        facet_wrap(~City, nrow = 4) +
        scale_y_continuous(labels=percent) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, colour ='black')) +
        labs(title = 'Correlation Between Shape and City', y = 'Density')
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

**Conclusion**

As we can see from the graph percentages of observations of certain shapes of objects are much higher in certain towns than in others. If variables shape and city were not correlated we would expect to see approximately uniform distribution of percentages of observed shapes. 




###Conditional Percents as Evidence of a Relationship

From <http://sites.stat.psu.edu/~ajw13/stat200_notes/12_assoc/10_assoc_print.htm> :
Two categorical variables are related in the sample if at least two rows noticeably differ 
in the pattern of row percents.


```r
row_percents <- prop.table(table, 1)
row_percents <- as.data.frame.matrix(row_percents)
sub <- subset(row_percents, select = Changing:Unknown)
row.names(sub) = NULL
```

Rows/Cities with percent of observed shapes 5 times average:

```r
m <- row_percents[apply(sub, MARGIN = 1, function(x) any(x > (rowMeans(row_percents))*5)),] 
apply(m, 1, max)
```

```
##           Aurora       Canon City Colorado Springs        Englewood 
##        0.2641509        0.2666667        0.2589286        0.2800000 
##           Golden          Greeley  Highlands Ranch           Parker 
##        0.2777778        0.2500000        0.4857143        0.2666667 
##           Pueblo         Thornton 
##        0.3333333        0.3333333
```

**Conclusion**

There are 10 rows/cities that noticeably differ in the pattern of row percents.This can also be seen from the graph of correlation between shape and city. 

