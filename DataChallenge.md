
# Data Challenge








## 1. Distribution of sightings per year


```r
library(ggplot2)
ggplot(data, aes(x = Year)) + geom_histogram(binwidth = 1, aes(fill = ..count..)) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Frequency')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


**Histogram with both density and counts on y-axis**:


```r
ggplot(data, aes(x = Year)) + geom_histogram(aes(y = ..density..), binwidth = 1, col = 'red', fill = 'green', alpha = 0.5) + 
        labs(title = 'Distribution of Sightings per Year', y = 'Density') + geom_density()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


## 2.  Correlation between shape and city







### Chi-Square Test of Independence

```r
expected <- chisq.test(table)$expected
```

Many of the expected counts are less than 5 => accuracy of any p-value calculated from the chi-squared distribution may be doubtful.


```r
chisq.test(table)
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

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

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

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

**Conclusion**

As we can see from the graph percentages of observations of certain shapes of objects are much higher in certain towns than in others. If variables shape and city were not correlated we would expect to see approximately uniform distribution of percentages of observed shapes. 


###Conditional Percents as Evidence of a Relationship

Two categorical variables are related in the sample if at least two rows noticeably differ 
in the pattern of row percents.




Rows/Cities with percent of observed shapes 5 times average:

```
##           Aurora       Canon City Colorado Springs        Englewood 
##        0.2641509        0.2666667        0.2589286        0.2800000 
##           Golden          Greeley  Highlands Ranch           Parker 
##        0.2777778        0.2500000        0.4857143        0.2666667 
##           Pueblo         Thornton 
##        0.3333333        0.3333333
```

**Conclusion**
There are 10 rows/cities that noticeably differ in the pattern of row percents.This can also be seen from
the graph of correlation between shape and city. 

