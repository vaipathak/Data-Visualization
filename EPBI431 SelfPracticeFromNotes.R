#Love EPBI 431 PARTA

#PartA 4 2016 - 
#The National Youth fitness survey data (nyfs1)

#Let's load some libraries and set the wd

library(dplyr); library(tidyr); library(tibble); library(devtools)
library(grid); library(ggplot2); library(gridExtra); library(viridis)

setwd("D:/EPBI 431_432 Notes/Notes/2016/Data2016/PartA")

#First let's take a look at the data
nyfs1 <- read.csv("nyfs1.CSV")

View(nyfs1)
#Turn this data frame into a more useful "tibble":

nyfs1 <- tbl_df(nyfs1)

#Size of the dataframe - 1416 rows (or subjects) and 7 columns (or variables)
dim(nyfs1) 

nyfs1
#Looking at the variable "nyfs1" on its own, we can see a nice tibble and a breakdown of all the columns in terms 
#of whether it is an "int", "factor", or "double"
#The "tibble" function is just a new way of looking at a dataframe. It surplants the older command of "str". ie: 
str(nyfs1)

#Sex is listed as a factor (categorial) with 2 levels - Female and Male.
#If I want to know how many males and females there are, I can build a little table using the dplyr function:

dplyr::select(nyfs1, sex) %>%
  table() 

#We can also add the marginal totals of each to get the sum
dplyr::select(nyfs1, sex) %>%
  table() %>%
  addmargins() ## add marginal totals

#And finally, we can also look at the proportions: 
dplyr::select(nyfs1, sex) %>%
  table() %>%
  prop.table() ## look at the proportions


#Note Age is typically a "continuous" variable, but here it's used as a discrete variable. 
#We can get a table of the number of different ages within the dataset again with the dplyr function!
dplyr::select(nyfs1, age.exam) %>%
  table() %>%
  addmargins() ## Breaks down number of kids at all the different ages

##Note for columns identified as "num" - they usually have decimals and make poor tables, instead use the 
#"summary" command: 
select(nyfs1, bmi) %>%
  summary()

#Let's convert the waist.circ (waist circumfrance) from centimeters to inches and add a column 
nyfs2 <- nyfs1
nyfs2$waist.in <- nyfs2$waist.circ * 0.394

##That's a lot of decimals! Let's round it down a bit to 1 decimal place
nyfs2$waist.in <- round(nyfs2$waist.in, 1)

##Great now we have the waist in inches as well. Let's re-order the dataframe so that waist in inches comes right after
#waist in cm. Note the "," in the beginning -> This keeps all the rows in the same order. 
nyfs2 <- nyfs2[, c(1,2,3,4,5,6,8,7)]

##Do the same thing for tricep.skinfold - no need to rearrange the df since it shows up right after tricep.skinfold!

nyfs2$triceps.in <- nyfs2$triceps.skinfold * 0.394
nyfs2$triceps.in <- round(nyfs2$triceps.in, 1)

##Histograms and Variants

##Let's make a basic histogram using ggplot2 
ggplot(data = nyfs1, aes(x = bmi)) + geom_histogram()
## What does this histogram tell us? Well it's very basic and shows 30 different breaks or 'bins' as standard 
## (hence why there's a note that says "pick a better value binwidth) -> see notes 5.1.1 on the 
## "Freedman-Diaconis" rule on how to pick binwidth! bw = the Freedman-Diaconis equation:

bw <- 2 * IQR(nyfs1$bmi) / length(nyfs1$bmi) ^ (1/3)  #Again this is the Freedman-Diaconis binwith equation.
ggplot(data = nyfs1, aes(x = bmi)) +
  geom_histogram(binwidth=bw, color = "red", fill = "blue")

## Much better graph, but not completely finished. So lets add labels and such. We can use use the Freedman-Diaconis
## Equation or just give an arbitrary number of bins

ggplot(data = nyfs1, aes(x=bmi)) +
  geom_histogram(bins=25, color = "black", fill = "dodgerblue") +
  labs(title = "Histogram of Body Mass Index Results in the nyfs1 data", 
       x = "Body Mass Index", y = "# of patients")
# And now we have a more polished looking graph!

##Side Note: aes = Aesthetics mapping -> Aesthetic mappings describe how variables in the data are mapped to visual 
#properties (aesthetics) of geoms. Aesthetic mappings can be set in ggplot2 and in individual layers.

##Side note - the color package "viridis" has a decent color pallet and the package describes four color scales
#(viridis, magma, plasma and inferno) that are designed to be colorful, robust to colorblindness and gray scale
#printing, and perceptually uniform, which means (as the package authors describe it) that values close to
#each other have similar-appearing colors and values far away from each other have more different-appearing
#colors, consistently across the range of values.

## 5.3 Stem and Leaf Graph

# A stem and leaf display shows the actual data value while retaining the shape of a histogram. It is generally 
#used for small sample sizes (around 10 - 200 observations)

#Therefore, let's take a small sample of the nyfs1 data 

set.seed(431) # set a seed for the random sampling so we can replicate the results. Set.seed is a random number generator
sampleA <- sample_n(nyfs1, 150, replace=FALSE) # draws a sample of 150 unique rows from nyfs1
stem(sampleA$bmi) # builds a stem-and-leaf for those 150 sampled BMI values

#We can use the stem.leaf function in the aplpack package to obtain a fancier version of the stem-and-leaf
#plot, that identifies outlying values. Below, we display this new version for the random sample of 150 BMI
#observations we developed earlier.
aplpack::stem.leaf(sampleA$bmi)

#We can also produce back-to-back stem and leaf plots to compare, for instance, body-mass index by sex.
#***The filter function searches "sampleA" and picks out the selected query***
#*** Another important note: the "stem.leaf.backback" function in the library "aplpack" allows for a COMPARATIVE
#"back to back" stem and leaf graph.
samp.F <- filter(sampleA, sex=="Female") 
samp.M <- filter(sampleA, sex=="Male")
aplpack::stem.leaf.backback(samp.F$bmi, samp.M$bmi)

##5.4 The Dot Plot to Display a Distribution

# We can plot the distribution of a single continuous variable using the dotplot geom:
ggplot(data = nyfs1, aes(x=bmi)) +
  geom_dotplot(dotsize = 0.05, binwidth=1) +
  scale_y_continuous(NULL, breaks=NULL) + #Hides the y axis since it's meaningless in this case
  labs(title = "Dotplot of nyfs1 Body-Mass Index Data",
       x = "Body Mass Index")

# 5.5 The Frequency Polygon

# We can plot the distribution of a single continuous variable using the freqpoly geom:
ggplot(data = nyfs1, aes(x=bmi)) +
  geom_freqpoly(binwidth = 1, color = "dodgerblue") + #Note binwidth of 1 gives a better visual understanding
  labs(title = "Frequency Polygon of nyfs1 Body Mass Index Data",
      x = "Body Mass Index", y = "# of patients")

#5.6 Plotting the Probability Density Function

# This just smooths out the bumps in a histogram or in the "frequency polygon" above: 
ggplot(data = nyfs1, aes(x=bmi)) +
  geom_density(kernel="gaussian", color = "dodgerblue") +
  labs(title="Density of nyfs1 Body Mass Index data", 
       x="Body Mass Index", y="Probability Density Function")

# 5. Comparing a Histogram to a Normal Distribution 

#Normal distribution is a function that represents the distribution of many random variables as a symmetrical 
#bell-shaped graph. This helps to better understand the distribution of data and we can superimpose it on a histogram.

ggplot(nyfs1, aes(x=bmi)) +
  geom_histogram(aes(y=..density..), bins=25, fill="papayawhip", color="seagreen") +
  stat_function(fun=dnorm,
                args = list(mean=mean(nyfs1$bmi), sd=sd(nyfs1$bmi)),
                lwd=1.5, col="blue") +
  geom_text(aes(label=paste("Mean", round(mean(nyfs1$bmi),1),
                            ", SD", round(sd(nyfs1$bmi),1))),
            x = 35, y = 0.02, color="blue", fontface="italic") +
  labs(title = "nyfs1 BMI values with Normal Distribution Superimposed",
       x = "Body Mass Index", y = "Probability Density Function")

##Notes for stat_function and it's components:
#stat_function (from R help) -> Function of ggplot. Makes it easy to superimpose a function on top of an existing plot.
#dnorm is the "probability density function" -> a function of a continuous random variable, whose integral across an 
#interval gives the probability that the value of the variable lies within the same interval 
#"args" is a list of additional arguments to pass to "fun" (dnorm)

##Some Questions to consider: 
#Does it seem as though the Normal model (as shown in the blue density curve) is an effective approximation
#to the observed distribution shown in the bars of the histogram?
#We'll return shortly to the questions:
#  . Does a Normal distribution model fit our data well? and
#  . If the data aren't Normal, but we want to use a Normal model anyway, what should we do?

#But first, we'll demonstrate an approach to building a histogram of counts (rather than a probability density)
#and then superimposing a Normal model.
## ggplot of counts of bmi with Normal model superimposed
## Source: https://stat.ethz.ch/pipermail/r-help//2009-September/403220.html
ggplot(nyfs1, aes(x = bmi)) +
  geom_histogram(bins = 30, fill = "papayawhip", color = "black") +
  stat_function(fun = function(x, mean, sd, n)
    n * dnorm(x = x, mean = mean, sd = sd),
    args = with(nyfs1,
                c(mean = mean(bmi), sd = sd(bmi), n = length(bmi))),
    col = "blue", lwd = 1.5) +
  geom_text(aes(label = paste("Mean", round(mean(nyfs1$bmi),1),
                              ", SD", round(sd(nyfs1$bmi),1))),
            x = 30, y = 50, color="blue", fontface = "italic") +
  labs(title = "Histogram of BMI, with Normal Model",
       x = "Body-Mass Index", y = "Counts of BMI values")

# Chapter 6 Boxplots for Summarizing Distributions

#Boxplots can also help to get a sense of the generalized distribution of the data

boxplot(nyfs1$bmi, col="yellow", horizontal=T, xab="Body-Mass Index",
        main = "BMI for 1416 kids in the NYFS")
#horizontal=T makes the plot horizontal instead of hte default vertical. 
#Boxplot edges (of yellow box) are drawn so that edges are representative of the 25th percentile and 75th percentile.
#The thick black line is the median (50th percentile)
#Dots at the end are outliers -> outliers are defined as any point above the upper fence or below the lower fence. 
#The definitions of the fences are based on theinter-quartile range (IQR).
#If IQR = 75th percentile - 25th percentile, then the upper fence is 75th percentile + 1.5 IQR, and the lower
#fence is 25th percentile - 1.5 IQR.
#So for these BMI data,
summary(nyfs1$bmi)
IQR(nyfs1$bmi) #IQR = 5.1
#. the upper fence is located at 20.9 + 1.5(5.1) = 28.55
#. the lower fence is located at 15.8 - 1.5(5.1) = 8.15

#6.1 Boxplot for One variable
### Note: Using ggplot to run a single distribution boxplot is tricky since needs to be given some axses.
ggplot(nyfs1, aes(x=1, y=bmi)) +
  geom_boxplot(fill="yellow") +
  coord_flip() + #makes it horizontal
  labs(title = "Boxplot of BMI for 1416 kids in the NYFS",
       y="Body Mass Index",
       x="")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#coord_flip (from RHelp)-> Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal. 
#This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y.
#theme (from RHelp) -> Use theme() to modify individual components of a theme, allowing you to control the appearance
#of all non-data components of the plot. theme() only affects a single plot.
#axis.text.y -> y axis tick labels (element_text; inherits from axis.text)
#axis.ticks.y -> y axis tick marks (element_line; inherits from axis.ticks)

##6.2 Boxplots are MOST often used for Comparisons

#Here we can copare the bmi with respect to the child's sex:
ggplot(nyfs1, aes(x=factor(sex), y=bmi, fill=factor(sex))) +   
  geom_boxplot()

#Note here that this shows males and females separated on the x axis and the bmi ranges on the y axis.

#Now we can use boxplots to look at the comparison of BMI levels accross the four ORDINAL categories in the 
#bmi.cat variable

ggplot(nyfs1, aes(x=factor(bmi.cat), y=bmi, fill = factor(bmi.cat))) +
  geom_boxplot() +
  scale_fill_viridis(discrete=TRUE) + #Uses the "viridis palette" to identify color choices
  labs(title="Observed BMI by BMI Percentile Category in nyfs1 data",
       x="BMI Categories", y="BMI")

#This boxplot gives a great graphical interpretation of how the children's BMI is split with regard to their categories.
#Note that the BMI categories incorporate additional information (in particular the age and sex of the child)
#beyond the observed BMI, and so the observed BMI levels overlap quite a bit across the four categories.
#Let's improve it further

#Let's turn the boxes in the horizontal direction, and get rid of the perhaps unnecessary bmi.cat labels.

ggplot(nyfs1, aes(x=factor(bmi.cat), y=bmi, fill=factor(bmi.cat))) +
  geom_boxplot() +
  scale_fill_viridis(discrete=T) +
  coord_flip() +
  guides(fill=FALSE) +
  labs(title="Observed BMI by BMI Percentile Category in nyfs1 data", x="")
#Note: so technically the x axis is actually the y axis and vice versa, the coord_flip function just flips the plot
#horizontally. 

#Chapter 7 - Normal Distribution: 
#Normal Distribution -> a function that represents the distribution of many random variables as a symmetrical 
#bell-shaped graph.

###### Why is Normal Distribution Important? The normal distribution is also important not necessarily due a random
#variable following a normal distribution, but also because the Central Limit Theorem tells us that that sampling 
#distribution of other non-normal distributions approaches a normal distribution as the sample size increase. 
#That is a very powerful statement and allows us to perform Hypothesis Testing testing on all sort of data.
#A normal distribution of data is important in identfying large changes and calculating Z-scores!



###Tools for assessing Normality: 
#We have several tools for assessing Normality of a single batch of data, including:
#. a histogram with superimposed Normal distribution
#. histogram variants (like the boxplot) which provide information on the center, spread and shape of a
#distribution
#. the Empirical Rule for interpretation of a standard deviation
#. a specialized normal Q-Q plot (also called a normal probability plot or normal quantile-quantile plot)
#designed to reveal differences between a sample distribution and what we might expect from a normal
#distribution of a similar number of values with the same mean and standard deviation

# For a set of measurements that follows a Normal distribution, the interval:
#. Mean ± Standard Deviation contains approximately 68% of the measurements;
#. Mean ± 2(Standard Deviation) contains approximately 95% of the measurements;
#. Mean ± 3(Standard Deviation) contains approximately all (99.7%) of the measurements.
#Again, most data sets do not follow a Normal distribution. We will occasionally think about transforming or
#re-expressing our data to obtain results which are better approximated by a Normal distribution, in part so
#that a standard deviation can be more meaningful.
#For the BMI data we have been studying, here again are some summary statistics. . .
mosaic::favstats(nyfs1$bmi)
#min  Q1   median Q3 max   mean     sd      n  missing
#11.9 15.8 17.7 20.9 38.8 18.79866 4.08095 1416 0
#The mean is 18.8 and the standard deviation is 4.08, so if the data really were Normally distributed, we'd
#expect to see:
#  . About 68% of the data in the range (14.72, 22.88). In fact, 1074 of the 1416 BMI values are in this
#range, or 75.8%.
#. About 95% of the data in the range (10.64, 26.96). In fact, 1344 of the 1416 BMI values are in this
#range, or 94.9%.
#. About 99.7% of the data in the range (6.56, 31.04). In fact, 1393 of the 1416 BMI values are in this
#range, or 98.4%.
#So, based on this Empirical Rule approximation, do the BMI data seem to be well approximated by a Normal
#distribution? My ans:Looking at the skew of the BMI histogram, no.

####We can also use Z-scores (which are the number of standard deviations away from the median) to observe 
#outlying values.
#For EXAMPLE: Z-Score = (Score - mean) / SD
#So for BMI, the Z-score for the MAX (38.8) would be: (38.8-18.7) / 4.08 = 4.9
#Recall that the Empirical Rule suggests that if a variable follows a Normal distribution, it would have
#approximately 95% of its observations falling inside a Z score of (-2, 2), and 99.74% falling inside a Z score
#range of (-3, 3).

#So let's see if the "age.exam" category works
ggplot(nyfs1, aes(x=age.exam)) +
  geom_histogram(aes(y = ..density..), binwidth=1, fill = "papayawhip", color = "seagreen") +
  stat_function(fun = dnorm,
                args = list(mean = mean(nyfs1$age.exam), sd = sd(nyfs1$age.exam)),
                lwd = 1.5, col = "blue") + #Note - lwd is "line width"
  geom_text(aes(label = paste("Mean", round(mean(nyfs1$age.exam),1),
                              ", SD", round(sd(nyfs1$age.exam),1))),
            x = 13, y = 0.1, color="blue", fontface = "italic") +
  labs(title = "nyfs1 Age values with Normal Distribution Superimposed",
       x = "Age at Exam (years)", y = "Probability Density Function")

#mosaic::favstats(nyfs1$age.exam)
#min Q1 median Q3 max   mean     sd     n   missing
# 3  6     9   12 16 8.855226 3.680271 1416  0
#The mean is 8.86 and the standard deviation is 3.68 so if the age.exam data really were Normally distributed,
#we'd expect to see:
#  . About 68% of the data in the range (5.17, 12.54). In fact, 781 of the 1416 Age values are in this range,
#or 55.2%.
#. About 95% of the data in the range (1.49, 16.22). In fact, 1416 of the 1416 Age values are in this range,
#or 100%.
#. About 99.7% of the data in the range (-2.19, 19.9). In fact, 1416 of the 1416 Age values are in this
#range, or 100%.
#How does the Normal approximation work for age, according to the Empirical Rule?
#Again not a great distribution, but close. 

#7.5 Normal Q-Q plot

#The idea of a Normal probability plot (Normal quantile-quantile plot - Q-Q) is that it plots the observed sample 
#values (on the vertical axis) and then, on the horizontal, the expected or theoretical quantiles that would be 
#observed in a standard normal distribution (a Normal distribution with mean 0 and standard deviation 1) 
#with the same number of observations. 

#Essentially a Q-Q plot can tell if data is normalized or not - if there is a straight line, the data is normalized. 
#If the plot is curved in either direction, then the data is skewed (right or left skew depending on how it looks 
#compared to the reference line. Behavior in the "tails" can tell things about the outliers 
#(which could be heavy-tailed [more outliers than expected] or light-tailed)


ggplot(nyfs1, aes(sample = bmi)) +
  geom_point(stat="qq") +
  theme_bw() # eliminate the gray background

#We can also add a reference line (indicated in red)
qqnorm(nyfs1$bmi)
qqline(nyfs1$bmi, col="red", lwd=2)

#A fancy Q-Q plot can also be installed as a function. You have to run the function first before being able to utilize
#it in your script: 

##Function for gg_qq: 
gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  if(!is.null(labels)){
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() +
    geom_abline(intercept = coef[1], slope = coef[2], color="red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    labs(y = "Ordered Sample Data", x = "Standard Normal Quantiles")
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  # coef ## can print 25th and 75h percentiles if desired by removing the single # in this line
}


#Confidence Interval of 95 example: For example, a 95% confidence interval covers 95% of the normal curve -- 
#the probability of observing a value outside of this area is less than 0.05. Because the normal curve is symmetric, 
#half of the area is in the left tail of the curve, and the other half of the area is in the right tail of the curve.

#NOW we can run the following QQ plot: 

gg_qq(nyfs1$bmi)

#The following is an example of a proper normal distribution for a random sample of 200 observations simulated.
set.seed(123431) # so the results can be replicated
# simulate 200 observations from a Normal(20, 5) distribution and place them
# in the d variable within the temp.1 data frame
temp.1 <- data.frame(d = rnorm(200, mean = 20, sd = 5))
# left plot - basic Normal Q-Q plot of simulated data
p1 <- ggplot(temp.1, aes(sample = d)) +
  geom_point(stat="qq") +
  labs(y = "Ordered Simulated Sample Data")
# right plot - histogram with superimposed normal distribution
p2 <- ggplot(temp.1, aes(x = d)) +
  geom_histogram(aes(y = ..density..), bins=25, fill = "papayawhip", color = "seagreen") +
  stat_function(fun = dnorm, args = list(mean = mean(temp.1$d), sd = sd(temp.1$d)),
                lwd = 1.5, col = "blue") +
  labs(x = "Simulated Sample Data")
grid.arrange(p1, p2, ncol=2, top ="200 observations from a simulated Normal distribution")

#### grid.arrange allows for multiple graphs NOTE that you have to assign the ggplots to a value first (p1, p2)

#7.6.2 Skew is indicated by monotonic curves in the Normal Q-Q plot

#Data that come from a skewed distribution appear to curve away from a straight line in the Q-Q plot.
set.seed(123431) # so the results can be replicated
# simulate 200 observations from a beta(5, 2) distribution into the e1 variable
# simulate 200 observations from a beta(1, 5) distribution into the e2 variable
temp.2 <- data.frame(e1 = rbeta(200, 5, 2), e2 = rbeta(200, 1, 5))
p1 <- ggplot(temp.2, aes(sample = e1)) +
  geom_point(stat="qq", color = "orchid") +
  labs(y = "Ordered Sample e1", title = "Beta(5,2) sample")
p2 <- ggplot(temp.2, aes(sample = e2)) +
  geom_point(stat="qq", color = "orange") +
  labs(y = "Ordered Sample e2", title = "Beta(1,5) sample")
grid.arrange(p1, p2, ncol=2, top ="200 observations from simulated Beta distributions")

#Note the bends away from a straight line in each sample. The non-Normality may be easier to see in a
#histogram.

p1 <- ggplot(temp.2, aes(x = e1)) +
  geom_histogram(aes(y = ..density..), binwidth=0.1, fill="orchid1", color="white") +
  stat_function(fun =dnorm, args = list(mean(temp.2$e1), sd=sd(temp.2$e1)),
                col="blue") +
  labs(x = "Sample e1", title = "Beta (5,2) sample: Left Skew")
p2 <- ggplot(temp.2, aes(x = e2)) +
  geom_histogram(aes(y = ..density..), binwidth=0.1, fill = "orange1", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(temp.2$e2), sd = sd(temp.2$e2)),
                col = "blue") +
  labs(x = "Sample e2", title = "Beta(1,5) sample: Right Skew")

grid.arrange(p1, p2, ncol=2,
             bottom ="Histograms with Normal curve superimposed")

#In each of these pairs of plots, we see the same basic result.
#. The left plot (for data e1) shows left skew, with a longer tail on the left hand side and more clustered
#data at the right end of the distribution.
#. The right plot (for data e2) shows right skew, with a longer tail on the right hand side, the mean larger
#than the median, and more clustered data at the left end of the distribution.

# You may want to see the lines to help you see what's happening in the Q-Q plots. You can do this with our
#fancy approach, or with the qqnorm-qqline combination from base R.
par(mfrow=c(1,3)) #Splits to allow 3 graphs in one window
qqnorm(temp.2$e1, col="orchid", main = "Beta(5,2): Left Skew",
       ylab="Sample Quantiles for e1")
qqline(temp.2$e1)
boxplot(temp.2$e1, temp.2$e2, names= c("Beta(5,2)","Beta(1,5)"),
        col=c("orchid","orange"), main="Boxplots for e1 and e2")
qqnorm(temp.2$e2, col = "orange", main = "Beta (1,5): Right Skew",
       ylab="Sample Quantiles for e2")
qqline(temp.2$e2)
par(mfrow=c(1,1)) #Returns to normal 1 window graph


##7.7 Returning to the BMI data.
#Skewness is indicated by curves in the Normal Q-Q plot. Compare these two plots - the left is the
#original BMI data from the NYFS data frame, and the right plot shows the inverse of those values.

par(mfrow=c(1,2)) ## set up plot window for one row, two columns
qqnorm(nyfs1$bmi, main="Body-Mass Index (Right or Positive skew)", col="coral") #Shows a RIGHT or POSITIVE skew
qqline(nyfs1$bmi)
qqnorm(1/(nyfs1$bmi), main="1/BMI", col="darkcyan") #Inverse of the above BMI plot, more normal distribution
qqline(1/nyfs1$bmi)
par(mfrow=c(1,1))
#. The left plot shows fairly substantial right or positive skew
#. The right plot shows there's much less skew after the inverse has been taken.
#. Our conclusion is that a Normal model is a far better fit to 1/BMI than it is to BMI.

#The effect of taking the inverse here may be clearer from the histograms below, with Normal density functions
#superimposed.
p1 <- ggplot(nyfs1, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), binwidth=2, fill = "coral", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(nyfs1$bmi), sd = sd(nyfs1$bmi))) +
  labs(x = "Body-Mass Index", title = "raw (untransformed) BMI")
p2 <- ggplot(nyfs1, aes(x = 1/bmi)) +
  geom_histogram(aes(y = ..density..), binwidth=0.005, fill = "darkcyan", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(1/nyfs1$bmi), sd = sd(1/nyfs1$bmi))) +
  labs(x = "1 / Body-Mass Index", title = "Inverse of BMI (1/BMI)")
grid.arrange(p1, p2, ncol=2,
             top = textGrob("Comparing BMI to 1/BMI", gp=gpar(fontsize=15)))

# this approach to top label lets us adjust the size of type used in the main title
# note that you'll need to have called library(grid) or require(grid) for this to work properly
#grid.arrange -> Set up a gtable layout to place multiple grobs (Creating grid graphical objects, short ("grob"s)) 
#on a page. 

##Chapter 8 - Using Transformations to "Normalize" Distributions

#. When we are confronted with a variable that is not Normally distributed but that we wish was Normally
#distributed, it is sometimes useful to consider whether working with a transformation of the data will
#yield a more helpful result.
#####. Many statistical methods, including t tests and analyses of variance, assume Normal distributions.*****
#. We'll discuss using R to assess a range of what are called Box-Cox power transformations, via plots,
#mainly.
#Side note - The Variance is the average of the squared differences from the mean and the std dev is the square root
#of the variance.

## 8.1 The Ladder of Power Transformations

# There are times that data needs to be transformed in order to obtain a normal distribution. One of the possible ways
#to do this is to follow a transformation ladder
#As we move further away from the identity function (power = 1) we change the shape more and more in the
#same general direction.
#. For instance, if we try a logarithm, and this seems like too much of a change, we might try a square
#root instead.
#. Note that this ladder (which like many other things is due to John Tukey) uses the logarithm for the
#"power zero" transformation rather than the constant, which is what x0 actually is.
#. If the variable x can take on negative values, we might take a different approach. If x is a count of
#something that could be zero, we often simply add 1 to x before transformation.

## 8.3 Transformation Example: 

#Sample 1 Untransformed, fitted with normal density
set.seed(431432); data1 <- data.frame(sample1 = rchisq(n=100,df=2)) #random number generator
ggplot(data1, aes(x=sample1)) +
  geom_histogram(aes(y=..density..), bins=30, fill="dodgerblue", col="white") +
  stat_function(fun=dnorm, lwd=1.5, col="purple",  #fun=dnrom -> lot a normal curve
                args=list(mean=mean(data1$sample1),sd=sd(data1$sample1))) + 
  annotate("text", x=4, y=0.3, col="purple",
           label=paste("Mean = ", round(mean(data1$sample1),2),
                       ", SD = ", round(sd(data1$sample1),2))) +
  labs(title= "Sample 1, Untransformed, with fitted Normal density")

# Does squaring sample 1 help to normalize it?
ggplot(data1, aes(x=sample1^2)) +
  geom_histogram(aes(y=..density..), bins=30, fill="dodgerblue", col="white") +
  stat_function(fun=dnorm, lwd = 1.5, col="purple", 
                args=list(mean=mean(data1$sample1^2), sd=sd(data1$sample1^2))) +
  annotate("text", x=10, y=0.2, col="purple",
           label=paste("Mean = ", round(mean(data1$sample1^2),2),
                       ", SD = ", round(sd(data1$sample1^2),2))) +
  labs(title="Sample 1, squared with fitted Normal Density")

#Nope, still skewed

#Does taking the Log help normalize the data?

ggplot(data1, aes(x = log(sample1))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "dodgerblue", col="white") +
  stat_function(fun = dnorm, lwd = 1.5, col = "purple",
                args = list(mean = mean(log(data1$sample1)), sd = sd(log(data1$sample1)))) +
  annotate("text", x = -2, y = 0.3, col = "purple",
           label = paste("Mean = ", round(mean(log(data1$sample1)),2),
                         ", SD = ", round(sd(log(data1$sample1)),2))) +
  labs(title = "Logarithm of Sample 1, with fitted Normal density")
#Nope still skewed

#Does taking the square root of the sample help normalize the data?

ggplot(data1, aes(x=sqrt(sample1))) +
  geom_histogram(aes(y=..density..), bins=30, fill="dodgerblue", col="white") +
  stat_function(fun=dnorm, lwd=1.5, col="purple",
                args=list(mean = mean(sqrt(data1$sample1)), sd = sd(sqrt(data1$sample1)))) +
  annotate("text", x = 0.45, y = 0.7, col = "purple",
           label = paste("Mean = ", round(mean(sqrt(data1$sample1)),2),
                         ", SD = ", round(sd(sqrt(data1$sample1)),2))) +
  labs(title = "Square Root of Sample 1, with fitted Normal density")

#YES this one works! 

#annotate function (R help) -> This function adds geoms to a plot, but unlike a typical geom function, the properties
#of the geoms are not mapped from variables of a data frame, but are instead passed in as vectors. This is useful for
#adding small annotations (such as text labels) or if you have your data in vectors, and for some reason don't want 
#to put them in a data frame.
#In this case, "annotate" is annotating the Mean and SD with respect to the normal density stat function

# 8.4 Performing a quick comparison of the four data transformations simultaneously:  
#Histogram comparisons:
p1 <- ggplot(data1, aes(x = sample1)) + geom_histogram(bins = 30, fill = "dodgerblue") #Unchanged Sample1
p2 <- ggplot(data1, aes(x = sample1^2)) + geom_histogram(bins = 30, fill = "magenta") #Squared Sapmple1
p3 <- ggplot(data1, aes(x = sqrt(sample1))) + geom_histogram(bins = 30, fill = "seagreen") #Square root of Sample1
p4 <- ggplot(data1, aes(x = log(sample1))) + geom_histogram(bins = 30, fill = "tomato") #log of Sample1
grid.arrange(p1,p2,p3,p4, nrow=2, top="Comparison of Transformations with Histograms") #nrow = #of plots per row

#QQplot comparisons:
p1 <- ggplot(data1, aes(sample = sample1)) + geom_point(stat="qq", col = "dodgerblue") #Unchanged sample1
p2 <- ggplot(data1, aes(sample = sample1^2)) + geom_point(stat="qq", col = "magenta") #Squared Sample1
p3 <- ggplot(data1, aes(sample = sqrt(sample1))) + geom_point(stat="qq", col = "seagreen") #Square Root of sample1
p4 <- ggplot(data1, aes(sample = log(sample1))) + geom_point(stat="qq", col = "tomato") #log of Sample1
grid.arrange(p1, p2, p3, p4, nrow=2, top="Comparison of Transformations: Normal Q-Q Plots") #nrow = #of plots per row
rm(p1, p2, p3, p4)


##*** 8.5 Multiple ways of running through the "Transformation Ladder": 
#Frequency Polygon plots:
p1 <- ggplot(data1, aes(x = sample1^3)) + geom_freqpoly(col = 1) #Cubed sample1
p2 <- ggplot(data1, aes(x = sample1^2)) + geom_freqpoly(col = 5) #Squared Sample1
p3 <- ggplot(data1, aes(x = sample1)) + geom_freqpoly(col = "black") #Non-transformed sample1
p4 <- ggplot(data1, aes(x = sqrt(sample1))) + geom_freqpoly(col = 5) #Square Root of sample1
p5 <- ggplot(data1, aes(x = log(sample1))) + geom_freqpoly(col = 6) #log of sample1
p6 <- ggplot(data1, aes(x = 1/sqrt(sample1))) + geom_freqpoly(col = 1) #1 over the square root of sample1
p7 <- ggplot(data1, aes(x = 1/sample1)) + geom_freqpoly(col = 6) #inverse of sample1
p8 <- ggplot(data1, aes(x = 1/(sample1^2))) + geom_freqpoly(col = 1) #1 over sample1 squared
p9 <- ggplot(data1, aes(x = 1/(sample1^3))) + geom_freqpoly(col = 5) #1 over sample1 cubed
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3,
             top="Ladder of Power Transformations")

#Normal QQplots:

p1 <- ggplot(data1, aes(sample = sample1^3)) +
  geom_point(stat="qq", col = 1) + labs(title = "x^3")  #cubed sample1
p2 <- ggplot(data1, aes(sample = sample1^2)) +
  geom_point(stat="qq", col = 5) + labs(title = "x^2")  #squared sample1
p3 <- ggplot(data1, aes(sample = sample1)) +
  geom_point(stat="qq", col = "black") + labs(title = "x") #unchanged sample1
p4 <- ggplot(data1, aes(sample = sqrt(sample1))) +
  geom_point(stat="qq", col = 5) + labs(title = "sqrt(x)") #sqrt sample 1
p5 <- ggplot(data1, aes(sample = log(sample1))) +
  geom_point(stat="qq", col = 6) + labs(title = "log x") #log of sample1
p6 <- ggplot(data1, aes(sample = 1/sqrt(sample1))) +
  geom_point(stat="qq", col = 1) + labs(title = "1/sqrt(x)") #1 over sqrt sample1
p7 <- ggplot(data1, aes(sample = 1/sample1)) +
  geom_point(stat="qq", col = 6) + labs(title = "1/x") #1 over sample1
p8 <- ggplot(data1, aes(sample = 1/(sample1^2))) +
  geom_point(stat="qq", col = 1) + labs(title = "1/(x^2)") #1 over sample1 squared
p9 <- ggplot(data1, aes(sample = 1/(sample1^3))) +
  geom_point(stat="qq", col = 5) + labs(title = "1/(x^3)") #1 over sample1 cubed
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3,
             bottom="Ladder of Power Transformations")


###Chapter 9 - Assessing skew

#The "describe" function in the "Psych" library can provide additional numerical information on data. ie:
psych::describe(nyfs1$bmi)

#vars       n mean   sd  median trimmed  mad  min  max range skew kurtosis   se
#X1    1 1416 18.8 4.08   17.7   18.24  3.26 11.9 38.8  26.9 1.35     1.97 0.11


#This package provides, in order, the following. . .
#. n = the sample size
#. mean = the sample mean
#. sd = the sample standard deviation
#. median = the median, or 50th percentile
#. trimmed = mean of the middle 80% of the data -> This is a 20% trimmed mean (bottom 10% and top 10% of BMIs are
#            removed). mean(nyfs1$bmi, trim=.1)
#. mad = median absolute deviation -> fancier alternative to the IQR (and a bit more robust) which, in large sample
#        sample sizes, for data that follow a Normal Distribution, will be (in expectation) equal to the standard dev.
#        Essentially, the MAD is the median of the absolute deviations from the middle, multiplied by a constant
#        (1.4826) to yield asymptotically normal consistency.
#. min = minimum value in the sample
#. max = maximum value in the sample
#. range = max - min
#. skew = skewness measure, described below (indicates degree of asymmetry)
#. kurtosis = kurtosis measure -> an indicator of whether the distribution is heavy-tailed or light-tailed as compared
#             to a Normal distribution
#. se = standard error of the sample mean = stdev / square root of sample size, useful in inference
#       [in this case: sd(nyfs1$bmi)/sqrt(length(nyfs1$bmi))]

#Note on Kurtosis - 
#Another measure of a distribution's shape that can be found in the psych library is the kurtosis. Kurtosis is
#an indicator of whether the distribution is heavy-tailed or light-tailed as compared to a Normal distribution.
#Positive kurtosis means more of the variance is due to outliers - unusual points far away from the mean
#relative to what we might expect from a Normally distributed data set with the same standard deviation.
#. A Normal distribution will have a kurtosis value near 0, a distribution with similar tail behavior to
#what we would expect from a Normal is said to be mesokurtic
#. Higher kurtosis values (meaningfully higher than 0) indicate that, as compared to a Normal distribution,
#the observed variance is more the result of extreme outliers (i.e. heavy tails) as opposed to being the
#result of more modest sized deviations from the mean. These heavy-tailed, or outlier prone, distributions
#are sometimes called leptokurtic.
#. Kurtosis values meaningfully lower than 0 indicate light-tailed data, with fewer outliers than we'd
#expect in a Normal distribution. Such distributions are sometimes referred to as platykurtic, and include
#distributions without outliers, like the Uniform distribution.

# 9.4 The "Describe" function in Hmisc
# The describe function in Hmisc knows enough to separate numerical from categorical variables, and give you separate
#(and detailed) summaries for each. -> For categorical variables, it counts total # of observations (n), number of 
#missing values, and number of unique categories (along with counts and percentages falling in each category)
# -> For numerical variables, counts obs, missing values, unique values, info value for data (indicates how continuous
# a variable is ie: a score of 1 is a completely continuous variable while score near 0 indicate lots of "ties" and 
#very few unique values), sample mean, and sample percentiles (quantiles) of the data.
Hmisc::describe(nyfs1)

#9.5 xda from GitHub for numerical summaries for exploratory data analysis (downloaded from github)
#library(devtools)
#install_github("ujjwalkarn/xda")


xda::numSummary(nyfs1)

#Most of the elements of this numSummary should be familiar. Some new pieces include:
#  . nunique = number of unique values
#  . nzeroes = number of zeroes
#  . noutlier = number of outliers (using a standard that isn't entirely transparent to me)
#  . miss = number of rows with missing value
#  . miss% = percentage of total rows with missing values ((miss/n)*100)
#  . 5% = 5th percentile value of that variable (value below which 5 percent of the observations may be found)

###**** 9.6 What Summaries to Report:*****
# It is usually helpful to focus on the shape, center and spread of a distribution.
#. If the data are skewed, report the median and IQR (or the three middle quantiles). You may want to
#include the mean and standard deviation, but you should point out why the mean and median differ.
#The fact that the mean and median do not agree is a sign that the distribution may be skewed. A
#histogram will help you make that point.
#. If the data are symmetric, report the mean and standard deviation, and possibly the median and IQR
#as well.
#. If there are clear outliers and you are reporting the mean and standard deviation, report them with the
#outliers present and with the outliers removed. The differences may be revealing. The median and IQR
#are not likely to be seriously affected by outliers.


###Chapter 10 - Summarizing data within subgroups

##10.1 Using dplyr and summarize to build a tibble of summary information

nyfs1 %>%
  group_by(sex) %>%
  select(bmi, waist.circ, sex) %>%
  summarise_each(funs(median))
#group_by {dplyr} -> Most data operations are useful done on groups defined by variables in the the dataset. 
#                    The group_by function takes an existing tbl and converts it into a grouped tbl where operations are 
#                    performed "by group": ie group_by(.data, ...*, add = FALSE) (*... = variables to group by)
#select() {dplyr} -> keeps only the variables you mention; rename() keeps all variables.
#summarise_each -> Apply one or more functions to one or more columns 
#funs -> List of function calls, generated by funs, or a character vector of function names.
temp <- nyfs1 %>%
  group_by(bmi.cat) %>%
  summarize(mean(waist.circ), sd(waist.circ), median(waist.circ),
            skew_1 = round((mean(waist.circ) - median(waist.circ)) / sd(waist.circ),3)) #notice the 3 = three decimals points
knitr::kable(temp)
#skew_1 is just defined as the equation "round((mean(waist.circ) - median(waist.circ)) / sd(waist.circ),3)"

#kable {knitr} -> This is a very simple table generator. It is simple by design. It is not intended to replace any other
#                 R packages for making tables.

##10.2 - Using the "by" function to summarize groups numerically
#We can summarize our data numerically in multiple ways, but to obtain data on each individual BMI subgroup
#separately, we'll use the by function. ***

by(nyfs1$waist.circ, nyfs1$bmi.cat, Hmisc::describe)

#This essentially breaks down bmi for all groups (underweight, normal weight, overweight, and obese) and provides a
#detailed summary for each through the "describe" function. 


###Chapter 11 - Comparing Distributions Across Subgroups Graphically

##11.1 - Boxplots to relate an outcome to a categorical predictor
#Boxplots are much more useful when comparing samples of data. For instance, consider this comparison
#boxplot describing the triceps skinfold results across the four levels of BMI category.

ggplot(nyfs1, aes(x=bmi.cat, y=triceps.skinfold)) +
  geom_boxplot()

##11.2 - Augmenting the Boxplot with the Sample Mean
#Often, we want to augment such a plot, perhaps with the sample mean within each category, so as to
#highlight skew (in terms of whether the mean is meaningfully different from the median.)

ggplot(nyfs1, aes(x=bmi.cat, y=triceps.skinfold)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="dodgerblue")

##11.3 - Adding notches to a Boxplot 

#Notches are like "confidence regions" around the median. If the notches do not overlap, as in this situation, this 
#provides some evidence that the medians in the populations represented by these samples are in fact different:

ggplot(nyfs1, aes(x=bmi.cat, y=triceps.skinfold, fill = bmi.cat)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_viridis(discrete=TRUE, option="plasma") +
  labs(title="Triceps Skinfold by BMI category", 
       x="BMI Percentile category", y="Triceps Skinfold (mm)")

#option -> A character string indicating the colormap option to use. Four options are available: "magma" (or "A"),
#"inferno" (or "B"), "plasma" (or "C"), and "viridis" (or "D", the default option).

#There is no overlap between the notches for each of the four categories, so we might reasonably conclude that
#the true median tricep skinfold values across the four categories are statistically significantly different.


#Here's just an example of boxplots where the notches overlap:
ggplot(nyfs1, aes(x=sex, y=age.exam, fill=sex)) +
  geom_boxplot(notch=TRUE) +
  guides(fill="none") + ##drops the legend
  labs(title+"Age by Sex", x="", y="Age (in years)")
#guides {ggplot2} -> guides for each scale can be set scale-by-scale with the "guide" argument, or en masse with "guides()"


#11.4 - Using Multiple Histograms to Make Comparisons 

#Can make an array of histograms to describe multiple groups of data, using ggplot2 and the notion of
#facetting our plot.
ggplot(nyfs1, aes(x=triceps.skinfold, fill=sex)) +
  geom_histogram(binwidth=2,color="black") +
  facet_wrap(~ sex) +
  guides(fill="none") +
  labs(title = "Triceps Skinfold by Sex")
#facet_wrap wraps a 1d sequence of panels into 2d. This is generally a better use of screen space than facet_grid 
#because most displays are roughly rectangular.

#11.5 Using Multiple Density Plots to Make Comparisons:

ggplot(nyfs1, aes(x=waist.circ, fill=bmi.cat)) +
  geom_density() +
  facet_wrap(~ bmi.cat) +
  scale_fill_viridis(discrete=T) +
  guides(fill="none") +
  labs(title="Waist Circumference by BMI Category")

#Plotting all densities on top of each other (semi-transparent)
ggplot(nyfs1, aes(x=waist.circ, fill=bmi.cat)) +
  geom_density(alpha=0.3) +
  scale_fill_viridis(discrete=T) +
  labs(title="Waist Circumference by BMI Category")

#But transparent density is a bit messy above - works better when comparing two groups (ie sex):
ggplot(nyfs1, aes(x=waist.circ, fill=sex)) +
  geom_density(alpha=0.5) +
  labs(title="Waist Circumference by Sex")


## A violin plot is another method of looking at the distribution of datasets
ggplot(nyfs1, aes(x=sex, y=triceps.skinfold, fill = sex)) +
  geom_violin(trim=TRUE) +
  guides(fill = "none") +
  labs(title = "Triceps Skinfold by Sex")

ggplot(nyfs1, aes(x=bmi.cat, y=waist.circ, fill = bmi.cat)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=.1, outlier.colour=NA, color = c(rep("white",2), rep("black",2))) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=3) +
  scale_fill_viridis(discrete=T) +
  guides(fill = "none") +
  labs(title = "Waist Circumference by BMI Category in nyfs1",
       x = "BMI category", y = "Waist Circumference")

###Chapter 12 - Mean Closing Force and Propodus Height of Crab Claws (crabs)
setwd("D:/EPBI 431_432 Notes/Notes/2016/Data2016/PartA")

crabs <- read.csv("crabs.csv")
crabs <- tbl_df(crabs)

#A table of the complete dataset: 

knitr::kable(crabs)

##Knitr reference: https://yihui.name/knitr/ *
## kable - simple table generator for LaTeX, HTML, Markdown (Not intended to replace any other R table-making package)

#We can look at the summary of force and heights for all crabs: 
crabs %>% 
  dplyr::select(force, height) %>%
  psych::describe()

#But it would be better to see them by category of crab:***
crabs %>%
  group_by(species) %>%
  summarize(mean(force), median (force), mean(height), median(height))

###Chapter 13 - Studying an Association with Correlations and Scatterplots

#Plot describing force on the basis of height, across all 38 crabs
ggplot(crabs, aes(x=height, y=force)) +
  geom_point()

#Adding shapes and color:
ggplot(crabs, aes(x=height, y=force, color=species, shape=species)) +
  geom_point(size=2) +
  labs(title="Crab Claw Force By Size", 
       x="Claw's propodus heigh (mm)", y="Mean closing Force (N)")

#We can also facet out the data from each species into its own plot: 

ggplot(crabs, aes(x=height, y=force, color=species, shape=species)) +
  geom_point(size=2) +
  facet_wrap(~ species) +
  guides(color="none", shape="none") +
  labs(title="Crab Claw Force by Size", 
       x="Claw'spropodus height (mm)", y="Mean Closing Force (N)")

# Many times, we would like to identify a pattern in our scatterplot so we can optionally add a smoothed loess line
# to the scatter plot: 

ggplot(crabs, aes(x = height, y = force)) +
  geom_point() +
  geom_smooth(se = FALSE) + # se = display confidence interval around smooth? (TRUE by default, see level to control
  labs(title = "Crab Claw Force by Size", #Note level = 0.95 by default
       x = "Claw's propodus height (mm)", y = "Mean closing force (N)")

#It works for the multi-faceted plots as well: 
ggplot(crabs, aes(x=height, y=force, color=species, shape=species)) +
  geom_point(size=2) +
  facet_wrap(~ species) +
  guides(color="none", shape="none") +
  geom_smooth(se=FALSE) + # se = display confidence interval around smooth? (TRUE by default, see "level" to control)
  labs(title="Crab Claw Force by Size", #Note level = 0.95 by default
       x="Claw'spropodus height (mm)", y="Mean Closing Force (N)")

#By removing "se=FALSE" we can add confidence interval shading to our plots (they are set to 0.95 by default):
ggplot(crabs, aes(x=height, y=force, color=species, shape=species)) +
  geom_point(size=2) +
  facet_wrap(~ species) +
  guides(color="none", shape="none") +
  geom_smooth() +
  labs(title="Crab Claw Force by Size with Confidence Intervals", 
       x="Claw'spropodus height (mm)", y="Mean Closing Force (N)")



ggplot(crabs, aes(x = height, y = force)) +
  geom_point() +
  geom_smooth() + # se = display confidence interval around smooth? (TRUE by default, see "level" to control) 
  labs(title = "Crab Claw Force by Size", #Note level = 0.95 by default
       x = "Claw's propodus height (mm)", y = "Mean closing force (N)")

  
#A loess smooth is a method of fitting a local polynomial regression model that R uses as its generic smooth
#for scatterplots like this with fewer than 1000 observations. Think of the loess as a way of fitting a curve
#to data by tracking (at point x) the points within a neighborhood of point x, with more emphasis given to
#points near x. It can be adjusted by tweaking two specific parameters, in particular:
#  . a "span" parameter (defaults to 0.75) which is also called "alpha" in the literature, that controls the degree of
#    smoothing (essentially, how larger the neighborhood should be), and
#  . a degree parameter (defaults to 2) which specifies the degree of polynomial to be used. Normally, this
#    is either 1 or 2 - more complex functions are rarely needed for simple scatterplot smoothing.
#From R help: span -> Controls the amount of smoothing for the default loess smoother. Smaller numbers produce wigglier
#lines, larger numbers produce smoother lines

#Chapter 14 - Fitting a Linear Regression Model 

#Is a "least sqaures model" likely to be an effective choice here? 


mod <- lm(force ~ height, data = crabs) #force is the "outcome" variable and height is the "predictor" variable
ggplot(crabs, aes(x=height, y=force)) +
  geom_point() +
  geom_smooth(method = "lm", color="red") +
  labs(title="Crab Claw Force by Size with Linear Regression Model", 
       x="Claw's propodus height (mm)", y= "Mean closing force (N)") +
  annotate("text", x=11, y=0, color="red", fontface="italic",
           label= paste("Force -", signif(coef(mod)[1],3), " + ",
                        signif(coef(mod)[2],3), " Height "))
#annotate ->  a geom that adds text labels to data that's in a vector. In this case a linear regression model (lm)
## the "paste" function combines (concatenates) all vectors and strings together
## the "signif" function rounds the values in its first argument to the specified number of significant digits (in this
## case - 3) ie signif(x, digits=3) where in this case, x=coef(mod)[2] (3 is the digits).
## the "coef" is a generic function which extracts model coefficients from objects returned by modeling functions(in this
## case the coefficient from an lm -> "mod")  
## Note for "coef" -> the [1] gives the line intercept (-11.1) and [2] gives the slope of the first predictor (2.63)

## 14.1 Fitting a Regression Line 

#The variable mod contains the linear regression model that predicts the effect of height on force in the data "crabs"
#A "summary function" can provide better detail of the fitted linear regression model: 
summary(mod)
#lm(force ~ height, data = crabs)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-16.7945  -3.8113  -0.2394   4.1444  16.8814 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -11.0869     4.6224  -2.399   0.0218 *  
#  height        2.6348     0.5089   5.177 8.73e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 6.892 on 36 degrees of freedom
#Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4109 
#F-statistic:  26.8 on 1 and 36 DF,  p-value: 8.73e-06

#Again the "outcome" variable in this model is "force" and the "predictor" variable is "height" 
#The "straight line model" for these data fitted by least squares is force = -11.1 + 2.63 height
#***The slope of height is positive, which indicates that as height increases, the force will also increase - 
#specifically, for every mm the height increases, the force increases by 2.63 Newtons!

#The multiple R-squared (squared correlation coeffcient) is 0.427, which implies that 42.7% of the 
#variation in force is explained using this linear model with height. It also implies that the Pearson 
#correlation between force and height is the square root of 0.427 (or 0.653)

##The Pearson correlation coefficient assesses how well the relationship between X and Y can be described using a 
#linear function where -1 is a strong negative correlation, 0 is no correlation, and 1 is a strong positive correlation

#For the "crabs" data, the Pearson correlation of Force and Height is:
cor(crabs$force, crabs$height) # = 0.653 which signifies a strong positive correlation with respect to Height and Force

### 14.4 The Correx1 example

# The correx1 data file contains six different sets of (x,y) points, identified by the "set" variable.


correx1 <- read.csv("corr-ex1.csv")
correx1 <- tbl_df(correx1)
Hmisc::describe(correx1)

##Reminder: The describe function in Hmisc knows enough to separate numerical from categorical variables, and give you 
#separate (and detailed) summaries for each. -> For categorical variables, it counts total # of observations (n), number
#of missing values, and number of unique categories (along with counts and percentages falling in each category)
# -> For numerical variables, counts obs, missing values, unique values, info value for data (indicates how continuous
# a variable is ie: a score of 1 is a completely continuous variable while score near 0 indicate lots of "ties" and 
#very few unique values), sample mean, and sample percentiles (quantiles) of the data.


##Output for Hmsic::describe(correx1):
#correx1
#3 Variables 277 Observations
#---------------------------------------------------------------------------
#  set
#n missing unique
#277 0 6
#Alex Bonnie Colin Danielle Earl Fiona
#Frequency 62 37 36 70 15 57
#% 22 13 13 25 5 21
#---------------------------------------------------------------------------
#  x
#n missing unique Info Mean .05 .10 .25 .50
#277 0 181 1 46.53 11.54 15.79 29.49 46.15
#89
#.75 .90 .95
#63.33 79.33 84.15
#lowest : 5.897 7.436 7.692 9.744 10.000
#highest: 93.846 94.103 94.615 95.128 98.205
#---------------------------------------------------------------------------
#  y
#n missing unique Info Mean .05 .10 .25 .50
#277 0 162 1 49.06 13.38 16.38 30.38 46.92
#.75 .90 .95
#68.08 84.62 88.62
#lowest : 7.308 7.692 9.231 10.385 11.154
#highest: 91.154 92.308 92.692 93.462 95.385
#---------------------------------------------------------------------------

### Breaking it down and looking at "Data Set Alex" only

ggplot(filter(correx1, set == "Alex"), aes(x=x, y=y)) +
  geom_point() +
  labs(title = "Correx1: Data Set Alex")

#Here I filtered the dataset "Correx1" for the set variable = to "Alex" only and then mapped the x and y coordinates
#The plot shows a negative correlation between x and y. 

#I can add a Loess smooth line as well to the above graph: 

setA <- dplyr::filter(correx1, set=="Alex")

ggplot(setA, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(col="blue") +
  labs(title="correx1: Alex with Loess smooth")

#Now to put it all together with a linear model: 

ggplot(setA, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  labs(title="correx1: Alex with fitted Linear Model") +
  annotate("text", x=75, y=75, col = "purple", size=6,
           label=paste("Pearson r = ", signif(cor(setA$x, setA$y), 3))) +
  annotate("text", x=50, y=15, col="red", size=5,
           label=paste("y = ", signif(coef(lm(setA$y ~ setA$x))[1],3),
                       signif(coef(lm(setA$y ~ setA$x))[2],2),"x"))

##Taking a look at Bonnie's dataset: 

setB <- dplyr::filter(correx1, set == "Bonnie")

ggplot(setB, aes(x=x, y=y)) +
  geom_point() +
  labs(title = "correx1: Dataset Bonnie")

#Seems to be more of a positive correlation with Bonnie's Data - let's check with the LM: 
ggplot(setB, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  labs(title="correx1: Bonnie with fitted Linear Model") +
  annotate("text", x=25, y=60, col="purple", size = 6,
           label=paste("Pearson r = ", signif(cor(setB$y, setB$x), 2))) +
  annotate("text", x=50, y=15, col="red", size = 5,
           label=paste("y = ", signif(coef(lm(setB$y ~ setB$x))[1],3), " + ",
                       signif(coef(lm(setB$y ~ setB$x))[2],2), "x"))
#Confirmed positive correlation between x and y for Bonnie

##Taking a look at the Pearson correlations for all individuals in the correx1 dataset: 
tab1 <- correx1 %>%
  group_by(set) %>%
  summarise("Pearson r" = round(cor(x, y, use="complete"),2))


knitr::kable(tab1)

# |set      | Pearson r|
# |:--------|---------:|
# |Alex     |     -0.97|
# |Bonnie   |      0.80|
# |Colin    |     -0.80|
# |Danielle |      0.00|
# |Earl     |     -0.01|
# |Fiona    |      0.00|

#According to the table, only Alex, Bonnie, and Colin have a correlation. Danielle, Earl, and Fiona have no correlation
#since the Pearson r is close to 0. Colin and Bonnie look like they would have a very similar scatter plot due to the 
#coefficient (Just that Colin's will be negative) - I can build a few plots to compare and facet wrap them

setBC <- dplyr::filter(correx1, set == "Bonnie" | set == "Colin")

ggplot(setBC, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  facet_wrap(~ set)

rm(setA,setB,setBC)
##Scatterplot shows an extreme outlier in Colin's graph that is affecting the dataset - important to take into account
#when looking at statistical numbers!


### 14.6  The Spearman Rank Correlation

## The Spearman rank correlation looks at the relationship between X and Y in a "monotone" function - meaning that Y MUST
## either be strictly increasing or decreasing as X increases or decreases. 
## Similar to Pearson Correlation where dimensions fall between -1 and 1
## Because of the "monotone" function, a positive Spearman correlation corresponds to an increasing (BUT NOT LINEAR)
## association between X and Y, while a negative Spearman correlation corresponds to a decreasing (AGAIN NOT LINEAR)
## association.



cor(crabs$force, crabs$height) # = 0.653 -> Pearson Correlation
cor(crabs$force, crabs$height, method="spearman") # = 0.657 -> Spearman Correlation
# In this case, both are very similar 


### Spearman vs Pearson examples: 

spear1 <- read.csv("spear1.csv")
spear2 <- read.csv("spear2.csv")
spear3 <- read.csv("spear3.csv")
spear4 <- read.csv("spear4.csv")

ggplot(spear1, aes(x=x, y=y)) +
  geom_point() +
  labs(title = "Spearman vs Pearson, Example1") +
  annotate("text", x=-10,y=20,  #Remember, need to run the plot in order to see where to place this data on the plot
           label=paste("Pearson r = ",
                       signif(cor(spear1$x, spear1$y),2),
                       ", Spearman r = ",
                       signif(cor(spear1$x, spear1$y, method="spearman"),2)))
#Positive correlation (non-linear) with Pearson and Spearman both showing similar values

ggplot(spear2, aes(x = x, y = y)) +
  geom_point(col = "purple") +
  geom_smooth(se = FALSE) + #adding a loess smooth without confidence intervals
  labs(title = "Spearman vs. Pearson, Example 2") +
  annotate("text", x = 10, y = 20, col = "purple",
           label = paste("Pearson r = ",
                         signif(cor(spear2$x, spear2$y),2),
                         ", Spearman r = ",
                         signif(cor(spear2$x, spear2$y, method = "spearman"),2)))
#Negative correlation (non-linear) with Pearson and Spearman both showing similar values




### 15 Checking to see if a Linear Model is Appropriate for the Data

# Looking back at the crabs data we can look at the log-log relationship of height vs force where the log of the force
# is predicted by the log of the height. 
## A log-log model is appropriate when we think that percentage increases in X (Height) lead to constant percentage 
## increases in Y (Force)

ggplot(crabs, aes(x=log(height), y=log(force))) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title = "Log-Log Model for Crabs data")

## Details of the Log-Log model

modelLL <- lm(log(force)~log(height), data=crabs)
summary(modelLL)

#Call:
#  lm(formula = log(force) ~ log(height), data = crabs)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.5657 -0.4450  0.1884  0.4798  1.2422 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -2.7104     0.9251  -2.930  0.00585 ** 
#  log(height)   2.2711     0.4284   5.302 5.96e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.6748 on 36 degrees of freedom
#Multiple R-squared:  0.4384,	Adjusted R-squared:  0.4228 
#F-statistic: 28.11 on 1 and 36 DF,  p-value: 5.96e-06

## Here the regression equation is log(force) = -2.71 + 2.27log(height)
#So, for example, if we found a crab with propodus height = 10 mm, our prediction for that crab's claw force
#(in Newtons) based on this log-log model would be. . .
# * log(force) = -2.71 + 2.27 log(10)
# * log(force) = -2.71 + 2.27 x 2.303
# * log(force) = 2.519
# * and so predicted force = exp(2.519) = 12.4 Newtons

## How does this compare with the original lm equation :
#Original:
#model.Lin <- lm(force ~ height, data=crabs)
#summary(model.Lin)  
#
#Call:
#  lm(formula = force ~ height, data = crabs)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-16.7945  -3.8113  -0.2394   4.1444  16.8814 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -11.0869     4.6224  -2.399   0.0218 *  
#  height        2.6348     0.5089   5.177 8.73e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 6.892 on 36 degrees of freedom
#Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4109 
#F-statistic:  26.8 on 1 and 36 DF,  p-value: 8.73e-06

## So the original equation for lm of force to height was force = -11.1 + 2.63 height
##So, for example, if we found a crab with propodus height = 10 mm, our prediction for that crab's claw force
#(in Newtons) based on this linear model would be. . .
# * force = -11.087 + 2.635 x 10
# * force = -11.087 + 26.348
# * so predicted force = 15.3 Newtons.
# So, it looks like the two models give different predictions.

###** Before comparing, the original lm with the log-log, there's also the "predict" function which can give a simpler
###   way of making predictions: 
predict(model.Lin, data.frame(height=10), interval="prediction")
#Results in: 
#     fit      lwr      upr
# 1 15.26133 1.048691 29.47397
# Which matches the "15.3 Newtons" above. 
# The other numbers (lwr and upr) mean that the linear model's predicted force associated with a single new crab claw
# with propodus height 10mm is 15.3 Newtons, and that a 95% prediction interval for the true value of such a force is 
#between 1.0 an 29.5 Newtons.

##The same can be done with log-log -> ** IMPORTANT to NOTE that if we only run the "predict fucntion" on the log-log
##we will get the log(force) answer and not the force in Newtons so we have to exponentiate by adding "exp" 
exp(predict(modelLL, data.frame(height=10), interval="prediction"))
#Results in:
#     fit      lwr      upr
#1 12.41736 3.082989 50.01341
#Which again matches the "12.4 Newtons" in the log-log model above.
#Once more, the numbers (lwr and upr) mean that the log-log model's predicted force associated with a single new crab
#claw with a propodus of 10mm is 12.4 Newtons, and that a 95% prediction interval for the true value of such a force is
#between 3.1 and 50 Newtons. 


### Now I can compare the predictions of the two models graphically: 

loglogdat <- data.frame(height = crabs$height, force = exp(predict(modelLL)))
#The "loglogdat" variable is providing the log-log ratio value of the force based on height. As the equation above shows
#the height at a certain point can be predicted by plugging the height (ie 10mm above), in the equation and receiving
#the log value of the Force (and since I am already taking into the log of height in the modelLL, I do NOT need to take
#another log of height in "loglogdat")
##Note -> The predict function, when not given a new data frame, will use the
##existing predictor values that are in our crabs data. Such predictions are often called fitted values.

#Graph to compare log-log model to linear model:
ggplot(crabs, aes(x=height, y=force)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, col="blue", linetype=2) +
  geom_line(data=loglogdat, col="red", linetype=2, size=1) +
  annotate("text", 7, 12, label="Linear Model", col="blue") +
  annotate("text", 10, 8, label="Log-Log Model", col="red") +
  labs(title = "Comparing the Linear and Log-Log Models for Crab Claw Data")



rm(loglogdat, model.Lin, modelLL)
  

### Chapter 16 - The Western Collaborative Group Study Data (wcgs)


wcgs <- read.csv("wcgs.csv")
wcgs <- tbl_df(wcgs)

#Codebook for Variables in the WCGS data frame (tibble)
#Name     Stored As       Type        Details (units, levels, etc.)
# id       integer      (nominal)      ID #, nominal and uninteresting
# age      integer      quantitative    age, in years - no decimal places
# agec     factor (5)   (ordinal)      age: 35-40, 41-45, 46-50, 51-55, 56-60
# height   integer      quantitative   height, in inches
# weight   integer      quantitative   weight, in pounds
# lnwght   number       quantitative   natural logarithm of weight
# wghtcat  factor (4)   (ordinal)      wt: < 140, 140-170, 170-200, > 200
# bmi      number       quantitative   body-mass index:
#                                      703 * weight in lb / (height in in)2
#sbp       integer      quantitative   systolic blood pressure, in mm Hg
#lnsbp     number       quantitative   natural logarithm of sbp
#dbp       integer      quantitative   diastolic blood pressure, mm Hg
#chol      integer      quantitative   total cholesterol, mg/dL
#behpat    factor (4)   (nominal)      behavioral pattern: A1, A2, B3 or B4
#dibpat    factor (2)   (binary)       behavioral pattern: A or B
#smoke     factor (2)   (binary)       cigarette smoker: Yes or No
#ncigs     integer      quantitative   number of cigarettes smoked per day
#arcus     integer      (nominal)      arcus senilis present (1) or absent (0)
#chd69     factor (2)   (binary)       Coronary Heart Disease (CHD) event: Yes or No
#typchd69  integer      (4 levels)     event: 0 = no CHD, 1 = MI or SD,
#                                      2 = silent MI, 3 = angina
#time169   integer      quantitative   follow-up time in days
#t1        number       quantitative   heavy-tailed (random draws)
#uni       number       quantitative   light-tailed (random draws)

## If needed "Hmisc::describe" can be used to give detailed information for each variable, "mosaic::favstats(wcgs$variable)
## for any of the variables to get the min, mean, med, max, sd, n missing, and IQR for any desired variable in wcgs.
## And psych::describe(wcgs$variable) can be used to obtain vars, n mean, sd median, mad, min, max, range, skew, and
## kurtosis stats.

### Start of simple with a Histogram of the systolic blood pressure (sbp): 
ggplot(wcgs, aes(x=sbp)) +
  geom_histogram(bins=30, fill="aquamarine", col="blue") +
  labs(title="Histogram of Systolic BP from wcgs data")
#Notice there is a right skew 

#Adding a normal distribution to the above histogram:

ggplot(wcgs, aes(x=sbp)) +
  geom_histogram(aes(y=..density..), bins=30, fill="aquamarine", col="blue") +
  stat_function(fun=dnorm, lwd=1.5, col="darkblue",
                args=list(mean=mean(wcgs$sbp), sd=sd(wcgs$sbp))) +
  annotate("text", x=200, y=0.01, col="darkblue",
           label=paste("Mean = ", round(mean(wcgs$sbp),2),
                       ", SD = ", round(sd(wcgs$sbp),2))) +
  labs(title="Histogram of WCGS Systolic BP with Normal Distribution")
  

### Describing Outlying Values with Z-Scores

## Using the "Hmisc" and "psych" package, the maximum systolic blood pressure value is "230" and is also the most extreme
## value in the dataset. 

Hmisc::describe(wcgs$sbp)
#wcgs$sbp 
#n  missing distinct     Info     Mean      Gmd      .05      .10      .25      .50 
#3154        0       62    0.996    128.6    16.25      110      112      120      126 
#.75      .90      .95 
#136      148      156 
#
#lowest :  98 100 102 104 106, highest: 200 208 210 212 230

psych::describe(wcgs$sbp)
#     vars    n      mean    sd    median   trimmed    mad   min   max  range  skew   kurtosis   se
#X1    1     3154   128.63  15.12   126      127.22   11.86   98   230   132    1.2     2.79    0.27

# A Z-score can be used to measure how extreme the sbp of 230 is compared to the dataset. In this case the maximum value
# of 230 is 6.71 standard deviations away from the mean and therefore has a Z-score of 6.7. The minimum systolic blood
# pressure of 98 is 2.03 standard devations BELOW the mean and therefore has a z-score of -2. 


## Since the SBP normal histogram shows a right skew, I can run an inverse of the data and see if this provides a normal
## distribution. 

p1 <- ggplot(wcgs, aes(x=sbp)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="orchid1", color="white") +
  stat_function(fun=dnorm, args=list(mean=mean(wcgs$sbp), sd=sd(wcgs$sbp)), col="blue") +
  labs(x="SBP", title="SBP in WCGS data")

p2 <- ggplot(wcgs, aes(x=1/sbp)) +
  geom_histogram(aes(y= ..density..), bins=30, fill="orange1", color="white") +
  stat_function(fun=dnorm, args=list(mean=mean(1/wcgs$sbp), sd=sd(1/wcgs$sbp)), col="blue") +
  labs(x="1/SBP", title="1/SBP in WCGS data")
  
grid.arrange(p1, p2, ncol=2,
               bottom= "Assessing Normality of SBP and 1/SBP in WCGS data")

### Running some comparisons of data:

## Comparing SBP by Smoking Status with Overlapping Densities

ggplot(wcgs, aes(x=sbp, fill=smoke)) +
  geom_density(alpha=0.3) +
  scale_fill_viridis(discrete=T) +
  labs(title="Systolic Blood Pressure by Smoking Status")
# According to this chart, there does not seemt to be a large difference between the SBP of smokers vs non-smokers ->
# which suggests some other factors in this population could be a result of the higher SBP.

## Comparing SBP by Cardiopulminarty Heart Disease (CHD) event with facetted Histograms SBP = integer, CHD = categorical

ggplot(wcgs, aes(x=sbp, fill=chd69)) +
  geom_histogram(bins=20, color="black") +
  facet_wrap(~ chd69) +
  guides(fill="none") +
  labs(title="Systolic BP by Cardiopulminary Heart Disease Status")

## Making a Boxplot of SBP by weight category

ggplot(wcgs, aes(x=wghtcat, y=sbp, fill=wghtcat)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  guides(fill="none") +
  labs(title="Boxplot of Systolic BP by Weight Category in WCGS data", x="Weight Category",
       y="Systolic Blood Pressure")
# This gives the best representation of how sbp is affected by weight
# A summary of a linear model can also show which variables affect SBP
summary(lm(wcgs$sbp~wcgs$weight+wcgs$smoke))

#Call:
#lm(formula = wcgs$sbp ~ wcgs$weight + wcgs$smoke)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-29.078 -10.097  -2.409   7.669 100.000 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   96.80819    2.17332  44.544   <2e-16 ***
#wcgs$weight    0.18440    0.01243  14.831   <2e-16 ***
#wcgs$smokeYes  1.01941    0.52511   1.941   0.0523 .  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 14.62 on 3151 degrees of freedom
#Multiple R-squared:  0.06525,	Adjusted R-squared:  0.06466 
#F-statistic:   110 on 2 and 3151 DF,  p-value: < 2.2e-16

## The linear model also agrees with weight having more effect on sbp than smoking does. 

# The Weight categories can be re-arranged to look neater since R placed them out of order. This can be done by adding
# a new variable to coincide with the wghtcat variable to reorder : 

levels(wcgs$wghtcat)
wcgs$weight.f <- factor(wcgs$wghtcat, level=c("< 140", "140-170", "170-200", "> 200"))

table(wcgs$weight.f, wcgs$wghtcat)

#Here's the graph re-ordered
ggplot(wcgs, aes(x=weight.f, y=sbp, fill=weight.f)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  guides(fill="none") +
  labs(title="Boxplot of Systolic BP by Reordered Weight Category in WCGS",
       x="Weight Category, Reordered", y="Systolic Blood Pressure")


