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
#If we want to know how many males and females there are, we can build a little table using the dplyr function:

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


## 8.1 The Ladder of Power Transformations



#Confidence Interval of 95 example: For example, a 95% confidence interval covers 95% of the normal curve -- 
#the probability of observing a value outside of this area is less than 0.05. Because the normal curve is symmetric, 
#half of the area is in the left tail of the curve, and the other half of the area is in the right tail of the curve.


