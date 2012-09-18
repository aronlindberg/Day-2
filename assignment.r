
# Load everything up
library(TraMineR)
data(biofam)

# Print the data
print(biofam)
names(biofam)
biofam$birthyr

# Create an age variable
biofam <- cbind(biofam, 2002-biofam$birthyr)
names(biofam) <- c(names(biofam)[1:dim(biofam)[2]-1], "age")

# Print summary statistics for sample
summary(biofam)

# Print age summary statistics for women
summary(biofam[biofam$sex=="woman",]$age)

# Print age summary statistics by sex
by(biofam$age, biofam$sex, summary)

# Add a cohort factor to the biofam data frame grouping the birth years into the categories 1900-1929, 1930-1939, 1940-1949, 1950-1959

biofam <- cbind(biofam, cut(biofam$birthyr,c(1900,1930,1940,1950,1959), labels=c("1900-1929", "1930-1939", "1940-1949", "1950-1959"), right=F))
names(biofam) <- c(names(biofam)[1:dim(biofam)[2]-1], "cohort")

# Generate an histogram of the distribution of birthyear using the above birth year classes (look at the help of the hist function for how to do that).

hist(biofam$birthyr, breaks=(c(by(biofam$birthyr, biofam$cohort, min), max(biofam$birthyr))))

# Alternative version
hist(biofam$birthyr, breaks=c(1900,1930,1940,1950,1959))
  
# Produce a frequency table of the cohort factor.
table(biofam$cohort)
  
# Cross tabulate the cohort with the state at 25 years old.
table(biofam$cohort, biofam$a25)  

# Fit a logistic regression for the probability to be married with a child and having left home at 25 years old in terms of the language of the questionnaire and the sex. Comment the results.

# Logistic regression for left/married/child @ 25
summary(glm(a25==6~plingu02+sex,family=binomial,data=biofam))

# Calculate odds ratios (percentages) for significant effects in previous regression
100*(exp(glm(a25==6~plingu02+sex,family=binomial,data=biofam)$coeff)[4]-1)
# Women 92 per cent more likely than men

100*(exp(glm(a25==6~plingu02+sex,family=binomial,data=biofam)$coeff)[3]-1)
# Italian 62% less likely than germans     