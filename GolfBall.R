# The test statistics are mean (chi-square) / the least common ball (min freq), is the min count reasonable? / variance/ the most common ball (max freq),

# null hypothesis: all th numbers are equally likely
NumberOfSims <- 10000
golfMinFreq <- rep(0,NumberOfSims) # The least common number
golfMaxFreq <- rep(0,NumberOfSims) # The most common number
golfVar<- rep(0,NumberOfSims) # The number of simulations

# Generate test statistics under null hypothesis
for (sim in 1:NumberOfSims){
  DataSize <- 486
  randomNumbers <- sample(1:4, DataSize,replace = TRUE)
  golfMinFreq[sim] <- min(table(randomNumbers))
  golfMaxFreq[sim] <- max(table(randomNumbers))
  golfVar[sim] <- var(table(randomNumbers))
}

# Create a summary data
df <- matrix(c(golfMean, golfMinFreq, golfMaxFreq, golfVar), nrow = NumberOfSims, ncol = 4)
colnames(df) <- c("mean", "min", "max", "variance")
rownames(df) <- range(1:NumberOfSims)
df <- as.table(df)
head(df)

# our dataset
# our dataset
ourData<- c(137, 138, 107, 104)

# Calculate observed data test statistics value
dataMinFreq <- min(ourData)
dataMaxFreq <- max(ourData)
dataVar <- var(ourData)

pMinFreq <- sum((golfMinFreq < dataMinFreq))/NumberOfSims
pMaxFreq <- sum((golfMaxFreq > dataMaxFreq))/NumberOfSims
pVar <- sum((golfVar > dataVar))/NumberOfSims

nullprobs<- c(.25,.25,.25,.25)
(Xsq <- chisq.test(ourData, p=nullprobs))  # Prints test summary

