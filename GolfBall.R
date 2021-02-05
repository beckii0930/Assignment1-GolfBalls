# The test statistics are mean (chi-square) / the least common ball (min freq), is the min count reasonable? / variance/ the most common ball (max freq),

# null hypothesis: all th numbers are equally likely
NumberOfSims <- 10000
golfMean <- rep(0,NumberOfSims) #The average number
golfMinFreq <- rep(0,NumberOfSims) # The least common number
golfMaxFreq <- rep(0,NumberOfSims) # The most common number
golfVar<- rep(0,NumberOfSims) # The number of simulations

# Generate test statistics under null hypothesis
for (sim in 1:NumberOfSims){
  DataSize <- 486
  randomNumbers <- sample(1:4, DataSize,replace = TRUE)
  golfMean[sim] <- mean(randomNumbers)
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
ourData<- rep(1, DataSize)
for (i in 1:138){
  ourData[137+i] = 2
}
for (i in 1:107){
  ourData[137+138+i] = 3
}
for (i in 1:107){
  ourData[137+138+107+i] = 4
}
dataMean <- mean(ourData)
dataMinFreq <- min(table(ourData))
dataMaxFreq <- max(table(ourData))
dataVar <- var(table(ourData))

