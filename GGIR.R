# axivity data

#install.packages("GGIR")

library(GGIR)

# Load example dataset
data <- read.csv("data/EP6_20_391_act.csv")

names(data) <- c("Timestamp","X", "Y", "Z")

data <- preprocess(method = "vector", data = data)

# FAIL!