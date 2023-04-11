# axivity data

#install.packages("GGIR")

library(GGIR)

# Load example dataset
data <- read.csv("data/EP6_20_391_act.csv")

names(data) <- c("Timestamp","X", "Y", "Z")

data <- preprocess(method = "vector", data = data)

# FAIL!

GGIR(
  mode=1,
  datadir="data/ggir_input",
  outputdir="data/ggir_output",
  studyname="EPHOR",
  verbose=T
)