source("bsplines.R")
source("inference.R")

ytest <- c(80.289, 79.4151,78.2884,77.0354,75.7902)
ttest <- c(0,1.2,2.3,3.7,4.8)
model <- readRDS("model.rds")

probabilities <- run_infer(list(ttest), list(ytest), model)