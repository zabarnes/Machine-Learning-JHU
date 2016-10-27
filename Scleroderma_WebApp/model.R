modelFilePath <- "model/model.rds"

train <- function(tt, yt){ #tt is times, yt is markers (for set of patients) (lists)
  source("model/bsplines.R")
  source("model/kernels.R")
  source("model/psm.R")
  
  #set training parameters
  opt <- list(
    num_subtypes = 9     ## How many subpopulations to use.
    , bb           = bb    ## The B-spline basis object.
    , noise_v      = 1.0   ## The random noise variance.
    , const_v      = 16.0  ## The magnitude of the long-term kernel component.
    , ou_v         = 64.0  ## The magnitude of the short-term kernel component.
    , ou_l         = 4.0   ## The length-scale of the short-term kernel component.
    , maxiter      = 100   ## The max. number of EM iterations to run.
    , tol          = 1e-4  ## The relative improvement threshold to use as a stopping point.
  )
  
  #fit the model with info
  fit <- run_em(tt, yt, opt)
  
  #save the model
  saveRDS(fit, modelFilePath)
}

predict <- function(t, y, years, numSub, xRange, yRange, modelPoints=1){ #t is times, y is markers (for individual) (list)
  source("model/bsplines.R")
  source("model/inference.R")
  
  start <- t[[1]][1]
  end <- min(t[[1]][modelPoints] + years, 22)
  points <- 100
  
  t_model <- list(t[[1]][1:modelPoints])
  y_model <- list(y[[1]][1:modelPoints])
  
  #read in model from file
  model <- readRDS(modelFilePath)
  
  #calculate probabilities of subtypes
  subProbabilities <- modelProb(t_model, y_model, model)
  
  #calculate the individualized trajectories for each subtype
  subTrajectories <- modelTraj(t_model, y_model, model, start, end, points)
  
  #plot the most likely trajectories 
  #plotTrajectories <- modelPlot(t, y, subTrajectories, subProbabilities, start, end, points, xRange, yRange, model$kfn, numberLines=numSub)
  plotTrajectories <- suppressWarnings(gPlot(t, y, subTrajectories, subProbabilities, start, end, points, xRange, yRange, model$kfn, modelPoints, numberLines=numSub))
  
}