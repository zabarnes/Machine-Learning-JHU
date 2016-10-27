modelFilePath <- "nips15-model/nips_model.rds"

train <- function(train){ #tt is times, yt is markers (for set of patients) (lists)
  source("nips15-model/functions.R")
  set.seed(1)
  
  model <- fit_model(train)
  #model <- readRDS(modelFilePath)
   xgrid <- seq(0, 20, 0.5)
   Xgrid <- model$basis(xgrid)
#   matplot(xgrid, Xgrid %*% model$param$B)
  
  inferences <- lapply(train, apply_model, model)
  posteriors <- combine(inferences, "posterior", .a = "rbind")
  subtypes <- data_frame(ptid = data$ptid, subtype = apply(posteriors, 1, which.max))
  pfvc <- left_join(pfvc, subtypes, "ptid")
  
  #print(inferences)
  
#   p <- ggplot(pfvc) + xlim(0, 20) + ylim(0, 120)
#   p <- p + geom_line(aes(years_seen_full, pfvc, group = ptid))
#   p <- p + facet_wrap(~ subtype)
#   print(p)
  
  saveRDS(model, modelFilePath)
}

predict_traj <- function(datum, years, numSub, xRange, yRange, modelPoints=1, togMass, minMass){ #t is times, y is markers (for individual) (list)
  cat("Fitting....")
  source("nips15-model/plotter.R")
  source("nips15-model/functions.R")
   #start <- datum$x[1]
   start <- 0
   end <- max(years,25)
   points <- 100
   
   model <- readRDS(modelFilePath)
   X <- seq(start,end,len=100)

   modelPoints <- max(1,modelPoints)
   
   usage_datum <- datum
   usage_datum$x <- usage_datum$x[1:modelPoints]
   usage_datum$y <- usage_datum$y[1:modelPoints]
   inferences <- apply_model(usage_datum, model, X)
   cat("success\n")
   cat("Plotting....")
   if(togMass == 1 ){
     plot <- gplot_nips_dynamic(datum, inferences, start, end, points, xRange, yRange, modelPoints, numberLines=numSub, minMass)
   }
   else if(togMass == 2){
     #plot <- plot_gvis(datum, inferences, start, end, points, xRange, yRange, modelPoints, numberLines=numSub, minMass)
     plot <- interactive_plot(datum, inferences, start, end, points, xRange, yRange, modelPoints, numberLines=numSub, minMass)
   }
   else{
    plot <- gplot_nips(datum, inferences, start, end, points, xRange, yRange, modelPoints, numberLines=numSub)
    
   }
   cat("success\n")
   return(plot)
}

show_meds <- function(p, med_history){
  #remove empty columns
  #print(med_history[,c(-1,-2)])
  med_history <- med_history[, colSums(med_history) > 0]
  med_history <- med_history[rowSums(med_history[,c(-1,-2)]) > 0, ]
  #print(med_history)
  return(plot_meds(p, med_history))
}



