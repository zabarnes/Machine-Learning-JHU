# Scleroderma Web Application
# Zachary Barnes, Peter Schulam, Suchi Saria, PhD.
# Dr. Suchi Saria's Lab @ Johns Hopkins University

# Code to handle all database queries and retrievals

# DBI package provides easy database queries using query strings
# RMySQL package provides simple connection to SQL Server
library(dplyr)

default_server <- "127.0.0.1"
default_port <- "1433"
default_username <- "zbarnes"
default_database <- "sclerodata"


psdo_database <- function(set = c(1)){
  database_rds <- readRDS("database_file.rds")
  train <- database_rds$train
  pt_info_data <- database_rds$pt_info_data
  all_ids <- database_rds$all_ids
  ids <- database_rds$ids
  meds <- database_rds$meds

  # print(str(all_ids))
  # print(str(ids))

  # print(setdiff(all_ids$ptid,ids$ptid))

  thisEnv <- environment()
  
  me <- list(
    thisEnv <- thisEnv,

     getEnv = function()
     {
              return(get("thisEnv",thisEnv))
     },
    
    search_model = function(patientID){
      index <- match(patientID, ids$ptid)
      #print(str(train[[index]]))
      return(train[[index]])
    },
    
    search_info = function(patientID){
      #return(t(unique(select(pfvc[pfvc$ptid == patientID,],ptid,aca,scl,female,afram))))
      d <- pt_info_data[pt_info_data$ingPtID == patientID, ]

      race <- c("Other","White", "Af. Am.", "Indian","Asian", "Arabic", "Am. Ind.", "Pac. Is.", "Unknown")
      sex <- c("Male", "Female")
      copd <- c("No", "Yes","","","","","","","","Unknown")
      sub <- c("Sine","Limited (MCPs)","Limited (elbow, knee)", "Diffuse")

      print(sex[as.numeric(d$strFemale)])
      d$strRaceId1 <- race[d$strRaceId1+1]
      d$strFemale <- sex[as.numeric(d$strFemale)+1]
      d$COPD <- copd[as.numeric(d$COPD)+1]
      print(d$Subtype)
      d$Subtype <- sub[as.numeric(d$Subtype)+1]
      colnames(d) <- c("Patient ID", "Age", "Sex", "Race","Scl Sub.","GI Score","COPD","Muscle Score")
      d
    },
    
    getIDs = function(){
      #distinct(select(pfvc, ptid))
      #print(str(all_ids))
      #return(ids)

      return(get("ids",thisEnv))
      #return(ids$ptid)
    },

    getMeds = function(patientID){
        med_ind <- (meds$ptid == patientID)
        return(meds[med_ind,])
    },
    
    train_search = function(){
      return(train)
    },

    update_data = function(data){
      print(length(train))
      #train <- c(train,data)

      #ids <- data.frame(ptid=c(ids$ptid,data$ptid))
      index <- length(train) + 1
      #index <- match(data$ptid, ids$ptid)

      train[[index]] <- data

      return(assign("train",train,thisEnv))
      #assign("ids",ids,thisEnv)

      #print(train[[index]])
    },
    update_ids = function(data){
      ids <- as.tbl(data.frame(ptid=c(ids$ptid,data$ptid)))
      return(assign("ids",ids,thisEnv))
    },
    add_point = function(patientID, x, y){
       index <- match(patientID, ids$ptid)
       current_points <- train[[index]]
       current_points$x <- sort(append(current_points$x,as.numeric(x)))
       ind <- match(x,current_points$x)
       current_points$y <- append(current_points$y,as.numeric(y),after=ind-1)
       current_points$bias <- append(current_points$bias,current_points$bias[1])
       train[[index]] <- current_points

       print(train[[index]])
       return(assign("train",train,thisEnv))
    }

  )

  assign('this',me,envir=thisEnv)
  class(me) <- append(class(me),"Database")
  return(me)

}

rambo_database <- function(server = default_server, port = default_port, database = default_database, username = default_username, set = c(1)){
  cat("Establishing connection.....")
  library(DBI)
  library(RJDBC)
  source("nips15-model/functions.R")
  
  #password <- readline("Enter database password: ")
  password <- "2e2ceaebb8bbdc312a3d61e70cbf24ea!"

  
  default_driver <- "net.sourceforge.jtds.jdbc.Driver"
  default_driver_location <- "etc/jtds-1.2.8-dist/jtds-1.2.8.jar"
  driver <- JDBC(default_driver, default_driver_location)
 
  connection_string <- paste("jdbc:jtds:sqlserver://",server,":",port,";DatabaseName=",database,sep = "")
  auth_string <- "windows domain"
  connection <- dbConnect(driver, connection_string, domain = auth_string, user = username, password = password)
  cat("success\n")

  ## BEGIN DOWNLOADING IMPORTANT DATA

  cat("Downloading patient data.....")
  pfvc_select_string <- "SELECT ingPtID, STUDY_DATE, stppFVCpred FROM dbo.tPFT"
  in_string <- ""
  # if(!is.null(set)){
  #   in_string <- paste("'",set,"'",sep="",collapse=", ")
  #   in_string <- paste("WHERE ingPtID IN (",in_string,")",sep="")
  # }
  pfvc_select_string <- paste(pfvc_select_string,in_string)
  pfvc <- dbGetQuery(connection, pfvc_select_string)
  
  colnames(pfvc) <- c("ptid", "years_seen_full", "pfvc")
  
  pfvc$years_seen_full <- sapply(pfvc$years_seen_full, function(str){
    as.numeric(as.Date(str, "%Y-%m-%d %H:%M:%S"))/365
  })
  
  pfvc$female = 0
  pfvc$afram = 0
  pfvc$aca = 0
  pfvc$scl = 0
  
  info_select_string <- "SELECT ingPtID, strFemale, strRaceId1 FROM dbo.tPtData"
  info_select_string <- paste(info_select_string,in_string)
  info <- dbGetQuery(connection, info_select_string)
  
  colnames(info) <- c("ptid","female","afram")
  
  afram <- (info$afram == 2)
  info$afram[afram] <- 1
  info$afram[!(afram)] <- 0
  
  dis_select_string <- "SELECT ingPtID, strACA, strScl70 FROM dbo.tSera"
  dis_select_string <- paste(dis_select_string,in_string)
  dis <- dbGetQuery(connection, dis_select_string)
  colnames(dis) <- c("ptid","aca","scl")
  
  dis$aca[dis$aca != 1] <- 0
  dis$scl[dis$scl != 1] <- 0

  #med_string = "SELECT ingPtID, dtmMedFormDate, ysnPred, ysnMtx, ysnAztp, ysnCtxn, ysnDPen, ysnHdrxchl, ysnMncl, ysnColch, ysnTNFInh, ysnMMF, ysnLeflunomide, ysnIVIG FROM dbo.tMeds"
  med_string <- "SELECT ingPtID, dtmMedFormDate, ysnPred, ysnCtxn, ysnMMF FROM dbo.tMeds"
  med_string <- paste(med_string,in_string)
  
  meds <- dbGetQuery(connection, med_string)
  meds$dtmMedFormDate <- sapply(meds$dtmMedFormDate, function(str){
    as.numeric(as.Date(str, "%Y-%m-%d %H:%M:%S"))/365
  })
  #colnames(meds) <- c("ptid","year_given","pred","mtx","aztp","ctxn","dpen","hdrxchl","mncl","colch","tnfinh","mmf","leflunomide","ivig")
  colnames(meds) <- c("ptid","year_given","Pred","Cytox","MMF")

  pt_info_string <- "SELECT ingPtID, dtmDOB, strFemale, strRaceId1, Subtype FROM dbo.tPtData"
  pt_info_string <- paste(pt_info_string,in_string)
  pt_info_data <- dbGetQuery(connection,pt_info_string)

  pt_info_data$dtmDOB <- sapply(pt_info_data$dtmDOB, function(str){
    curTime <- as.numeric(Sys.Date())/365
    birth <- as.numeric(as.Date(str, "%Y-%m-%d %H:%M:%S"))/365
    return(floor(curTime-birth))
  })

  visit_string <- "SELECT ingPtID, lkpGI_Status, COPD,lkpMuscleSev FROM dbo.tVisit"
  if(!is.null(set)){}
  visit_data <- dbGetQuery(connection,visit_string)

  pt_data <- visit_data %>% group_by(ingPtID) %>% do(GI_Score = .$lkpGI_Status, COPD= .$COPD, MUSC_Score =.$lkpMuscleSev)
  pt_data <- na.omit(pt_data)
  pt_data <- as.tbl(pt_data)

  pt_info_data$GI_Score <- -1
  pt_info_data$COPD <- -1
  pt_info_data$MUSC_Score <- -1

  
   id_set <- intersect(pt_info_data$ingPtID,pt_data$ingPtID)
   for(k in id_set){
    info_ind <- pt_info_data$ingPtID == k
    data_ind <- pt_data$ingPtID == k
    pt_info_data$GI_Score[info_ind] <- tail(pt_data$GI_Score[data_ind][[1]],1)
    pt_info_data$COPD[info_ind] <- tail(pt_data$COPD[data_ind][[1]],1)
    pt_info_data$MUSC_Score[info_ind] <- tail(pt_data$MUSC_Score[data_ind][[1]],1)
   }
    

  cat("success\n")
  #print(as.tbl(meds))
  cat("Construction...\n")
  set <- intersect(info$ptid,pfvc$ptid)
  set <- intersect(set, dis$ptid)
  int <- 1
  for(i in set){
    done <- ceiling(int*100/length(set))
    #print(done)
    row <- paste(replicate(ceiling(done/2),"="), collapse = "")
    cat(paste("|",row,"| ",done,"%",sep="")," \r")
    flush.console()
    ind <- (pfvc$ptid == i)
    ind_inf <- (info$ptid == i)
    ind_dis <- (dis$ptid == i)
    ind_med <- (meds$ptid == i)
    pfvc$female[ind] <- info$female[ind_inf]
    pfvc$afram[ind] <- info$afram[ind_inf]
    pfvc$aca[ind] <- max(dis$aca[ind_dis])
    pfvc$scl[ind] <- max(dis$scl[ind_dis])

    bias <- min(unlist(pfvc$years_seen_full[ind]))
    pfvc$bias[ind] <- bias
    pfvc$years_seen_full[ind] <- pfvc$years_seen_full[ind] - bias
    meds$year_given[ind_med] <- meds$year_given[ind_med] - bias
    int <- int +1 
  }
  cat("\nsuccess\n")
  
  cat("Formatting...\n")
  meds <- na.omit(meds)
  meds <- as.tbl(meds)
  
  pfvc$ptid <- as.integer(pfvc$ptid)
  pfvc$female <- as.integer(pfvc$female)
  pfvc$afram <- as.integer(pfvc$afram)
  pfvc$aca <- as.integer(pfvc$aca)
  pfvc$scl <- as.integer(pfvc$scl)
  
  pfvc <- na.omit(pfvc)
  pfvc <- as.tbl(pfvc)
  
  data <- group_by(pfvc, ptid) %>% do(datum = make_datum(.))
  
  ids <- distinct(select(data, ptid))

  all_ids <-  distinct(select(pfvc, ptid))

  # if(!is.null(set))
  #   ids <- ids[ids$ptid==set,]
  
  train <- data[["datum"]]

  cat("\nsuccess\n")

  database_rds <- list(train = train, pfvc=pfvc, pt_info_data = pt_info_data, all_ids = all_ids, ids = ids, meds = meds)

  saveRDS(database_rds, "database_file.rds")

  
  thisEnv <- environment()
  
  me <- list(
    thisEnv <- thisEnv,
    
    search_model = function(patientID){
      index <- match(patientID, ids$ptid)
      #print(train[[index]])
      return(train[[index]])
    },
    
    search_info = function(patientID){
      #return(t(unique(select(pfvc[pfvc$ptid == patientID,],ptid,aca,scl,female,afram))))
      d <- pt_info_data[pt_info_data$ingPtID == patientID, ]

      race <- c("Other","White", "Af. Am.", "Indian","Asian", "Arabic", "Am. Ind.", "Pac. Is.", "Unknown")
      sex <- c("Male", "Female")
      copd <- c("No", "Yes","","","","","","","","Unknown")

      print(sex[as.numeric(d$strFemale)])
      d$strRaceId1 <- race[d$strRaceId1+1]
      d$strFemale <- sex[as.numeric(d$strFemale)+1]
      d$COPD <- copd[as.numeric(d$COPD)+1]
      colnames(d) <- c("Pt ID", "Age", "Sex", "Race","Scl Sub.","GI","COPD","Muscle")
      d
    },
    
    getIDs = function(){
      #distinct(select(pfvc, ptid))
      return(all_ids)
    },

    getMeds = function(patientID){
        med_ind <- (meds$ptid == patientID)
        return(meds[med_ind,])
    },
    
    train_search = function(){
      return(train)
    }
  )
}


#defaultFile <- "model/data/benchmark_pfvc.csv"
defaultFile <- "model/data/benchmark_pfvc_abs.csv"

csvDatabase <- function(filename = defaultFile){
  #library(readr)
  source("nips15-model/functions.R")
  thisEnv <- environment()
  #read in the database from csv
  benchmark <- read_csv(filename)
  
  data <- group_by(benchmark, ptid) %>% do(datum = make_datum(.))
  train <- data[["datum"]]
  
  dataset <- benchmark %>% group_by(ptid) %>% do(t = .$years_seen_full_abs, y = .$pfvc)
  #dataset <- benchmark %>% group_by(ptid)
  
  info <- select(benchmark, ptid, female, afram, hxrp, age_rp_onset, scl, fold)
  
  ids <- distinct(select(info, ptid))
  
  #print(dataset)
  
  tt <- dataset$t
  yt <- dataset$y
  
  nobs <- vapply(tt, length, numeric(1))
  minobs <- vapply(tt, min, numeric(1))
  valid <- minobs <= 2 & nobs > 2
  
  #print(valid)
#    dataset <- dataset[valid,]
#    info <- info[valid,]
#   
#   #only use valid datapoints
#   tt <- tt[valid]
#   yt <- yt[valid]

  #print(dataset[dataset$ptid==2,])
  
  me<-list(
    thisEnv = thisEnv,
    
    search = function(patientID){
      pat_info <- info[info$ptid == patientID, ]
      t(as.matrix(pat_info[1,]))
      
    },
    
    searchModel = function(patientID){
      return(dataset[dataset$ptid==patientID,])
    },
    
    getIDs = function(){
      return(ids)
    },
    
    getTrain = function(){
      return(train)
    },
    
    searchNips = function(patientID){
      index = match(patientID, ids$ptid)
      #print(index)
      return(train[[index]])
    }
    
  )
  
}