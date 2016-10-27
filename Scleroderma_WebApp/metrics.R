
categories_1 <- function(){
	#find metrcs for each category
	#data <- get_individual_rambo()
	data <- get_individual_disk()
	valid_data <- get_valid_individuals(data)
	cat_data <- categorize_individuals(valid_data)
}

get_individual_rambo <- function(){
	#get all individuals from db
	source("database.R")
	db <- rambo_database()
	d <- db$train()
	saveRDS(d,"allPatients.rds")
}

get_individual_disk <- function(){
	readRDS("allPatients.rds")
}

get_valid_individuals <- function(data){
	#trim set of individuals to the ones that are 
	#print(length(data))
	valid_data <- lapply(data,function(l){
		if(length(l$x) >= min_obs & max(l$x) >= min_years)
			return(l)
		else
			return(NULL)
		})
	valid_data <- Filter(Negate(is.null), valid_data)
	#print(length(valid_data))
}

most_valid_traj <- function(l) {
	#print(str(l))
	max_mass <- max(l$posterior)
	valid_num_pred <- length(l$act_y) - length(l$seen_y)
	#print(max_mass)
	if(max_mass>=min_sub_mass){
		ind <- match(max_mass, l$posterior)
		l$max_traj <- l$ynew_hat[,ind]
		l$diff <- l$max_traj - l$act_y
		l$correct <- 0

		ind <- abs(l$diff)<threshold
		l$correct[ind] <- 1
		l$correct[!ind] <- 0
		l$use_correct <- tail(l$correct,valid_num_pred)
		l$correct_x <- tail(l$act_x,valid_num_pred)
		#print(l)
		return(l)
	}
	else
	  return(NULL)
}

min_sat_traj <- function(l,model){
	#if(is.null(l))
	#e	return(NULL)
	#print(str(l))
	ind<-1
	x_all<-l$x
	y_all<-l$y
	x_new<-0
	y_new<-0
	points_aval <- length(x_all)
	while(max(x_new)<min_years){
		ind<-ind+1
		if(ind > points_aval)
			return(NULL)
		x_new <- l$x[1:ind]
		#print(x_new)
	}
	l$x <- x_new
	l$y <- l$y[1:ind]
	l$x_all <- x_all
	l$y_all <- y_all
	#print(str(l))
	l <- Filter(Negate(is.null), l)
	m <- apply_model(l,model,x_all)
	num_points = length(x_all)
	#print(str(m))
	while(max(m$posterior)<min_sub_mass){
		ind <- ind + 1
		#print(str(l))
		#print(ind)
		#print(num_points)
		if(ind >= num_points)
			return(NULL)
		l$x <- x_all[1:ind]
		l$y <- y_all[1:ind]
		#print(str(l))
		m <- apply_model(l,model,x_all)
		#print(ind)
		#print(max(m$posterior))
	}
	#print(max(m$posterior))
	m$act_y <- y_all
	m$act_x <- x_all - tail(l$x,1)
	m$seen_y <- l$y
	return(m)
}

sep <- function(l){
	 pts <- length(l$act_y) - length(l$seen_y)
	 if(length(l$use_correct) == 0)
	   return(FALSE)
	 m <- mean(l$seen_y)
	 lower_bound <- m - m*perc_bound
	 val_points <- tail(l$max_traj,pts)
	 pnts_above <- val_points[val_points>=lower_bound]
	 perc_above <- length(pnts_above)/length(val_points)
	# perc <- val_points/length(val_points)
	 if(perc_above >= group_mass)
	 	return(TRUE)
	 else
	 	return(FALSE)
}
sep_subtype <- function(l){
	max_mass <- max(l$posterior)
	max_sub <- match(max_mass,l$posterior)

	#print(sum(l$posterior))
	total_stable_mass <- sum(l$posterior[stable_set])

	is_max_stable <- max_sub %in% stable_set
	is_sub_mass <- total_stable_mass >= min_sub_mass

	return(is_max_stable && is_sub_mass)
}

plot_traj <- function(l){

}

get_prob <- function(l){
   	total_pnts <- 0
   	weighted_prob <-0
   	for(i in l){
   	  #print(l)
   	  ind<-i$correct_x<temporal_bound
   	  c <- i$use_correct[ind]
   	  #print(l)

   	  #print(num_pts)
   	  if(length(c) == 0)
   	      next
   		pts <- length(c)
   		p <- sum(c)/pts
   		weighted_prob <- weighted_prob + p*pts
   		#print(weighted_prob)
   		total_pnts <- total_pnts + pts
   	}
   	prob <- weighted_prob/total_pnts
}

plot_groups <- function(grp){
  #print(str(grp))
}

categorize_individuals <- function(valid_data){
	#split individuals into relevant categories
	source("nips15-model/functions.R")
	model <- readRDS("nips15-model/nips_model.rds")
   	#X <- seq(0,25,len=100)
   	#inferences <- apply_model(valid_data, model, X)
   	#inferences <- lapply(valid_data, apply_model, model, X)
   	#print(str(valid_data))
   	inferences <- lapply(valid_data, min_sat_traj, model)
   	inferences <- Filter(Negate(is.null), inferences)

   	#print(str(inferences))

   	#discard traj < mass
   	data_valid_traj <- lapply(inferences, most_valid_traj)
   	
   	#ind <- sapply(data_valid_traj, sep)
   	ind <- sapply(data_valid_traj, sep_subtype)
   	
   	decl <- data_valid_traj[!ind]
   	incl <- data_valid_traj[ind]
   	#print(str(data_valid_traj))

   	incl_prob <- get_prob(incl)
   	decl_prob <- get_prob(decl)

   	#plot_groups(incl)
   	

 	inc_statement <- paste("Stable:", length(incl),"ind. w/",incl_prob)
 	dec_statement <- paste("Else:", length(decl),"ind. w/",decl_prob)

 	print(inc_statement)
 	print(dec_statement)
 	cat("---------------\n\n")

   	# print(length(incl))
   	# print(length(decl))

   	# print(incl_prob)
   	# print(decl_prob)
}

#range <- 10 #range for seperating categories

p_bound_set <- c(0,.05,.1,.15,.25,.4)
group_mass_set <- c(.75,.85,.9,.95,.99)
min_obs_set <- c(1,2,3,5,7,10)
min_years_set <- c(0,1,2,3,5,7,10)
threshold_set <- c(2,5,10,15,20,25,35)

min_sub_mass_set <- c(.05,.1,.20,.30,.40,.50,.60,.70,.80,.85,.90,.95,.99)

temporal_bound_set <-c(2,5,10,15)

#stable_set <- c(1,2,3,5,6)
stable_set <- c(1)
uns <- c(4,7,8)


perc_bound <- .15
group_mass <- .98 #mass nec. to belong to a cat
min_obs <- 2
min_years <- 2
min_sub_mass <- .15
threshold <- 10

temporal_bound <- 5

# for(p in p_bound_set){
# 	perc_bound <- p
# 	print(paste("Bounding by ", p, "of mean"))
# 	categories_1()
# }

# for(t in threshold_set){
# 	threshold <- t
# 	print(paste("Thresholding at ", t, "of traj"))
# 	categories_1()
# }
# for(obs in min_obs_set){
# 	min_obs <- obs
# 	print(paste("Min Obs of ", obs))
# 	categories_1()
# }
# for(y in min_years_set){
# 	min_years <- y
# 	print(paste("Min yrs of ", y))
# 	categories_1()
# }
# for(m in min_sub_mass_set){
# 	min_sub_mass <- m
# 	print(paste("Min mass for stable is ", m))
# 	categories_1()
# }
for(t in temporal_bound_set){
	temporal_bound <- t
	print(paste("For points pred in ", t, "years"))
	categories_1()
}


