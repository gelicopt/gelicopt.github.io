# Evaluates the percentage of proposals that match a given sign mapping set (mappings)
# This corresponds to the "training" (overfitted) guessability score 
guessability <- function(mappings, proposals){
    map <- mappings[,2] # I take the second column of the frame that includes the actual sign names

    individal.agreement <- function(sign, proposals){
        correct <- proposals[proposals == sign]
        return(length(correct)/length(proposals))
    }
    
    mean(mapply(individal.agreement, map, as.data.frame(t(proposals))))
}

# Cross validation guessability score based on the Leave-One-Out Cross-Validation method
# mapper: the sign mapper that perform the optimization
guessability.loocv <- function(proposals, mapper){
	guess <- function(index){
		mappings <- mapper(proposals[,-index])
		guessability(mappings, proposals[,index])
	}
	
	partial <- sapply(1:ncol(proposals), function(i) guess(i))
	
	mean(partial)
} 

# It assesses the evolution of the guessability scores (training and cross-validation)
# as the number of participants increases (minimum sample size = 3)
guessability.evolution <- function(proposals, mapper){
  N <- ncol(proposals)
  range <- 3:N
  
  guess.matrix <- sapply(range, function(n){
    subsample <- proposals[,1:n]
    sample.voc <- mapper(subsample)
    
    training.score <- guessability(sample.voc, subsample)
    validation.score <- guessability.loocv(subsample, mapper)
#    cat("N = ",n, ", Gtr = ", training.score , ", Gval = ", validation.score, "\n", sep = "")
   
    c(validation.score, training.score)
  })
  
  df <- as.data.frame(t(guess.matrix ))
  names(df) <- c("validation", "training")
  df$N <- range

  return(df)
}

