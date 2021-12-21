library(RcppHungarian)

#
# This sign mapper assigns the most frequently proposed sign to each referent. It should be fast but it does not guarantee
# that signs are not duplicated.
#
# Arguments: ess = the elicited sign set. It should be a data frame with:
#  - referents as rows
#  - participants as columns
#  - the proposed sign in each cell, in string format
#  - referent names as row names
#
# Returns: a vector of signs to be used in the sign-based gesture recognizer.
#
mode_sign_mapper = function(ess, filter = NA) {
  # Computes mode. See https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  result = apply(ess, 1, mode)
  
  names(result) = row.names(ess)
  voc <- filter_vocabulary(ess, result, filter)

  return(data.frame(ref = 1:length(voc), sign = voc))
}
#
# This sign mapper assigns a random sign to each referent. It is silly but might come useful for testing.
# It guarantees that signs are not duplicated.
#
# Arguments: ess = the elicited sign set. It should be a data frame with:
#  - referents as rows
#  - participants as columns
#  - the proposed sign in each cell, in string format
#  - referent names as row names
#
# Returns: a vector of signs to be used in the sign-based gesture recognizer.
#
random_sign_mapper = function(ess, filter = NA) {
  all_signs = unique(c(t(ess)))
  # I added a trick to deal with the situation when the number of observed signs is lower than the number of referents
  # The extra referents will be assigned a "?" as sign
  result = c(sample(all_signs, min(nrow(ess), length(all_signs)), FALSE), rep("?", max(0, nrow(ess) - length(all_signs))))
  
  names(result) = row.names(ess)
  voc <- filter_vocabulary(ess, result, filter)

  return(data.frame(ref = 1:length(voc), sign = voc))
}

#
# This sign mapper uses the Hungarian algorithm to find an optimal mapping between signs and referents, based on how
# often signs are proposed. It guarantees that signs are not duplicated.
#
# Arguments: ess = the elicited sign set. It should be a data frame with:
#  - referents as rows
#  - participants as columns
#  - the proposed sign in each cell, in string format
#  - referent names as row names
#  filter: which filter to use to reject signs (and replace them by "?")
#  citerion: optimization criterion, which can be either "counts" (number of occurences) or "agreements"
#
# Returns: a vector of signs to be used in the sign-based gesture recognizer.
#
hungarian_sign_mapper = function(ess, filter = NA) {
  
  n_referents = nrow(ess)
  all_signs = unique(c(t(ess)))
  n_signs = length(all_signs)
  
  # Create a matrix for holding the match score between each possible sign and each possible referent
  match_scores = matrix(0, nrow = n_referents, ncol = n_signs)
  
  # Give a score to each sign/referent pairing. Here we use the number of times the sign has been proposed.
  for (r in 1:n_referents) {
    t = table(t(ess[r,])) # signs proposed and their number of occurrences for that referent
    match_scores[r, match(names(t), all_signs)] = t # report occurrences in the matrix
  }
    
  # Turn match scores into costs (the hungarian solver won't take negative values but it should be indifferent to translations)
  costs = max(match_scores) - match_scores
  
  # Compute the optimal assignment
  best_assignment = HungarianSolver(costs)$pairs
  
  # Format the result into a vector
  best <- best_assignment[,2]
  best[best == 0] <- length(all_signs) + 1
  result = all_signs[best]
  result[is.na(result)] <- "?"
  # I added a trick to deal with the situation when the number of observed signs is lower than the number of referents
  # The extra referents will be assigned a "?" as sign
  
  names(result) = row.names(ess)[best_assignment[,1]]
  
  voc <- filter_vocabulary(ess, result, filter)

  return(data.frame(ref = 1:length(voc), sign = voc))
}

# This sign mapper is based on Wobbrock et al. (2005, 2009) conflict resolution approach.
# It guarantees that signs are not duplicated. The original algorithm is rough, not fully specified.
# In particular, it does not specify the order of comparisons or what happens in case of equalities.
# Improving the algorithm is not easy. Better implementations are inevitably complex.
#
# Arguments: ess = the elicited sign set. It should be a data frame with:
#  - referents as rows
#  - participants as columns
#  - the proposed sign in each cell, in string format
#  - referent names as row names
#
# Returns: a vector of signs to be used in the sign-based gesture recognizer.
#
wobbrock_sign_mapper <- function(ess = NA, filter = NA){
    n_referents = nrow(ess)
    all_signs = unique(c(t(ess)))
    n_signs = length(all_signs)
    
    which.is.max <- function(x){
        if(max(x) < 0) return(0) # To deal with cases of non-available signs
        else return(which.max(x))
    }
    
    # Create a matrix for holding the match score between each possible sign and each possible referent
    match_scores = matrix(0, nrow = n_referents, ncol = n_signs)
    
    # Give a score to each sign/referent pairing. Here we use the number of times the sign has been proposed.
    for (r in 1:n_referents) {
        t = table(t(ess[r,])) # signs proposed and their number of occurrences for that referent
        match_scores[r, match(names(t), all_signs)] = t # report occurrences in the matrix
    }

    for (i in 1:n_referents){ # I repeat several times to resolve conflicts
        # Find sign columns with maximum number of occurences
        max_col <- apply(match_scores, 1, which.is.max)
        names(max_col) = 1:length(max_col)
        
        df <- as.data.frame(max_col)
        list <- split(df, df$max_col)
        groups <- lapply(list, rownames)
        
        reset <- function(group){
            col <- max_col[group[1]]
            
            if(length(group) > 1){
                scores <- match_scores[as.numeric(group), col]
                match_scores[as.numeric(group)[-which.max(scores)], col] <<- -1 # Change global variable
            }
        }
        
        if(length(groups) < length(max_col)) lapply(groups, reset)
        else break # All conflicts resolved
    }

    max_col <- apply(match_scores, 1, which.is.max)
    
    max_col[max_col == 0] <- length(max_col + 1) # Those are various hacks to assign a "?" sign when necessary
    result <- all_signs[max_col]
    result[is.na(result)] <- "?"
    
    names(result) = row.names(ess)
    
    voc <- filter_vocabulary(ess, result, filter)
    return(data.frame(ref = 1:length(voc), sign = voc))
}


################## Vocabulary Filtering Functions #############################
# In absence of a better sign alternative for a referent (e.g., because better signs are already taken),
# a mapper may assign a "bad" sign, i.e., a sign with few only or no proposals for this given referent.
# In such cases, we may prefer to assign a "?" sign to indicate the problem.


# Filters out the sign (by replacing it with "?") if its agreement is low, below min (The minimum value to be further explored)
agreement_filter <- function(sign, proposals, min = .01) {
    n <- sum(proposals == sign)
    N <- length(proposals)
    a = n*(n-1)/(N*(N-1))

    if(a < min) return("?") else return(sign)
}

filter_vocabulary <- function(ess, vocabulary, filter = agreement_filter){
    if(is.na(filter)) return(vocabulary)
    else{
        list <- as.list(as.data.frame(t(ess)))
        mapply(filter, vocabulary, list)
    }
}
