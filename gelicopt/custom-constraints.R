#https://www.r-orms.org/mixed-integer-linear-programming/practicals/problem-course-assignment/
library(dplyr)

library(ompr) # Model library 
library(ompr.roi) 
library(ROI.plugin.glpk) # Model solver

library(foreach)
library(doParallel)

results.matrix <- function(solution, n_referents, n_signs) {
  res = matrix(0, nrow = n_referents, ncol = n_signs)
  assignments <- solution[solution$value == 1,]
  for(r in 1:nrow(assignments)){
    ass <- assignments[r, ]
    res[ass$i, ass$j] <- 1
  }

  res
}


# Replace the signs in the groups by their positions in the vector of observed signs 
getPositions <- function(groups, signs){
  groups_ <- NULL

  for(i in 1:nrow(groups)){
    group <- mapply(function(j){which(groups[i,j]==signs)}, 1:2)
    if(!is.list(g)) groups_ <- rbind(groups_, group)
  }

  groups_
}

# Creates a n_signs x n_sings matrix with TRUE values for valid pairs
getGroupMatrix <- function(groupsPos, n_signs){
  m <- matrix(data = FALSE , ncol = n_signs, nrow = n_signs)

  for(row in 1:nrow(groupsPos)){
    group <- groupsPos[row,]

    for(i in 1:(length(group) - 1)){
      for(j in (i+1):length(group)){
        m[group[i],group[j]] <- TRUE
        # Notice that I do not change m[group[j],group[i]]. If one of the two is TRUE, I'm OK.
      }
    }
  }

  return(m)
}

# Initizalizes the list of constraints by taking all possible pairs that are not valid groups 
init_constraints <- function(group_matrix){
  groups <- which(group_matrix==FALSE, arr.ind=T)
  groups <- groups[groups[,"row"] < groups[,"col"],]

  lapply(1:nrow(groups), function(i) { groups[i,] } )
}

# Unifies the list of constraints. This results in a minimum number of constraints.
unify <- function(constraints, compact_list, group_matrix){
  if(length(constraints) == 0) return(compact_list)
  else {
    con <- constraints[[1]] # Create a new compact constraint starting from the first pair in the list
    constraints[1] <- NULL # And remove it...
    index <- 1
    while(length(constraints) >= index){ #And now, try to merge the remaining constraint pairs
      con_ <- constraints[[index]]
      valid <- FALSE

      for(s_ in con_){ #All elements in the pair need to form invalid groups with the elements of the current constraint
        if(valid) break;
        for(s in con){
          if(group_matrix[s, s_] || group_matrix[s_, s]){
            valid <- TRUE
            break;
          } 
        }
      }

      if(!valid){
        con <- union(con, con_)
        constraints[index] <- NULL #Remove this item from the list
      } else {
        index <- index + 1
      }
    }

    if(length(compact_list) == 0){
      compact_list <- list(con)
    }
    else compact_list[[length(compact_list) + 1]] <- con

    unify(constraints, compact_list, group_matrix)
  }
}

# Creates a list of constraints, as defined by the groups
createConstraints <- function(group_matrix, fast = TRUE){  
  if(fast) unify(init_constraints(group_matrix), list(), group_matrix)
  else init_constraints(group_matrix)
}


# 1. ess: a data frame with elicited sign proposals (rows: referents, columns: participants)
# 2. signs: a vector of unique sings in ess (optional, it will be created if absent or NULL)
# 3. legitimatePairs: a matrix of legitimate pairs of signs, where signs are identified by their positions in the signs vector.  
#    If we omit it, the default value is NULL, and th emapper will allow multiple signs to be mapped to each referent. 
#    If, instead, we assign it a NA value, then exactly one sign will be assigned to each referent (as with the Hungarian algorithm).
# 4. maxRefConstraints: a matrix that specifies which signs can be mapped to more than one referent, and the maximum number of referents for each
#    Again, signs are identified by their positions in the signs vector.
#    if we omit it, the maximum number of referents will be one for all signs.
# 5. aggregate: if TRUE, we aggregate constraints for faster optimization (otherwise, it may never terminate)
custom_constraints_mapper <- function(ess, signs = NULL, legitimatePairs = NULL, maxRefConstraints = NULL, aggregate = TRUE){
  n_referents = nrow(ess)

  if(is.null(signs)) signs = unique(c(t(ess)))
  n_signs = length(signs)

  # Identify which signs (exclusive_signs) can be assigned to a single referent
  exclusive_signs <- 1:n_signs
  if(!is.null(maxRefConstraints)) {
    shareable_signs <- maxRefConstraints[,1]
    exclusive_signs <- exclusive_signs[-c(shareable_signs)]
  } else {
    shareable_signs <- NULL
  }
  
  which.is.max <- function(x){
      if(max(x) < 0) return(0) # To deal with cases of non-available signs
      else return(which.max(x))
  }
  
  # Create a matrix for holding the match score between each possible sign and each possible referent
  match_scores <- matrix(0, nrow = n_referents, ncol = n_signs)
  
  # Give a score to each sign/referent pairing. Here we use the number of times the sign has been proposed.
  for (r in 1:n_referents) {
      t <- table(t(ess[r,])) # signs proposed and their number of occurrences for that referent
      match_scores[r, match(names(t), signs)] = t # report occurrences in the matrix
  }

  # The MIP model
  model <- MIPModel() %>%
    # 1 iff sign j is assigned to referent i
    add_variable(assignment[i, j], i = 1:n_referents, j = 1:n_signs, type = "binary") %>%

    # maximize the preferences
    set_objective(sum_expr(match_scores[i, j] * assignment[i, j], i = 1:n_referents, j = 1:n_signs)) %>%

    # Each sign is assigned to at most one referent.
    add_constraint(sum_expr(assignment[i, j], i = 1:n_referents) <= 1, j = exclusive_signs, .show_progress_bar = FALSE) #%>%

    # Those can be assigned to multiple referents.
    if(!is.null(shareable_signs)){
      for(k in shareable_signs){
        maxRef <- maxRefConstraints[maxRefConstraints[,1]==k, 2]
        model <- add_constraint(model, sum_expr(assignment[i, j], i = 1:n_referents) <= maxRef, j = k, .show_progress_bar = FALSE)
      }
   }

    # add_constraint(sum_expr(assignment[i, j], i = 1:n_referents) <= 2, j = shareable_signs, .show_progress_bar = FALSE)

  if(is.null(legitimatePairs)) {
    # Each referent is assigned to at least one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) >= 1, i = 1:n_referents, .show_progress_bar = FALSE)
  } else if(is.na(legitimatePairs)){
    # Each referent is assigned to exactly one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) == 1, i = 1:n_referents, .show_progress_bar = FALSE)
  } else {
    # Each referent is assigned to at least one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) >= 1, i = 1:n_referents, .show_progress_bar = FALSE)

    # But not all of them...
    group_matrix <- getGroupMatrix(legitimatePairs, n_signs)
    constraints <- createConstraints(group_matrix, fast = aggregate)
  
    for(constr in constraints){ 
      model <- add_constraint(model, sum_expr(assignment[i, j], j = constr) <= 1, i = 1:n_referents, .show_progress_bar = FALSE)
    }
  }

  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))

  solution <- get_solution(result, assignment[i,j])
  df <- solution[solution$value == 1,]
  
  df$j <- signs[df$j] # map back the correct sign symbols

  mappings <- df[order(df$i),2:3]
  colnames(mappings) <- c("ref", "sign")
  mappings <- cbind(refname = rownames(ess)[mappings[,1]], mappings) # there may be multiple rows per referent
  rownames(mappings) <- NULL

  return(mappings)
}


# Cross validation guessability score based on the Leave-One-Out Cross-Validation method
# CoresNum: number of CPU cores to use in parallel 
guessability.loocv <- function(proposals, signs = NULL, legitimatePairs = NULL, maxRefConstraints = NULL, CoresNum = 1){
  guess <- function(index){
    mappings <- custom_constraints_mapper(proposals[,-index], signs, legitimatePairs, maxRefConstraints = NULL, aggregate = TRUE)
    guessability(mappings, as.data.frame(proposals[,index]))
  }

  #Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
  registerDoParallel(CoresNum)  # use multicore, set to the number of our cores
  results <- foreach(pid = 1:ncol(proposals), .combine=cbind) %dopar% {
    guess(pid)
  }
  
  mean(results)
} 

