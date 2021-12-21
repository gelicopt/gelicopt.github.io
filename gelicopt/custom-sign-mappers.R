# This code still needs some cleaning.

#https://www.r-orms.org/mixed-integer-linear-programming/practicals/problem-course-assignment/
library(dplyr)

library(ompr) # Model library 
library(ompr.roi) 
library(ROI.plugin.glpk) # Model solver

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


# shareable_signs: Signs that can be part of multiple referents
optimal_group_mapper <- function(ess, groupsPos = NULL, fast = TRUE){
  n_referents = nrow(ess)
  all_signs = unique(c(t(ess)))
  n_signs = length(all_signs)

  which.is.max <- function(x){
      if(max(x) < 0) return(0) # To deal with cases of non-available signs
      else return(which.max(x))
  }
  
  # Create a matrix for holding the match score between each possible sign and each possible referent
  match_scores <- matrix(0, nrow = n_referents, ncol = n_signs)
  
  # Give a score to each sign/referent pairing. Here we use the number of times the sign has been proposed.
  for (r in 1:n_referents) {
      t <- table(t(ess[r,])) # signs proposed and their number of occurrences for that referent
      match_scores[r, match(names(t), all_signs)] = t # report occurrences in the matrix
  }

  # The MIP model
  model <- MIPModel() %>%
    # 1 iff sign j is assigned to referent i
    add_variable(assignment[i, j], i = 1:n_referents, j = 1:n_signs, type = "binary") %>%

    # maximize the preferences
    set_objective(sum_expr(match_scores[i, j] * assignment[i, j], i = 1:n_referents, j = 1:n_signs)) %>%

    # Each sign is assigned to at most one referent.
    add_constraint(sum_expr(assignment[i, j], i = 1:n_referents) <= 1, j = 1:n_signs, .show_progress_bar = FALSE)

  if(is.null(groupsPos)) {
    # Each referent is assigned to at least one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) >= 1, i = 1:n_referents, .show_progress_bar = FALSE)
  } else if(is.na(groupsPos)){
    # Each referent is assigned to exactly one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) == 1, i = 1:n_referents, .show_progress_bar = FALSE)
  } else {
    # Each referent is assigned to at least one sign
    model <- add_constraint(model, sum_expr(assignment[i, j], j = 1:n_signs) >= 1, i = 1:n_referents, .show_progress_bar = FALSE)

    # But not all of them...
    group_matrix <- getGroupMatrix(groupsPos, n_signs)
    constraints <- createConstraints(group_matrix, fast)
    for(constr in constraints){
   #print(constr)   
      model <- add_constraint(model, sum_expr(assignment[i, j], j = constr) <= 1, i = 1:n_referents, .show_progress_bar = FALSE)
    }
  }

  result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))

  solution <- get_solution(result, assignment[i,j])
  df <- solution[solution$value == 1,]
  
  df$j <- all_signs[df$j] # map back the correct sign symbols

  #df[order(df$i),]
  
  #all_signs[df[order(df$i),]$j]
  df[order(df$i),2:3]

#  res <- results.matrix(get_solution(result, assignment[i,j]), n_referents, n_signs)

#  res
}

