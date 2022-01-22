# Here, as example we use a real dataset by Weigel al. (2014) on on-skin gestures.
# Please, refer to our online case study at https://gelicopt.github.io
# Author: Theophanis Tsandilas

rm(list=ls()) # Clean up R's memory

source("gelicopt/sign-mappers.R")
source("gelicopt/guessability.R")
source("gelicopt/inference.R")
source("gelicopt/custom-constraints.R")

# helper function: extract only the modality from the sign strings
getModality <- function(signs){
    extractModality <- function(sign){
        unlist(strsplit(sign, "#"))[2]
    }

    mapply(extractModality, signs)
}

# helper function: extract only the location from the sign strings
getLocation <- function(signs){
    extractModality <- function(sign){
        unlist(strsplit(sign, "#"))[1]
    }

    mapply(extractModality, signs)
}


########################################
# Data preparation
########################################
data = read.csv("datasets/weigel2014-modalities.csv", stringsAsFactors=F)
nparticipants <- ncol(data)-1
nreferents <- nrow(data)

# Remove the "Referent" column to analyze the data
modalities <- data[2:ncol(data)]
row.names(modalities) <- data$Referent

data = read.csv("datasets/weigel2014-locations.csv", stringsAsFactors=F)
nparticipants <- ncol(data)-1
nreferents <- nrow(data)

# Remove the "Referent" column to analyze the data
locations <- data[2:ncol(data)]
row.names(locations) <- data$Referent

# Also build a table with location and modalities combined.
locations_modalities = data.frame(locations)
for (r in 1:nrow(locations_modalities)) {
  locations_modalities[r,] = paste0(locations_modalities[r,], "#", modalities[r,])
}
    

# We focos on the 33 first referents that represent standard commands and variations, 
# We disregard ones that represent emotions
locations_modalities <- locations_modalities[1:33,]
modalities <- modalities[1:33,]
locations <- locations[1:33,]


#########################################################################################
# Step 1: First, we simplify our signs by merging similar or equivalent body locations:
# Each line below, defines pairs of body locations that should be treated as equivalent
#########################################################################################
equivalent <- t(matrix(
      c("forearm+upperarm", "upperarm+forearm",
      "forearm+upperarm", "fullarm",
      "fingers+handback", "handback+fingers",
      "upperarm", "upperarm-joint"), nrow =2))

# This function replaces the second member of equivalent items by the first
replaceEquivalentSigns <- function(proposals, eq){
    df <- data.frame(lapply(proposals, function(x){gsub(eq[2],eq[1],x)}))
    rownames(df) <- rownames(proposals)

    df
}

### For each row for the matrix, I merge equivalent locations
for(r in 1:nrow(equivalent))
  locations_modalities <- replaceEquivalentSigns(locations_modalities, equivalent[r,])


#########################################################################################
# Step 2: Second, we define legitimate pairs of signs that share the same modality but are executed in different yet related parts of the body.
# Groups of body locations for inferring legitimate pairs of signs (modality must be the same)
#########################################################################################
group1 <- c("forearm+upperarm", "upperarm", "shoulder")
group2 <- c("fingers", "palm+fingers", "palm") 
group3 <- c("fingers+handback", "handback", "wrist")

# Find all unique signs
signs <- unique(c(t(locations_modalities))) 
nsigns <- length(signs)

# Find all pairs of signs that have the same modality
pairs <- t(combn(1:nsigns,2)) # These are all possible pairs of signs (represented by their index)
sameModalityWhich <- getModality(signs[pairs[,1]]) == getModality(signs[pairs[,2]])
legitimatePairs <- pairs[sameModalityWhich,]

# Find the pairs of signs that have the same modality 
# and their body location is in group1, group2, or group3
legitimatePairs <- rbind(
  legitimatePairs[getLocation(signs[legitimatePairs[,1]]) %in% group1 
                  & getLocation(signs[legitimatePairs[,2]]) %in% group1,],
  legitimatePairs[getLocation(signs[legitimatePairs[,1]]) %in% group2 
                  & getLocation(signs[legitimatePairs[,2]]) %in% group2,],
  legitimatePairs[getLocation(signs[legitimatePairs[,1]]) %in% group3 
                  & getLocation(signs[legitimatePairs[,2]]) %in% group3,]
)

# Sort the matrix by the first then the second column
legitimatePairs <- legitimatePairs[order(legitimatePairs[,2],decreasing=FALSE),] 
legitimatePairs <- legitimatePairs[order(legitimatePairs[,1],decreasing=FALSE),] 
cat("Number of legitimate pairs =", nrow(legitimatePairs), "\n")


#########################################################################################
# Step 3: Third, we specify the maximum number of referents to which a sign is mapped.
# By default, our optimization solver assumes that each individual sign can be mapped to no more than one referent. 
# For this study, however, we want to capture the fact that the "slide" modality provides a richer set of possibilities.
# We focus here on binary sliding directions (upwards vs. downwards), so we allow for up to two referents to share the same "slide" sign.
#########################################################################################

 # Which signs (their index) have a slide modality?
selectedSigns <- which(getModality(signs) %in% c("slide"))

# Those slides can be mapped to maximum 2 referents
maxRefs <- rep(2, length(selectedSigns)) 

# We combine them into a matrix
maxRefConstraints <- matrix(c(selectedSigns, maxRefs), ncol=2, dimnames=list(NULL, c("index","maxRef")))

#########################################################################################
# Step 4: We now have all the information that we need to solve the optimization problem. 
#########################################################################################

time <- system.time({
  mappings <- custom_constraints_mapper(locations_modalities, signs, legitimatePairs, maxRefConstraints)
  #mappings <-  hungarian_sign_mapper(locations_modalities)
})
cat("Time to complete the optimization (in sec): ", time[["elapsed"]], "\n")

#########################################################################################
# Step 5: Evaluation of the guessability of the derived mappings
#########################################################################################

guess.training <- guessability(mappings, locations_modalities)
cat("Observed training guessability = ", guess.training, "\n")

time <- system.time({
  guess.validation <- guessability.loocv(locations_modalities, signs, legitimatePairs, maxRefConstraint, CoresNum = 4)
})

cat("Time to complete the cross-validation (in sec): ", time[["elapsed"]], "\n")
cat("Cross-validated guessability = ", guess.validation, "\n")

