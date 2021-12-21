
# Generate a bootstrap sample of the most frequent sign for each referent
# mapper: The mapper used to assign signs to referents
# R: Number of bootstrap iterations
bootstrap <- function(df, mapper, filter = NA, R = 200){
    boots <- replicate(R, mapper(sample(df, replace = TRUE), filter = filter)[,"sign"])

    t(boots)
}

# For a given bootstrap sample (corresponding to a referent), return the signs for which confidence is at least conf 
getRefSigns <- function(boot.sample, conf.min = 0, conf.max = 1){
    tab <- table(boot.sample)
    sorted <- sort(tab/sum(tab), decreasing = T)

    pos1 <- match(sorted[sorted < conf.min][1], sorted) # position of the first item for which frequency < conf.min

    pos2 <-  match(sorted[sorted <= conf.max][1], sorted) # position of the first item for which frequency <= conf.max
    
    if(is.na(pos2)) return(NULL)
    else if(is.na(pos1)) return(tail(sorted, length(sorted) - pos2 + 1))
    else if(pos1 == 1) return(NULL)
    else if(pos1 > pos2) {
        v <- tail(sorted, length(sorted) - pos2 + 1)
        return(head(v, (pos1 - pos2)))
    }
    else return(NULL)
}

# Get the signs (as defined above) for all the referents, given their bootstrap samples for which confidence is at least conf 
getSigns <- function(boot.samples, conf.min = 0, conf.max = 1){
    apply(boot.samples, 2, getRefSigns, conf.min = conf.min, conf.max = conf.max)
}

# Turn the confidence score into a string
toStringFun <- function(conf, sep = ", ", digits = 2){
    paste(names(conf), format(conf, digits = digits), collapse = sep, sep =":")
}

# Present the signs and their confidence scores in a data frame of strings
flatten <- function(confs){
    as.vector(t(as.data.frame(lapply(confs, toStringFun))))
}

